// Hoisting and type checking

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const root = @import("root");
const TSAST = root.TSAST;
const AST = root.AST;
const Type = root.Type;
const Token = root.Token;
const Symbol = root.Symbol;
const SymbolTable = Symbol.SymbolTable;
const Scope = Symbol.Scope;
const Report = root.Report;

const Analyzer = @This();

const Error = error{
    UndefinedType,
    AnalyzerError,
    TypeMismatch,
} || Allocator.Error || Type.Error || SymbolTable.Error;

allocator: Allocator,
symbol_table: SymbolTable,
ast: AST.Program,
path: []const u8,
content: []const u8,
has_error: bool = false,

pub fn init(allocator: Allocator, ast: AST.Program, content: []const u8, path: []const u8) Allocator.Error!Analyzer {
    const symbol_table = try SymbolTable.init(allocator);

    return .{
        .allocator = allocator,
        .symbol_table = symbol_table,
        .ast = ast,
        .path = path,
        .content = content,
    };
}

pub fn deinit(self: *Analyzer) void {
    self.symbol_table.deinit();
}

pub fn analyze(self: *Analyzer) Error!TSAST.Program {
    var body = std.ArrayList(TSAST.Stmt).init(self.symbol_table.allocator);
    errdefer {
        for (body.items) |item| {
            item.deinit(self.allocator);
        }
        body.deinit();
    }

    for (self.ast.body) |stmt| {
        const item = self.analyze_stmt(stmt) catch |err| switch (err) {
            Error.OutOfMemory => return err,
            else => {
                self.has_error = true;
                continue;
            },
        };
        try body.append(item);
    }

    if (self.has_error) {
        return Error.AnalyzerError;
    }
    return TSAST.Program{
        .body = body,
    };
}

fn analyze_stmt(self: *Analyzer, stmt: AST.Stmt) Error!TSAST.Stmt {
    return switch (stmt.node.*) {
        .Var => |node| try self.analyze_var(node),
        .Const => |node| try self.analyze_const(node),
        .Expr => |expr| .{
            .Expr = (try self.analyze_expr(expr)).expr,
        },
    };
}

fn analyze_var(
    self: *Analyzer,
    node: AST.StmtNode.VarNode,
) Error!TSAST.Stmt {
    var tp = try Type.init_from(&self.symbol_table, node.tp);
    defer if (node.global) {
        self.symbol_table.declare_global(node.name, tp);
    } else {
        self.symbol_table.declare(node.name, tp);
    };

    if (node.global) {
        try self.symbol_table.define_global(node.name);
    } else {
        try self.symbol_table.define(node.name);
    }

    var expr: ?TSAST.Expr = null;
    errdefer if (expr) |e| e.deinit(self.allocator);
    if (node.value) |value| {
        expr = (try self.analyze_expr(value)).expr;
        if (node.global) {
            try self.symbol_table.assign_global(node.name.get_id_panic(), null);
        } else {
            try self.symbol_table.assign(node.name.get_id_panic(), null);
        }
        if (tp.is_auto()) { // Type inference if possible.
            tp = expr.?.get_type();
        } else if (!tp.can_assign(expr.?.get_type())) {
            self.report_error(
                "Type mismatch.",
                .{},
                node.name,
                .{
                    .msg = "Unmatched type.",
                    .len = node.name.len,
                    .column = node.name.column,
                },
            );
            return Error.TypeMismatch;
        }
    }

    return TSAST.Stmt{
        .Var = .{
            .name = node.name.kind.Identifier,
            .value = expr,
            .tp = tp,
            .global = node.global,
        },
    };
}

fn analyze_const(
    self: *Analyzer,
    node: AST.StmtNode.ConstNode,
) Error!TSAST.Stmt {
    var tp = try Type.init_from(&self.symbol_table, node.tp);
    defer if (node.global) {
        self.symbol_table.declare_global(node.name, tp);
    } else {
        self.symbol_table.declare(node.name, tp);
    };

    if (node.global) {
        try self.symbol_table.define_global_constant(node.name);
    } else {
        try self.symbol_table.define_constant(node.name);
    }

    var expr: ?TSAST.Expr = null;
    if (node.value) |value| {
        expr = (try self.analyze_expr(value)).expr;
        if (node.global) {
            try self.symbol_table.assign_global(node.name.get_id_panic(), null);
        } else {
            try self.symbol_table.assign(node.name.get_id_panic(), null);
        }
        if (tp.is_auto()) { // Infer the type if possible
            tp = expr.?.get_type();
        } else if (!tp.can_assign(expr.?.get_type())) {
            self.report_error(
                "Type mismatch.",
                .{},
                node.name,
                .{
                    .msg = "Unmatched type.",
                    .len = node.name.len,
                    .column = node.name.column,
                },
            );
            return Error.TypeMismatch;
        }
    }

    return TSAST.Stmt{
        .Const = .{
            .name = node.name.kind.Identifier,
            .value = expr,
            .tp = tp,
            .global = node.global,
        },
    };
}

const ExprResult = struct {
    expr: TSAST.Expr,
    tp: Type,
};

fn analyze_expr(
    self: *Analyzer,
    expr: AST.Expr,
) Error!ExprResult {
    return switch (expr.node.*) {
        .Binray => |bin| try self.analyze_binary(bin),
        .Unary => |un| try self.analyze_unary(un),
        .Grouping => |groupe| try self.analyze_expr(groupe),
        .String, .Number, .Boolean, .Nil => |tok| try self.analyze_constant_expr(tok),
        .Identifier => |tok| try self.analyze_id(tok),
    };
}

fn analyze_binary(
    self: *Analyzer,
    node: AST.ExprNode.BinaryNode,
) Error!ExprResult {
    const left = try self.analyze_expr(node.lhs);
    const right = try self.analyze_expr(node.rhs);

    const tp = left.tp.binary_op(node.op, right.tp) catch |err| switch (err) {
        Type.Error.ArithmaticOnNonNumberLeft => {
            self.report_error(
                "Arithmatic on non number.",
                .{},
                node.lhs.start,
                .{
                    .msg = "Excepected to be a number.",
                    .len = (node.lhs.end.column + node.lhs.end.len) - node.lhs.start.column,
                    .at = node.lhs.start.index,
                    .column = node.lhs.start.column,
                },
            );
            return err;
        },
        Type.Error.ArithmaticOnNonNumberRight => {
            self.report_error(
                "Arithmatic on non number.",
                .{},
                node.rhs.start,
                .{
                    .msg = "Excepected to be a number.",
                    .len = (node.rhs.end.column + node.rhs.end.len) - node.rhs.start.column,
                    .at = node.rhs.start.index,
                    .column = node.rhs.start.column,
                },
            );
            return err;
        },
        else => unreachable,
    };

    return ExprResult{
        .expr = TSAST.Expr{
            .Binary = .{
                .right = try TSAST.Expr.create(self.allocator, right.expr),
                .left = try TSAST.Expr.create(self.allocator, left.expr),
                .op = node.op.lexem,
                .tp = tp,
            },
        },
        .tp = tp,
    };
}

fn analyze_unary(
    self: *Analyzer,
    node: AST.ExprNode.UnaryNode,
) Error!ExprResult {
    const right = try self.analyze_expr(node.rhs);

    const tp = try right.tp.unary_op(node.op);

    return ExprResult{
        .expr = TSAST.Expr{
            .Unary = .{
                .op = node.op.lexem,
                .right = try TSAST.Expr.create(self.allocator, right.expr),
                .tp = tp,
            },
        },
        .tp = tp,
    };
}

fn analyze_constant_expr(
    self: *Analyzer,
    node: Token,
) Error!ExprResult {
    _ = self;
    switch (node.kind) {
        .StringLit => |v| return ExprResult{
            .expr = .{
                .String = .{
                    .v = v,
                    .tp = .{ .kind = .{ .Primitive = .String } },
                },
            },
            .tp = .{
                .kind = .{ .Primitive = .String },
            },
        },
        .NumberLit => return ExprResult{
            .expr = .{
                .Constant = .{
                    .v = node.lexem,
                    .tp = .{ .kind = .{ .Primitive = .Number } },
                },
            },
            .tp = .{
                .kind = .{ .Primitive = .Number },
            },
        },
        .BooleanLit => return ExprResult{
            .expr = .{
                .Constant = .{
                    .v = node.lexem,
                    .tp = .{ .kind = .{ .Primitive = .Boolean } },
                },
            },
            .tp = .{
                .kind = .{ .Primitive = .Boolean },
            },
        },
        .Nil => return ExprResult{
            .expr = .{
                .Constant = .{
                    .v = node.lexem,
                    .tp = .{ .kind = .{ .Primitive = .Nil } },
                },
            },
            .tp = .{
                .kind = .{ .Primitive = .Nil },
            },
        },
        else => @panic("Unimplemented"),
    }
}

fn analyze_id(
    self: *Analyzer,
    node: Token,
) Error!ExprResult {
    const tp = self.symbol_table.get_local_first(node.get_id_panic()) orelse return Error.UndefinedVariable;
    return ExprResult{
        .expr = .{
            .Constant = .{
                .v = node.lexem,
                .tp = tp.value_type,
            },
        },
        .tp = tp.value_type,
    };
}

fn report_error(
    self: *const Analyzer,
    comptime fmt: []const u8,
    args: anytype,
    tok: Token,
    caret: Report.Caret,
) void {
    Report.report_pro(
        self.content,
        self.path,
        caret,
        tok.index,
        tok.line,
        tok.column,
        .Error,
        fmt,
        args,
    );
}
