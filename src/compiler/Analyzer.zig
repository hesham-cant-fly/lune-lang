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

const Analyzer = @This();

const Error = error{
    UndefinedType,
} || Allocator.Error || Type.Error || SymbolTable.Error;

allocator: Allocator,
symbol_table: SymbolTable,
ast: AST.Program,

pub fn init(allocator: Allocator, ast: AST.Program) Allocator.Error!Analyzer {
    const symbol_table = try SymbolTable.init(allocator);

    return .{
        .allocator = allocator,
        .symbol_table = symbol_table,
        .ast = ast,
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
        switch (stmt.node.*) {
            .Var => |node| try body.append(
                try self.analyze_var(node),
            ),
            .Const => |node| try body.append(
                try self.analyze_const(node),
            ),
            .Expr => |expr| try body.append(.{
                .Expr = (try self.analyze_expr(expr)).expr,
            }),
        }
    }

    return TSAST.Program{
        .body = body,
        // .functions = undefined,
    };
}

fn analyze_var(
    self: *Analyzer,
    node: AST.StmtNode.VarNode,
) Error!TSAST.Stmt {
    const tp = try Type.init_from(&self.symbol_table, node.tp);

    try self.symbol_table.define(node.name, tp);

    var expr: ?TSAST.Expr = null;
    errdefer if (expr) |e| e.deinit(self.allocator);
    if (node.value) |value| {
        expr = (try self.analyze_expr(value)).expr;
        try self.symbol_table.assign(node.name.get_id_panic(), null);
    }

    self.symbol_table.declare(node.name);

    return TSAST.Stmt{
        .Var = .{
            .name = node.name.kind.Identifier,
            .value = expr,
        },
    };
}

fn analyze_const(
    self: *Analyzer,
    node: AST.StmtNode.ConstNode,
) Error!TSAST.Stmt {
    const tp = try Type.init_from(&self.symbol_table, node.tp);

    try self.symbol_table.define_constant(node.name, tp);

    var expr: ?TSAST.Expr = null;
    if (node.value) |value| {
        expr = (try self.analyze_expr(value)).expr;
        try self.symbol_table.assign(node.name.get_id_panic(), null);
    }

    self.symbol_table.declare(node.name);

    return TSAST.Stmt{
        .Var = .{
            .name = node.name.kind.Identifier,
            .value = expr,
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

    const tp = try left.tp.binary_op(node.op, right.tp);

    return ExprResult{
        .expr = TSAST.Expr{
            .Binary = .{
                .right = try TSAST.Expr.create(self.allocator, right.expr),
                .left = try TSAST.Expr.create(self.allocator, left.expr),
                .op = node.op.lexem,
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
            .expr = .{ .Constant = v },
            .tp = .{
                .kind = .{ .Primitive = .String },
            },
        },
        .NumberLit => return ExprResult{
            .expr = .{
                .Constant = node.lexem,
            },
            .tp = .{
                .kind = .{ .Primitive = .Number },
            },
        },
        .BooleanLit => return ExprResult{
            .expr = .{
                .Constant = node.lexem,
            },
            .tp = .{
                .kind = .{ .Primitive = .Boolean },
            },
        },
        .Nil => return ExprResult{
            .expr = .{
                .Constant = node.lexem,
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
        .expr = .{ .Constant = node.lexem },
        .tp = .{
            .kind = .{ .Reference = tp },
        },
    };
}
