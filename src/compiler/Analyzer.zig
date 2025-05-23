/// Hoisting and type checking
const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const lune = @import("lune");
const TSAST = lune.TSAST;
const AST = lune.AST;
const Type = lune.Type;
const Token = lune.Token;
const Symbol = lune.Symbol;
const SymbolTable = Symbol.SymbolTable;
const Scope = Symbol.Scope;
const Report = lune.Report;
const Module = lune.Module;

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

pub fn analyze(self: *Analyzer) Error!Module {
    // var body = std.ArrayList(TSAST.Stmt).init(self.symbol_table.allocator);
    // errdefer {
    //     for (body.items) |item| {
    //         item.deinit(self.allocator);
    //     }
    //     body.deinit();
    // }
    var body = TSAST.Block{};

    var body_iter = AST.BlockIter.init(&self.ast.body);
    while (body_iter.next()) |stmt| {
        const item = self.analyze_stmt(stmt.data) catch |err| switch (err) {
            Error.OutOfMemory => return err,
            else => {
                self.has_error = true;
                continue;
            },
        };
        const node = try self.allocator.create(TSAST.Block.Node);
        node.data = item;
        body.append(node);
    }

    if (self.has_error) {
        return Error.AnalyzerError;
    }
    return Module{
        .path = self.path,
        .exported_symbol = undefined,
        .tsast = .{
            .body = body,
        },
    };
}

fn analyze_stmt(self: *Analyzer, stmt: AST.Stmt) Error!TSAST.Stmt {
    return switch (stmt.node.*) {
        .Var => |node| try self.analyze_var(node),
        .Const => |node| try self.analyze_const(node),
        .DoEnd => |node| try self.analyze_doend(node),
        .Expr => |expr| .{
            .Expr = try self.analyze_expr(expr),
        },
    };
}

fn analyze_var(
    self: *Analyzer,
    node: AST.StmtNode.VarNode,
) Error!TSAST.Stmt {
    var tp = try Type.init_from(&self.symbol_table, node.tp);
    defer if (node.global) self.symbol_table.declare_global(node.name, tp) else self.symbol_table.declare(node.name, tp);

    if (node.global) {
        self.symbol_table.define_global(node.name) catch |err| switch (err) {
            SymbolTable.Error.RedefinitionOfVariable => {
                // TODO: Specifie where it is defined
                self.report_error(
                    "Redefinition of a variable.",
                    .{},
                    node.name,
                    .{
                        .msg = "This already exists.",
                        .column = node.name.column,
                    },
                );
                return err;
            },
            SymbolTable.Error.OutOfMemory => return err,
            else => unreachable,
        };
    } else {
        self.symbol_table.define(node.name) catch |err| switch (err) {
            SymbolTable.Error.RedefinitionOfVariable => {
                // TODO: Specifie where it is defined
                self.report_error(
                    "Redefinition of a variable.",
                    .{},
                    node.name,
                    .{
                        .msg = "This already exists.",
                        .column = node.name.column,
                    },
                );
                return err;
            },
            SymbolTable.Error.OutOfMemory => return err,
            else => unreachable,
        };
    }

    var expr: ?TSAST.Expr = null;
    // errdefer if (expr) |e| e.deinit(self.allocator);
    if (node.value) |value| {
        expr = try self.analyze_expr(value);
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
        expr = try self.analyze_expr(value);
        if (node.global) {
            try self.symbol_table.assign_global(node.name.get_id_panic(), null);
        } else {
            try self.symbol_table.assign(node.name.get_id_panic(), null);
        }
        if (tp.is_auto()) { // Infer the type if possible
            tp = expr.?.get_type();
        } else if (!tp.can_assign(expr.?.get_type())) {
            self.report_error(
                "can't assign a value of type `{s}` to `{s}`.",
                .{ tp, expr.?.get_type() },
                node.name,
                .{
                    .msg = "Unmatched type.",
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

fn analyze_doend(
    self: *Analyzer,
    stmt: AST.Block,
) Error!TSAST.Stmt {
    var block = TSAST.Block{};

    _ = try self.symbol_table.scope_start();
    defer self.symbol_table.scope_end();

    var block_iter = AST.BlockIter.init(&stmt);
    while (block_iter.next()) |node| {
        const st = try self.analyze_stmt(node.data);
        const nd = try self.allocator.create(TSAST.Block.Node);
        nd.data = st;
        block.append(nd);
    }

    return TSAST.Stmt{
        .DoEnd = block,
    };
}

fn analyze_expr(
    self: *Analyzer,
    expr: AST.Expr,
) Error!TSAST.Expr {
    return switch (expr.node.*) {
        .Binray => |bin| try self.analyze_binary(bin),
        .Unary => |un| try self.analyze_unary(un),
        .Grouping => |groupe| try self.analyze_expr(groupe),
        .Assign => |ass| try self.analyze_assignment(ass),
        .Cast => |cast| try self.analyze_cast(cast),
        .Call => |call| try self.analyze_call(call),
        .String, .Number, .Boolean, .Nil => |tok| try self.analyze_constant_expr(tok),
        .Identifier => |tok| try self.analyze_id(tok),
    };
}

fn analyze_binary(
    self: *Analyzer,
    node: AST.ExprNode.BinaryNode,
) Error!TSAST.Expr {
    const left = try self.analyze_expr(node.lhs);
    const right = try self.analyze_expr(node.rhs);

    const tp = left.get_type().binary_op(node.op, right.get_type()) catch |err| switch (err) {
        Type.Error.ArithmaticOnNonNumberLeft => {
            self.report_error(
                "Arithmatic on non number.",
                .{},
                node.lhs.start,
                .{
                    .msg = "Excepected to be a number.",
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
                    .column = node.rhs.start.column,
                },
            );
            return err;
        },
        else => unreachable,
    };

    return .{
        .Binary = .{
            .right = try TSAST.Expr.create(self.allocator, right),
            .left = try TSAST.Expr.create(self.allocator, left),
            .op = node.op.lexem,
            .tp = tp,
        },
    };
}

fn analyze_unary(
    self: *Analyzer,
    node: AST.ExprNode.UnaryNode,
) Error!TSAST.Expr {
    const right = try self.analyze_expr(node.rhs);

    const tp = try right.get_type().unary_op(node.op);

    if (node.op.kind == .Plus) {
        return right;
    } else {
        return TSAST.Expr{
            .Unary = .{
                .op = node.op.lexem,
                .right = try TSAST.Expr.create(self.allocator, right),
                .tp = tp,
            },
        };
    }
}

fn analyze_assignment(
    self: *Analyzer,
    node: AST.ExprNode.AssignNode,
) Error!TSAST.Expr {
    const vr = try self.analyze_assignable(node.vr);
    // errdefer vr.expr.deinit(self.allocator);

    const value = try self.analyze_expr(node.value);
    // errdefer value.deinit(self.allocator);

    const var_type = vr.symbol.value_type;
    const value_type = value.get_type();
    if (!var_type.can_assign(value_type)) {
        self.report_error(
            "Cannot assign value of type `{s}` to a value of type `{s}`.",
            .{ value_type, var_type },
            node.vr.start,
            .{
                .msg = "Mismatch type.",
                .column = node.value.start.column,
            },
        );
        return Error.TypeMismatch;
    }

    return TSAST.Expr{
        .Assign = .{
            .vr = try TSAST.Expr.create(self.allocator, vr.expr),
            .value = try TSAST.Expr.create(self.allocator, value),
            .tp = vr.expr.get_type(),
        },
    };
}

fn analyze_cast(
    self: *Analyzer,
    node: AST.ExprNode.CastNode,
) Error!TSAST.Expr {
    var value = try self.analyze_expr(node.value);
    // errdefer value.deinit(self.allocator);

    const tp = try Type.init_from(&self.symbol_table, node.tp);
    if (tp.is_auto()) {
        self.report_error(
            "Can't cast into `auto`.",
            .{},
            node.tp.start,
            .{
                .msg = "This is not a valid type for casting.",
                .column = node.tp.start.column,
            },
        );
        return Error.TypeMismatch;
    }
    value.set_type(tp);

    return value;
}

fn analyze_call(
    self: *Analyzer,
    node: AST.ExprNode.CallNode,
) Error!TSAST.Expr {
    const callee =
        self.analyze_expr(node.callee) catch |err| switch (err) {
        error.UndefinedVariable => {
            self.report_error(
                "Undefined identifier.",
                .{},
                node.callee.start,
                .{
                    .msg = "",
                    .column = node.callee.start.column,
                },
            );
            return err;
        },
        else => return err,
    };
    var args = std.DoublyLinkedList(TSAST.Expr){};

    const callee_type = callee.get_type();
    if (!callee_type.is_callable()) {
        self.report_error(
            "Cannot call a type of `{s}`.",
            .{callee.get_type()},
            node.callee.start,
            .{
                .msg = "Expected to be a callable type.",
                .column = node.callee.start.column,
            },
        );
        return Error.TypeMismatch;
    }

    const callee_args = callee_type.kind.Function.args;
    var i: usize = 0;

    // More type checking
    var current = node.args.first;
    var current_index: usize = 0;
    while (current) |nude| : (current_index += 1) { // Zig's fault
        current = nude.next;
        const expr = try self.analyze_expr(nude.data.expr);
        const current_arg = callee_args[i];

        switch (current_arg) {
            .Normal => |t| {
                if (!t.can_assign(expr.get_type())) {
                    self.report_error(
                        "the {d} argument of type `{s}` doesn't match `{s}`.",
                        .{ current_index, t, expr.get_type() },
                        node.callee.start,
                        .{
                            .msg = "",
                            .column = nude.data.expr.start.column,
                        },
                    );
                    return Error.TypeMismatch;
                }
                i += 1;
            },
            .Rest => |t| {
                if (!t.can_assign(expr.get_type())) {
                    self.report_error(
                        "the {d} argument of type `{s}` doesn't match `{s}`.",
                        .{ current_index, t, expr.get_type() },
                        node.callee.start,
                        .{
                            .msg = "",
                            .column = node.callee.start.column,
                        },
                    );
                    return Error.TypeMismatch;
                }
                i += 1;
            },
        }

        const nd = try self.allocator.create(std.DoublyLinkedList(TSAST.Expr).Node);
        nd.data = expr;
        args.append(nd);
    }

    return .{
        .Call = .{ .callee = try TSAST.Expr.create(self.allocator, callee), .args = args, .result_type = .{
            .kind = .Any,
        } },
    };
}

const Assignable = struct {
    symbol: *Symbol,
    expr: TSAST.Expr,
};
fn analyze_assignable(
    self: *Analyzer,
    node: AST.Expr,
) Error!Assignable {
    var symbol: *Symbol = undefined;
    var expr: TSAST.Expr = undefined;
    switch (node.node.*) {
        AST.ExprNode.Identifier => |id| {
            symbol = self.symbol_table.get_local_first(id.kind.Identifier) orelse {
                self.report_error(
                    "Undefined Variable '{s}'.",
                    .{id.kind.Identifier},
                    id,
                    .{
                        .msg = "",
                        .column = id.column,
                    },
                );
                return Error.UndefinedVariable;
            };
            expr = try self.analyze_expr(node);
        },
        else => {
            self.report_error(
                "You can only assign into a variable name.",
                .{},
                node.start,
                .{
                    .msg = "Expected to be a variable name.",
                    .column = node.start.column,
                },
            );
            return Error.AnalyzerError;
        },
    }

    symbol.assign(null) catch |err| switch (err) {
        error.AssignmentToType => {
            self.report_error(
                "Cannot assign into a type.",
                .{},
                node.start,
                .{
                    .msg = "here.",
                    .column = node.start.column,
                },
            );
            return err;
        },
        error.ReassignmentToConstant => {
            self.report_error(
                "Connot assign into a constant twice.",
                .{},
                node.start,
                .{
                    .msg = "Assignment accured here.",
                    .column = node.start.column,
                },
            );
            self.report_info(
                "The constant `{s}` is defined here.",
                .{symbol.name.lexem},
                symbol.name,
                .{
                    .msg = "Just here.",
                    .column = symbol.name.column,
                },
            );
            return err;
        },
        else => unreachable,
    };

    return .{
        .symbol = symbol,
        .expr = expr,
    };
}

fn analyze_constant_expr(
    self: *Analyzer,
    node: Token,
) Error!TSAST.Expr {
    _ = self;
    switch (node.kind) {
        .StringLit => |v| return .{
            .String = .{
                .v = v,
                .tp = .{ .kind = .{ .Primitive = .String } },
            },
        },
        .NumberLit => return .{
            .Constant = .{
                .v = node.lexem,
                .tp = .{ .kind = .{ .Primitive = .Number } },
            },
        },
        .BooleanLit => return .{
            .Constant = .{
                .v = node.lexem,
                .tp = .{ .kind = .{ .Primitive = .Bool } },
            },
        },
        .Nil => return .{
            .Constant = .{
                .v = node.lexem,
                .tp = .{ .kind = .{ .Primitive = .Nil } },
            },
        },
        else => @panic("Unimplemented"),
    }
}

fn analyze_id(
    self: *Analyzer,
    node: Token,
) Error!TSAST.Expr {
    const tp = self.symbol_table.get_local_first(node.get_id_panic()) orelse return Error.UndefinedVariable;
    return .{
        .Constant = .{
            .v = node.lexem,
            .tp = tp.value_type,
        },
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

fn report_info(
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
        .Info,
        fmt,
        args,
    );
}
