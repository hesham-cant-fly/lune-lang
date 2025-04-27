const std = @import("std");
const Allocator = std.mem.Allocator;
const root = @import("root");
const Token = root.Token;
const AST = root.AST;

const String = std.ArrayList(u8);
const Transpiler = @This();

pub const Error = error{} || Allocator.Error;
pub const Target = enum { Lua };

ast: AST.Program,
allocator: Allocator,

pub fn init(allocator: Allocator, ast: AST.Program) Transpiler {
    return .{
        .allocator = allocator,
        .ast = ast,
    };
}

pub fn compile(self: *const Transpiler, target: Target) Error!String {
    return switch (target) {
        .Lua => self.compile_lua(),
    };
}

fn compile_lua(self: *const Transpiler) Error!String {
    var result = String.init(self.allocator);
    errdefer result.deinit();

    for (self.ast.body) |stmt| {
        try compile_lua_stmt(stmt, &result);
    }
    // try compile_lua_expr(self.ast, &result);

    return result;
}

fn compile_lua_stmt(stmt: AST.Stmt, res: *String) Error!void {
    switch (stmt.node.*) {
        .Var => |vr| {
            try res.appendSlice("local ");
            try res.appendSlice(vr.name.lexem);
            try res.appendSlice(" = ");
            if (vr.value) |value| {
                try compile_lua_expr(value, res);
                res.items.len -= 1;
            } else {
                try res.appendSlice("nil");
            }
        },
        .Const => |con| {
            try res.appendSlice("local ");
            try res.appendSlice(con.name.lexem);
            try res.appendSlice(" = ");
            if (con.value) |value| {
                try compile_lua_expr(value, res);
                res.items.len -= 1;
            } else {
                try res.appendSlice("nil");
            }
        },
        .Expr => |expr| {
            try compile_lua_expr(expr, res);
            res.items.len -= 1;
        },
    }
    try res.append('\n');
}

fn compile_lua_expr(expr: AST.Expr, res: *String) Error!void {
    switch (expr.node.*) {
        .Binray => |bin| {
            try compile_lua_expr(bin.lhs, res);
            try compile_lua_op(bin.op, res);
            try compile_lua_expr(bin.rhs, res);
        },
        .Unary => |unary| {
            try compile_lua_op(unary.op, res);
            try compile_lua_expr(unary.rhs, res);
        },
        .String => |str| {
            try res.appendSlice(str.lexem);
            try res.append(' ');
        },
        .Number => |num| {
            try res.appendSlice(num.lexem);
            try res.append(' ');
        },
        .Identifier => |id| {
            try res.appendSlice(id.lexem);
            try res.append(' ');
        },
        .Boolean => |b| {
            try res.appendSlice(b.lexem);
            try res.append(' ');
        },
        .Grouping => |group| {
            try res.append('(');
            try compile_lua_expr(group, res);
            try res.appendSlice(") ");
        },
        .Nil => {
            try res.appendSlice("nil ");
        },
    }
}

fn compile_lua_op(tok: Token, res: *String) Error!void {
    try res.appendSlice(switch (tok.kind) {
        .Plus => "+ ",
        .Minus => "- ",
        .Star => "* ",
        .FSlash => "// ",
        .Hat => "^ ",

        else => unreachable,
    });
}
