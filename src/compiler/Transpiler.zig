const std = @import("std");
const Allocator = std.mem.Allocator;
const root = @import("root");
const Token = root.Token;
const TSAST = root.TSAST;

const String = std.ArrayList(u8);
const Transpiler = @This();

pub const Error = error{} || Allocator.Error;
pub const Target = enum { Lua };

ast: TSAST.Program,
allocator: Allocator,

pub fn init(allocator: Allocator, ast: TSAST.Program) Transpiler {
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

    for (self.ast.body.items) |stmt| {
        try compile_lua_stmt(stmt, &result);
    }
    // try compile_lua_expr(self.ast, &result);

    return result;
}

fn compile_lua_stmt(stmt: TSAST.Stmt, res: *String) Error!void {
    switch (stmt) {
        .Var => |vr| {
            if (vr.global) {
                try res.appendSlice("_G.");
            } else {
                try res.appendSlice("local ");
            }
            try res.appendSlice(vr.name);
            try res.appendSlice(" = ");
            if (vr.value) |value| {
                try compile_lua_expr(value, res, false);
                res.items.len -= 1;
            } else {
                try res.appendSlice("nil");
            }
        },
        .Const => |con| {
            if (con.global) {
                try res.appendSlice("_G.");
            } else {
                try res.appendSlice("local ");
            }
            try res.appendSlice(con.name);
            try res.appendSlice(" = ");
            if (con.value) |value| {
                try compile_lua_expr(value, res, false);
                res.items.len -= 1;
            } else {
                try res.appendSlice("nil");
            }
        },
        .Expr => |expr| {
            try compile_lua_expr(expr, res, true);
            res.items.len -= 1;
        },
    }
    try res.append('\n');
}

fn compile_lua_expr(expr: TSAST.Expr, res: *String, is_stmt: bool) Error!void {
    switch (expr) {
        .Binary => |bin| {
            if (is_stmt) {
                try res.appendSlice("_G._ = ");
            }
            try compile_lua_expr(bin.left.*, res, false);
            try res.appendSlice(bin.op);
            try res.append(' ');
            try compile_lua_expr(bin.right.*, res, false);
        },
        .Unary => |un| {
            if (is_stmt) {
                try res.appendSlice("_G._ = ");
            }
            try res.appendSlice(un.op);
            try res.append(' ');
            try compile_lua_expr(un.right.*, res, false);
        },
        .Assign => |ass| {
            try compile_lua_expr(ass.vr.*, res, false);
            try res.appendSlice("= ");
            try compile_lua_expr(ass.value.*, res, false);
        },
        .String => |str| {
            if (is_stmt) {
                try res.appendSlice("_G._ = ");
            }
            try res.append('"');
            try res.appendSlice(str.v);
            try res.appendSlice("\" ");
        },
        .Constant => |con| {
            if (is_stmt) {
                try res.appendSlice("_G._ = ");
            }
            try res.appendSlice(con.v);
            try res.append(' ');
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
