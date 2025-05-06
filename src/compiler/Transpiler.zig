const std = @import("std");
const Allocator = std.mem.Allocator;
const lune = @import("lune");
const Token = lune.Token;
const TSAST = lune.TSAST;

const String = std.ArrayList(u8);
const Transpiler = @This();

pub const Error = error{} || Allocator.Error;
pub const Target = enum { Lua };

ast: TSAST.Program,
allocator: Allocator,
indent_len: usize = 2,
indent: usize = 0,

pub fn init(allocator: Allocator, ast: TSAST.Program) Transpiler {
    return .{
        .allocator = allocator,
        .ast = ast,
    };
}

pub fn compile(self: *Transpiler, target: Target) Error!String {
    return switch (target) {
        .Lua => self.compile_lua(),
    };
}

fn compile_lua(self: *Transpiler) Error!String {
    var result = String.init(self.allocator);
    errdefer result.deinit();

    var body_iter = TSAST.BlockIter.init(&self.ast.body);
    while (body_iter.next()) |stmt| {
        try self.compile_lua_stmt(stmt.data, &result);
    }
    // try compile_lua_expr(self.ast, &result);

    return result;
}

fn compile_lua_stmt(self: *Transpiler, stmt: TSAST.Stmt, res: *String) Error!void {
    try self.append_indent(res);
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
        .DoEnd => |block| {
            try res.appendSlice("do\n");
            self.indent += self.indent_len;
            var block_iter = TSAST.BlockIter.init(&block);
            while (block_iter.next()) |st| {
                try self.compile_lua_stmt(st.data, res);
            }
            self.indent -= self.indent_len;
            try self.append_indent(res);
            try res.appendSlice("end");
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
        .Call => |call| {
            try compile_lua_expr(call.callee.*, res, false);
            res.items[res.items.len - 1] = '(';
            var current = call.args.first;
            while (current) |c| {
                current = c.next;
                try compile_lua_expr(c.data, res, false);
                res.items[res.items.len - 1] = ',';
                try res.append(' ');
            }
            res.items[res.items.len - 2] = ')';
            // try res.appendSlice(") ");
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

fn append_indent(self: *Transpiler, res: *String) Error!void {
    if (self.indent == 0) return;
    try res.ensureTotalCapacity(res.items.len + self.indent);
    @memset(res.items.ptr[res.items.len .. res.items.len + self.indent], ' ');
    res.items.len += self.indent;
}
