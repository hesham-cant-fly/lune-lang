const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const root = @import("root");
const Token = root.Token;

pub const Program = struct {
    start: Token,
    end: Token,
    body: []const Stmt,

    pub fn deinit(self: Program, allocator: Allocator) void {
        for (self.body) |stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.body);
    }
};

pub const StmtNode = union(enum) {
    Var: struct {
        name: Token,
        value: ?Expr,
    },
    Const: struct {
        name: Token,
        value: Expr,
    },
    Expr: Expr,

    pub fn create(allocator: Allocator, value: StmtNode) Allocator.Error!*StmtNode {
        const result = try allocator.create(StmtNode);
        result.* = value;
        return result;
    }
};

pub const Stmt = struct {
    start: Token,
    end: Token,
    node: *const StmtNode,

    pub fn init(start: Token, end: Token, stmt: *const StmtNode) Stmt {
        return .{
            .start = start,
            .end = end,
            .node = stmt,
        };
    }

    pub fn deinit(self: Stmt, allocator: Allocator) void {
        switch (self.node.*) {
            StmtNode.Var => |vr| {
                if (vr.value) |value| {
                    value.deinit(allocator);
                }
            },
            StmtNode.Const => |cons| {
                cons.value.deinit(allocator);
            },
            StmtNode.Expr => |expr| {
                expr.deinit(allocator);
            },
        }
        allocator.destroy(self.node);
    }
};

pub const ExprNode = union(enum) {
    Binray: struct {
        lhs: Expr,
        rhs: Expr,
        op: Token,
    },
    Unary: struct {
        rhs: Expr,
        op: Token,
    },
    String: Token,
    Number: Token,
    Identifier: Token,
    Boolean: Token,
    Grouping: Expr,

    pub fn create(allocator: Allocator, value: ExprNode) Allocator.Error!*ExprNode {
        const result = try allocator.create(ExprNode);
        result.* = value;
        return result;
    }
};

pub const Expr = struct {
    start: Token,
    end: Token,
    node: *const ExprNode,

    pub fn init(start: Token, end: Token, expr: *const ExprNode) Expr {
        return .{
            .start = start,
            .end = end,
            .node = expr,
        };
    }

    pub fn deinit(self: Expr, allocator: Allocator) void {
        switch (self.node.*) {
            ExprNode.Binray => |bin| {
                bin.lhs.deinit(allocator);
                bin.rhs.deinit(allocator);
            },
            ExprNode.Unary => |unary| {
                unary.rhs.deinit(allocator);
            },
            ExprNode.String, ExprNode.Number, ExprNode.Identifier, ExprNode.Boolean => {},
            ExprNode.Grouping => |expr| {
                expr.deinit(allocator);
            },
        }
        allocator.destroy(self.node);
    }
};
