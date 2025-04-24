const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const root = @import("root");
const Token = root.Token;

pub const ExprNode = union(enum) {
    Binray: struct {
        lhs: *const Expr,
        op: *const Token,
        rhs: *const Expr,
    },
    Unary: struct {
        op: *const Token,
        rhs: *const Expr,
    },
    String: *const Token,
    Number: *const Token,
};

pub const Expr = struct {
    start: *const Token,
    end: *const Token,
    node: *const ExprNode,

    pub fn create(
        allocator: Allocator,
        start: *const Token,
        end: *const Token,
        expr: *const Expr,
    ) Allocator.Error!*Expr {
        const result = try allocator.create(Expr);
        result.* = .{ .start = start, .end = end, .expr = expr };
        return result;
    }

    pub fn deinit(self: *Expr, allocator: Allocator) void {
        switch (self.node) {
            ExprNode.Binray => |bin| {
                bin.lhs.deinit(allocator);
                bin.rhs.deinit(allocator);
            },
            ExprNode.Unary => |unary| {
                unary.rhs.deinit(allocator);
            },
            ExprNode.String => |str| {
                str.deinit(allocator);
            },
            ExprNode.Number => |num| {
                num.deinit(allocator);
            },
        }
        allocator.destroy(self.node);
    }
};
