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
    pub const VarNode = struct {
        // TODO: Names list
        name: Token,
        tp: ?Type,
        value: ?Expr,
        global: bool = false,
    };
    pub const ConstNode = struct {
        // TODO: Names list
        name: Token,
        tp: ?Type,
        value: ?Expr,
        global: bool = false,
    };
    Var: VarNode,
    Const: ConstNode,
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
                if (vr.tp) |tp| {
                    tp.deinit(allocator);
                }
                if (vr.value) |value| {
                    value.deinit(allocator);
                }
            },
            StmtNode.Const => |cons| {
                if (cons.tp) |tp| {
                    tp.deinit(allocator);
                }
                if (cons.value) |value| {
                    value.deinit(allocator);
                }
            },
            StmtNode.Expr => |expr| expr.deinit(allocator),
        }
        allocator.destroy(self.node);
    }
};

pub const TypeNode = union(enum) {
    Optional: Type,
    Identifier: Token,
    Number,
    String,
    Bool,
    Auto,
    Any,

    pub fn create(allocator: Allocator, value: TypeNode) Allocator.Error!*TypeNode {
        const result = try allocator.create(TypeNode);
        result.* = value;
        return result;
    }
};

pub const Type = struct {
    start: Token,
    end: Token,
    node: *const TypeNode,

    pub fn init(start: Token, end: Token, node: *const Type) Type {
        return .{
            .start = start,
            .end = end,
            .node = node,
        };
    }

    pub fn deinit(self: Type, allocator: Allocator) void {
        switch (self.node.*) {
            .Optional => |opt| {
                opt.deinit(allocator);
            },
            .Identifier, .Number, .String, .Bool, .Auto, .Any => {},
        }

        allocator.destroy(self.node);
    }
};

pub const ExprNode = union(enum) {
    pub const BinaryNode = struct {
        lhs: Expr,
        rhs: Expr,
        op: Token,
    };
    pub const UnaryNode = struct {
        rhs: Expr,
        op: Token,
    };
    pub const AssignNode = struct {
        vr: Expr,
        value: Expr,
    };
    pub const CastNode = struct {
        value: Expr,
        tp: Type,
    };
    Grouping: Expr,
    Binray: BinaryNode,
    Unary: UnaryNode,
    Assign: AssignNode,
    Cast: CastNode,
    String: Token,
    Number: Token,
    Identifier: Token,
    Boolean: Token,
    Nil: Token,

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
            ExprNode.Grouping => |expr| {
                expr.deinit(allocator);
            },
            ExprNode.Binray => |bin| {
                bin.lhs.deinit(allocator);
                bin.rhs.deinit(allocator);
            },
            ExprNode.Unary => |unary| {
                unary.rhs.deinit(allocator);
            },
            .Assign => |as| {
                as.vr.deinit(allocator);
                as.value.deinit(allocator);
            },
            .Cast => |cast| {
                cast.tp.deinit(allocator);
                cast.value.deinit(allocator);
            },
            .String, .Number, .Identifier, .Boolean, .Nil => {},
        }
        allocator.destroy(self.node);
    }
};
