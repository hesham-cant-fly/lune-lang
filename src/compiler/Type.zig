const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const root = @import("root");
const Token = root.Token;
const AST = root.AST;

const Symbol = @import("./Symbol.zig");

const Type = @This();
pub const Error = error{
    UndefinedType,
    ArithmaticOnNonNumber,
} || Allocator.Error;

pub const Callable = struct {
    args: []const Arg,
    return_type: *const Type,

    pub const Arg = struct {
        tp: *const Type,
    };
};

// pub const TypeMeta = struct {
//     __add: ?Callable,
//     __sub: ?Callable,
//     __mul: ?Callable,
//     __div: ?Callable,
//     __mod: ?Callable,
//     __pow: ?Callable,
// };

pub const TypeKind = union(enum) {
    pub const PrimitiveKind = enum(u8) {
        Number,
        String,
        Boolean,
        Nil,
    };
    Primitive: PrimitiveKind,
    Function: Callable,
    Optional: *const Type,
    Reference: *const Symbol,
    Auto,

    pub fn create(allocator: Allocator, value: Type) Allocator.Error!*Type {
        const result = try allocator.create(Type);
        result.* = value;
        return result;
    }
};

kind: TypeKind,
// meta: ?*const TypeMeta = null,

pub fn init(kind: TypeKind) Type {
    return .{ .kind = kind };
}

pub fn init_from(table: *Symbol.SymbolTable, tp_node: ?AST.Type) Error!Type {
    if (tp_node) |tp_nd| {
        switch (tp_nd.node.*) {
            .Identifier => |id| {
                const tp = table.get_type(id.lexem);
                if (tp == null) return Error.UndefinedType;
                return Type{ .kind = .{ .Reference = tp.? } };
            },
            .Optional => |target_type| {
                const tp = try init_from(table, target_type);
                return Type{
                    .kind = .{ .Optional = try create(
                        table.allocator,
                        tp,
                    ) },
                };
            },
        }
    } else {
        return Type{ .kind = .Auto };
    }
}

pub fn create(allocator: Allocator, value: Type) Allocator.Error!*Type {
    const result = try allocator.create(Type);
    result.* = value;
    return result;
}

pub fn is_number(self: Type) bool {
    return switch (self.kind) {
        .Primitive => |pri| return pri == .Number,
        .Reference => |ref| ref.value_type.is_number(),
        else => false,
    };
}

pub fn binary_op(
    self: Type,
    op: Token,
    rhs: Type,
) Error!Type {
    switch (op.kind) {
        .Plus, .Minus, .Star, .FSlash, .Hat => {
            if (!self.is_number() or !rhs.is_number()) {
                return Error.ArithmaticOnNonNumber;
            }
            return Type{
                .kind = .{ .Primitive = .Number },
            };
        },

        else => @panic("Unimplemented"),
    }
}

pub fn unary_op(
    self: Type,
    op: Token,
) Error!Type {
    switch (op.kind) {
        .Plus, .Minus => {
            if (!self.is_number()) {
                return Error.ArithmaticOnNonNumber;
            }
            return Type{
                .kind = .{ .Primitive = .Number },
            };
        },
        else => @panic("Unimplemented"),
    }
}
