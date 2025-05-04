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
    ArithmaticOnNonNumberLeft,
    ArithmaticOnNonNumberRight,
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

        pub fn format(
            self: PrimitiveKind,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            try writer.print("{s}", .{
                switch (self) {
                    .Number => "number",
                    .String => "string",
                    .Boolean => "bool",
                    .Nil => "nil",
                },
            });
        }
    };
    Primitive: PrimitiveKind,
    Function: Callable,
    Optional: *const Type,
    Reference: *const Symbol,
    Auto,
    Any,
    Unknown,

    pub fn create(allocator: Allocator, value: Type) Allocator.Error!*Type {
        const result = try allocator.create(Type);
        result.* = value;
        return result;
    }

    pub fn format(
        self: TypeKind,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Primitive => |pri| try pri.format(fmt, options, writer),
            .Any => try writer.print("any", .{}),
            .Auto => try writer.print("auto", .{}),
            else => unreachable,
        }
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
            .Number => return .{ .kind = .{ .Primitive = .Number } },
            .String => return .{ .kind = .{ .Primitive = .String } },
            .Auto => return .{ .kind = .Auto },
            .Any => return .{ .kind = .Any },
        }
    } else {
        return Type{ .kind = .Auto };
    }
}

pub fn init_unkown() Type {
    return Type{
        .kind = .Unknown,
    };
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

pub fn is_auto(self: Type) bool {
    return switch (self.kind) {
        .Auto => true,
        .Reference => @panic("I don't think that you're really wants to check if a symbol is auto or something."),
        else => false,
    };
}

pub fn is_any(self: Type) bool {
    return switch (self.kind) {
        .Any => true,
        .Reference => |ref| ref.value_type.is_any(),
        else => false,
    };
}

pub fn format(
    self: Type,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    return self.kind.format(fmt, options, writer);
}

pub fn matches(self: Type, other: Type) bool {
    if (other.is_any()) return true;
    switch (self.kind) {
        .Primitive => |v| {
            if (other.kind != .Primitive) return false;
            return v == other.kind.Primitive;
        },
        .Optional => |v| {
            if (other.kind != .Optional) return false;
            return v.matches(other.kind.Optional.*);
        },
        .Reference => |sym| {
            return sym.value_type.matches(other);
        },
        .Function => @panic("tood"),
        .Auto, .Any => return true,
        .Unknown => @panic(""),
    }
}

pub inline fn can_assign(self: Type, other: Type) bool {
    return self.matches(other);
}

pub fn binary_op(
    self: Type,
    op: Token,
    other: Type,
) Error!Type {
    if (self.is_any() or other.is_any())
        return .{ .kind = .Any };
    switch (op.kind) {
        .Plus, .Minus, .Star, .FSlash, .Hat => {
            if (!self.is_number()) {
                return Error.ArithmaticOnNonNumberLeft;
            }
            if (!other.is_number()) {
                return Error.ArithmaticOnNonNumberRight;
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
    if (self.is_any()) return .{ .kind = .Any };
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
