const std = @import("std");

const fmt = std.fmt;

pub const Number = union(enum) {
    Float: f64,
    Int: i64,

    pub fn print(self: Number) void {
        switch (self) {
            .Float => |f| std.debug.print("Float({})", .{f}),
            .Int => |f| std.debug.print("Int({})", .{f}),
        }
    }

    pub fn float_from_string(data: []const u8) fmt.ParseFloatError!Number {
        return Number{ .Float = try fmt.parseFloat(f64, data) };
    }

    pub fn int_from_string(data: []const u8) fmt.ParseIntError!Number {
        return Number{ .Int = try fmt.parseInt(i64, data, 10) };
    }
};
