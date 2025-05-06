const std = @import("std");
const testing = std.testing;

const lexer_test = @import("./lexer.zig");

test {
    _ = lexer_test;
    try testing.expectEqual("heh", "heh");
}
