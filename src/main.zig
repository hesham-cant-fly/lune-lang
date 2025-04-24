const std = @import("std");

const pretty = @import("pretty");

pub const AST = @import("./ast.zig");
const Lexer = @import("./Lexer.zig");
pub const Number = @import("./Number.zig").Number;
pub const Token = @import("./Token.zig");
pub const TokenKind = Token.TokenKind;
pub const TokenList = Token.TokenList;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const content = "123";
    var lxr = try Lexer.init(allocator, content);
    defer lxr.deinit();
    const tokens = try lxr.scan();

    try pretty.print(allocator, tokens.items, .{});
}
