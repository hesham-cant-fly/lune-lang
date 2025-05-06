const std = @import("std");
const compiler = @import("lune");

const testing = std.testing;

const Token = compiler.Token;
const TokenKind = Token.TokenKind;

const Lexer = compiler.Lexer;

test "lexer: all tokens/syntax" {
    std.debug.print("here\n", .{});
    const source_code =
        \\local x = (20 + 10) * 12
        \\local var y: auto = 30 ^ 2 / -4;
        \\-- Comment
        \\global z: number = "40" as number
        \\const a: string = "huh"
        \\const huh: any = nil
        \\const heh = true
        \\do
        \\  const x: ?bool = false
        \\end
    ;
    var lexer = try Lexer.init(
        testing.allocator,
        source_code,
        "*a buffer*",
    );
    defer lexer.deinit();
    const tokens = try lexer.scan();
    // TODO: Actual testing :3
}
