const std = @import("std");

const pretty = @import("pretty");

const mem = std.mem;

pub const AST = @import("./ast.zig");
const Lexer = @import("./Lexer.zig");
const Parser = @import("./Parser.zig");
const Transpiler = @import("./Transpiler.zig");
pub const Number = @import("./Number.zig").Number;
pub const Token = @import("./Token.zig");
pub const TokenKindTag = Token.TokenKindTag;
pub const TokenKind = Token.TokenKind;
pub const TokenList = Token.TokenList;

const str = []const u8;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const content = try read_file_to_slice(allocator, "main.lune");
    defer allocator.free(content);

    var lxr = try Lexer.init(allocator, content);
    errdefer lxr.deinit();

    const tokens = try lxr.scan();
    var parser = Parser.init(allocator, tokens.items);
    const ast = try parser.parse();
    lxr.deinit();
    defer ast.deinit(allocator);

    // try pretty.print(allocator, ast, .{});
    const tr = Transpiler.init(allocator, ast);
    const res = try tr.compile(.Lua);
    defer res.deinit();

    const out_file = try std.fs.cwd().createFile("./out.lua", .{
        .truncate = true,
    });
    defer out_file.close();

    std.debug.print("{s}\n", .{res.items});
    try out_file.writeAll(res.items);
}

fn read_file_to_slice(allocator: mem.Allocator, path: str) !str {
    const file = try std.fs.cwd().openFile(path, .{
        .mode = .read_only,
    });
    defer file.close();

    return try file.readToEndAlloc(allocator, 999999999999);
}
