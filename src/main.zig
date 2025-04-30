const std = @import("std");
const mem = std.mem;

const pretty = @import("pretty");

const Analyzer = @import("./compiler/Analyzer.zig");
pub const AST = @import("./compiler/ast.zig");
const Lexer = @import("./compiler/Lexer.zig");
const Parser = @import("./compiler/Parser.zig");
pub const Report = @import("./compiler/reporter.zig");
pub const Symbol = @import("./compiler/Symbol.zig");
pub const Token = @import("./compiler/Token.zig");
pub const TokenKindTag = Token.TokenKindTag;
pub const TokenKind = Token.TokenKind;
pub const TokenList = Token.TokenList;
const Transpiler = @import("./compiler/Transpiler.zig");
pub const Type = @import("./compiler/Type.zig");
pub const TSAST = @import("./compiler/typesafe_ast.zig");
pub const Number = @import("./Number.zig").Number;
pub const termcolor = @import("./termcolor.zig");

const str = []const u8;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const content = try read_file_to_slice(allocator, "main.lune");
    defer allocator.free(content);

    var lxr = try Lexer.init(allocator, content, "main.lune");
    errdefer lxr.deinit();

    const tokens = try lxr.scan();
    var parser = Parser.init(allocator, tokens.items, content, "main.lune");
    const ast = try parser.parse();
    lxr.deinit();
    defer ast.deinit(allocator);

    var analyzer = try Analyzer.init(allocator, ast, content, "main.lune");
    defer analyzer.deinit();

    const tsast = try analyzer.analyze();
    defer tsast.deinit(allocator);
    // try pretty.print(allocator, tsast, .{});
    const tr = Transpiler.init(allocator, tsast);
    const res = try tr.compile(.Lua);
    defer res.deinit();

    const out_file = try std.fs.cwd().createFile("./out.lua", .{
        .truncate = true,
    });
    defer out_file.close();

    std.debug.print("{s}\n", .{res.items});
    try out_file.writeAll(res.items);
}

pub fn read_file_to_slice(allocator: mem.Allocator, path: str) !str {
    const file = try std.fs.cwd().openFile(path, .{
        .mode = .read_only,
    });
    defer file.close();

    return try file.readToEndAlloc(allocator, 999999999999);
}
