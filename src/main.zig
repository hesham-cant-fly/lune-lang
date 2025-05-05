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

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const content = try read_file_to_slice(allocator, "main.lune");
    defer allocator.free(content);

    var lxr = try Lexer.init(allocator, content, "main.lune");
    errdefer lxr.deinit();

    const tokens = try lxr.scan();
    var parser = Parser.init(arena.allocator(), tokens.items, content, "main.lune");
    const ast = try parser.parse();
    lxr.deinit();

    // try pretty.print(allocator, ast, .{
    //     .max_depth = 0,
    //     .tab_size = 4,
    //     .filter_field_names = .{
    //         .exclude = &.{ "start", "end" },
    //     },
    // });

    var analyzer = try Analyzer.init(arena.allocator(), ast, content, "main.lune");
    defer analyzer.deinit();

    const tsast = try analyzer.analyze();
    const tr = Transpiler.init(allocator, tsast);
    const res = try tr.compile(.Lua);
    defer res.deinit();

    const out_file = try std.fs.cwd().createFile("./out.lua", .{
        .truncate = true,
    });
    defer out_file.close();

    try out_file.writeAll(res.items);

    // std.time.sleep(std.time.ns_per_s * 30);
    std.debug.print("{s}\n", .{res.items});
}

pub fn read_file_to_slice(allocator: mem.Allocator, path: str) !str {
    const file = try std.fs.cwd().openFile(path, .{
        .mode = .read_only,
    });
    defer file.close();

    const size = try file.getEndPos();
    return try file.readToEndAlloc(allocator, size);
}
