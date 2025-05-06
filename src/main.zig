const std = @import("std");
const lune = @import("lune");
const mem = std.mem;

const pretty = @import("pretty");

const str = []const u8;

pub fn main() !void {
    if (std.os.argv.len == 1) {
        std.debug.print("needs some arguments\n", .{});
        return;
    }
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const file_name_zero = std.os.argv[1];
    var len: usize = 0;
    while (file_name_zero[len] != 0) : (len += 1) {}
    const file_name = file_name_zero[0..len];
    const content = try read_file_to_slice(allocator, file_name);
    defer allocator.free(content);

    var lxr = try lune.Lexer.init(allocator, content, file_name);
    errdefer lxr.deinit();

    const tokens = try lxr.scan();
    var parser = lune.Parser.init(arena.allocator(), tokens.items, content, file_name);
    const ast = try parser.parse();
    lxr.deinit();

    // try pretty.print(allocator, ast, .{
    //     .max_depth = 0,
    //     .tab_size = 4,
    //     .filter_field_names = .{
    //         .exclude = &.{ "start", "end" },
    //     },
    // });

    var analyzer = try lune.Analyzer.init(arena.allocator(), ast, content, file_name);
    defer analyzer.deinit();

    const tsast = try analyzer.analyze();
    var tr = lune.Transpiler.init(allocator, tsast);
    const res = try tr.compile(.Lua);
    defer res.deinit();

    // const out_file = try std.fs.cwd().createFile("./out.lua", .{
    //     .truncate = true,
    // });
    // defer out_file.close();

    // try out_file.writeAll(res.items);

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
