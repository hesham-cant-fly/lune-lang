const std = @import("std");
const lune = @import("lune");
const mem = std.mem;

const pretty = @import("pretty");
const clap = @import("clap");

const str = []const u8;

const main_params = clap.parseParamsComptime(
    \\-h, --help             Displays this help message
    \\-c, --compile <str>... Compile multiple files
    \\-o, --output <str>     Spicify the output folder
    \\
);

const my_parser = clap.parsers.default;

const MainArgs = clap.Result(clap.Help, &main_params, my_parser);

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_state.allocator();
    defer _ = gpa_state.deinit();

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &main_params, my_parser, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        clap.help(std.io.getStdErr().writer(), clap.Help, &main_params, .{}) catch {};
        return;
    }

    var compiler = lune.Compiler.init(gpa);
    defer compiler.deinit();

    const cwd = try std.fs.cwd().realpathAlloc(compiler.strings_arena.allocator(), ".");

    for (res.args.compile) |s| {
        const new_path = try std.fs.path.join(
            compiler.strings_arena.allocator(),
            &[_][]const u8{ cwd, s },
        );
        try compiler.process(new_path);
    }

    const outpath = if (res.args.output) |o|
        o
    else
        ".";

    try compiler.generate_to(outpath);
}
