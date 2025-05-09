const std = @import("std");

const mem = std.mem;
const heap = std.heap;

pub const Analyzer = @import("./compiler/Analyzer.zig");
pub const AST = @import("./compiler/ast.zig");
pub const Lexer = @import("./compiler/Lexer.zig");
pub const Parser = @import("./compiler/Parser.zig");
pub const Report = @import("./compiler/reporter.zig");
pub const Symbol = @import("./compiler/Symbol.zig");
pub const Token = @import("./compiler/Token.zig");
pub const TokenKindTag = Token.TokenKindTag;
pub const TokenKind = Token.TokenKind;
pub const TokenList = Token.TokenList;
pub const Transpiler = @import("./compiler/Transpiler.zig");
pub const Type = @import("./compiler/Type.zig");
pub const TSAST = @import("./compiler/typesafe_ast.zig");
pub const Number = @import("./Number.zig").Number;
pub const termcolor = @import("./termcolor.zig");
pub const Module = @import("./compiler/module.zig");

pub const Compiler = struct {
    gpa: mem.Allocator,
    strings_arena: heap.ArenaAllocator,
    trees_arena: heap.ArenaAllocator,
    genmodules: std.StringArrayHashMap(Module),

    pub fn init(gpa: mem.Allocator) Compiler {
        const result = Compiler{
            .gpa = gpa,
            .strings_arena = heap.ArenaAllocator.init(gpa),
            .trees_arena = heap.ArenaAllocator.init(gpa),
            .genmodules = std.StringArrayHashMap(Module).init(gpa),
        };
        return result;
    }

    pub fn deinit(self: *Compiler) void {
        self.strings_arena.deinit();
        self.trees_arena.deinit();
        var iter = self.genmodules.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.genmodules.deinit();
    }

    /// Processes a `lune` file into these three steps `Lexical Analysis`, `Syntactic Analysis` and `Semantic Checking`.
    /// If all of these steps succeed it will add a new module to `Compiler.genmodules` where the key is `path`.
    pub fn process(self: *Compiler, path: []const u8) !void {
        const source = try self.read_file_to_slice(path);

        var lxr = try Lexer.init(self.gpa, self.strings_arena.allocator(), source, path);
        defer lxr.deinit();

        const tokens = try lxr.scan();

        var ast_arena = std.heap.ArenaAllocator.init(self.gpa);
        defer ast_arena.deinit();

        var parser = Parser.init(ast_arena.allocator(), tokens.items, source, path);
        const program_ast = try parser.parse();

        var analyzer = try Analyzer.init(self.trees_arena.allocator(), program_ast, source, path);
        const module = try analyzer.analyze();
        try self.genmodules.put(path, module);
    }

    // TODO:
    pub fn generate_to(self: *Compiler, out_path: []const u8) !void {
        _ = self;
        _ = out_path;
    }

    fn replaceLuneWithLua(self: *Compiler, path: []const u8) ![]const u8 {
        if (std.mem.endsWith(u8, path, ".lune")) {
            const base_length = path.len - 5;
            var new_path = try self.strings_arena.allocator().alloc(u8, base_length + 4);
            std.mem.copyForwards(u8, new_path[0..base_length], path[0..base_length]);
            std.mem.copyForwards(u8, new_path[base_length..], ".lua");
            return new_path;
        }

        return path;
    }

    fn read_file_to_slice(self: *Compiler, path: []const u8) ![]const u8 {
        const file = try std.fs.cwd().openFile(path, .{
            .mode = .read_only,
        });
        defer file.close();

        const size = try file.getEndPos();
        return try file.readToEndAlloc(self.strings_arena.allocator(), size);
    }
};
