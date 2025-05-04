const std = @import("std");
const root = @import("root");

const Number = root.Number;

const Token = @This();
pub const TokenList = std.ArrayList(Token);

pub const TokenKindTag = enum {
    Var, // 'var'
    Const, // 'const'
    Global, // 'global'
    Local, // 'local'

    Nil, // 'nil'

    As, // 'as'

    Number, // 'number'
    String, // 'string'
    Bool, // 'bool'
    Any, // 'any'
    Auto, // 'auto'

    Identifier,
    StringLit,
    NumberLit,
    BooleanLit,

    Plus, // '+'
    Minus, // '-'
    Star, // '*'
    FSlash, // '/'
    Hat, // '^'

    Eq, // '='

    QuestionMark, // '?'
    Colon, // ':'
    SemiColon, // ';'
    Comma, // ','

    OpenParen, // '('
    CloseParen, // ')'
};

pub const TokenKind = union(TokenKindTag) {
    Var, // 'var'
    Const, // 'const'
    Global, // 'global'
    Local, // 'local'

    Nil, // 'nil'

    As, // 'as'

    Number, // 'number'
    String, // 'string'
    Bool, // 'bool'
    Any, // 'any'
    Auto, // 'auto'

    Identifier: []const u8,
    StringLit: []const u8,
    NumberLit: Number,
    BooleanLit: bool,

    Plus, // '+'
    Minus, // '-'
    Star, // '*'
    FSlash, // '/'
    Hat, // '^'

    Eq, // '='

    QuestionMark, // '?'
    Colon, // ':'
    SemiColon, // ';'
    Comma, // ','

    OpenParen, // '('
    CloseParen, // ')'

    pub fn print(self: TokenKind) void {
        const sprint = std.debug.print;
        switch (self) {
            .Var => sprint("Var", .{}),
            .Const => sprint("Const", .{}),

            .Nil => sprint("Nil", .{}),

            .Identifier => |id| sprint("Identifer({s})", .{id}),
            .StringLit => |str| sprint("StringLit({s})", .{str}),
            .NumberLit => |num| num.print(),
            .BooleanLit => |v| sprint("Boolean({})", .{v}),

            .Plus => sprint("Plus", .{}),
            .Minus => sprint("Minus", .{}),
            .Star => sprint("Star", .{}),
            .FSlash => sprint("FSlash", .{}),
            .Hat => sprint("DoubleStar", .{}),

            .Eq => sprint("Eq", .{}),

            .QuestionMark => sprint("QuestionMark", .{}),
            .Colon => sprint("Colon", .{}),
            .SemiColon => sprint("SemiColon", .{}),

            .OpenParen => sprint("OpenParen", .{}),
            .CloseParen => sprint("CloseParen", .{}),
        }
    }
};

kind: TokenKind,
lexem: []const u8,
line: usize,
column: usize,
index: usize = 0,
len: usize = 0,

pub fn init(kind: TokenKind, lexem: []const u8, line: usize, column: usize) Token {
    return .{
        .kind = kind,
        .lexem = lexem,
        .line = line,
        .column = column,
    };
}

pub fn print(self: Token) void {
    std.debug.print("Token {c} kind: ", .{'{'});
    self.kind.print();
    std.debug.print(", lexem: '{s}' {c}", .{ self.lexem, '}' });
}

pub fn print_list(self: TokenList) void {
    std.debug.print("[\n", .{});
    for (self.items) |item| {
        std.debug.print("  ", .{});
        item.print();
        std.debug.print(",\n", .{});
    }
    std.debug.print("]\n", .{});
}

pub fn get_id(self: Token) ?[]const u8 {
    if (self.kind == .Identifier)
        return self.lexem;
    return null;
}

pub fn get_id_panic(self: Token) []const u8 {
    return self.get_id() orelse @panic("Expected to be identifier.");
}
