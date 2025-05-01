const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const root = @import("root");
const Number = root.Number;
const Token = root.Token;
const TokenList = root.TokenList;
const TokenKind = root.TokenKind;
const Reporter = root.Report;

const Lexer = @This();

pub const KEYWORDS = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "var", .Var },
    .{ "const", .Const },
    .{ "global", .Global },
    .{ "local", .Local },
    .{ "nil", .Nil },
    .{ "number", .Number },
    .{ "string", .String },
    .{ "any", .Any },
    .{ "auto", .Auto },
    .{ "true", TokenKind{ .BooleanLit = true } },
    .{ "false", TokenKind{ .BooleanLit = false } },
});

pub const Error = error{
    InvalidToken,
} || Allocator.Error || std.fmt.ParseFloatError || std.fmt.ParseIntError;

chars: unicode.Utf8Iterator,
path: []const u8,
tokens: TokenList,
start: usize = 0,
current: usize = 0,
current_len: usize = 0,
line: usize = 1,
column: usize = 0,
current_char: u21,
previous_char: u21,
has_error: bool = false,
deinited: bool = false,

pub fn init(allocator: Allocator, content: []const u8, path: []const u8) !Lexer {
    const x = try unicode.Utf8View.init(content);
    var iter = x.iterator();
    const current_char = iter.nextCodepoint() orelse 0;
    return Lexer{
        .chars = iter,
        .tokens = TokenList.init(allocator),
        .current_char = current_char,
        .previous_char = 0,
        .path = path,
    };
}

pub fn deinit(self: *Lexer) void {
    if (!self.deinited)
        self.tokens.deinit();
    self.deinited = true;
}

pub fn scan(self: *Lexer) Error!TokenList {
    self.start = 0;
    self.current = 0;

    while (!self.is_at_end()) {
        self.start = self.get_current();
        self.current_len = 0;
        defer self.column += self.current_len;

        self.scan_lexem() catch |err| {
            if (err != Error.InvalidToken) return err;
            self.report_error("Invalid character `{u}`.", .{
                self.previous(),
            }, .{
                .msg = "Unexpected Char",
                .column = self.column,
            });
            self.has_error = true;
        };
    }

    if (self.has_error)
        return Error.InvalidToken;
    return self.tokens;
}

fn scan_lexem(self: *Lexer) Error!void {
    const ch = self.advance();

    switch (ch) {
        ' ', '\t', '\r' => {},
        '\n' => {
            self.line += 1;
            self.column = 0;
        },

        '+' => try self.add_token(.Plus),
        '-' => {
            if (self.match('-')) {
                while (self.peek() != '\n' and !self.is_at_end())
                    _ = self.advance();
            } else {
                try self.add_token(.Minus);
            }
        },
        '*' => try self.add_token(.Star),
        '^' => try self.add_token(.Hat),
        '/' => try self.add_token(.FSlash),

        '=' => try self.add_token(.Eq),

        '?' => try self.add_token(.QuestionMark),
        ':' => try self.add_token(.Colon),
        ';' => try self.add_token(.SemiColon),

        '(' => try self.add_token(.OpenParen),
        ')' => try self.add_token(.CloseParen),

        '"' => try self.scan_string(),

        else => {
            if (is_numiric(ch)) {
                try self.scan_number();
            } else if (is_alpha(ch)) {
                try self.scan_identifier();
            } else {
                return Error.InvalidToken;
            }
        },
    }
}

fn scan_identifier(self: *Lexer) Error!void {
    while (is_alphanum(self.peek())) {
        _ = self.advance();
    }

    const lexem = self.get_lexem();

    const kind = KEYWORDS.get(lexem) orelse TokenKind{ .Identifier = lexem };

    try self.add_token(kind);
}

fn scan_number(self: *Lexer) Error!void {
    var is_float = false;
    while (is_numiric(self.peek())) {
        _ = self.advance();
    }

    if (self.match('.')) {
        is_float = true;
        while (is_numiric(self.peek())) {
            _ = self.advance();
        }
    }

    const lexem = self.get_lexem();
    const num = if (is_float) try Number.float_from_string(lexem) else try Number.int_from_string(lexem);

    try self.add_token(TokenKind{ .NumberLit = num });
}

fn scan_string(self: *Lexer) Error!void {
    _ = self.advance();
    while (self.peek() != '"') {
        _ = self.advance();
    }
    _ = self.advance();

    const xs = self.get_content()[self.start + 1 .. self.get_current() - 1];
    const kind = TokenKind{ .StringLit = xs };
    try self.add_token(kind);
}

fn add_token(self: *Lexer, kind: TokenKind) Error!void {
    const lexem = self.get_lexem();
    var token = Token.init(
        kind,
        lexem,
        self.line,
        self.column,
    );
    token.index = self.start;
    token.len = self.current_len;
    try self.tokens.append(token);
}

fn match(self: *Lexer, ch: u21) bool {
    if (self.peek() == ch) {
        _ = self.advance();
        return true;
    }
    return false;
}

fn advance(self: *Lexer) u21 {
    if (self.is_at_end()) return 0;
    const prev = self.current_char;
    self.current_char = self.chars.nextCodepoint() orelse 0;
    self.previous_char = prev;

    var code_point: [4]u8 = undefined;
    const size = unicode.utf8Encode(self.current_char, &code_point) catch unreachable;
    self.current += size;

    self.current_len += 1;

    return prev;
}

inline fn peek(self: *const Lexer) u21 {
    return self.current_char;
}

inline fn previous(self: *const Lexer) u21 {
    return self.previous_char;
}

fn is_at_end(self: *const Lexer) bool {
    return self.get_current() >= self.get_content().len;
}

fn get_lexem(self: *const Lexer) []const u8 {
    return self.get_content()[self.start..self.get_current()];
}

inline fn get_current(self: *const Lexer) usize {
    return self.current;
}

inline fn get_content(self: *const Lexer) []const u8 {
    return self.chars.bytes;
}

fn report_error(
    self: *const Lexer,
    comptime fmt: []const u8,
    args: anytype,
    carets: Reporter.Caret,
) void {
    Reporter.report_pro(
        self.get_content(),
        self.path,
        carets,
        self.current,
        self.line,
        self.column,
        .Error,
        fmt,
        args,
    );
}

fn is_numiric(ch: u21) bool {
    return ch >= '0' and ch <= '9';
}

fn is_alpha(ch: u21) bool {
    return (ch >= 'a' and ch <= 'z') or
        (ch >= 'A' and ch <= 'Z') or
        ch == '_';
}

fn is_alphanum(ch: u21) bool {
    return is_numiric(ch) or is_alpha(ch);
}
