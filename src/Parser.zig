const std = @import("std");
const mem = std.mem;
const root = @import("root");
const Token = root.Token;
const TokenKind = root.TokenKind;
const AST = root.AST;

const Parser = @This();
pub const Error = error{
    InvalidToken,
    UnexpectedEOF,
    InvalidExpression,
    InvalidOperator,
    InvalidNumber,
    InvalidString,
} || mem.Allocator.Error;

allocator: mem.Allocator,
tokens: []const Token,
index: usize = 0,

pub fn init(allocator: mem.Allocator, tokens: []const Token) Parser {
    return Parser{
        .allocator = allocator,
        .tokens = tokens,
    };
}

fn peek(self: *Parser) *const Token {
    if (self.is_at_end()) return self.tokens[self.tokens.len - 1];
    return self.tokens[self.index];
}

fn previous(self: *const Parser) *const Token {
    if (self.index == 0) @panic("Cannot go before 0.");
    return self.tokens[self.index - 1];
}

fn advance(self: *Parser) *const Token {
    if (self.is_at_end())
        return self.tokens[self.tokens.len - 1];
    self.index += 1;
    return self.tokens[self.index - 1];
}

fn is_at_end(self: *const Parser) bool {
    return self.index >= self.tokens.len;
}
