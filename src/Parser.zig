const std = @import("std");
const mem = std.mem;
const root = @import("root");
const Token = root.Token;
const TokenKindTag = root.TokenKindTag;
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

pub fn parse(self: *Parser) Error!AST.Program {
    const start = self.peek();
    var body = std.ArrayList(AST.Stmt).init(self.allocator);
    errdefer {
        for (body.items) |stmt| {
            stmt.deinit(self.allocator);
        }
        body.deinit();
    }

    while (!self.is_at_end()) {
        const stmt = try self.parse_stmt();
        errdefer stmt.deinit(self.allocator);

        try body.append(stmt);
    }

    body.shrinkAndFree(body.items.len);
    const end = self.previous();

    return .{
        .start = start,
        .end = end,
        .body = body.items,
    };
}

fn parse_stmt(self: *Parser) Error!AST.Stmt {
    const start = self.peek();

    const node = switch (self.advance().kind) {
        .Var => try self.parse_var_stmt(),
        .Const => try self.parse_const_stmt(),
        else => try self.parse_expr_stmt(),
    };

    const end = self.previous();

    return AST.Stmt.init(start, end, node);
}

fn parse_expr_stmt(self: *Parser) Error!*const AST.StmtNode {
    const expr = try self.parse_expr();
    errdefer expr.deinit(self.allocator);

    _ = try self.consume(.SemiColon, "Expected a semicolon ';' at the end of an expression statement.");

    return try AST.StmtNode.create(self.allocator, .{
        .Expr = expr,
    });
}

fn parse_var_stmt(self: *Parser) Error!*const AST.StmtNode {
    const name = try self.consume(.Identifier, "Expected a name.");
    const value = if (self.consume_optional(.Eq) != null)
        try self.parse_expr()
    else
        null;
    _ = try self.consume(.SemiColon, "Expected a semi colon ';' at the end of a variable declaration.");

    return try AST.StmtNode.create(self.allocator, .{
        .Var = .{
            .name = name,
            .value = value,
        },
    });
}

fn parse_const_stmt(self: *Parser) Error!*const AST.StmtNode {
    const name = try self.consume(.Identifier, "Expected a name.");
    // TODO: Better error report
    _ = try self.consume(.Eq, "Expected '=' since a constant declaration is a single assignment.");
    const value = try self.parse_expr();
    _ = try self.consume(.SemiColon, "Expected a semin colon ';' at the end of a constant declaration.");

    return try AST.StmtNode.create(self.allocator, .{
        .Const = .{
            .name = name,
            .value = value,
        },
    });
}

fn parse_expr(self: *Parser) Error!AST.Expr {
    return self.parse_term();
}

fn parse_term(self: *Parser) Error!AST.Expr {
    var lhs = try self.parse_factor();
    errdefer lhs.deinit(self.allocator);

    while (self.match(&[_]TokenKindTag{ .Plus, .Minus })) |op| {
        const rhs = try self.parse_factor();
        const node = try AST.ExprNode.create(self.allocator, .{
            .Binray = .{
                .lhs = lhs,
                .rhs = rhs,
                .op = op,
            },
        });
        lhs = AST.Expr.init(lhs.start, rhs.end, node);
    }

    return lhs;
}

fn parse_factor(self: *Parser) Error!AST.Expr {
    var lhs = try self.parse_power();
    errdefer lhs.deinit(self.allocator);

    while (self.match(&[_]TokenKindTag{ .Star, .FSlash })) |op| {
        const rhs = try self.parse_power();
        const node = try AST.ExprNode.create(self.allocator, .{
            .Binray = .{
                .lhs = lhs,
                .rhs = rhs,
                .op = op,
            },
        });
        lhs = AST.Expr.init(lhs.start, rhs.end, node);
    }

    return lhs;
}

fn parse_power(self: *Parser) Error!AST.Expr {
    var lhs = try self.parse_unary();
    errdefer lhs.deinit(self.allocator);

    while (self.match_one(.DoubleStar)) |op| {
        const rhs = try self.parse_unary();
        const node = try AST.ExprNode.create(self.allocator, .{
            .Binray = .{
                .lhs = lhs,
                .rhs = rhs,
                .op = op,
            },
        });
        lhs = AST.Expr.init(lhs.start, rhs.end, node);
    }

    return lhs;
}

fn parse_unary(self: *Parser) Error!AST.Expr {
    while (self.match(&[_]TokenKindTag{ .Minus, .Plus })) |op| {
        const rhs = try self.parse_unary();
        const node = try AST.ExprNode.create(self.allocator, .{
            .Unary = .{
                .rhs = rhs,
                .op = op,
            },
        });
        return AST.Expr.init(op, rhs.end, node);
    }

    return self.parse_primary();
}

fn parse_primary(self: *Parser) Error!AST.Expr {
    if (self.match_one(.Identifier)) |tok| {
        return AST.Expr.init(
            tok,
            tok,
            try AST.ExprNode.create(self.allocator, .{
                .Identifier = tok,
            }),
        );
    }

    if (self.match_one(.NumberLit)) |tok| {
        return AST.Expr.init(
            tok,
            tok,
            try AST.ExprNode.create(self.allocator, .{
                .Number = tok,
            }),
        );
    }

    if (self.match_one(.StringLit)) |tok| {
        return AST.Expr.init(
            tok,
            tok,
            try AST.ExprNode.create(self.allocator, .{
                .String = tok,
            }),
        );
    }

    if (self.match_one(.BooleanLit)) |tok| {
        return AST.Expr.init(
            tok,
            tok,
            try AST.ExprNode.create(self.allocator, .{
                .Boolean = tok,
            }),
        );
    }

    if (self.match_one(.Nil)) |tok| {
        return AST.Expr.init(
            tok,
            tok,
            try AST.ExprNode.create(self.allocator, .{
                .Nil = tok,
            }),
        );
    }

    if (self.match_one(.OpenParen)) |start| {
        const expr = try self.parse_expr();
        errdefer expr.deinit(self.allocator);

        const end = try self.consume(.CloseParen, "Exepected ')'.");
        return AST.Expr.init(
            start,
            end,
            try AST.ExprNode.create(self.allocator, .{
                .Grouping = expr,
            }),
        );
    }

    std.debug.print("Invalid Token: '{s}'\n", .{self.peek().lexem});
    return Error.InvalidToken;
}

fn consume_optional(self: *Parser, kind: TokenKind) ?Token {
    if (self.check(kind)) return self.advance();
    return null;
}

fn consume(self: *Parser, kind: TokenKindTag, msg: []const u8) Error!Token {
    if (self.check(kind))
        return self.advance();

    std.debug.print("{s}\n", .{msg});
    return error.InvalidToken;
}

fn check(self: *const Parser, kind: TokenKindTag) bool {
    return self.peek().kind == kind;
}

fn match_one(self: *Parser, kind: TokenKindTag) ?Token {
    if (self.check(kind))
        return self.advance();
    return null;
}

fn match(self: *Parser, kinds: []const TokenKindTag) ?Token {
    for (kinds) |kind| {
        if (self.check(kind))
            return self.advance();
    }
    return null;
}

fn peek(self: *const Parser) Token {
    if (self.is_at_end()) return self.tokens[self.tokens.len - 1];
    return self.tokens[self.index];
}

fn previous(self: *const Parser) Token {
    if (self.index == 0) @panic("Cannot go before 0.");
    return self.tokens[self.index - 1];
}

fn advance(self: *Parser) Token {
    if (self.is_at_end())
        return self.tokens[self.tokens.len - 1];
    self.index += 1;
    return self.tokens[self.index - 1];
}

fn is_at_end(self: *const Parser) bool {
    return self.index >= self.tokens.len;
}
