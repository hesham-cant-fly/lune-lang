const std = @import("std");
const mem = std.mem;
const root = @import("root");
const Token = root.Token;
const TokenKindTag = root.TokenKindTag;
const TokenKind = root.TokenKind;
const AST = root.AST;
const Report = root.Report;

const Parser = @This();
pub const Error = error{
    InvalidToken,
    UnexpectedEOF,
    InvalidExpression,
    InvalidOperator,
    InvalidNumber,
    InvalidString,
    ParsingError,
} || mem.Allocator.Error;

allocator: mem.Allocator,
path: []const u8,
content: []const u8,
tokens: []const Token,
index: usize = 0,
has_error: bool = false,

pub fn init(allocator: mem.Allocator, tokens: []const Token, content: []const u8, path: []const u8) Parser {
    return Parser{
        .allocator = allocator,
        .path = path,
        .content = content,
        .tokens = tokens,
    };
}

pub fn parse(self: *Parser) Error!AST.Program {
    const start = self.peek();
    var body = AST.Block{};

    while (!self.is_at_end()) {
        if (self.match_one(.SemiColon)) |_| { // Ignoring semicolons
            continue;
        }
        const stmt = self.parse_stmt() catch |err| {
            if (err == Error.OutOfMemory)
                return err;
            self.sync();
            self.has_error = true;
            continue;
        };
        errdefer stmt.deinit(self.allocator);

        const node = try self.allocator.create(AST.Block.Node);
        node.data = stmt;
        body.append(node);
    }

    const end = self.previous();

    if (self.has_error) {
        return Error.ParsingError;
    }

    return .{
        .start = start,
        .end = end,
        .body = body,
    };
}

fn parse_stmt(self: *Parser) Error!AST.Stmt {
    const start = self.peek();

    const node = switch (self.advance().kind) {
        .Var => try self.parse_var_stmt(),
        .Const => try self.parse_const_stmt(),
        .Global => try self.parse_global(),
        .Local => try self.parse_local(),
        else => res: {
            self.index -= 1;
            break :res try self.parse_assign_stmt();
        },
    };

    const end = self.previous();

    return AST.Stmt.init(start, end, node);
}

fn parse_expr_stmt(self: *Parser) Error!*const AST.StmtNode {
    const expr = try self.parse_expr();
    errdefer expr.deinit(self.allocator);

    return try AST.StmtNode.create(self.allocator, .{
        .Expr = expr,
    });
}

fn parse_assign_stmt(self: *Parser) Error!*const AST.StmtNode {
    const ass = try self.parse_assign();
    errdefer ass.deinit(self.allocator);

    return try AST.StmtNode.create(self.allocator, .{
        .Expr = ass,
    });
}

fn parse_var_stmt(self: *Parser) Error!*AST.StmtNode {
    const name = try self.consume(.Identifier, "Expected a name.");
    const tp = if (self.match_one(.Colon)) |_|
        try self.parse_type()
    else
        null;
    errdefer if (tp) |t| t.deinit(self.allocator);
    const value = if (self.match_one(.Eq)) |_|
        try self.parse_expr()
    else
        null;
    errdefer if (value) |v| v.deinit(self.allocator);

    return try AST.StmtNode.create(self.allocator, .{
        .Var = .{
            .name = name,
            .tp = tp,
            .value = value,
        },
    });
}

fn parse_const_stmt(self: *Parser) Error!*AST.StmtNode {
    const name = try self.consume(.Identifier, "Expected a name.");
    const tp = if (self.match_one(.Colon)) |_|
        try self.parse_type()
    else
        null;
    errdefer if (tp) |t| t.deinit(self.allocator);
    const value = if (self.match_one(.Eq)) |_|
        try self.parse_expr()
    else
        null;
    errdefer if (value) |v| v.deinit(self.allocator);

    return try AST.StmtNode.create(self.allocator, .{
        .Const = .{
            .name = name,
            .tp = tp,
            .value = value,
        },
    });
}

fn parse_global(self: *Parser) Error!*const AST.StmtNode {
    var node: *const AST.StmtNode = undefined;
    switch (self.advance().kind) {
        .Var => {
            const nd = try self.parse_var_stmt();
            nd.Var.global = true;
            node = nd;
        },
        .Const => {
            const nd = try self.parse_const_stmt();
            nd.Const.global = true;
            node = nd;
        },
        else => {
            self.index -= 1;
            const nd = try self.parse_var_stmt();
            nd.Var.global = true;
            node = nd;
        },
    }
    return node;
}

fn parse_local(self: *Parser) Error!*const AST.StmtNode {
    return switch (self.advance().kind) {
        .Var => try self.parse_var_stmt(),
        .Const => try self.parse_const_stmt(),
        else => {
            self.index -= 1;
            return self.parse_var_stmt();
        },
    };
}

fn parse_type(self: *Parser) Error!AST.Type {
    const start = self.peek();
    const node = switch (self.peek().kind) {
        .Identifier => |_| try AST.TypeNode.create(self.allocator, .{
            .Identifier = self.advance(),
        }),
        .QuestionMark => try self.parse_optional_type(),
        .Number => res: {
            _ = self.advance();
            break :res try AST.TypeNode.create(self.allocator, .Number);
        },
        .String => res: {
            _ = self.advance();
            break :res try AST.TypeNode.create(self.allocator, .String);
        },
        .Bool => res: {
            _ = self.advance();
            break :res try AST.TypeNode.create(self.allocator, .Bool);
        },
        .Auto => res: {
            _ = self.advance();
            break :res try AST.TypeNode.create(self.allocator, .Auto);
        },
        .Any => res: {
            _ = self.advance();
            break :res try AST.TypeNode.create(self.allocator, .Any);
        },
        else => return Error.InvalidToken,
    };
    const end = self.previous();

    return .{
        .start = start,
        .end = end,
        .node = node,
    };
}

fn parse_optional_type(self: *Parser) Error!*const AST.TypeNode {
    const child_type = try self.parse_type();
    return AST.TypeNode.create(self.allocator, .{
        .Optional = child_type,
    });
}

fn parse_expr(self: *Parser) Error!AST.Expr {
    return self.parse_cast();
}

fn parse_assign(self: *Parser) Error!AST.Expr {
    const start = self.peek();
    var vr = try self.parse_expr();
    errdefer vr.deinit(self.allocator);

    if (self.match_one(.Eq)) |_| {
        const value = try self.parse_expr();
        errdefer value.deinit(self.allocator);
        const end = self.previous();

        return AST.Expr{
            .start = start,
            .end = end,
            .node = try AST.ExprNode.create(self.allocator, .{
                .Assign = .{
                    .value = value,
                    .vr = vr,
                },
            }),
        };
    }

    return vr;
}

fn parse_cast(self: *Parser) Error!AST.Expr {
    var value = try self.parse_term();
    errdefer value.deinit(self.allocator);

    while (self.match_one(.As)) |_| {
        const tp = try self.parse_type();
        const node = try AST.ExprNode.create(self.allocator, .{
            .Cast = .{
                .value = value,
                .tp = tp,
            },
        });
        value = AST.Expr.init(value.start, tp.end, node);
    }

    return value;
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

// TODO: Make it right-associative
fn parse_power(self: *Parser) Error!AST.Expr {
    var lhs = try self.parse_unary();
    errdefer lhs.deinit(self.allocator);

    while (self.match_one(.Hat)) |op| {
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

    const tok = self.peek();
    self.report_error("Invalid Token: '{s}'.", .{tok.lexem}, tok, .{
        .msg = "Expected an expression.",
        .column = tok.column,
    });
    return Error.InvalidToken;
}

fn consume(self: *Parser, kind: TokenKindTag, msg: []const u8) Error!Token {
    if (self.check(kind))
        return self.advance();

    self.report_error("{s}", .{msg}, self.peek(), Report.Caret{
        .msg = msg,
        .column = self.peek().column,
    });
    return error.InvalidToken;
}

fn consume_semicolon(self: *Parser, msg: []const u8) Error!Token {
    if (self.check(.SemiColon))
        return self.advance();

    self.report_error("{s}", .{msg}, self.previous(), Report.Caret{
        .msg = msg,
        .column = self.previous().column,
    });
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

fn sync(self: *Parser) void {
    _ = self.advance();
    while (!self.is_at_end()) {
        switch (self.peek().kind) {
            .Var, .Const => return,
            .SemiColon => {
                _ = self.advance();
                return;
            },
            else => {},
        }
        _ = self.advance();
    }
}

fn report_error(
    self: *const Parser,
    comptime fmt: []const u8,
    args: anytype,
    tok: Token,
    caret: Report.Caret,
) void {
    std.debug.print("Parsing Error\n", .{});
    Report.report_pro(
        self.content,
        self.path,
        caret,
        tok.index,
        tok.line,
        tok.column,
        .Error,
        fmt,
        args,
    );
}
