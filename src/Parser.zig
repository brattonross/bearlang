const std = @import("std");
const Lexer = @import("./Lexer.zig");
const Token = Lexer.Token;

const Parser = @This();

lexer: *Lexer,
current_token: Token,
peek_token: Token,

pub fn init(lexer: *Lexer) !Parser {
    var parser = Parser{ .lexer = lexer, .current_token = undefined, .peek_token = undefined };
    try parser.advance();
    try parser.advance();
    return parser;
}

pub fn next(self: *Parser) !?Statement {
    return if (self.current_token.kind == .eof) null else try self.parseStatement();
}

fn advance(self: *Parser) !void {
    self.current_token = self.peek_token;
    self.peek_token = try self.lexer.nextToken();
}

fn parseStatement(self: *Parser) !Statement {
    return switch (self.current_token.kind) {
        .Let => self.parseLetStatement(),
        else => self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) !Statement {
    _ = self;
    return error.Todo;
}

fn parseExpressionStatement(self: *Parser) !Statement {
    const expression = try self.parseExpression(.lowest);
    return .{ .expression = expression };
}

fn parseExpression(self: *Parser, precedence: Precedence) !Expression {
    _ = precedence;

    // prefix parse fns
    const left = switch (self.current_token.kind) {
        .identifier => try self.parseIdentifier(),
        else => return error.InvalidExpression,
    };

    return left;
}

fn parseIdentifier(self: *Parser) !Expression {
    _ = self;
}

const Statement = union(enum) {
    identifier: Identifier,
    let: LetStatement,
    expression: Expression,
};

const Identifier = []const u8;

const LetStatement = struct {
    name: Identifier,
    value: *Expression,
};

const Expression = union(enum) {
    number: f64,
};

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
};

test "identifier" {
    var lexer = Lexer.init("foobar");
    var parser = try Parser.init(&lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("foobar", statement.?.identifier);
}
