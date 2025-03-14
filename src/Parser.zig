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
        .let => self.parseLetStatement(),
        else => self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) !Statement {
    try self.advance();

    const name = self.current_token.lexeme;
    try self.advance();

    if (self.current_token.kind != .assign) {
        return error.InvalidLetStatement;
    }
    try self.advance();

    const value = try self.parseExpression(.lowest);

    const let = LetStatement{ .name = name, .value = value };
    return .{ .let = let };
}

fn parseExpressionStatement(self: *Parser) !Statement {
    const expression = try self.parseExpression(.lowest);
    return .{ .expression = expression };
}

fn parseExpression(self: *Parser, precedence: Precedence) !Expression {
    _ = precedence;

    // prefix parse fns
    const left = switch (self.current_token.kind) {
        .identifier => self.parseIdentifier(),
        .number => try self.parseNumber(),
        else => return error.InvalidExpression,
    };

    return left;
}

fn parseIdentifier(self: *Parser) Expression {
    return .{ .identifier = self.current_token.lexeme };
}

fn parseNumber(self: *Parser) !Expression {
    const value = try std.fmt.parseFloat(f64, self.current_token.lexeme);
    return .{ .number = value };
}

const Statement = union(enum) {
    let: LetStatement,
    expression: Expression,
};

const Identifier = []const u8;

const LetStatement = struct {
    name: Identifier,
    value: Expression,
};

const Expression = union(enum) {
    identifier: Identifier,
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
    try std.testing.expectEqualStrings("foobar", statement.?.expression.identifier);
}

test "integer" {
    var lexer = Lexer.init("5");
    var parser = try Parser.init(&lexer);

    const statement = try parser.next();
    try std.testing.expectEqual(5.0, statement.?.expression.number);
}

test "float" {
    var lexer = Lexer.init("5.123");
    var parser = try Parser.init(&lexer);

    const statement = try parser.next();
    try std.testing.expectEqual(5.123, statement.?.expression.number);
}

test "let" {
    var lexer = Lexer.init("let foo = 5");
    var parser = try Parser.init(&lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("foo", statement.?.let.name);
    try std.testing.expectEqual(5.0, statement.?.let.value.number);
}
