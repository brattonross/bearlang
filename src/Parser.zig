const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("./Lexer.zig");
const Token = Lexer.Token;

const Parser = @This();

allocator: Allocator,
lexer: *Lexer,
current_token: Token,
peek_token: Token,

pub fn init(allocator: Allocator, lexer: *Lexer) !Parser {
    var parser = Parser{
        .allocator = allocator,
        .lexer = lexer,
        .current_token = undefined,
        .peek_token = undefined,
    };
    try parser.advance();
    try parser.advance();
    return parser;
}

pub fn next(self: *Parser) !?*Statement {
    return if (self.current_token.kind == .eof) null else try self.parseStatement();
}

fn advance(self: *Parser) !void {
    self.current_token = self.peek_token;
    self.peek_token = try self.lexer.nextToken();
}

fn parseStatement(self: *Parser) !*Statement {
    return switch (self.current_token.kind) {
        .let => self.parseLetStatement(),
        else => self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) !*Statement {
    try self.advance();

    const name = self.current_token.lexeme;
    try self.advance();

    if (self.current_token.kind != .assign) {
        return error.InvalidLetStatement;
    }
    try self.advance();

    const value = try self.parseExpression(.lowest);

    const let = LetStatement{ .name = name, .value = value };
    const statement = try self.allocator.create(Statement);
    statement.* = .{ .let = let };
    return statement;
}

fn parseExpressionStatement(self: *Parser) !*Statement {
    const expression = try self.parseExpression(.lowest);
    const statement = try self.allocator.create(Statement);
    statement.* = .{ .expression = expression };
    return statement;
}

fn parseExpression(self: *Parser, precedence: Precedence) anyerror!*Expression {
    // prefix parse fns
    var left = switch (self.current_token.kind) {
        .identifier => try self.parseIdentifier(),
        .number => try self.parseNumber(),
        .string => try self.parseString(),
        else => return error.InvalidExpression,
    };

    while (@intFromEnum(precedence) < @intFromEnum(tokenPrecedence(self.peek_token.kind))) {
        // infix parse fns
        left = switch (self.peek_token.kind) {
            .left_paren => blk: {
                // before: still on ident
                try self.advance();
                // after: now on left_paren
                break :blk try self.parseCallExpression(left);
            },
            else => break,
        };
    }

    return left;
}

fn parseIdentifier(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .identifier = self.current_token.lexeme };
    return expression;
}

fn parseNumber(self: *Parser) !*Expression {
    const value = try std.fmt.parseFloat(f64, self.current_token.lexeme);
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .number = value };
    return expression;
}

fn parseString(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .string = self.current_token.lexeme };
    return expression;
}

fn parseCallExpression(self: *Parser, left: *Expression) !*Expression {
    const args = try self.parseExpressionList(.right_paren);
    const call = CallExpression{ .function = left, .arguments = args };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .call = call };
    return expression;
}

fn parseExpressionList(self: *Parser, end: Token.Kind) !std.ArrayList(*Expression) {
    var args = std.ArrayList(*Expression).init(self.allocator);
    if (self.peek_token.kind == end) {
        try self.advance();
        return args;
    }

    try self.advance();
    try args.append(try self.parseExpression(.lowest));

    while (self.peek_token.kind == .comma) {
        try self.advance();
        try self.advance();
        try args.append(try self.parseExpression(.lowest));
    }

    if (self.peek_token.kind != end) {
        return error.InvalidExpressionList;
    }

    // we are left sitting on the last argument, so advance past it and the closing paren
    try self.advance();
    try self.advance();
    return args;
}

pub const Statement = union(enum) {
    let: LetStatement,
    expression: *Expression,
};

pub const Identifier = []const u8;

pub const LetStatement = struct {
    name: Identifier,
    value: *Expression,
};

pub const Expression = union(enum) {
    identifier: Identifier,
    number: f64,
    string: []const u8,
    call: CallExpression,
};

pub const CallExpression = struct {
    function: *Expression,
    arguments: std.ArrayList(*Expression),
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

fn tokenPrecedence(kind: Token.Kind) Precedence {
    return switch (kind) {
        .left_paren => .call,
        else => .lowest,
    };
}

test "identifier" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("foobar");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("foobar", statement.?.expression.identifier);
}

test "integer" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("5");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqual(5.0, statement.?.expression.number);
}

test "float" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("5.123");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqual(5.123, statement.?.expression.number);
}

test "let" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("let foo = 5");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("foo", statement.?.let.name);
    try std.testing.expectEqual(5.0, statement.?.let.value.number);
}

test "print" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("print(\"foo bar\")");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("print", statement.?.expression.call.function.identifier);
    try std.testing.expectEqual(1, statement.?.expression.call.arguments.items.len);
    try std.testing.expectEqualStrings("foo bar", statement.?.expression.call.arguments.items[0].string);

    try std.testing.expectEqual(null, try parser.next());
}
