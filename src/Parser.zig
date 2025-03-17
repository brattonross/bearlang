const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("./Lexer.zig");
const Token = Lexer.Token;

const Parser = @This();

allocator: Allocator,
lexer: *Lexer,
current_token: Token,

pub fn init(allocator: Allocator, lexer: *Lexer) !Parser {
    var parser = Parser{
        .allocator = allocator,
        .lexer = lexer,
        .current_token = undefined,
    };
    try parser.advance();
    return parser;
}

pub fn next(self: *Parser) !?*Statement {
    return if (self.current_token.kind == .eof) null else try self.parseStatement();
}

fn advance(self: *Parser) !void {
    self.current_token = try self.lexer.nextToken();
}

/// If the current token kind matches, advance the parser, else return an error.
fn advanceExpect(self: *Parser, expected: Token.Kind) !void {
    if (self.current_token.kind == expected) {
        try self.advance();
    } else {
        std.log.err("expected next token to be {}, got {}\n", .{ expected, self.current_token.kind });
        return error.UnexpectedToken;
    }
}

fn parseStatement(self: *Parser) !*Statement {
    return switch (self.current_token.kind) {
        .let => self.parseLetStatement(),
        .@"for" => self.parseForStatement(),
        .@"break" => self.parseBreakStatement(),
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

fn parseForStatement(self: *Parser) !*Statement {
    try self.advance(); // advance past `for`

    var initial: ?*Statement = null;
    var condition: ?*Expression = null;
    var after: ?*Expression = null;

    if (self.current_token.kind == .left_paren) {
        try self.advanceExpect(.left_paren);
        if (self.current_token.kind == .let) {
            initial = try self.parseStatement();
            try self.advanceExpect(.semicolon);
            condition = try self.parseExpression(.lowest);
            try self.advanceExpect(.semicolon);
            after = try self.parseExpression(.lowest);
        } else {
            condition = try self.parseExpression(.lowest);
        }
        try self.advanceExpect(.right_paren);
    }

    const block = try self.parseBlockStatement();

    const @"for" = ForStatement{
        .initial = initial,
        .condition = condition,
        .after = after,
        .block = block,
    };

    const statement = try self.allocator.create(Statement);
    statement.* = .{ .@"for" = @"for" };
    return statement;
}

fn parseBreakStatement(self: *Parser) !*Statement {
    const statement = try self.allocator.create(Statement);
    statement.* = .{ .@"break" = {} };
    try self.advance();
    return statement;
}

fn parseBlockStatement(self: *Parser) !BlockStatement {
    try self.advanceExpect(.left_brace);

    var statements = std.ArrayList(*Statement).init(self.allocator);

    while (self.current_token.kind != .eof and self.current_token.kind != .right_brace) {
        const statement = try self.parseStatement();
        try statements.append(statement);
    }

    try self.advanceExpect(.right_brace);

    return .{ .statements = statements };
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
        .true, .false => try self.parseBoolean(),
        .bang, .minus => try self.parsePrefixExpression(),
        .@"if" => try self.parseIfExpression(),
        else => return error.InvalidExpression,
    };

    while (@intFromEnum(precedence) < @intFromEnum(tokenPrecedence(self.current_token.kind))) {
        // infix parse fns
        left = switch (self.current_token.kind) {
            .equals,
            .not_equals,
            .less_than,
            .less_than_equals,
            .greater_than,
            .greater_than_equals,
            .@"and",
            .@"or",
            .plus,
            .plus_equals,
            .minus,
            .minus_equals,
            .slash,
            .slash_equals,
            .modulo,
            .modulo_equals,
            => try self.parseInfixExpression(left),
            .left_paren => try self.parseCallExpression(left),
            else => break,
        };
    }

    return left;
}

fn parseIdentifier(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .identifier = self.current_token.lexeme };
    try self.advance();
    return expression;
}

fn parseNumber(self: *Parser) !*Expression {
    const value = try std.fmt.parseFloat(f64, self.current_token.lexeme);
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .number = value };
    try self.advance();
    return expression;
}

fn parseString(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .string = self.current_token.lexeme };
    try self.advance();
    return expression;
}

fn parseBoolean(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .boolean = self.current_token.kind == .true };
    try self.advance();
    return expression;
}

fn parsePrefixExpression(self: *Parser) !*Expression {
    const operator = self.current_token.lexeme;
    try self.advance();
    const right = try self.parseExpression(.prefix);

    const prefix = PrefixExpression{
        .operator = operator,
        .right = right,
    };
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .prefix = prefix };
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
    try self.advance(); // advance past start

    var args = std.ArrayList(*Expression).init(self.allocator);
    if (self.current_token.kind == end) {
        try self.advance();
        return args;
    }

    try args.append(try self.parseExpression(.lowest));

    while (self.current_token.kind == .comma) {
        try self.advance();
        try args.append(try self.parseExpression(.lowest));
    }

    if (self.current_token.kind != end) {
        return error.InvalidExpressionList;
    }
    try self.advance(); // advance past end

    return args;
}

fn parseInfixExpression(self: *Parser, left: *Expression) !*Expression {
    const operator = self.current_token.lexeme;
    const precedence = tokenPrecedence(self.current_token.kind);
    try self.advance();
    const right = try self.parseExpression(precedence);
    const infix = InfixExpression{
        .left = left,
        .operator = operator,
        .right = right,
    };

    // assignment should only be done with identifiers on the left
    if ((std.mem.eql(u8, "+=", operator) or std.mem.eql(u8, "-=", operator) or std.mem.eql(u8, "*=", operator) or std.mem.eql(u8, "/=", operator)) and left.* != .identifier) {
        return error.InvalidAssignmentOperation;
    }

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .infix = infix };
    return expression;
}

fn parseIfExpression(self: *Parser) !*Expression {
    try self.advanceExpect(.@"if");
    try self.advanceExpect(.left_paren);
    const condition = try self.parseExpression(.lowest);
    try self.advanceExpect(.right_paren);
    const consequence = try self.parseBlockStatement();
    const alternative = if (self.current_token.kind == .@"else") blk: {
        try self.advanceExpect(.@"else");
        break :blk try self.parseBlockStatement();
    } else null;

    const @"if" = IfExpression{
        .condition = condition,
        .consequence = consequence,
        .alternative = alternative,
    };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .@"if" = @"if" };
    return expression;
}

pub const Statement = union(enum) {
    let: LetStatement,
    expression: *Expression,
    @"for": ForStatement,
    @"break": void,
    block: BlockStatement,
};

pub const Identifier = []const u8;

pub const LetStatement = struct {
    name: Identifier,
    value: *Expression,
};

pub const ForStatement = struct {
    initial: ?*Statement,
    condition: ?*Expression,
    after: ?*Expression,
    block: BlockStatement,
};

pub const BlockStatement = struct {
    statements: std.ArrayList(*Statement),
};

pub const Expression = union(enum) {
    identifier: Identifier,
    number: f64,
    string: []const u8,
    boolean: bool,
    prefix: PrefixExpression,
    infix: InfixExpression,
    call: CallExpression,
    @"if": IfExpression,
};

pub const PrefixExpression = struct {
    operator: []const u8,
    right: *Expression,
};

pub const InfixExpression = struct {
    left: *Expression,
    operator: []const u8,
    right: *Expression,
};

pub const CallExpression = struct {
    function: *Expression,
    arguments: std.ArrayList(*Expression),
};

pub const IfExpression = struct {
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

const Precedence = enum {
    lowest,
    @"or",
    @"and",
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
};

fn tokenPrecedence(kind: Token.Kind) Precedence {
    return switch (kind) {
        .@"or" => .@"or",
        .@"and" => .@"and",
        .equals, .not_equals => .equals,
        .less_than, .less_than_equals, .greater_than, .greater_than_equals => .less_greater,
        .plus, .plus_equals, .minus, .minus_equals => .sum,
        .slash, .slash_equals, .asterisk, .asterisk_equals, .modulo, .modulo_equals => .product,
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

test "string concat" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("\"foo\" + \"bar\"");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const infix = statement.?.expression.infix;
    try std.testing.expectEqualStrings("foo", infix.left.string);
    try std.testing.expectEqualStrings("+", infix.operator);
    try std.testing.expectEqualStrings("bar", statement.?.expression.infix.right.string);
}

test "bang prefix" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("!false");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const prefix = statement.?.expression.prefix;
    try std.testing.expectEqualStrings("!", prefix.operator);
    try std.testing.expectEqual(false, prefix.right.boolean);
}

test "equals" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("true == true");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const infix = statement.?.expression.infix;
    try std.testing.expectEqual(true, infix.left.boolean);
    try std.testing.expectEqualStrings("==", infix.operator);
    try std.testing.expectEqual(true, infix.right.boolean);
}

test "not equals" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("true != true");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const infix = statement.?.expression.infix;
    try std.testing.expectEqual(true, infix.left.boolean);
    try std.testing.expectEqualStrings("!=", infix.operator);
    try std.testing.expectEqual(true, infix.right.boolean);
}

test "greater than" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("1 > 3");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const infix = statement.?.expression.infix;
    try std.testing.expectEqual(1.0, infix.left.number);
    try std.testing.expectEqualStrings(">", infix.operator);
    try std.testing.expectEqual(3.0, infix.right.number);
}

test "for without condition" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(
        \\for {
        \\  break
        \\}
    );
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const @"for" = statement.?.@"for";

    try std.testing.expectEqual(null, @"for".condition);

    const block = @"for".block;
    try std.testing.expectEqual(1, block.statements.items.len);
    try std.testing.expectEqual(.@"break", block.statements.items[0].*);
}

test "for with condition" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("for (x > 10) { x -= 1 }");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const @"for" = statement.?.@"for";

    const condition = @"for".condition.?.infix;
    try std.testing.expectEqualStrings("x", condition.left.identifier);
    try std.testing.expectEqualStrings(">", condition.operator);
    try std.testing.expectEqual(10.0, condition.right.number);

    const block = @"for".block;
    try std.testing.expectEqual(1, block.statements.items.len);

    const block_infix = block.statements.items[0].expression.infix;
    try std.testing.expectEqualStrings("x", block_infix.left.identifier);
    try std.testing.expectEqualStrings("-=", block_infix.operator);
    try std.testing.expectEqual(1.0, block_infix.right.number);
}

test "for traditional" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("for (let i = 0; i < 3; i += 1) {}");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const @"for" = statement.?.@"for";

    const initial = @"for".initial.?;
    try std.testing.expectEqualStrings("i", initial.let.name);
    try std.testing.expectEqual(0, initial.let.value.number);

    const condition = @"for".condition.?.infix;
    try std.testing.expectEqualStrings("i", condition.left.identifier);
    try std.testing.expectEqualStrings("<", condition.operator);
    try std.testing.expectEqual(3, condition.right.number);

    const after = @"for".after.?.infix;
    try std.testing.expectEqualStrings("i", after.left.identifier);
    try std.testing.expectEqualStrings("+=", after.operator);
    try std.testing.expectEqual(1, after.right.number);

    const block = @"for".block;
    try std.testing.expectEqual(0, block.statements.items.len);
}

test "if/else" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("if (true) { x } else { y }");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const @"if" = statement.?.expression.@"if";

    const condition = @"if".condition;
    try std.testing.expectEqual(true, condition.boolean);

    const consequence = @"if".consequence;
    try std.testing.expectEqual(1, consequence.statements.items.len);
    try std.testing.expectEqualStrings("x", consequence.statements.items[0].expression.identifier);

    const alternative = @"if".alternative.?;
    try std.testing.expectEqual(1, alternative.statements.items.len);
    try std.testing.expectEqualStrings("y", alternative.statements.items[0].expression.identifier);
}
