const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("./Lexer.zig");
const Token = Lexer.Token;

const Parser = @This();

pub const ParseError = error{
    InvalidAssignment,
    DuplicateStructKey,
} || Lexer.LexError || Allocator.Error || std.fmt.ParseFloatError;

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
        return ParseError.UnexpectedToken;
    }
}

fn parseStatement(self: *Parser) ParseError!*Statement {
    return switch (self.current_token.kind) {
        .let => self.parseLetStatement(),
        .@"for" => self.parseForStatement(),
        .@"break" => self.parseBreakStatement(),
        .function => self.parseFunctionStatement(),
        .@"return" => self.parseReturnStatement(),
        else => self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) !*Statement {
    const let_token = self.current_token;
    try self.advanceExpect(.let);

    const ident_token = self.current_token;
    try self.advanceExpect(.identifier);

    try self.advanceExpect(.assign);

    const value = try self.parseExpression(.lowest);

    const let = LetStatement{
        .token = let_token,
        .name = .{
            .token = ident_token,
            .lexeme = ident_token.lexeme,
        },
        .value = value,
    };
    const statement = try self.allocator.create(Statement);
    statement.* = .{ .let = let };
    return statement;
}

fn parseForStatement(self: *Parser) !*Statement {
    const token = self.current_token;
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
        .token = token,
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

fn parseFunctionStatement(self: *Parser) !*Statement {
    const token = self.current_token;
    try self.advanceExpect(.function);

    const name = try self.parseIdentifierExpression();
    const parameters = try self.parseExpressionList(.right_paren);
    const body = try self.parseBlockStatement();

    const function = FunctionStatement{
        .token = token,
        .name = name.identifier,
        .parameters = parameters,
        .body = body,
    };

    const statement = try self.allocator.create(Statement);
    statement.* = .{ .function = function };
    return statement;
}

fn parseReturnStatement(self: *Parser) !*Statement {
    const token = self.current_token;
    try self.advanceExpect(.@"return");

    const return_value = try self.parseExpression(.lowest);

    const @"return" = ReturnStatement{
        .token = token,
        .return_value = return_value,
    };

    const statement = try self.allocator.create(Statement);
    statement.* = .{ .@"return" = @"return" };
    return statement;
}

fn parseBlockStatement(self: *Parser) !BlockStatement {
    const token = self.current_token;
    try self.advanceExpect(.left_brace);

    var statements = std.ArrayList(*Statement).init(self.allocator);

    while (self.current_token.kind != .eof and self.current_token.kind != .right_brace) {
        const statement = try self.parseStatement();
        try statements.append(statement);
    }

    try self.advanceExpect(.right_brace);

    return .{ .token = token, .statements = statements };
}

fn parseExpressionStatement(self: *Parser) !*Statement {
    const expression = try self.parseExpression(.lowest);
    const statement = try self.allocator.create(Statement);
    statement.* = .{ .expression = expression };
    return statement;
}

fn parseExpression(self: *Parser, precedence: Precedence) ParseError!*Expression {
    // prefix parse fns
    var left = switch (self.current_token.kind) {
        .identifier => try self.parseIdentifierExpression(),
        .number => try self.parseNumber(),
        .string => try self.parseString(),
        .true, .false => try self.parseBoolean(),
        .bang, .minus => try self.parsePrefixExpression(),
        .@"if" => try self.parseIfExpression(),
        .function => try self.parseFunctionExpression(),
        .left_brace => try self.parseStructExpression(),
        else => return ParseError.UnexpectedToken,
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
            .asterisk,
            .asterisk_equals,
            .slash,
            .slash_equals,
            .modulo,
            .modulo_equals,
            .assign,
            => try self.parseInfixExpression(left),
            .left_paren => try self.parseCallExpression(left),
            .dot, .left_square_bracket => try self.parseAccessorExpression(left),
            else => break,
        };
    }

    return left;
}

fn parseIdentifierExpression(self: *Parser) !*Expression {
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .identifier = try self.parseIdentifier() };
    return expression;
}

fn parseIdentifier(self: *Parser) !Identifier {
    const identifier = Identifier{
        .token = self.current_token,
        .lexeme = self.current_token.lexeme,
    };
    try self.advanceExpect(.identifier);
    return identifier;
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
    const token = self.current_token;
    const operator = self.current_token.lexeme;
    try self.advance();
    const right = try self.parseExpression(.prefix);

    const prefix = PrefixExpression{
        .token = token,
        .operator = operator,
        .right = right,
    };
    const expression = try self.allocator.create(Expression);
    expression.* = .{ .prefix = prefix };
    return expression;
}

fn parseFunctionExpression(self: *Parser) !*Expression {
    const token = self.current_token;
    try self.advanceExpect(.function);

    const parameters = try self.parseExpressionList(.right_paren);
    const body = try self.parseBlockStatement();

    const function = FunctionExpression{
        .token = token,
        .parameters = parameters,
        .body = body,
    };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .function = function };
    return expression;
}

fn parseCallExpression(self: *Parser, left: *Expression) !*Expression {
    const token = self.current_token;
    const args = try self.parseExpressionList(.right_paren);
    const call = CallExpression{ .token = token, .function = left, .arguments = args };

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
        return ParseError.UnexpectedToken;
    }
    try self.advance(); // advance past end

    return args;
}

fn parseInfixExpression(self: *Parser, left: *Expression) !*Expression {
    const token = self.current_token;
    const operator = self.current_token.lexeme;
    const precedence = tokenPrecedence(self.current_token.kind);
    try self.advance();
    const right = try self.parseExpression(precedence);
    const infix = InfixExpression{
        .token = token,
        .left = left,
        .operator = operator,
        .right = right,
    };

    // check that assignment is legal
    if (isAssignmentOperator(operator) and left.* != .accessor and left.* != .identifier) {
        return ParseError.InvalidAssignment;
    }

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .infix = infix };
    return expression;
}

const eql = std.mem.eql;
fn isAssignmentOperator(op: []const u8) bool {
    if (eql(u8, "=", op)) {
        return true;
    } else if (eql(u8, "+=", op)) {
        return true;
    } else if (eql(u8, "-=", op)) {
        return true;
    } else if (eql(u8, "*=", op)) {
        return true;
    } else if (eql(u8, "/=", op)) {
        return true;
    } else {
        return false;
    }
}

fn parseIfExpression(self: *Parser) !*Expression {
    const token = self.current_token;

    try self.advanceExpect(.@"if");
    try self.advanceExpect(.left_paren);
    const condition = try self.parseExpression(.lowest);
    try self.advanceExpect(.right_paren);
    const consequence = try self.parseBlockStatement();
    const alternative: ?IfExpression.Alternative = if (self.current_token.kind == .@"else") blk: {
        try self.advanceExpect(.@"else");
        if (self.current_token.kind == .@"if") {
            break :blk .{ .expression = try self.parseIfExpression() };
        } else {
            break :blk .{ .block = try self.parseBlockStatement() };
        }
    } else null;

    const @"if" = IfExpression{
        .token = token,
        .condition = condition,
        .consequence = consequence,
        .alternative = alternative,
    };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .@"if" = @"if" };
    return expression;
}

fn parseStructExpression(self: *Parser) !*Expression {
    const token = self.current_token;
    try self.advanceExpect(.left_brace);

    var map = std.AutoHashMap(*Expression, *Expression).init(self.allocator);
    while (true) { // TODO: better condition
        switch (self.current_token.kind) {
            .identifier => {
                const assign_infix = try self.parseExpression(.lowest);
                std.debug.assert(assign_infix.* == .infix);
                std.debug.assert(eql(u8, "=", assign_infix.infix.operator));

                const key = assign_infix.infix.left;
                const value = assign_infix.infix.right;
                const result = try map.getOrPut(key);
                if (result.found_existing) {
                    return ParseError.DuplicateStructKey;
                } else {
                    result.value_ptr.* = value;
                }
                if (self.current_token.kind == .comma) {
                    try self.advance();
                }
            },
            .right_brace => break,
            else => return ParseError.UnexpectedToken,
        }
    }
    try self.advanceExpect(.right_brace);

    const @"struct" = StructExpression{
        .token = token,
        .map = map,
    };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .@"struct" = @"struct" };
    return expression;
}

fn parseAccessorExpression(self: *Parser, left: *Expression) !*Expression {
    const token = self.current_token;

    var accessor: *Expression = undefined;
    switch (self.current_token.kind) {
        .dot => {
            try self.advance();
            accessor = try self.parseExpression(.assign);
        },
        .left_square_bracket => {
            try self.advance();
            accessor = try self.parseExpression(.lowest);
            try self.advanceExpect(.right_square_bracket);
        },
        else => return ParseError.UnexpectedToken,
    }

    const exp = AccessorExpression{
        .token = token,
        .parent = left,
        .key = accessor,
    };

    const expression = try self.allocator.create(Expression);
    expression.* = .{ .accessor = exp };
    return expression;
}

pub const Statement = union(enum) {
    let: LetStatement,
    expression: *Expression,
    @"for": ForStatement,
    @"break": void,
    block: BlockStatement,
    function: FunctionStatement,
    @"return": ReturnStatement,

    pub fn format(self: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .let => writer.print("{}", .{self.let}),
            .expression => writer.print("{}", .{self.expression}),
            .@"for" => writer.print("{}", .{self.@"for"}),
            .@"break" => writer.writeAll("break"),
            .block => writer.print("{}", .{self.block}),
            .function => writer.print("{}", .{self.function}),
            .@"return" => writer.print("{}", .{self.@"return"}),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    lexeme: []const u8,

    pub fn format(self: Identifier, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.lexeme});
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: *Expression,

    pub fn format(self: LetStatement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {} = {}", .{ self.token.lexeme, self.name, self.value.* });
    }
};

pub const ForStatement = struct {
    token: Token,
    initial: ?*Statement,
    condition: ?*Expression,
    after: ?*Expression,
    block: BlockStatement,

    pub fn format(self: ForStatement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("for ");
        if (self.initial) |initial| {
            try writer.print("({}; {}; {}) ", .{ initial.*, self.condition.?.*, self.after.?.* });
        } else if (self.condition) |condition| {
            try writer.print("({}) ", .{condition.*});
        }
        try writer.print("{}", .{self.block});
    }
};

pub const FunctionStatement = struct {
    token: Token,
    name: Identifier,
    parameters: std.ArrayList(*Expression),
    body: BlockStatement,

    pub fn format(self: FunctionStatement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("fn {}(", .{self.name});
        for (self.parameters.items, 0..) |param, i| {
            try writer.print("{}", .{param.*});
            if (i < self.parameters.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.print(") {}\n", .{self.body});
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: *Expression,

    pub fn format(self: ReturnStatement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("return {}", .{self.return_value.*});
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(*Statement),

    pub fn format(self: BlockStatement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll("{\n");
        for (self.statements.items) |item| {
            try writer.print("\t{}\n", .{item.*});
        }
        try writer.writeAll("}");
    }
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
    function: FunctionExpression,
    @"struct": StructExpression,
    accessor: AccessorExpression,

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .identifier => writer.print("{}", .{self.identifier}),
            .number => writer.print("{d}", .{self.number}),
            .string => writer.print("{s}", .{self.string}),
            .boolean => writer.print("{}", .{self.boolean}),
            .prefix => writer.print("{}", .{self.prefix}),
            .infix => writer.print("{}", .{self.infix}),
            .call => writer.print("{}", .{self.call}),
            .@"if" => writer.print("{}", .{self.@"if"}),
            .function => writer.print("{}", .{self.function}),
            .@"struct" => writer.print("{}", .{self.@"struct"}),
            .accessor => writer.print("{}", .{self.accessor}),
        };
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn format(self: PrefixExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}{}", .{ self.operator, self.right.* });
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn format(self: InfixExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{} {s} {}", .{ self.left.*, self.operator, self.right.* });
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    arguments: std.ArrayList(*Expression),

    pub fn format(self: CallExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}(", .{self.function.*});
        for (self.arguments.items, 0..) |arg, i| {
            try writer.print("{}", .{arg.*});
            if (i < self.arguments.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(")");
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?Alternative,

    pub const Alternative = union(enum) {
        block: BlockStatement, // else block
        expression: *Expression, // else if expression

        pub fn format(self: Alternative, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try switch (self) {
                .block => writer.print("{}", .{self.block}),
                .expression => writer.print("{}", .{self.expression.*}),
            };
        }
    };

    pub fn format(self: IfExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("if ({}) {}", .{ self.condition.*, self.consequence });
        if (self.alternative) |alt| {
            try writer.print(" else {}", .{alt});
        }
    }
};

pub const FunctionExpression = struct {
    token: Token,
    parameters: std.ArrayList(*Expression),
    body: BlockStatement,

    pub fn format(self: FunctionExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("fn(");
        for (self.parameters.items, 0..) |param, i| {
            try writer.print("{}", .{param.*});
            if (i < self.parameters.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.print(") {}", .{self.body});
    }
};

pub const StructExpression = struct {
    token: Token,
    map: std.AutoHashMap(*Expression, *Expression),

    pub fn format(self: StructExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("{{\n");
        var iter = self.map.iterator();
        while (iter.next()) |entry| {
            try writer.print("\t{}: {}\n", .{ entry.key_ptr.*.*, entry.value_ptr.*.* });
        }
        try writer.writeAll("}}");
    }
};

pub const AccessorExpression = struct {
    token: Token,
    parent: *Expression,
    key: *Expression,

    pub fn format(self: AccessorExpression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}.{}", .{ self.parent.*, self.key.* });
    }
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
    accessor,
    assign,
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
        .dot, .left_square_bracket => .accessor,
        .assign => .assign,
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
    try std.testing.expectEqualStrings("foobar", statement.?.expression.identifier.lexeme);
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
    try std.testing.expectEqualStrings("foo", statement.?.let.name.lexeme);
    try std.testing.expectEqual(5.0, statement.?.let.value.number);
}

test "print" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("print(\"foo bar\")");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    try std.testing.expectEqualStrings("print", statement.?.expression.call.function.identifier.lexeme);
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
    try std.testing.expectEqualStrings("x", condition.left.identifier.lexeme);
    try std.testing.expectEqualStrings(">", condition.operator);
    try std.testing.expectEqual(10.0, condition.right.number);

    const block = @"for".block;
    try std.testing.expectEqual(1, block.statements.items.len);

    const block_infix = block.statements.items[0].expression.infix;
    try std.testing.expectEqualStrings("x", block_infix.left.identifier.lexeme);
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
    try std.testing.expectEqualStrings("i", initial.let.name.lexeme);
    try std.testing.expectEqual(0, initial.let.value.number);

    const condition = @"for".condition.?.infix;
    try std.testing.expectEqualStrings("i", condition.left.identifier.lexeme);
    try std.testing.expectEqualStrings("<", condition.operator);
    try std.testing.expectEqual(3, condition.right.number);

    const after = @"for".after.?.infix;
    try std.testing.expectEqualStrings("i", after.left.identifier.lexeme);
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
    try std.testing.expectEqualStrings("x", consequence.statements.items[0].expression.identifier.lexeme);

    const alternative = @"if".alternative.?.block;
    try std.testing.expectEqual(1, alternative.statements.items.len);
    try std.testing.expectEqualStrings("y", alternative.statements.items[0].expression.identifier.lexeme);
}

test "function statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("fn plus(a, b) { return a + b }");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();
    const func = statement.?.function;

    try std.testing.expectEqual(.function, func.token.kind);
    try std.testing.expectEqualStrings("plus", func.name.lexeme);

    try std.testing.expectEqual(2, func.parameters.items.len);
    try std.testing.expectEqualStrings("a", func.parameters.items[0].identifier.lexeme);
    try std.testing.expectEqualStrings("b", func.parameters.items[1].identifier.lexeme);

    try std.testing.expectEqual(1, func.body.statements.items.len);
    const return_stmt = func.body.statements.items[0].@"return";
    try std.testing.expectEqual(.@"return", return_stmt.token.kind);
    try std.testing.expectEqualStrings("a", return_stmt.return_value.infix.left.identifier.lexeme);
    try std.testing.expectEqualStrings("+", return_stmt.return_value.infix.operator);
    try std.testing.expectEqualStrings("b", return_stmt.return_value.infix.right.identifier.lexeme);
}

test "function expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("let plus = fn(a, b) { return a + b }");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();

    const let = statement.?.let;
    try std.testing.expectEqualStrings("plus", let.name.lexeme);

    const func = let.value.function;
    try std.testing.expectEqual(.function, func.token.kind);

    try std.testing.expectEqual(2, func.parameters.items.len);
    try std.testing.expectEqualStrings("a", func.parameters.items[0].identifier.lexeme);
    try std.testing.expectEqualStrings("b", func.parameters.items[1].identifier.lexeme);

    try std.testing.expectEqual(1, func.body.statements.items.len);
    const return_stmt = func.body.statements.items[0].@"return";
    try std.testing.expectEqual(.@"return", return_stmt.token.kind);
    try std.testing.expectEqualStrings("a", return_stmt.return_value.infix.left.identifier.lexeme);
    try std.testing.expectEqualStrings("+", return_stmt.return_value.infix.operator);
    try std.testing.expectEqualStrings("b", return_stmt.return_value.infix.right.identifier.lexeme);
}

test "empty struct" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("{}");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();

    const @"struct" = statement.?.expression.@"struct";

    try std.testing.expectEqual(0, @"struct".map.count());
}

test "struct" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("{ a = \"string\", b = 1, c = {} }");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();

    const @"struct" = statement.?.expression.@"struct";

    try std.testing.expectEqual(3, @"struct".map.count());

    var iter = @"struct".map.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.identifier.lexeme;
        if (std.mem.eql(u8, "a", key)) {
            try std.testing.expectEqualStrings("string", entry.value_ptr.*.string);
        } else if (std.mem.eql(u8, "b", key)) {
            try std.testing.expectEqual(1, entry.value_ptr.*.number);
        } else if (std.mem.eql(u8, "c", key)) {
            const value = entry.value_ptr.*.@"struct";
            try std.testing.expectEqual(0, value.map.count());
        } else unreachable;
    }
}

test "accessor" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("car.wheels");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();

    const accessor = statement.?.expression.accessor;

    try std.testing.expectEqualStrings("car", accessor.parent.identifier.lexeme);
    try std.testing.expectEqualStrings("wheels", accessor.key.identifier.lexeme);
}

test "accessor assign" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("pokedex.entries = 5");
    var parser = try Parser.init(allocator, &lexer);

    const statement = try parser.next();

    const infix = statement.?.expression.infix;

    const accessor = infix.left.accessor;
    try std.testing.expectEqualStrings("pokedex", accessor.parent.identifier.lexeme);
    try std.testing.expectEqualStrings("entries", accessor.key.identifier.lexeme);

    try std.testing.expectEqualStrings("=", infix.operator);

    try std.testing.expectEqual(5, infix.right.number);
}
