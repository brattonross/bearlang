const std = @import("std");

pub const Token = struct {
    kind: Kind,
    lexeme: []const u8,
    line: u32,

    pub const Kind = enum {
        eof,

        identifier,
        number,
        string,

        assign, // =
        plus, // +
        minus, // -
        equals, // ==
        not_equals, // !=
        plus_equals, // +=
        minus_equals, // -=
        less_than, // <
        greater_than, // >
        less_than_equals, // <=
        greater_than_equals, // >=
        asterisk, // *
        asterisk_equals, // *=
        slash, // /
        slash_equals, // /=
        bang, // !
        @"and",
        @"or",

        left_paren, // (
        right_paren, // )
        left_brace, // {
        right_brace, // }
        comma, // ,

        let,
        @"while",
        true,
        false,
    };
};

const Lexer = @This();

src: []const u8,
pos: u32,
line: u32,

pub fn init(src: []const u8) Lexer {
    return .{ .src = src, .pos = 0, .line = 1 };
}

pub fn nextToken(self: *Lexer) !Token {
    while (self.currentLexeme()) |current| {
        if (isWhitespace(current)) {
            self.advance();
        } else {
            break;
        }
    }

    var token = Token{ .kind = .eof, .lexeme = "", .line = self.line };

    const start_pos = self.pos;
    if (self.currentLexeme()) |current| switch (current) {
        '=' => {
            if (self.peekLexeme() == '=') {
                token.kind = .equals;
                self.advance();
            } else {
                token.kind = .assign;
            }
        },
        '+' => {
            if (self.peekLexeme() == '=') {
                token.kind = .plus_equals;
                self.advance();
            } else {
                token.kind = .plus;
            }
        },
        '-' => {
            if (self.peekLexeme() == '=') {
                token.kind = .minus_equals;
                self.advance();
            } else {
                token.kind = .minus;
            }
        },
        '!' => {
            if (self.peekLexeme() == '=') {
                token.kind = .not_equals;
                self.advance();
            } else {
                token.kind = .bang;
            }
        },
        '<' => {
            if (self.peekLexeme() == '=') {
                token.kind = .less_than_equals;
                self.advance();
            } else {
                token.kind = .less_than;
            }
        },
        '>' => {
            if (self.peekLexeme() == '=') {
                token.kind = .greater_than_equals;
                self.advance();
            } else {
                token.kind = .greater_than;
            }
        },
        '*' => {
            if (self.peekLexeme() == '=') {
                token.kind = .asterisk_equals;
                self.advance();
            } else {
                token.kind = .asterisk;
            }
        },
        '/' => {
            if (self.peekLexeme() == '=') {
                token.kind = .slash_equals;
                self.advance();
            } else {
                token.kind = .slash;
            }
        },
        '(' => token.kind = .left_paren,
        ')' => token.kind = .right_paren,
        '{' => token.kind = .left_brace,
        '}' => token.kind = .right_brace,
        ',' => token.kind = .comma,
        '"' => return self.scanString(),
        else => {
            if (isAlpha(current)) {
                return self.scanIdentifier();
            } else if (isDigit(current)) {
                return self.scanNumber();
            } else {
                return error.UnexpectedCharacter;
            }
        },
    };

    if (token.kind != .eof and token.lexeme.len == 0) {
        token.lexeme = self.src[start_pos .. self.pos + 1];
    }

    self.advance();
    return token;
}

fn scanIdentifier(self: *Lexer) Token {
    const start_pos = self.pos;

    while (self.currentLexeme()) |current| {
        if (!isAlpha(current)) break;
        self.advance();
    }

    const lexeme = self.src[start_pos..self.pos];
    var kind: Token.Kind = .identifier;
    if (std.mem.eql(u8, "let", lexeme)) {
        kind = .let;
    } else if (std.mem.eql(u8, "true", lexeme)) {
        kind = .true;
    } else if (std.mem.eql(u8, "false", lexeme)) {
        kind = .false;
    } else if (std.mem.eql(u8, "and", lexeme)) {
        kind = .@"and";
    } else if (std.mem.eql(u8, "or", lexeme)) {
        kind = .@"or";
    } else if (std.mem.eql(u8, "while", lexeme)) {
        kind = .@"while";
    }

    return .{ .kind = kind, .lexeme = lexeme, .line = self.line };
}

fn scanNumber(self: *Lexer) Token {
    const start_pos = self.pos;

    while (self.currentLexeme()) |current| {
        if (!isDigit(current)) break;
        self.advance();
    }

    if (self.currentLexeme()) |maybe_period| if (maybe_period == '.') {
        if (self.peekLexeme()) |peek| if (isDigit(peek)) {
            self.advance();

            while (self.currentLexeme()) |current| {
                if (!isDigit(current)) break;
                self.advance();
            }
        };
    };

    return .{ .kind = .number, .lexeme = self.src[start_pos..self.pos], .line = self.line };
}

fn scanString(self: *Lexer) !Token {
    self.advance(); // advance past opening `"`
    const start_pos = self.pos;
    while (true) {
        if (self.currentLexeme()) |current| {
            if (current == '"') break;
            self.advance();
        } else {
            return error.UnterminatedString;
        }
    }
    self.advance(); // advance past closing `"`
    return .{ .kind = .string, .lexeme = self.src[start_pos .. self.pos - 1], .line = self.line };
}

fn advance(self: *Lexer) void {
    if (self.pos >= self.src.len) {
        return;
    }

    if (self.currentLexeme()) |current| if (current == '\n') {
        self.line += 1;
    };
    self.pos += 1;
}

fn peekLexeme(self: Lexer) ?u8 {
    return self.lexemeAt(self.pos + 1);
}

fn currentLexeme(self: Lexer) ?u8 {
    return self.lexemeAt(self.pos);
}

fn lexemeAt(self: Lexer, pos: u32) ?u8 {
    return if (pos >= self.src.len) null else self.src[pos];
}

fn isAlpha(c: u8) bool {
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\r' or c == '\n';
}

test "identifier" {
    var lexer = Lexer.init("foobar");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.identifier, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("foobar", token.lexeme);
}

test "integer" {
    var lexer = Lexer.init("1234");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.number, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("1234", token.lexeme);
}

test "float" {
    var lexer = Lexer.init("1234.56");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.number, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("1234.56", token.lexeme);
}

test "string" {
    var lexer = Lexer.init("\"foo bar\"");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.string, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("foo bar", token.lexeme);
}

test "assign" {
    var lexer = Lexer.init("=");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.assign, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("=", token.lexeme);
}

test "plus" {
    var lexer = Lexer.init("+");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.plus, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("+", token.lexeme);
}

test "parens" {
    var lexer = Lexer.init("()");

    var token = try lexer.nextToken();
    try std.testing.expectEqual(.left_paren, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("(", token.lexeme);

    token = try lexer.nextToken();
    try std.testing.expectEqual(.right_paren, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings(")", token.lexeme);
}

test "braces" {
    var lexer = Lexer.init("{}");

    var token = try lexer.nextToken();
    try std.testing.expectEqual(.left_brace, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("{", token.lexeme);

    token = try lexer.nextToken();
    try std.testing.expectEqual(.right_brace, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("}", token.lexeme);
}

test "let" {
    var lexer = Lexer.init("let");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.let, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("let", token.lexeme);
}

test "and" {
    var lexer = Lexer.init("foo and bar");

    const test_cases = [_]struct {
        kind: Token.Kind,
        lexeme: []const u8,
    }{
        .{ .kind = .identifier, .lexeme = "foo" },
        .{ .kind = .@"and", .lexeme = "and" },
        .{ .kind = .identifier, .lexeme = "bar" },
    };

    for (test_cases) |test_case| {
        const token = try lexer.nextToken();
        try std.testing.expectEqual(test_case.kind, token.kind);
        try std.testing.expectEqualStrings(test_case.lexeme, token.lexeme);
    }
}

test "or" {
    var lexer = Lexer.init("foo or bar");

    const test_cases = [_]struct {
        kind: Token.Kind,
        lexeme: []const u8,
    }{
        .{ .kind = .identifier, .lexeme = "foo" },
        .{ .kind = .@"or", .lexeme = "or" },
        .{ .kind = .identifier, .lexeme = "bar" },
    };

    for (test_cases) |test_case| {
        const token = try lexer.nextToken();
        try std.testing.expectEqual(test_case.kind, token.kind);
        try std.testing.expectEqualStrings(test_case.lexeme, token.lexeme);
    }
}

test "equals" {
    var lexer = Lexer.init("true == false");

    const test_cases = [_]struct {
        kind: Token.Kind,
        lexeme: []const u8,
    }{
        .{ .kind = .true, .lexeme = "true" },
        .{ .kind = .equals, .lexeme = "==" },
        .{ .kind = .false, .lexeme = "false" },
    };
    for (test_cases) |test_case| {
        const token = try lexer.nextToken();
        try std.testing.expectEqual(test_case.kind, token.kind);
        try std.testing.expectEqual(1, token.line);
        try std.testing.expectEqualStrings(test_case.lexeme, token.lexeme);
    }
}

test "not equals" {
    var lexer = Lexer.init("!=");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.not_equals, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("!=", token.lexeme);
}

test "plus equals" {
    var lexer = Lexer.init("+=");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.plus_equals, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("+=", token.lexeme);
}

test "minus equals" {
    var lexer = Lexer.init("-=");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.minus_equals, token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("-=", token.lexeme);
}

test "while" {
    var lexer = Lexer.init("while");

    const token = try lexer.nextToken();
    try std.testing.expectEqual(.@"while", token.kind);
    try std.testing.expectEqual(1, token.line);
    try std.testing.expectEqualStrings("while", token.lexeme);
}
