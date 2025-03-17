const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("./Parser.zig");

// expressions
const Expression = Parser.Expression;
const PrefixExpression = Parser.PrefixExpression;
const InfixExpression = Parser.InfixExpression;
const CallExpression = Parser.CallExpression;

// statements
const Statement = Parser.Statement;
const LetStatement = Parser.LetStatement;
const WhileStatement = Parser.WhileStatement;
const BlockStatement = Parser.BlockStatement;

const Interpreter = @This();

allocator: Allocator,
parser: *Parser,
environment: *Environment,

pub fn init(allocator: Allocator, parser: *Parser, environment: *Environment) Interpreter {
    return .{
        .allocator = allocator,
        .parser = parser,
        .environment = environment,
    };
}

pub fn run(self: *Interpreter) !?Value {
    var value: ?Value = null;
    while (try self.parser.next()) |statement| {
        value = try self.evalStatement(statement.*);
    }
    return value;
}

fn evalStatement(self: *Interpreter, statement: Statement) anyerror!?Value {
    return switch (statement) {
        .let => try self.evalLetStatement(statement.let),
        .@"while" => try self.evalWhileStatement(statement.@"while"),
        .block => try self.evalBlockStatement(statement.block),
        .expression => try self.evalExpression(statement.expression.*),
    };
}

fn evalLetStatement(self: *Interpreter, let: LetStatement) !?Value {
    if (try self.evalExpression(let.value.*)) |value| {
        try self.environment.set(let.name, value);
        return null;
    } else {
        return error.InvalidLetStatement;
    }
}

fn evalWhileStatement(self: *Interpreter, statement: WhileStatement) !?Value {
    while (true) {
        const condition = try self.evalExpression(statement.condition.*);
        if (isTruthy(condition)) {
            // TODO: consider `break` and `return`
            _ = try self.evalBlockStatement(statement.block);
        } else {
            break;
        }
    }

    // TODO: could be treated as an expression
    return null;
}

fn evalBlockStatement(self: *Interpreter, block: BlockStatement) !?Value {
    for (block.statements.items) |statement| {
        // TODO: consider `break` and `return`
        _ = try self.evalStatement(statement.*);
    }

    // TODO: could be treated as an expression
    return null;
}

fn isTruthy(exp: ?Value) bool {
    return if (exp) |e| switch (e) {
        .boolean => e.boolean == true,
        else => true,
    } else false;
}

fn evalExpression(self: *Interpreter, expression: Expression) anyerror!?Value {
    return switch (expression) {
        .identifier => self.evalIdentifier(expression.identifier),
        .string => .{ .string = expression.string },
        .number => .{ .number = expression.number },
        .boolean => .{ .boolean = expression.boolean },
        .prefix => try self.evalPrefixExpression(expression.prefix),
        .infix => try self.evalInfixExpression(expression.infix),
        .call => try self.evalCallExpression(expression.call),
    };
}

fn evalIdentifier(self: *Interpreter, identifier: []const u8) ?Value {
    return self.environment.get(identifier);
}

fn evalPrefixExpression(self: *Interpreter, prefix: PrefixExpression) !?Value {
    const right = try self.evalExpression(prefix.right.*);
    if (std.mem.eql(u8, "!", prefix.operator)) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, "-", prefix.operator)) {
        return try evalMinusPrefixOperatorExpression(right);
    } else {
        return error.UnknownPrefixOperator;
    }
}

fn evalBangOperatorExpression(right: ?Value) Value {
    return if (right) |r| switch (r) {
        .string, .number => .{ .boolean = false },
        .boolean => .{ .boolean = !right.?.boolean },
    } else .{ .boolean = true };
}

fn evalMinusPrefixOperatorExpression(right: ?Value) !Value {
    if (right == null or right.? != .number) {
        return error.UnknownOperator;
    }
    return .{ .number = -right.?.number };
}

fn evalInfixExpression(self: *Interpreter, infix: InfixExpression) !?Value {
    const maybe_left = try self.evalExpression(infix.left.*);
    const maybe_right = try self.evalExpression(infix.right.*);

    if (maybe_left == null or maybe_right == null) {
        return error.InfixTypeMismatch;
    }

    const left = maybe_left.?;
    const right = maybe_right.?;

    if (left == .string and right == .string) {
        return try self.evalStringInfixExpression(infix.operator, left.string, right.string);
    } else if (left == .number and right == .number) {
        const value = try evalNumberInfixExpression(infix.operator, left.number, right.number);
        if (isAssignmentInfixExpression(infix)) {
            try self.environment.set(infix.left.identifier, value);
        }
        return value;
    } else if (left == .boolean and right == .boolean) {
        return try evalBooleanInfixExpression(infix.operator, left.boolean, right.boolean);
    } else {
        return error.InfixTypeMismatch;
    }
}

fn isAssignmentInfixExpression(infix: InfixExpression) bool {
    if (infix.left.* != .identifier) {
        return false;
    }
    return std.mem.eql(u8, "+=", infix.operator) or std.mem.eql(u8, "-=", infix.operator) or std.mem.eql(u8, "*=", infix.operator) or std.mem.eql(u8, "/=", infix.operator);
}

fn evalStringInfixExpression(self: *Interpreter, operator: []const u8, left: []const u8, right: []const u8) !Value {
    if (std.mem.eql(u8, "+", operator)) {
        const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left, right });
        return .{ .string = result };
    } else {
        return error.InfixUnknownOperator;
    }
}

fn evalNumberInfixExpression(operator: []const u8, left: f64, right: f64) !Value {
    if (std.mem.eql(u8, "+", operator) or std.mem.eql(u8, "+=", operator)) {
        return .{ .number = left + right };
    } else if (std.mem.eql(u8, "-", operator) or std.mem.eql(u8, "-=", operator)) {
        return .{ .number = left - right };
    } else if (std.mem.eql(u8, "*", operator) or std.mem.eql(u8, "*=", operator)) {
        return .{ .number = left * right };
    } else if (std.mem.eql(u8, "/", operator) or std.mem.eql(u8, "/=", operator)) {
        return .{ .number = left / right };
    } else if (std.mem.eql(u8, "<", operator)) {
        return .{ .boolean = left < right };
    } else if (std.mem.eql(u8, "<=", operator)) {
        return .{ .boolean = left <= right };
    } else if (std.mem.eql(u8, ">", operator)) {
        return .{ .boolean = left > right };
    } else if (std.mem.eql(u8, ">=", operator)) {
        return .{ .boolean = left >= right };
    } else {
        std.log.err("unhandled number infix operator `{s}`", .{operator});
        return error.InfixUnknownOperator;
    }
}

fn evalBooleanInfixExpression(operator: []const u8, left: bool, right: bool) !Value {
    if (std.mem.eql(u8, "and", operator)) {
        return .{ .boolean = left and right };
    } else if (std.mem.eql(u8, "or", operator)) {
        return .{ .boolean = left or right };
    } else {
        return error.InfixUnknownOperator;
    }
}

fn evalCallExpression(self: *Interpreter, call: CallExpression) !?Value {
    const fn_name = call.function.identifier;
    if (std.mem.eql(u8, "print", fn_name)) {
        for (call.arguments.items) |arg| {
            const value = try self.evalExpression(arg.*);
            if (value) |v| {
                try std.io.getStdOut().writer().print("{}", .{v});
            } else {
                try std.io.getStdOut().writeAll("nil");
            }
        }
        try std.io.getStdOut().writeAll("\n");
        return null;
    } else {
        std.debug.panic("calling non-builtin functions is not implemented", .{});
    }
}

pub const Environment = struct {
    items: std.StringHashMap(Value),

    pub fn init(allocator: Allocator) Environment {
        return .{ .items = std.StringHashMap(Value).init(allocator) };
    }

    pub fn get(self: Environment, key: []const u8) ?Value {
        return self.items.get(key);
    }

    pub fn set(self: *Environment, key: []const u8, value: Value) !void {
        try self.items.put(key, value);
    }
};

const Value = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .string => try writer.writeAll(self.string),
            .number => try writer.print("{}", .{self.number}),
            .boolean => try writer.print("{}", .{self.boolean}),
        }
    }
};
