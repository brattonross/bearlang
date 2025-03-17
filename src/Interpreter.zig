const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("./Parser.zig");

// expressions
const Expression = Parser.Expression;
const PrefixExpression = Parser.PrefixExpression;
const InfixExpression = Parser.InfixExpression;
const CallExpression = Parser.CallExpression;
const IfExpression = Parser.IfExpression;

// statements
const Statement = Parser.Statement;
const LetStatement = Parser.LetStatement;
const ForStatement = Parser.ForStatement;
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

pub fn run(self: *Interpreter) !Value {
    var value = Value{ .void = {} };
    while (try self.parser.next()) |statement| {
        value = try self.evalStatement(statement.*);
    }
    return value;
}

fn evalStatement(self: *Interpreter, statement: Statement) anyerror!Value {
    return switch (statement) {
        .let => try self.evalLetStatement(statement.let),
        .@"for" => try self.evalForStatement(statement.@"for"),
        .block => try self.evalBlockStatement(statement.block),
        .expression => try self.evalExpression(statement.expression.*),
        else => .{ .void = {} },
    };
}

fn evalLetStatement(self: *Interpreter, let: LetStatement) !Value {
    const value = try self.evalExpression(let.value.*);
    if (value == .void) {
        return error.InvalidLetStatement;
    }

    try self.environment.set(let.name, value);
    return .{ .void = {} };
}

fn evalForStatement(self: *Interpreter, statement: ForStatement) !Value {
    if (statement.initial) |initial| {
        // TODO: ignored?
        _ = try self.evalStatement(initial.*);
    }

    for_loop: while (true) {
        const should_iter = if (statement.condition) |condition| isTruthy(try self.evalExpression(condition.*)) else true;
        if (should_iter) {
            for (statement.block.statements.items) |s| {
                if (s.* == .@"break") {
                    break :for_loop;
                }
                _ = try self.evalStatement(s.*);
            }
        } else {
            break;
        }

        if (statement.after) |after| {
            _ = try self.evalExpression(after.*);
        }
    }

    return .{ .void = {} };
}

fn evalBlockStatement(self: *Interpreter, block: BlockStatement) !Value {
    var value = Value{ .void = {} };
    for (block.statements.items) |statement| {
        value = try self.evalStatement(statement.*);
    }
    return value;
}

fn isTruthy(exp: Value) bool {
    return switch (exp) {
        .boolean => exp.boolean == true,
        else => true,
    };
}

fn evalExpression(self: *Interpreter, expression: Expression) anyerror!Value {
    return switch (expression) {
        .identifier => try self.evalIdentifier(expression.identifier),
        .string => .{ .string = expression.string },
        .number => .{ .number = expression.number },
        .boolean => .{ .boolean = expression.boolean },
        .prefix => try self.evalPrefixExpression(expression.prefix),
        .infix => try self.evalInfixExpression(expression.infix),
        .call => try self.evalCallExpression(expression.call),
        .@"if" => try self.evalIfExpression(expression.@"if"),
    };
}

fn evalIdentifier(self: *Interpreter, identifier: []const u8) !Value {
    return self.environment.get(identifier) orelse error.UnknownIdentifier;
}

fn evalPrefixExpression(self: *Interpreter, prefix: PrefixExpression) !Value {
    const right = try self.evalExpression(prefix.right.*);
    if (std.mem.eql(u8, "!", prefix.operator)) {
        return try evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, "-", prefix.operator)) {
        return try evalMinusPrefixOperatorExpression(right);
    } else {
        return error.UnknownPrefixOperator;
    }
}

fn evalBangOperatorExpression(right: Value) !Value {
    return switch (right) {
        .boolean => .{ .boolean = !right.boolean },
        else => .{ .boolean = false },
    };
}

fn evalMinusPrefixOperatorExpression(right: ?Value) !Value {
    if (right == null or right.? != .number) {
        return error.UnknownOperator;
    }
    return .{ .number = -right.?.number };
}

fn evalInfixExpression(self: *Interpreter, infix: InfixExpression) !Value {
    const left = try self.evalExpression(infix.left.*);
    const right = try self.evalExpression(infix.right.*);

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
    } else if (std.mem.eql(u8, "==", operator)) {
        return .{ .boolean = left == right };
    } else if (std.mem.eql(u8, "!=", operator)) {
        return .{ .boolean = left != right };
    } else if (std.mem.eql(u8, "<", operator)) {
        return .{ .boolean = left < right };
    } else if (std.mem.eql(u8, "<=", operator)) {
        return .{ .boolean = left <= right };
    } else if (std.mem.eql(u8, ">", operator)) {
        return .{ .boolean = left > right };
    } else if (std.mem.eql(u8, ">=", operator)) {
        return .{ .boolean = left >= right };
    } else if (std.mem.eql(u8, "%", operator) or std.mem.eql(u8, "%=", operator)) {
        return .{ .number = @mod(left, right) };
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

fn evalCallExpression(self: *Interpreter, call: CallExpression) !Value {
    var stdout = std.io.getStdOut().writer();

    const fn_name = call.function.identifier;
    if (std.mem.eql(u8, "print", fn_name)) {
        for (call.arguments.items) |arg| {
            const value = try self.evalExpression(arg.*);
            if (value == .void) {
                return error.InvalidArgument;
            }
            try stdout.print("{}", .{value});
        }
        try stdout.writeAll("\n");
        return .{ .void = {} };
    } else {
        std.debug.panic("calling non-builtin functions is not implemented", .{});
    }
}

fn evalIfExpression(self: *Interpreter, ife: IfExpression) !Value {
    if (isTruthy(try self.evalExpression(ife.condition.*))) {
        return try self.evalBlockStatement(ife.consequence);
    } else if (ife.alternative) |alt| switch (alt) {
        .block => return try self.evalBlockStatement(alt.block),
        .expression => return try self.evalIfExpression(alt.expression.@"if"),
    } else {
        return .{ .void = {} };
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

pub const Value = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    void: void,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .string => try writer.writeAll(self.string),
            .number => try writer.print("{d}", .{self.number}),
            .boolean => try writer.print("{}", .{self.boolean}),
            else => {
                // TODO:
            },
        }
    }
};
