const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("./Parser.zig");
const Identifier = Parser.Identifier;

// expressions
const Expression = Parser.Expression;
const PrefixExpression = Parser.PrefixExpression;
const InfixExpression = Parser.InfixExpression;
const CallExpression = Parser.CallExpression;
const IfExpression = Parser.IfExpression;
const FunctionExpression = Parser.FunctionExpression;
const StructExpression = Parser.StructExpression;
const AccessorExpression = Parser.AccessorExpression;

// statements
const Statement = Parser.Statement;
const LetStatement = Parser.LetStatement;
const ForStatement = Parser.ForStatement;
const FunctionStatement = Parser.FunctionStatement;
const ReturnStatement = Parser.ReturnStatement;
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
        value = try self.evalStatement(statement.*, self.environment);
    }
    return value;
}

fn evalStatement(self: *Interpreter, statement: Statement, env: *Environment) anyerror!Value {
    return switch (statement) {
        .let => try self.evalLetStatement(statement.let, env),
        .@"for" => try self.evalForStatement(statement.@"for", env),
        .block => try self.evalBlockStatement(statement.block, env),
        .expression => try self.evalExpression(statement.expression.*, env),
        .function => try self.evalFunctionStatement(statement.function, env),
        .@"return" => try self.evalReturnStatement(statement.@"return", env),
        else => .{ .void = {} },
    };
}

fn evalLetStatement(self: *Interpreter, let: LetStatement, env: *Environment) !Value {
    const value = try self.evalExpression(let.value.*, env);
    if (value == .void) {
        std.debug.print("invalid let statement: expected value {} to return non-void value.\n", .{let.value.*});
        return error.InvalidLetStatement;
    }

    try env.set(let.name.lexeme, value);
    return .{ .void = {} };
}

fn evalForStatement(self: *Interpreter, statement: ForStatement, env: *Environment) !Value {
    if (statement.initial) |initial| {
        // TODO: ignored?
        _ = try self.evalStatement(initial.*, env);
    }

    for_loop: while (true) {
        const should_iter = if (statement.condition) |condition| blk: {
            break :blk isTruthy(try self.evalExpression(condition.*, env));
        } else true;
        if (should_iter) {
            for (statement.block.statements.items) |s| {
                if (s.* == .@"break") {
                    break :for_loop;
                }
                _ = try self.evalStatement(s.*, env);
            }
        } else {
            break;
        }

        if (statement.after) |after| {
            _ = try self.evalExpression(after.*, env);
        }
    }

    return .{ .void = {} };
}

fn evalFunctionStatement(_: *Interpreter, function: FunctionStatement, env: *Environment) !Value {
    const fn_value = FunctionValue{
        .name = function.name,
        .parameters = function.parameters,
        .body = function.body,
        .env = env,
    };
    try env.set(function.name.lexeme, .{ .function = fn_value });

    return .{ .void = {} };
}

fn evalReturnStatement(self: *Interpreter, ret: ReturnStatement, env: *Environment) !Value {
    const value = try self.allocator.create(Value);
    value.* = try self.evalExpression(ret.return_value.*, env);
    return .{ .@"return" = value };
}

fn evalBlockStatement(self: *Interpreter, block: BlockStatement, env: *Environment) !Value {
    for (block.statements.items) |statement| {
        const value = try self.evalStatement(statement.*, env);
        if (value == .@"return") {
            return value;
        }
    }
    return .{ .void = {} };
}

fn isTruthy(exp: Value) bool {
    return switch (exp) {
        .boolean => exp.boolean == true,
        else => true,
    };
}

fn evalExpression(self: *Interpreter, expression: Expression, env: *Environment) anyerror!Value {
    return switch (expression) {
        .identifier => try self.evalIdentifier(expression.identifier, env),
        .string => .{ .string = expression.string },
        .number => .{ .number = expression.number },
        .boolean => .{ .boolean = expression.boolean },
        .prefix => try self.evalPrefixExpression(expression.prefix, env),
        .infix => try self.evalInfixExpression(expression.infix, env),
        .call => try self.evalCallExpression(expression.call, env),
        .@"if" => try self.evalIfExpression(expression.@"if", env),
        .function => self.evalFunctionExpression(expression.function, env),
        .@"struct" => try self.evalStructExpression(expression.@"struct", env),
        .accessor => try self.evalAccessorExpression(expression.accessor, env),
    };
}

fn evalIdentifier(_: *Interpreter, identifier: Identifier, env: *Environment) !Value {
    return env.get(identifier.lexeme) orelse error.UnknownIdentifier;
}

fn evalPrefixExpression(self: *Interpreter, prefix: PrefixExpression, env: *Environment) !Value {
    const right = try self.evalExpression(prefix.right.*, env);
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

fn evalInfixExpression(self: *Interpreter, infix: InfixExpression, env: *Environment) !Value {
    const left = try self.evalExpression(infix.left.*, env);
    const right = try self.evalExpression(infix.right.*, env);

    if (left == .string and right == .string) {
        return try self.evalStringInfixExpression(infix.operator, left.string, right.string);
    } else if (left == .number and right == .number) {
        const value = try evalNumberInfixExpression(infix.operator, left.number, right.number);
        if (isAssignmentInfixExpression(infix)) {
            try env.set(infix.left.identifier.lexeme, value);
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

fn evalFunctionExpression(_: *Interpreter, function: FunctionExpression, env: *Environment) Value {
    const value = Value{
        .function = .{
            .name = null,
            .parameters = function.parameters,
            .body = function.body,
            .env = env,
        },
    };
    return value;
}

fn evalCallExpression(self: *Interpreter, call: CallExpression, env: *Environment) !Value {
    var stdout = std.io.getStdOut().writer();

    const fn_name = call.function.identifier;

    // TODO: handle shadowing builtin functions
    if (env.get(fn_name.lexeme)) |value| {
        if (value != .function) {
            return error.IdentifierNotAFunction;
        }
        const function = value.function;

        var arguments = std.ArrayList(Value).init(self.allocator);
        for (call.arguments.items) |arg| {
            const arg_value = try self.evalExpression(arg.*, env);
            try arguments.append(arg_value);
        }

        var fn_env = Environment.initEnclosed(self.allocator, function.env);
        for (function.parameters.items, 0..) |param, i| {
            try fn_env.set(param.identifier.lexeme, arguments.items[i]);
        }

        const result = try self.evalBlockStatement(function.body, &fn_env);
        if (result == .@"return") {
            return result.@"return".*;
        }
        return .{ .void = {} };
    } else if (std.mem.eql(u8, "print", fn_name.lexeme)) {
        for (call.arguments.items) |arg| {
            const value = try self.evalExpression(arg.*, env);
            if (value == .void) {
                return error.InvalidArgument;
            }
            try stdout.print("{}", .{value});
        }
        try stdout.writeAll("\n");
        return .{ .void = {} };
    } else {
        return error.UnknownIdentifier;
    }
}

fn evalIfExpression(self: *Interpreter, ife: IfExpression, env: *Environment) !Value {
    if (isTruthy(try self.evalExpression(ife.condition.*, env))) {
        return try self.evalBlockStatement(ife.consequence, env);
    } else if (ife.alternative) |alt| switch (alt) {
        .block => return try self.evalBlockStatement(alt.block, env),
        .expression => return try self.evalIfExpression(alt.expression.@"if", env),
    } else {
        return .{ .void = {} };
    }
}

fn evalStructExpression(self: *Interpreter, @"struct": StructExpression, env: *Environment) !Value {
    var map = std.StringHashMap(Value).init(self.allocator);

    var iter = @"struct".map.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.identifier.lexeme;
        const value = try self.evalExpression(entry.value_ptr.*.*, env);
        try map.put(key, value); // TODO: do we care about clobbering
    }

    return .{ .@"struct" = .{ .map = map } };
}

fn evalAccessorExpression(self: *Interpreter, accessor: AccessorExpression, env: *Environment) !Value {
    const parent_value = try self.evalExpression(accessor.parent.*, env);
    const key = accessor.key.identifier.lexeme; // TODO: assumes identifier
    return switch (parent_value) {
        .@"struct" => |@"struct"| @"struct".map.get(key) orelse error.UnknownStructKey,
        else => std.debug.panic("attempting to access property of unhandled type {s}", .{@tagName(parent_value)}),
    };
}

pub const Environment = struct {
    items: std.StringHashMap(Value),
    outer: ?*Environment = null,

    pub fn init(allocator: Allocator) Environment {
        return .{ .items = std.StringHashMap(Value).init(allocator) };
    }

    pub fn initEnclosed(allocator: Allocator, outer: *Environment) Environment {
        var env = Environment.init(allocator);
        env.outer = outer;
        return env;
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
    function: FunctionValue,
    @"return": *Value,
    @"struct": StructValue,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) anyerror!void {
        _ = fmt;
        _ = options;
        switch (self) {
            .string => try writer.print("{s}", .{self.string}),
            .number => try writer.print("{d}", .{self.number}),
            .boolean => try writer.print("{}", .{self.boolean}),
            .function => try writer.print("{}", .{self.function}),
            .@"return" => try writer.print("{}", .{self.@"return"}),
            .void => try writer.writeAll("(void)"),
            .@"struct" => try writer.print("{}", .{self.@"struct"}),
        }
    }
};

const FunctionValue = struct {
    name: ?Identifier,
    parameters: std.ArrayList(*Expression),
    body: BlockStatement,
    env: *Environment,

    pub fn format(self: FunctionValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("fn");
        if (self.name) |name| {
            try writer.print(" {}", .{name});
        }
        try writer.writeAll("(");
        for (self.parameters.items, 0..) |param, i| {
            try writer.print("{}", .{param.identifier});
            if (i < self.parameters.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(") {{\n");
        for (self.body.statements.items) |statement| {
            try writer.print("{}", .{statement});
            try writer.writeAll("\n");
        }
        try writer.writeAll("}}");
    }
};

const StructValue = struct {
    map: std.StringHashMap(Value),

    pub fn format(self: StructValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("{{");
        var iter = self.map.iterator();
        while (iter.next()) |entry| {
            try writer.print("{s}", .{entry.key_ptr.*});
            try writer.writeAll(": ");
            try writer.print("{}", .{entry.value_ptr.*});
            if (iter.index < self.map.count() - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("}}");
    }
};

const Lexer = @import("./Lexer.zig");

test "accessor" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(
        \\let car = { wheels = 4 }
        \\car.wheels
    );
    var parser = try Parser.init(allocator, &lexer);
    var env = Environment.init(allocator);
    var interpreter = Interpreter.init(allocator, &parser, &env);

    const value = try interpreter.run();

    try std.testing.expectEqual(4, value.number);
}
