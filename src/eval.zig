const std = @import("std");
const Parser = @import("./Parser.zig");
const Expression = Parser.Expression;
const CallExpression = Parser.CallExpression;
const Statement = Parser.Statement;

pub fn eval(parser: *Parser) !?Value {
    var value: ?Value = null;
    while (try parser.next()) |statement| {
        value = try evalStatement(statement.*);
    }
    return value;
}

fn evalStatement(statement: Statement) !?Value {
    return switch (statement) {
        .expression => try evalExpression(statement.expression.*),
        else => std.debug.panic("unhandled {}", .{statement}),
    };
}

fn evalExpression(expression: Expression) anyerror!?Value {
    return switch (expression) {
        .call => try evalCallExpression(expression.call),
        .string => evalStringExpression(expression.string),
        else => std.debug.panic("unimplemented: {}", .{expression}),
    };
}

fn evalStringExpression(string: []const u8) Value {
    return .{ .string = string };
}

fn evalCallExpression(call: CallExpression) !?Value {
    const fn_name = call.function.identifier;
    if (std.mem.eql(u8, "print", fn_name)) {
        for (call.arguments.items) |arg| {
            const value = try evalExpression(arg.*);
            if (value) |v| {
                try std.io.getStdOut().writer().print("{}\n", .{v});
            } else {
                try std.io.getStdOut().writeAll("nil\n");
            }
        }
        return null;
    } else {
        std.debug.panic("calling non-builtin functions is not implemented", .{});
    }
}

const Value = union(enum) {
    string: []const u8,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .string => try writer.writeAll(self.string),
        }
    }
};
