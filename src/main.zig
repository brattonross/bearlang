const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("./Lexer.zig");
const Parser = @import("./Parser.zig");
const Interpreter = @import("./Interpreter.zig");
const Environment = Interpreter.Environment;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    const stdout = std.io.getStdOut().writer().any();
    const stdin = std.io.getStdIn().reader().any();

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.next();
    if (args.next()) |file_path| {
        try runFile(allocator, stdout, file_path);
    } else {
        try runREPL(allocator, stdin, stdout);
    }
}

fn runREPL(allocator: Allocator, stdin: std.io.AnyReader, stdout: std.io.AnyWriter) !void {
    var env = Environment.init(allocator);

    while (true) {
        try stdout.writeAll(">> ");
        if (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |src| {
            const value = try runSrc(allocator, src, &env);
            try stdout.print("{?}\n", .{value});
        }
    }
}

fn runFile(allocator: Allocator, stdout: std.io.AnyWriter, file_path: [:0]const u8) !void {
    var cwd = std.fs.cwd();
    const src = try cwd.readFileAlloc(allocator, file_path, 1024);
    var env = Environment.init(allocator);
    const value = try runSrc(allocator, src, &env);
    try stdout.print("{?}\n", .{value});
}

fn runSrc(allocator: Allocator, src: []const u8, env: *Environment) !?Interpreter.Value {
    var lexer = Lexer.init(src);
    var parser = try Parser.init(allocator, &lexer);
    var interpreter = Interpreter.init(allocator, &parser, env);
    return try interpreter.run();
}

test {
    _ = @import("./Lexer.zig");
    _ = @import("./Parser.zig");
    _ = @import("./Interpreter.zig");
}
