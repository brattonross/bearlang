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

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.next();
    if (args.next()) |file_path| {
        try runFile(allocator, file_path);
        _ = arena.reset(.free_all);

        while (args.next()) |path| {
            try runFile(allocator, path);
            _ = arena.reset(.free_all);
        }
    } else {
        try runREPL(allocator);
    }
}

fn runREPL(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    var env = Environment.init(allocator);

    while (true) {
        try stdout.writeAll(">> ");
        if (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |src| {
            const value = try runSrc(allocator, src, &env);
            if (value != .void) {
                try stdout.print("{}\n", .{value});
            }
        }
    }
}

fn runFile(allocator: Allocator, file_path: [:0]const u8) !void {
    var cwd = std.fs.cwd();
    const src = try cwd.readFileAlloc(allocator, file_path, 1024);
    var env = Environment.init(allocator);
    _ = try runSrc(allocator, src, &env);
}

fn runSrc(allocator: Allocator, src: []const u8, env: *Environment) !Interpreter.Value {
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
