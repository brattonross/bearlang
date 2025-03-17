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
    } else {
        try runREPL(allocator);
    }
}

fn runREPL(allocator: Allocator) !void {
    var stdout = std.io.getStdOut().writer();
    var stdin = std.io.getStdIn().reader();

    while (true) {
        try stdout.writeAll(">> ");
        if (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |src| {
            try runSrc(allocator, src);
        }
    }
}

fn runFile(allocator: Allocator, file_path: [:0]const u8) !void {
    var cwd = std.fs.cwd();
    const src = try cwd.readFileAlloc(allocator, file_path, 1024);
    try runSrc(allocator, src);
}

fn runSrc(allocator: Allocator, src: []const u8) !void {
    var lexer = Lexer.init(src);
    var parser = try Parser.init(allocator, &lexer);
    var env = Environment.init(allocator);
    var interpreter = Interpreter.init(allocator, &parser, &env);
    _ = try interpreter.run();
}

test {
    _ = @import("./Lexer.zig");
    _ = @import("./Parser.zig");
    _ = @import("./Interpreter.zig");
}
