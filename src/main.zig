const std = @import("std");
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const lsp = @import("./lsp.zig");
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

    const stderr = std.io.getStdErr().writer();

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.next();
    if (args.next()) |command| {
        if (std.mem.eql(u8, "lsp", command)) {
            return try runLSPServer(allocator);
        } else if (std.mem.eql(u8, "version", command)) {
            try std.io.getStdOut().writeAll("0.0.0\n");
            return;
        }

        runFile(allocator, command) catch |err| {
            try stderr.print("ERROR: {}\n", .{err});
        };
        _ = arena.reset(.free_all);

        while (args.next()) |path| {
            runFile(allocator, path) catch |err| {
                try stderr.print("ERROR: {}\n", .{err});
            };
            _ = arena.reset(.free_all);
        }
    } else {
        try runREPL(allocator);
    }
}

fn runREPL(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    const stderr = std.io.getStdErr().writer();

    var env = Environment.init(allocator);

    while (true) {
        try stdout.writeAll(">> ");
        if (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |src| {
            const value = runSrc(allocator, src, &env) catch |err| {
                try stderr.print("ERROR: {}\n", .{err});
                continue;
            };
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

fn runLSPServer(allocator: Allocator) !void {
    var server = lsp.Server{ .allocator = allocator };
    try server.start();
}

pub const std_options: std.Options = .{
    .logFn = log,
};

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const allocator = std.heap.page_allocator;
    const file = getLogFile(allocator) catch |err| {
        std.debug.print("failed to get log file: {}\n", .{err});
        return;
    };
    defer file.close();

    const stat = file.stat() catch |err| {
        std.debug.print("failed to get stat of log file: {}\n", .{err});
        return;
    };
    file.seekTo(stat.size) catch |err| {
        std.debug.print("failed to seek to end of log file: {}\n", .{err});
        return;
    };

    var writer = file.writer();

    const now = std.time.timestamp();
    const formatted_now = formatUnixTimestamp(allocator, now) catch |err| {
        std.debug.print("failed to get current instant: {}\n", .{err});
        return;
    };
    defer allocator.free(formatted_now);

    writer.print("{s} ", .{formatted_now}) catch |err| {
        std.debug.print("failed to print log message timestamp: {}\n", .{err});
        return;
    };

    const prefix = "[" ++ comptime message_level.asText() ++ "] " ++ "(" ++ @tagName(scope) ++ ") ";
    writer.print(prefix ++ format ++ "\n", args) catch |err| {
        std.debug.print("failed to print log message: {}\n", .{err});
        return;
    };
}

fn getLogFile(allocator: Allocator) !std.fs.File {
    var log_filepath: []const u8 = "log.txt";
    var cmd_args = try std.process.argsWithAllocator(allocator);
    defer cmd_args.deinit();

    while (cmd_args.next()) |cmd_arg| {
        if (std.mem.eql(u8, "--log-file", cmd_arg)) {
            if (cmd_args.next()) |arg| {
                log_filepath = arg;
                break;
            }
        }
    }
    var cwd = std.fs.cwd();
    return cwd.openFile(log_filepath, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => try cwd.createFile(log_filepath, .{}),
        else => return err,
    };
}

fn formatUnixTimestamp(allocator: Allocator, secs: i64) ![]u8 {
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(secs) };
    const day_seconds = epoch.getDaySeconds();
    const epoch_day = epoch.getEpochDay();
    const year_day = epoch_day.calculateYearDay();

    const seconds = (@as(usize, day_seconds.getHoursIntoDay()) * 3600) + (@as(usize, day_seconds.getMinutesIntoHour()) * 60) + @as(usize, day_seconds.getSecondsIntoMinute());
    const minutes = seconds / 60;
    const hours = minutes / 60;

    const year = year_day.year;
    const month_and_day = year_day.calculateMonthDay();
    const month = month_and_day.month.numeric();
    const day = month_and_day.day_index + 1;

    const hour = hours % 24;
    const minute = minutes % 60;
    const second = seconds % 60;

    return std.fmt.allocPrint(allocator, "{d}/{d:0>2}/{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}", .{ year, month, day, hour, minute, second });
}

test {
    _ = @import("./Lexer.zig");
    _ = @import("./Parser.zig");
    _ = @import("./Interpreter.zig");
    _ = @import("./lsp.zig");
}
