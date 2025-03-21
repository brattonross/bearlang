const std = @import("std");
const Allocator = std.mem.Allocator;
const AnyReader = std.io.AnyReader;
const AnyWriter = std.io.AnyWriter;
const eql = std.mem.eql;

pub const Server = struct {
    allocator: Allocator,
    stdin: AnyReader = std.io.getStdIn().reader().any(),
    stdout: AnyWriter = std.io.getStdOut().writer().any(),

    pub fn start(self: Server) !void {
        std.log.info("starting LSP server...", .{});
        var request_arena = std.heap.ArenaAllocator.init(self.allocator);
        const allocator = request_arena.allocator();

        while (true) {
            const message = try self.readMessage(allocator);
            const request = try std.json.parseFromSliceLeaky(Request, allocator, message, .{ .ignore_unknown_fields = true });

            std.log.info("received message with method \"{s}\"", .{request.method});

            if (eql(u8, "initialize", request.method)) {
                const initialize_request = try std.json.parseFromSliceLeaky(InitializeRequest, allocator, message, .{ .ignore_unknown_fields = true });
                std.log.info("connected to {s} {?s}", .{ initialize_request.params.clientInfo.name, initialize_request.params.clientInfo.version });

                const response = InitializeResponse{
                    .jsonrpc = "2.0",
                    .id = initialize_request.id,
                    .result = .{
                        .capabilities = .{},
                        .serverInfo = .{
                            .name = "bearls",
                            .version = "0.0.0",
                        },
                    },
                };
                const encoded_response = try encode(allocator, response);
                std.log.debug(">> {s}", .{encoded_response});
                try self.stdout.writeAll(encoded_response);
            }

            if (!request_arena.reset(.retain_capacity)) {
                std.log.warn("failed to reset request arena", .{});
            }
        }
    }

    fn readMessage(self: Server, allocator: Allocator) ![]u8 {
        var content_length: usize = 0;

        while (true) {
            var bytes = try self.stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 4096) orelse return error.FailedToReadHeader;
            if (bytes.len == 1) {
                break; // end of header
            }
            bytes = bytes[0 .. bytes.len - 1];
            std.log.debug("<< {s}", .{bytes});
            if (std.mem.startsWith(u8, bytes, "Content-Length: ")) {
                const content_length_str = bytes[16..bytes.len];
                content_length = try std.fmt.parseInt(usize, content_length_str, 10);
            }
        }

        const buf = try allocator.alloc(u8, content_length);
        errdefer allocator.free(buf);

        _ = try self.stdin.read(buf);
        const content = buf[0..content_length];
        std.log.debug("<< {s}", .{content});

        return content;
    }

    fn encode(allocator: Allocator, message: anytype) ![]u8 {
        const json = try std.json.stringifyAlloc(allocator, message, .{});
        errdefer allocator.free(json);
        const encoded = try std.fmt.allocPrint(allocator, "Content-Length: {d}\r\n\r\n{s}", .{ json.len, json });
        errdefer allocator.free(encoded);
        return encoded;
    }
};

const Request = struct {
    method: []const u8,
};

const InitializeRequest = struct {
    id: usize,
    params: struct {
        clientInfo: struct {
            name: []const u8,
            version: ?[]const u8 = null,
        },
    },
};

const InitializeResponse = struct {
    jsonrpc: []const u8,
    id: usize,
    result: struct {
        capabilities: struct {
            textDocumentSync: u2 = 1,
        },
        serverInfo: ?struct {
            name: []const u8,
            version: []const u8,
        } = null,
    },
};
