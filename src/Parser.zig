const Lexer = @import("./Lexer.zig");

const Parser = @This();

lexer: *Lexer,

pub fn init(lexer: *Lexer) Parser {
    return .{ .lexer = lexer };
}
