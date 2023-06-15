const std = @import("std");
const scanner_ = @import("scanner.zig");
const Scanner = scanner_.Scanner;
const TokenType = scanner_.TokenType;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: usize = std.math.maxInt(usize);
    while (true) {
        const token = scanner.scan_token();
        if (line != token.line) {
            line = token.line;
            std.debug.print("{d: >4} ", .{line});
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{} '{s}'\n", .{ token.type, token.lexeme });

        if (token.type == .TOKEN_EOF) break;
    }
}
