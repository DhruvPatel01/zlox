const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const vm = @import("vm.zig");
const common = @import("common.zig");
const InterpreterError = @import("vm.zig").InterpretError;

fn repl() void {
    const reader = std.io.getStdIn().reader();

    var buffer: [1024]u8 = undefined;
    while (true) {
        std.debug.print("> ", .{});
        const status = reader.readUntilDelimiterOrEof(&buffer, '\n');
        if (status) |source| {
            if (source == null) {
                std.debug.print("\nBye!\n", .{});
                break;
            }
            vm.interpret(source.?) catch {};
        } else |_| {
            break;
        }
    }
}

fn run_file(path: [*:0]const u8) void {
    const source_status = std.fs.cwd().readFileAlloc(std.heap.c_allocator, std.mem.span(path), std.math.maxInt(u32));
    if (source_status) |source| {
        defer std.heap.c_allocator.free(source);
        vm.interpret(source) catch |err| switch (err) {
            InterpreterError.CompileError => {
                std.os.exit(65);
            },
            InterpreterError.RuntimeError => {
                std.os.exit(70);
            },
            else => unreachable,
        };
    } else |_| {
        std.debug.print("Could not open file \"{s}\".\n", .{path});
        std.os.exit(74);
    }
}

pub fn main() !void {
    vm.init();

    const args = std.os.argv;
    switch (args.len) {
        1 => repl(),
        2 => run_file(args[1]),
        else => {
            std.debug.print("Usage: zlox [path]\n", .{});
            std.os.exit(64);
        },
    }
    vm.free();
}
