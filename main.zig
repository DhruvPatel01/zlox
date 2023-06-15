const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const common = @import("common.zig");
const InterpreterError = @import("vm.zig").InterpretError;

fn repl(vm: *VM) void {
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

fn run_file(vm: *VM, path: [*:0]const u8) void {
    const source_status = std.fs.cwd().readFileAlloc(common.allocator, std.mem.span(path), std.math.maxInt(u32));
    if (source_status) |source| {
        defer common.allocator.free(source);
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
    var vm = VM{};
    vm.init();

    const args = std.os.argv;
    switch (args.len) {
        1 => repl(&vm),
        2 => run_file(&vm, args[1]),
        else => {
            std.debug.print("Usage: zlox [path]\n", .{});
            std.os.exit(64);
        },
    }
    vm.free();
}
