const std = @import("std");

const object = @import("object.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const common = @import("common.zig");
const compiler = @import("compiler.zig");

const MAX_FRAMES = 64;
const STACK_MAX = 64 * 256;

pub const InterpretError = error{ CompileError, RuntimeError };

pub const VM = struct {
    frame_count: usize = 0,
    chunk: *Chunk = undefined,
    ip: [*]u8 = undefined,
    stack: [STACK_MAX]Value = undefined,
    stack_top: [*]Value = undefined,
    objects: ?*object.Obj = null,
};

fn reset_stack() void {
    vm.stack_top = &vm.stack;
}

fn runtime_error(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
    const instruction = @ptrToInt(vm.ip) - @ptrToInt(vm.chunk.code.items.ptr) - 1;
    const line = vm.chunk.lines.items[instruction];
    std.debug.print("[line {}] in script\n", .{line});
    reset_stack();
}

fn push(value: Value) void {
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

fn pop() Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

fn peek(distance: usize) Value {
    const ptr = vm.stack_top - 1 - distance;
    return ptr[0];
}

pub fn init() void {
    reset_stack();
    vm.objects = null;
}

fn free_objects() void {
    var obj = vm.objects;
    while (obj != null) {
        var next = obj.?.next;
        obj.?.free_object();
        obj = next;
    }
}

pub fn free() void {
    free_objects();
}

pub fn interpret(source: []const u8) InterpretError!void {
    var chunk = Chunk.init();
    defer chunk.free();

    if (!compiler.compile(source, &chunk))
        return error.CompileError;

    vm.chunk = &chunk;
    vm.ip = chunk.code.items.ptr;
    return run();
}

fn concatenate() void {
    const b = @fieldParentPtr(object.ObjString, "obj", pop().Obj);
    const a = @fieldParentPtr(object.ObjString, "obj", pop().Obj);

    var final = common.allocator.alloc(u8, a.chars.len + b.chars.len) catch unreachable;
    @memcpy(final[0..a.chars.len], a.chars);
    @memcpy(final[a.chars.len..], b.chars);

    const result = Value{ .Obj = &object.ObjString.take(final).obj };
    push(result);
}

fn run() InterpretError!void {
    while (true) {
        if (comptime common.debug_trace_execution) {
            std.debug.print("          ", .{});
            const value_stack_total_len = @ptrToInt(vm.stack_top) - @ptrToInt(&vm.stack);
            const value_size: usize = @sizeOf(Value);
            const value_stack_len = value_stack_total_len / value_size;

            for (0..value_stack_len) |i| {
                std.debug.print("[ ", .{});
                vm.stack[i].print();
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});

            const top = @ptrToInt(vm.ip);
            const base = @ptrToInt(vm.chunk.code.items.ptr);
            _ = vm.chunk.disassemble_instruction(top - base);
        }
        var instruction = @intToEnum(OpCode, read_byte());
        switch (instruction) {
            .OP_CONSTANT => {
                const constant = read_constant();
                push(constant);
            },
            .OP_NIL => push(Value.nil()),
            .OP_TRUE => push(Value.boolean(true)),
            .OP_FALSE => push(Value.boolean(false)),
            .OP_POP => unreachable,
            .OP_GET_LOCAL => unreachable,
            .OP_SET_LOCAL => unreachable,
            .OP_DEFINE_GLOBAL => unreachable,
            .OP_GET_GLOBAL => unreachable,
            .OP_SET_GLOBAL => unreachable,
            .OP_SET_UPVALUE => unreachable,
            .OP_GET_UPVALUE => unreachable,
            .OP_SET_PROPERTY => unreachable,
            .OP_GET_PROPERTY => unreachable,
            .OP_GET_SUPER => unreachable,
            .OP_EQUAL => {
                const b = pop();
                const a = pop();
                push(Value.boolean(a.values_equal(b)));
            },
            .OP_NOT_EQUAL => {
                const b = pop();
                const a = pop();
                push(Value.boolean(!a.values_equal(b)));
            },
            .OP_GREATER => try binary_op(.OP_GREATER),
            .OP_GREATER_EQUAL => try binary_op(.OP_GREATER_EQUAL),
            .OP_LESS => try binary_op(.OP_LESS),
            .OP_LESS_EQUAL => try binary_op(.OP_LESS_EQUAL),
            .OP_ADD => {
                if (object.is_string(peek(0)) and object.is_string(peek(1))) {
                    concatenate();
                } else if (peek(0) == Value.Number and peek(1) == Value.Number) {
                    const b = pop().Number;
                    const a = pop().Number;
                    push(Value.number(a + b));
                } else {
                    runtime_error("Operands must be two numbers or two strings.", .{});
                    return error.RuntimeError;
                }
            },
            .OP_SUBTRACT => try binary_op(.OP_SUBTRACT),
            .OP_MULTIPLY => try binary_op(.OP_MULTIPLY),
            .OP_DIVIDE => try binary_op(.OP_DIVIDE),
            .OP_NOT => push(Value.boolean(pop().is_falsy())),
            .OP_NEGATE => {
                const ptr = vm.stack_top - 1;
                switch (ptr[0]) {
                    .Number => ptr[0].Number = -ptr[0].Number,
                    else => {
                        runtime_error("Oprand must be a number.", .{});
                        return error.RuntimeError;
                    },
                }
            },
            .OP_PRINT => {
                const value = pop();
                value.print();
                std.debug.print("\n", .{});
                return;
            },
            .OP_JUMP => unreachable,
            .OP_JUMP_IF_FALSE => unreachable,
            .OP_LOOP => unreachable,
            .OP_CALL => unreachable,
            .OP_INVOKE => unreachable,
            .OP_SUPER_INVOKE => unreachable,
            .OP_CLOSURE => unreachable,
            .OP_CLOSE_UPVALUE => unreachable,
            .OP_RETURN => {
                const value = pop();
                value.print();
                std.debug.print("\n", .{});
                return;
            },
            .OP_CLASS => unreachable,
            .OP_INHERIT => unreachable,
            .OP_METHOD => unreachable,
        }
    }
}

inline fn read_byte() u8 {
    defer vm.ip += 1;
    return vm.ip[0];
}

inline fn read_constant() Value {
    return vm.chunk.values.items[read_byte()];
}

inline fn binary_op(comptime op: OpCode) InterpretError!void {
    if (peek(0) != Value.Number or peek(1) != Value.Number) {
        runtime_error("Operands must be numbers.", .{});
        return error.RuntimeError;
    }
    const b = pop().Number;
    const a = pop().Number;

    const result = switch (op) {
        .OP_ADD => Value.number(a + b),
        .OP_SUBTRACT => Value.number(a - b),
        .OP_MULTIPLY => Value.number(a * b),
        .OP_DIVIDE => Value.number(a / b),
        .OP_GREATER => Value.boolean(a > b),
        .OP_GREATER_EQUAL => Value.boolean(a >= b),
        .OP_LESS => Value.boolean(a < b),
        .OP_LESS_EQUAL => Value.boolean(a <= b),
        else => unreachable,
    };
    push(result);
}

pub var vm = VM{};
