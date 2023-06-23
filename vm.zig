const std = @import("std");

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

    fn reset_stack(self: *VM) void {
        self.stack_top = &self.stack;
    }

    fn runtime_error(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        const instruction = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.lines.items[instruction];
        std.debug.print("[line {}] in script\n", .{line});
        self.reset_stack();
    }

    fn push(self: *VM, value: Value) void {
        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn peek(self: *VM, distance: usize) Value {
        const ptr = self.stack_top - 1 - distance;
        return ptr[0];
    }

    pub fn init(vm: *VM) void {
        vm.reset_stack();
    }

    pub fn free(vm: *VM) void {
        _ = vm;
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
        var chunk = Chunk.init();
        defer chunk.free();

        if (!compiler.compile(source, &chunk))
            return error.CompileError;

        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;
        return self.run();
    }

    fn run(self: *VM) InterpretError!void {
        while (true) {
            if (comptime common.debug_trace_execution) {
                std.debug.print("          ", .{});
                const value_stack_total_len = @ptrToInt(self.stack_top) - @ptrToInt(&self.stack);
                const value_size: usize = @sizeOf(Value);
                const value_stack_len = value_stack_total_len / value_size;

                for (0..value_stack_len) |i| {
                    std.debug.print("[ ", .{});
                    self.stack[i].print();
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});

                const top = @ptrToInt(self.ip);
                const base = @ptrToInt(self.chunk.code.items.ptr);
                _ = self.chunk.disassemble_instruction(top - base);
            }
            var instruction = @intToEnum(OpCode, self.read_byte());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                .OP_NIL => self.push(Value.nil()),
                .OP_TRUE => self.push(Value.boolean(true)),
                .OP_FALSE => self.push(Value.boolean(false)),
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
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.boolean(a.values_equal(b)));
                },
                .OP_NOT_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.boolean(!a.values_equal(b)));
                },
                .OP_GREATER => try self.binary_op(.OP_GREATER),
                .OP_GREATER_EQUAL => try self.binary_op(.OP_GREATER_EQUAL),
                .OP_LESS => try self.binary_op(.OP_LESS),
                .OP_LESS_EQUAL => try self.binary_op(.OP_LESS_EQUAL),
                .OP_ADD => try self.binary_op(.OP_ADD),
                .OP_SUBTRACT => try self.binary_op(.OP_SUBTRACT),
                .OP_MULTIPLY => try self.binary_op(.OP_MULTIPLY),
                .OP_DIVIDE => try self.binary_op(.OP_DIVIDE),
                .OP_NOT => self.push(Value.boolean(self.pop().is_falsy())),
                .OP_NEGATE => {
                    const ptr = self.stack_top - 1;
                    switch (ptr[0]) {
                        .Number => ptr[0].Number = -ptr[0].Number,
                        else => {
                            self.runtime_error("Oprand must be a number.", .{});
                            return error.RuntimeError;
                        },
                    }
                },
                .OP_PRINT => {
                    const value = self.pop();
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
                    const value = self.pop();
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

    inline fn read_byte(self: *VM) u8 {
        defer self.ip += 1;
        return self.ip[0];
    }

    inline fn read_constant(self: *VM) Value {
        return self.chunk.values.items[self.read_byte()];
    }

    inline fn binary_op(self: *VM, comptime op: OpCode) InterpretError!void {
        if (self.peek(0) != Value.Number or self.peek(1) != Value.Number) {
            self.runtime_error("Operands must be numbers.", .{});
            return error.RuntimeError;
        }
        const b = self.pop().Number;
        const a = self.pop().Number;

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
        self.push(result);
    }
};
