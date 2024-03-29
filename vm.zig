const std = @import("std");

const object = @import("object.zig");
const ObjType = object.ObjType;
const gc = @import("gc.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const common = @import("common.zig");
const compiler = @import("compiler.zig");
const Table = @import("table.zig").Table;

const MAX_FRAMES = 64;
const STACK_MAX = 64 * 256;

pub const InterpretError = error{ CompileError, RuntimeError };

const CallFrame = struct {
    closure: *object.ObjClosure,
    ip: [*]u8,
    slots: [*]Value,

    inline fn read_byte(frame: *CallFrame) u8 {
        frame.ip += 1;
        return (frame.ip - 1)[0];
    }

    inline fn read_short(frame: *CallFrame) u16 {
        defer frame.ip += 2;
        return (@as(u16, frame.ip[0]) << 8) | frame.ip[1];
    }

    inline fn read_constant(frame: *CallFrame) Value {
        return frame.closure.function.chunk.values.items[frame.read_byte()];
    }

    inline fn read_string(frame: *CallFrame) *object.ObjString {
        return frame.read_constant().as(object.ObjString);
    }
};

pub const VM = struct {
    frames: [MAX_FRAMES]CallFrame = undefined,
    frame_count: u16 = 0,
    stack: [STACK_MAX]Value = undefined,
    stack_top: [*]Value = undefined,
    globals: Table = undefined,
    strings: Table = undefined,
    init_string: ?*object.ObjString = null,
    open_upvalues: ?*object.ObjUpvalue = null,
    objects: ?*object.Obj = null,
    gray_stack: std.ArrayList(*object.Obj) = undefined,
};

fn reset_stack() void {
    vm.stack_top = &vm.stack;
    vm.frame_count = 0;
}

fn runtimeError(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});

    var i: i32 = @as(i32, @intCast(vm.frame_count)) - 1;
    while (i >= 0) : (i -= 1) {
        const frame = &vm.frames[@intCast(i)];
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr) - 1;
        const line = frame.closure.function.chunk.lines.items[instruction];
        std.debug.print("[line {}] in ", .{line});
        if (frame.closure.function.name == null) {
            std.debug.print("script\n", .{});
        } else {
            std.debug.print("{s}()\n", .{frame.closure.function.name.?.chars});
        }
    }

    reset_stack();
}

fn defineNative(name: []const u8, function: object.NativeFn) void {
    push(Value.obj(&object.ObjString.copy(name).obj));
    push(Value.obj(&object.ObjNative.allocate(function).obj));
    _ = vm.globals.set(@fieldParentPtr(object.ObjString, "obj", peek(1).as_obj()), peek(0));
    _ = pop();
    _ = pop();
}

pub inline fn push(value: Value) void {
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

pub inline fn pop() Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

fn popN(n: u8) void {
    vm.stack_top -= n;
}

inline fn peek(distance: usize) Value {
    const ptr = vm.stack_top - 1 - distance;
    return ptr[0];
}

inline fn call(closure: *object.ObjClosure, arg_count: u8) bool {
    if (closure.function.arity != arg_count) {
        runtimeError("Expected {} arguments but got {}.", .{ closure.function.arity, arg_count });
        return false;
    }

    if (vm.frame_count == MAX_FRAMES) {
        runtimeError("Stack overflow.", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items.ptr;
    frame.slots = vm.stack_top - arg_count - 1;
    return true;
}

fn callValue(callee: Value, arg_count: u8) bool {
    if (callee.is_obj()) {
        switch (callee.as_obj().type) {
            .OBJ_BOUND_METHOD => {
                const bound = callee.as(object.ObjBoundMethod);
                var stack_position = vm.stack_top - arg_count - 1;
                stack_position[0] = bound.receiver;
                return call(bound.method, arg_count);
            },
            .OBJ_CLASS => {
                const klass = callee.as(object.ObjClass);
                var stack_position = vm.stack_top - arg_count - 1;
                stack_position[0] = Value.obj(&object.ObjInstance.allocate(klass).obj);
                var initializer: Value = undefined;
                if (klass.methods.get(vm.init_string.?, &initializer)) {
                    return call(initializer.as(object.ObjClosure), arg_count);
                } else if (arg_count != 0) {
                    runtimeError("Expected 0 arguments but got {}.", .{arg_count});
                    return false;
                }
                return true;
            },
            .OBJ_CLOSURE => {
                return call(callee.as(object.ObjClosure), arg_count);
            },
            .OBJ_NATIVE => {
                const func = callee.as(object.ObjNative).function;
                const result = func(arg_count, vm.stack_top - arg_count);
                vm.stack_top -= (arg_count + 1);
                push(result);
                return true;
            },
            else => {},
        }
    }
    runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn invokeFromClass(class: *object.ObjClass, name: *object.ObjString, arg_count: u8) bool {
    var method: Value = undefined;
    if (!class.methods.get(name, &method)) {
        runtimeError("Undefined property '{s}'.", .{name.chars});
        return false;
    }
    return call(method.as(object.ObjClosure), arg_count);
}

fn invoke(name: *object.ObjString, arg_count: u8) bool {
    var receiver = peek(arg_count);
    if (!receiver.is(.OBJ_INSTANCE)) {
        runtimeError("Only instances have methods.", .{});
        return false;
    }

    var instance = receiver.as(object.ObjInstance);

    var value: Value = undefined;
    if (instance.fields.get(name, &value)) {
        (vm.stack_top - arg_count - 1)[0] = value;
        return callValue(value, arg_count);
    }
    return invokeFromClass(instance.klass, name, arg_count);
}
fn bindMethod(klass: *object.ObjClass, name: *object.ObjString) bool {
    var method: Value = undefined;
    if (!klass.methods.get(name, &method)) {
        runtimeError("Undefined property '{s}'.", .{name.chars});
        return false;
    }

    const bound = object.ObjBoundMethod.allocate(peek(0), method.as(object.ObjClosure));
    _ = pop();
    push(Value.obj(&bound.obj));
    return true;
}

fn captureUpvalue(local: *Value) *object.ObjUpvalue {
    var prev_upvalue: ?*object.ObjUpvalue = null;
    var upvalue = vm.open_upvalues;
    while (upvalue != null and @intFromPtr(local) < @intFromPtr(upvalue.?.location)) {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and @intFromPtr(local) == @intFromPtr(upvalue.?.location)) {
        return upvalue.?;
    }

    const created_upvalue = object.ObjUpvalue.allocate(local);
    created_upvalue.next = upvalue;
    if (prev_upvalue == null) {
        vm.open_upvalues = created_upvalue;
    } else {
        prev_upvalue.?.next = created_upvalue;
    }
    return created_upvalue;
}

fn closeUpvalues(last: [*]Value) void {
    while (vm.open_upvalues != null and @intFromPtr(vm.open_upvalues.?.location) >= @intFromPtr(last)) {
        var upvalue = vm.open_upvalues.?;
        vm.open_upvalues = upvalue.next;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
    }
}

fn defineMethod(name: *object.ObjString) void {
    const method = peek(0);
    var klass = peek(1).as(object.ObjClass);
    _ = klass.methods.set(name, method);
    _ = pop();
}

pub fn init() void {
    reset_stack();
    vm.objects = null;
    vm.gray_stack = std.ArrayList(*object.Obj).init(gc.arena_allocator);
    vm.globals.init();
    vm.strings.init();
    vm.init_string = null; //next line may trigger gc, which reads this field
    vm.init_string = object.ObjString.copy("init");
    vm.open_upvalues = null;

    defineNative("clock", clockNative);
}

fn free_objects() void {
    var obj = vm.objects;
    while (obj != null) {
        const next = obj.?.next;
        obj.?.free_object();
        obj = next;
    }

    vm.gray_stack.shrinkAndFree(0);
}

pub fn free() void {
    vm.globals.free();
    vm.strings.free();
    vm.init_string = null;
    free_objects();
}

pub fn interpret(source: []const u8) InterpretError!void {
    const function = compiler.compile(source);

    if (function == null)
        return error.CompileError;

    push(Value.obj(&function.?.obj));
    const closure = object.ObjClosure.allocate(function.?);
    _ = pop();
    push(Value.obj(&closure.obj));
    _ = call(closure, 0);

    return run();
}

fn concatenate() void {
    const b = @fieldParentPtr(object.ObjString, "obj", peek(0).as_obj());
    const a = @fieldParentPtr(object.ObjString, "obj", peek(1).as_obj());

    var final = common.allocator.alloc(u8, a.chars.len + b.chars.len) catch unreachable;
    @memcpy(final[0..a.chars.len], a.chars);
    @memcpy(final[a.chars.len..], b.chars);

    const result = Value.obj(&object.ObjString.take(final).obj);
    popN(2);
    push(result);
}

fn run() InterpretError!void {
    var frame = &vm.frames[vm.frame_count - 1];
    while (true) {
        if (comptime common.debug_trace_execution) {
            std.debug.print("          ", .{});
            const value_stack_total_len = @intFromPtr(vm.stack_top) - @intFromPtr(frame.slots);
            const value_size: usize = @sizeOf(Value);
            const value_stack_len = value_stack_total_len / value_size;

            for (0..value_stack_len) |i| {
                std.debug.print("[ ", .{});
                frame.slots[i].print(std.io.getStdErr().writer(), false);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});

            const top = @intFromPtr(frame.ip);
            const base = @intFromPtr(frame.closure.function.chunk.code.items.ptr);
            _ = frame.closure.function.chunk.disassemble_instruction(top - base);
        }
        const instruction: OpCode = @enumFromInt(frame.read_byte());
        switch (instruction) {
            .OP_CONSTANT => {
                const constant = frame.read_constant();
                push(constant);
            },
            .OP_NIL => push(Value.nil()),
            .OP_TRUE => push(Value.boolean(true)),
            .OP_FALSE => push(Value.boolean(false)),
            .OP_POP => _ = pop(),
            .OP_POPN => popN(frame.read_byte()),
            .OP_GET_LOCAL => {
                const slot = frame.read_byte();
                push(frame.slots[slot]);
            },
            .OP_SET_LOCAL => {
                const slot = frame.read_byte();
                frame.slots[slot] = peek(0);
            },
            .OP_DEFINE_GLOBAL => {
                const name = frame.read_string();
                _ = vm.globals.set(name, peek(0));
                _ = pop();
            },
            .OP_GET_GLOBAL => {
                const name = frame.read_string();
                var value: Value = undefined;
                if (!vm.globals.get(name, &value)) {
                    runtimeError("Undefined variable '{s}'.", .{name.chars});
                    return error.RuntimeError;
                }
                push(value);
            },
            .OP_SET_GLOBAL => {
                const name = frame.read_string();
                if (vm.globals.set(name, peek(0))) {
                    _ = vm.globals.delete(name);
                    runtimeError("Undefined variable '{s}'.", .{name.chars});
                    return error.RuntimeError;
                }
            },
            .OP_SET_UPVALUE => {
                const slot = frame.read_byte();
                frame.closure.upvalues[slot].?.location.* = peek(0);
            },
            .OP_GET_UPVALUE => {
                const slot = frame.read_byte();
                push(frame.closure.upvalues[slot].?.location.*);
            },
            .OP_SET_PROPERTY => {
                const obj = peek(1);

                if (!obj.is(.OBJ_INSTANCE)) {
                    runtimeError("Only instances have fields.", .{});
                    return error.RuntimeError;
                }

                var instance = obj.as(object.ObjInstance);
                _ = instance.fields.set(frame.read_string(), peek(0));
                const value = pop();
                _ = pop();
                push(value);
            },
            .OP_GET_PROPERTY => {
                const obj = peek(0);
                if (!obj.is(.OBJ_INSTANCE)) {
                    runtimeError("Only instances have properties.", .{});
                    return error.RuntimeError;
                }

                const instance = obj.as(object.ObjInstance);
                const name = frame.read_string();
                var value: Value = undefined;
                if (instance.fields.get(name, &value)) {
                    _ = pop(); //instance
                    push(value);
                } else if (!bindMethod(instance.klass, name)) {
                    return error.RuntimeError;
                }
            },
            .OP_GET_SUPER => {
                const name = frame.read_string();
                const super_class = pop().as(object.ObjClass);

                if (!bindMethod(super_class, name)) {
                    return error.RuntimeError;
                }
            },
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
                if (peek(0).is(.OBJ_STRING) and peek(1).is(.OBJ_STRING)) {
                    concatenate();
                } else if (peek(0).is_number() and peek(1).is_number()) {
                    const b = pop().as_number();
                    const a = pop().as_number();
                    push(Value.number(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.", .{});
                    return error.RuntimeError;
                }
            },
            .OP_SUBTRACT => try binary_op(.OP_SUBTRACT),
            .OP_MULTIPLY => try binary_op(.OP_MULTIPLY),
            .OP_DIVIDE => try binary_op(.OP_DIVIDE),
            .OP_NOT => push(Value.boolean(pop().is_falsy())),
            .OP_NEGATE => {
                const ptr = vm.stack_top - 1;
                if (ptr[0].is_number()) {
                    ptr[0] = Value.number(-ptr[0].as_number());
                } else {
                    runtimeError("Operand must be a number.", .{});
                    return error.RuntimeError;
                }
            },
            .OP_PRINT => {
                const value = pop();
                value.print(std.io.getStdOut().writer(), true);
            },
            .OP_JUMP => {
                const offset = frame.read_short();
                frame.ip += offset;
            },
            .OP_JUMP_IF_NEQUAL => {
                const b = pop();
                const offset = frame.read_short();
                if (!peek(0).values_equal(b)) {
                    frame.ip += offset;
                }
            },
            .OP_JUMP_IF_FALSE => {
                const offset = frame.read_short();
                if (peek(0).is_falsy()) {
                    frame.ip += offset;
                }
            },
            .OP_JUMP_IF_TRUE => {
                const offset = frame.read_short();
                if (!peek(0).is_falsy()) {
                    frame.ip += offset;
                }
            },
            .OP_LOOP => {
                const offset = frame.read_short();
                frame.ip -= offset;
            },
            .OP_CALL => {
                const arg_count = frame.read_byte();
                if (!callValue(peek(arg_count), arg_count)) {
                    return error.RuntimeError;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .OP_INVOKE => {
                const method = frame.read_string();
                const arg_count = frame.read_byte();
                if (!invoke(method, arg_count)) {
                    return error.RuntimeError;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .OP_SUPER_INVOKE => {
                const name = frame.read_string();
                const argc = frame.read_byte();
                const superclass = pop().as(object.ObjClass);
                if (!invokeFromClass(superclass, name, argc)) {
                    return error.RuntimeError;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .OP_CLOSURE => {
                const function = @fieldParentPtr(object.ObjFunction, "obj", frame.read_constant().as_obj());
                const closure = object.ObjClosure.allocate(function);
                push(Value.obj(&closure.obj));

                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.read_byte();
                    const index = frame.read_byte();
                    if (is_local == 1) {
                        upvalue.* = captureUpvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .OP_CLOSE_UPVALUE => {
                closeUpvalues(vm.stack_top - 1);
                _ = pop();
            },
            .OP_RETURN => {
                const result = pop();
                closeUpvalues(frame.slots);
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    _ = pop();
                    return;
                }

                vm.stack_top = frame.slots;
                push(result);
                frame = &vm.frames[vm.frame_count - 1];
            },
            .OP_CLASS => {
                push(Value.obj(&object.ObjClass.allocate(frame.read_string()).obj));
            },
            .OP_INHERIT => {
                var superclass = peek(1);
                if (!superclass.is(.OBJ_CLASS)) {
                    runtimeError("Superclass must be a class.", .{});
                    return error.RuntimeError;
                }

                var subclass = peek(0).as(object.ObjClass);
                subclass.methods.add_all(&superclass.as(object.ObjClass).methods);
                _ = pop(); //subclass
            },
            .OP_METHOD => defineMethod(frame.read_string()),
        }
    }
}

inline fn binary_op(comptime op: OpCode) InterpretError!void {
    if (!peek(0).is_number() or !peek(1).is_number()) {
        runtimeError("Operands must be numbers.", .{});
        return error.RuntimeError;
    }
    const b = pop().as_number();
    const a = pop().as_number();

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

inline fn toFloat(i: anytype) f64 {
    return @floatFromInt(i);
}

fn clockNative(_: u8, _: [*]Value) Value {
    const ms: f64 = toFloat(std.time.milliTimestamp()) / toFloat(std.time.ms_per_s);
    return Value.number(ms);
}
