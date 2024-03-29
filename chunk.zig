const std = @import("std");
const print = std.debug.print;

const common = @import("common.zig");
const object = @import("object.zig");
const vm = @import("vm.zig");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const exit = @import("std").os.exit;

const allocator = common.allocator;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,

    OP_TRUE,
    OP_FALSE,

    OP_POP,
    OP_POPN,

    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_SET_UPVALUE,
    OP_GET_UPVALUE,
    OP_SET_PROPERTY,
    OP_GET_PROPERTY,

    OP_GET_SUPER,

    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_GREATER,
    OP_GREATER_EQUAL,
    OP_LESS,
    OP_LESS_EQUAL,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,

    OP_NOT,
    OP_NEGATE,

    OP_PRINT,

    OP_JUMP,
    OP_JUMP_IF_NEQUAL, // jumps if top two items are not equal. Regardless, pops the top item.
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_TRUE,
    OP_LOOP,
    OP_CALL,
    OP_INVOKE,
    OP_SUPER_INVOKE,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,

    OP_CLASS,
    OP_INHERIT,
    OP_METHOD,
};

pub const Chunk = struct {
    const Self = @This();

    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    values: std.ArrayList(Value),

    pub fn init(self: *Self) void {
        self.code = std.ArrayList(u8).init(allocator);
        self.lines = std.ArrayList(u32).init(allocator);
        self.values = std.ArrayList(Value).init(allocator);
    }

    pub fn write_op(self: *Self, code: OpCode, line: u32) void {
        errdefer {
            exit(1);
        }
        try self.code.append(@intFromEnum(code));
        try self.lines.append(line);
    }

    pub fn write_byte(self: *Self, byte: u8, line: u32) void {
        errdefer {
            exit(1);
        }
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn add_const(self: *Self, value: Value) u16 {
        vm.push(value);
        self.values.append(value) catch unreachable;
        _ = vm.pop();
        return @intCast(self.values.items.len - 1);
    }

    pub fn free(self: *Self) void {
        errdefer {
            exit(1);
        }
        self.code.deinit();
        self.lines.deinit();
        self.values.deinit();
    }

    pub fn disassemble(self: *const Self, name: []const u8) void {
        print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(self: *const Self, offset: usize) usize {
        print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{d:4} ", .{self.lines.items[offset]});
        }

        const instruction: OpCode = @as(OpCode, @enumFromInt(self.code.items[offset]));
        switch (instruction) {
            .OP_CONSTANT => return self.constant_instruction("OP_CONSTANT", offset),
            .OP_NIL => return simple_instructoin("OP_NIL", offset),
            .OP_TRUE => return simple_instructoin("OP_TRUE", offset),
            .OP_FALSE => return simple_instructoin("OP_FALSE", offset),
            .OP_POP => return simple_instructoin("OP_POP", offset),
            .OP_POPN => return self.byte_instruction("OP_POPN", offset),
            .OP_GET_LOCAL => return self.byte_instruction("OP_GET_LOCAL", offset),
            .OP_SET_LOCAL => return self.byte_instruction("OP_SET_LOCAL", offset),
            .OP_DEFINE_GLOBAL => return self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            .OP_GET_GLOBAL => return self.constant_instruction("OP_GET_GLOBAL", offset),
            .OP_SET_GLOBAL => return self.constant_instruction("OP_SET_GLOBAL", offset),
            .OP_SET_UPVALUE => return self.byte_instruction("OP_SET_UPVALUE", offset),
            .OP_GET_UPVALUE => return self.byte_instruction("OP_GET_UPVALUE", offset),
            .OP_SET_PROPERTY => return self.constant_instruction("OP_SET_PROPERTY", offset),
            .OP_GET_PROPERTY => return self.constant_instruction("OP_GET_PROPERTY", offset),
            .OP_GET_SUPER => return self.constant_instruction("OP_GET_SUPER", offset),
            .OP_EQUAL => return simple_instructoin("OP_EQUAL", offset),
            .OP_NOT_EQUAL => return simple_instructoin("OP_NOT_EQUAL", offset),
            .OP_GREATER => return simple_instructoin("OP_GREATER", offset),
            .OP_GREATER_EQUAL => return simple_instructoin("OP_GREATER_EQUAL", offset),
            .OP_LESS => return simple_instructoin("OP_LESS", offset),
            .OP_LESS_EQUAL => return simple_instructoin("OP_LESS_EQUAL", offset),
            .OP_ADD => return simple_instructoin("OP_ADD", offset),
            .OP_SUBTRACT => return simple_instructoin("OP_SUBTRACT", offset),
            .OP_MULTIPLY => return simple_instructoin("OP_MULTIPLY", offset),
            .OP_DIVIDE => return simple_instructoin("OP_DIVIDE", offset),
            .OP_NOT => return simple_instructoin("OP_NOT", offset),
            .OP_NEGATE => return simple_instructoin("OP_NEGATE", offset),
            .OP_PRINT => return simple_instructoin("OP_PRINT", offset),
            .OP_JUMP => return self.jump_instruction("OP_JUMP", 1, offset),
            .OP_JUMP_IF_NEQUAL => return self.jump_instruction("OP_JUMP_IF_NEQUAL", 1, offset),
            .OP_JUMP_IF_FALSE => return self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            .OP_JUMP_IF_TRUE => return self.jump_instruction("OP_JUMP_IF_TRUE", 1, offset),
            .OP_LOOP => return self.jump_instruction("OP_LOOP", -1, offset),
            .OP_CALL => return self.byte_instruction("OP_CALL", offset),
            .OP_INVOKE => return self.invokeInstruction("OP_INVOKE", offset),
            .OP_SUPER_INVOKE => return self.invokeInstruction("OP_SUPER_INVOKE", offset),
            .OP_CLOSURE => {
                const constant = self.code.items[offset + 1];
                print("{s:<16} {d:4} '", .{ "OP_CLOSURE", constant });
                self.values.items[constant].print(std.io.getStdErr().writer(), false);
                print("'\n", .{});

                const function = @fieldParentPtr(object.ObjFunction, "obj", self.values.items[constant].Obj);
                var j: u16 = 0;
                while (j < 2 * @as(u16, function.upvalue_count)) : (j += 2) {
                    const str = if (self.code.items[offset + j + 2] == 1) "local" else "upvalue";
                    const index = self.code.items[offset + j + 3];
                    print("{d:4}      |                     {s} {d}\n", .{ offset, str, index });
                }
                return offset + 2 + j;
            },
            .OP_CLOSE_UPVALUE => return simple_instructoin("OP_CLOSE_UPVALUE", offset),
            .OP_RETURN => return simple_instructoin("OP_RETURN", offset),
            .OP_CLASS => return self.constant_instruction("OP_CLASS", offset),
            .OP_INHERIT => return simple_instructoin("OP_INHERIT", offset),
            .OP_METHOD => return self.constant_instruction("OP_METHOD", offset),
        }
    }

    fn simple_instructoin(name: []const u8, offset: usize) usize {
        print("{s}\n", .{name});
        return offset + 1;
    }

    fn constant_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        print("{s:<16} {d:4} '", .{ name, constant });
        self.values.items[constant].print(std.io.getStdErr().writer(), false);
        print("'\n", .{});
        return offset + 2;
    }

    fn invokeInstruction(self: *const Self, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const arg_count = self.code.items[offset + 2];
        print("{s:<16} ({d} args) {d:4} '", .{ name, arg_count, constant });
        self.values.items[constant].print(std.io.getStdErr().writer(), false);
        print("'\n", .{});
        return offset + 3;
    }

    fn byte_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        print("{s:<16} {d:4}\n", .{ name, slot });
        return offset + 2;
    }

    fn jump_instruction(self: *const Self, name: []const u8, comptime sign: i8, offset: usize) usize {
        const jump = @as(u16, self.code.items[offset + 1]) << 8 | self.code.items[offset + 2];

        var jumpOffset: usize = undefined;
        if (comptime (sign > 0)) {
            jumpOffset = offset + 3 + jump;
        } else {
            jumpOffset = offset + 3 - jump;
        }
        print("{s:<16} {d:4} -> {d}\n", .{ name, offset, jumpOffset });
        return offset + 3;
    }
};
