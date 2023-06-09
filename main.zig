const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var chunk = Chunk.init();

    var value = Value.number(1.2);
    chunk.write_op(.OP_CONSTANT, 0);
    chunk.write_u8(chunk.write_const(value), 0);

    value = Value.number(3.4);
    chunk.write_op(.OP_CONSTANT, 0);
    chunk.write_u8(chunk.write_const(value), 0);

    chunk.write_op(.OP_ADD, 0);

    value = Value.number(5.6);
    chunk.write_op(.OP_CONSTANT, 0);
    chunk.write_u8(chunk.write_const(value), 0);

    chunk.write_op(.OP_DIVIDE, 0);
    chunk.write_op(.OP_NEGATE, 0);

    chunk.write_op(.OP_RETURN, 0);

    var vm = VM{};
    vm.init();

    try vm.interpret(&chunk);
    chunk.disassemble("test chunk");
    chunk.free();
}
