const std = @import("std");

const value_ = @import("value.zig");
const Value = value_.Value;
const common = @import("common.zig");
const VM = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;

pub const ObjType = enum {
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj = null,

    fn allocate(comptime Type: type) *Type {
        var obj = common.allocator.create(Type) catch unreachable;
        obj.obj.type = switch (Type) {
            ObjString => .OBJ_STRING,
            ObjFunction => .OBJ_FUNCTION,
            ObjNative => .OBJ_NATIVE,
            else => @compileError("Unknown type"),
        };
        obj.obj.next = VM.vm.objects;
        VM.vm.objects = &obj.obj;
        return obj;
    }

    pub fn free_object(obj: *Obj) void {
        switch (obj.type) {
            .OBJ_STRING => {
                const str_obj = @fieldParentPtr(ObjString, "obj", obj);
                common.allocator.free(str_obj.chars);
                common.allocator.destroy(str_obj);
            },
            .OBJ_FUNCTION => {
                const function = @fieldParentPtr(ObjFunction, "obj", obj);
                function.chunk.free();
                common.allocator.destroy(function);
            },
            .OBJ_NATIVE => {
                const function = @fieldParentPtr(ObjNative, "obj", obj);
                common.allocator.destroy(function);
            },
            else => unreachable,
        }
    }

    pub fn print(obj: *Obj, writer: anytype, comptime newline: bool) !void {
        switch (obj.type) {
            .OBJ_STRING => {
                const str_obj = @fieldParentPtr(ObjString, "obj", obj);
                try writer.print("{s}", .{str_obj.chars});
            },
            .OBJ_FUNCTION => {
                const function = @fieldParentPtr(ObjFunction, "obj", obj);
                if (function.name == null) {
                    try writer.print("<script>", .{});
                } else {
                    try writer.print("<fn {s}>", .{function.name.?.chars});
                }
            },
            .OBJ_NATIVE => {
                try writer.print("<native fn>", .{});
            },
            else => unreachable,
        }
        if (newline) {
            try writer.print("\n", .{});
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8 = undefined,
    hash: u32 = undefined,

    fn allocate(chars: []const u8, hash: u32) *ObjString {
        var obj = Obj.allocate(ObjString);
        obj.chars = chars;
        obj.hash = hash;
        _ = VM.vm.strings.set(obj, Value.Nil);
        return obj;
    }

    pub fn copy(chars: []const u8) *ObjString {
        const hash = hash_string(chars);
        const interned = VM.vm.strings.findString(chars, hash);
        if (interned != null) {
            return interned.?;
        }
        var str = common.allocator.alloc(u8, chars.len) catch unreachable;
        @memcpy(str, chars);
        return allocate(str, hash);
    }

    pub fn take(chars: []const u8) *ObjString {
        const hash = hash_string(chars);
        const interned = VM.vm.strings.findString(chars, hash);
        if (interned != null) {
            common.allocator.free(chars);
            return interned.?;
        }
        return allocate(chars, hash);
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u16 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn allocate() *ObjFunction {
        var obj = Obj.allocate(ObjFunction);
        obj.arity = 0;
        obj.name = null;
        obj.chunk.init();
        return obj;
    }
};

pub const NativeFn = *const fn (arg_count: u8, args: [*]Value) Value;
pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,

    pub fn allocate(function: NativeFn) *ObjNative {
        var obj = Obj.allocate(ObjNative);
        obj.function = function;
        return obj;
    }
};

inline fn is_ObjType(value: Value, obj_type: ObjType) bool {
    return value == Value.Obj and value.Obj.type == obj_type;
}

pub inline fn is_string(value: Value) bool {
    return is_ObjType(value, .OBJ_STRING);
}

fn hash_string(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}
