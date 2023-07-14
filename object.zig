const std = @import("std");

const value_ = @import("value.zig");
const Value = value_.Value;
const common = @import("common.zig");
const VM = @import("vm.zig");

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
            else => unreachable,
        }
    }

    pub fn print(obj: *Obj) void {
        switch (obj.type) {
            .OBJ_STRING => {
                const str_obj = @fieldParentPtr(ObjString, "obj", obj);
                std.debug.print("{s}", .{str_obj.chars});
            },
            else => unreachable,
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
        return obj;
    }

    pub fn copy(chars: []const u8) *ObjString {
        const hash = hash_string(chars);
        var str = common.allocator.alloc(u8, chars.len) catch unreachable;
        @memcpy(str, chars);
        return allocate(str, hash);
    }

    pub fn take(chars: []const u8) *ObjString {
        const hash = hash_string(chars);
        return allocate(chars, hash);
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
