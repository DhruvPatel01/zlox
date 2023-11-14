const std = @import("std");

const value_ = @import("value.zig");
const Value = value_.Value;
const common = @import("common.zig");
const VM = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;
const Table = @import("table.zig").Table;

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
    const Self = @This();

    type: ObjType,
    is_marked: bool,
    next: ?*Obj = null,

    fn allocate(comptime Type: type) *Type {
        var obj = common.allocator.create(Type) catch unreachable;
        obj.obj.type = switch (Type) {
            ObjString => .OBJ_STRING,
            ObjClosure => .OBJ_CLOSURE,
            ObjClass => .OBJ_CLASS,
            ObjInstance => .OBJ_INSTANCE,
            ObjFunction => .OBJ_FUNCTION,
            ObjNative => .OBJ_NATIVE,
            ObjUpvalue => .OBJ_UPVALUE,
            else => @compileError("Unknown type"),
        };
        obj.obj.is_marked = false;
        obj.obj.next = VM.vm.objects;
        VM.vm.objects = &obj.obj;

        if (comptime common.debug_log_gc)
            std.debug.print("{*} allocate {}\n", .{ obj, @sizeOf(Type) });
        return obj;
    }

    pub inline fn downcast(self: *Self, comptime Type: type) *Type {
        return @fieldParentPtr(Type, "obj", self);
    }

    pub fn free_object(self: *Obj) void {
        if (comptime common.debug_log_gc)
            std.debug.print("{*} free {}\n", .{ self, self.type });

        switch (self.type) {
            .OBJ_STRING => {
                const str_obj = self.downcast(ObjString);
                common.allocator.free(str_obj.chars);
                common.allocator.destroy(str_obj);
            },
            .OBJ_UPVALUE => {
                const upvalue = self.downcast(ObjUpvalue);
                common.allocator.destroy(upvalue);
            },
            .OBJ_FUNCTION => {
                const function = self.downcast(ObjFunction);
                function.chunk.free();
                common.allocator.destroy(function);
            },
            .OBJ_NATIVE => {
                const function = self.downcast(ObjNative);
                common.allocator.destroy(function);
            },
            .OBJ_CLOSURE => {
                const closure = self.downcast(ObjClosure);
                common.allocator.free(closure.upvalues);
                common.allocator.destroy(closure);
            },
            .OBJ_CLASS => {
                const klass = self.downcast(ObjClass);
                common.allocator.destroy(klass);
            },
            .OBJ_INSTANCE => {
                const instance = self.downcast(ObjInstance);
                instance.fields.free();
                common.allocator.destroy(instance);
            },
            else => unreachable,
        }
    }

    pub fn print(self: *Obj, writer: anytype, comptime newline: bool) !void {
        switch (self.type) {
            .OBJ_STRING => {
                const str_obj = self.downcast(ObjString);
                try writer.print("{s}", .{str_obj.chars});
            },
            .OBJ_UPVALUE => {
                try writer.print("upvalue", .{});
            },
            .OBJ_CLOSURE => {
                const closure = self.downcast(ObjClosure);
                try print(&closure.function.obj, writer, false);
            },
            .OBJ_CLASS => {
                const klass = self.downcast(ObjClass);
                try writer.print("{s}", .{klass.name.chars});
            },
            .OBJ_FUNCTION => {
                const function = self.downcast(ObjFunction);
                if (function.name == null) {
                    try writer.print("<script>", .{});
                } else {
                    try writer.print("<fn {s}>", .{function.name.?.chars});
                }
            },
            .OBJ_NATIVE => {
                try writer.print("<native fn>", .{});
            },
            .OBJ_INSTANCE => {
                try writer.print("{s} instance", .{self.downcast(ObjInstance).klass.name.chars});
            },
            else => {
                std.debug.print("Unhandled type {}\n", .{self.type});
                unreachable;
            },
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
        VM.push(.{ .Obj = &obj.obj }); //garbage collection may be triggered next
        _ = VM.vm.strings.set(obj, Value.Nil);
        _ = VM.pop();
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

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn allocate(value: *Value) *ObjUpvalue {
        var upvalue = Obj.allocate(ObjUpvalue);
        upvalue.location = value;
        upvalue.next = null;
        upvalue.closed = Value.Nil;
        return upvalue;
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
    upvalue_count: u16,

    pub fn allocate(function: *ObjFunction) *ObjClosure {
        var upvalues = common.allocator.alloc(?*ObjUpvalue, function.upvalue_count) catch unreachable;
        for (upvalues) |*upvalue| upvalue.* = null;
        var closure = Obj.allocate(ObjClosure);
        closure.function = function;
        closure.upvalues = upvalues;
        closure.upvalue_count = function.upvalue_count;
        return closure;
    }
};

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,

    pub fn allocate(name: *ObjString) *ObjClass {
        var klass = Obj.allocate(ObjClass);
        klass.name = name;
        return klass;
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    klass: *ObjClass,
    fields: Table,

    pub fn allocate(klass: *ObjClass) *ObjInstance {
        var instance = Obj.allocate(ObjInstance);
        instance.fields.init();
        instance.klass = klass;
        return instance;
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u16 = 0,
    upvalue_count: u16 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn allocate() *ObjFunction {
        var obj = Obj.allocate(ObjFunction);
        obj.arity = 0;
        obj.upvalue_count = 0;
        obj.name = null;
        obj.chunk.init(); // doesn't allocate any memory
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
