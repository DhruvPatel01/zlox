const std = @import("std");

const common = @import("common.zig");
const obj = @import("object.zig");
const Value = @import("value.zig").Value;
const vm_ = @import("vm.zig");
const table_ = @import("table.zig");
const compiler = @import("compiler.zig");

const GC_HEAP_GROW_FACTOR: usize = 2;

pub var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
pub const arena_allocator = arena.allocator();

pub const GCAllocator = struct {
    const Self = @This();

    parent_allocator: std.mem.Allocator,
    next_gc: usize = 1024 * 1024,
    bytes_allocated: usize = 0,

    pub fn init(parent_allocator: std.mem.Allocator) Self {
        return .{ .parent_allocator = parent_allocator };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{ .ptr = self, .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        } };
    }

    fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ra: usize) ?[*]u8 {
        var self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated += len;
        if (comptime common.debug_stress_gc) {
            self.collect();
        } else {
            if (self.bytes_allocated > self.next_gc) self.collect();
        }
        const result = self.parent_allocator.rawAlloc(len, log2_ptr_align, ra);
        return result;
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_ptr_align: u8, new_len: usize, ra: usize) bool {
        var self: *Self = @ptrCast(@alignCast(ctx));
        const success = self.parent_allocator.rawResize(buf, log2_ptr_align, new_len, ra);
        if (success and buf.len < new_len) {
            self.bytes_allocated += new_len - buf.len;
            if (comptime common.debug_stress_gc) {
                self.collect();
            } else {
                if (self.bytes_allocated > self.next_gc) self.collect();
            }
        }
        return success;
    }

    fn free(ctx: *anyopaque, buf: []u8, log2_ptr_align: u8, ret_addr: usize) void {
        var self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated -= buf.len;
        self.parent_allocator.rawFree(buf, log2_ptr_align, ret_addr);
    }

    pub fn collect(self: *Self) void {
        if (comptime common.debug_log_gc)
            std.debug.print("-- gc begin\n", .{});

        const before = self.bytes_allocated;

        markRoots();
        traceReferences();
        vm_.vm.strings.removeWhite();
        sweep();

        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        const after = self.bytes_allocated;

        if (comptime common.debug_log_gc) {
            std.debug.print("-- gc end\n", .{});
            std.debug.print("   collected {} bytes (from {} to {}) next at {}\n", .{ before - after, before, after, self.next_gc });
        }
    }
};

fn markRoots() void {
    const stack_top = @intFromPtr(vm_.vm.stack_top);
    var current = vm_.vm.stack[0..].ptr;
    while (@intFromPtr(current) < stack_top) : (current += 1) {
        markValue(current[0]);
    }

    var i: usize = 0;
    while (i < vm_.vm.frame_count) : (i += 1) {
        markObject(&vm_.vm.frames[i].closure.obj);
    }

    var upvalue = vm_.vm.open_upvalues;
    while (upvalue != null) : (upvalue = upvalue.?.next) {
        markObject(&upvalue.?.obj);
    }

    markTable(&vm_.vm.globals);
    compiler.markCompilerRoots();
    if (vm_.vm.init_string != null) // TODO: "this" shouldn't be checked every time. Make it static!
        markObject(&vm_.vm.init_string.?.obj);
}

fn markTable(table: *table_.Table) void {
    var i: usize = 0;
    while (i < table.capacity) : (i += 1) {
        const entry = &table.entries[i];
        if (entry.key != null) {
            // std.debug.print("Marking the key {s}\n", .{entry.key.?.chars});
            markObject(&entry.key.?.obj);
        }
        // std.debug.print("Marking the value {}\n", .{entry.value});
        markValue(entry.value);
    }
}

inline fn markValue(value: Value) void {
    if (value.is_obj()) {
        markObject(value.as_obj());
    }
}

inline fn markArray(values: []Value) void {
    for (values) |value| {
        markValue(value);
    }
}

pub fn markObject(object: ?*obj.Obj) void {
    if (object == null or object.?.is_marked) return;
    if (comptime common.debug_log_gc) {
        std.debug.print("{*} mark of type {} - ", .{ object.?, object.?.type });
        object.?.print(std.io.getStdErr().writer(), true) catch unreachable;
    }
    object.?.is_marked = true;

    vm_.vm.gray_stack.append(object.?) catch {
        std.os.exit(1);
    };
}

fn traceReferences() void {
    while (vm_.vm.gray_stack.items.len > 0) {
        const object = vm_.vm.gray_stack.pop();
        blackenObject(object);
    }
}

fn sweep() void {
    var previous: ?*obj.Obj = null;
    var object = vm_.vm.objects;
    while (object != null) {
        if (object.?.is_marked) {
            object.?.is_marked = false;
            previous = object;
            object = object.?.next;
        } else {
            const unreached = object.?;
            object = unreached.next;
            if (previous == null) {
                vm_.vm.objects = object;
            } else {
                previous.?.next = object;
            }
            unreached.free_object();
        }
    }
}

fn blackenObject(object: *obj.Obj) void {
    if (comptime common.debug_log_gc) {
        std.debug.print("{*} blacken ", .{object});
        object.print(std.io.getStdErr().writer(), true) catch unreachable;
    }

    switch (object.type) {
        .OBJ_STRING, .OBJ_NATIVE => {},
        .OBJ_INSTANCE => {
            const instance = object.downcast(obj.ObjInstance);
            markObject(&instance.klass.obj);
            markTable(&instance.fields);
        },
        .OBJ_BOUND_METHOD => {
            const method_obj = object.downcast(obj.ObjBoundMethod);
            markValue(method_obj.receiver);
            markObject(&method_obj.method.obj);
        },
        .OBJ_UPVALUE => {
            const upvalue: *obj.ObjUpvalue = object.downcast(obj.ObjUpvalue);
            markValue(upvalue.closed);
        },
        .OBJ_FUNCTION => {
            const function: *obj.ObjFunction = object.downcast(obj.ObjFunction);
            if (function.name != null)
                markObject(&function.name.?.obj);
            markArray(function.chunk.values.items);
        },
        .OBJ_CLOSURE => {
            const closure: *obj.ObjClosure = object.downcast(obj.ObjClosure);
            markObject(&closure.function.obj);
            for (closure.upvalues) |upvalue| {
                if (upvalue != null) markObject(&upvalue.?.obj);
            }
        },
        .OBJ_CLASS => {
            const klass = object.downcast(obj.ObjClass);
            markTable(&klass.methods);
            markObject(&klass.name.obj);
        },
    }
}
