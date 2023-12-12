const std = @import("std");

const common = @import("common.zig");
const obj = @import("object.zig");
const Value = @import("value.zig").Value;

const TABLE_MAX_LOAD: f64 = 0.75;

const Entry = struct {
    key: ?*obj.ObjString,
    value: Value,
};

pub const Table = struct {
    entries: [*]Entry,
    capacity: usize,
    count: usize,
    load_capacity: usize,

    pub fn init(self: *Table) void {
        self.entries = undefined;
        self.count = 0;
        self.capacity = 0;
        self.load_capacity = 0;
    }

    pub fn free(self: *Table) void {
        common.allocator.free(self.entries[0..self.capacity]);
        self.init();
    }

    fn findEntry(entries: [*]Entry, capacity: usize, key: *obj.ObjString) *Entry {
        var index = key.hash & (capacity - 1);
        var tombston: ?*Entry = null;
        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.is_nil()) {
                    if (tombston == null) {
                        return entry;
                    } else {
                        return tombston.?;
                    }
                } else {
                    if (tombston == null) {
                        tombston = entry;
                    }
                }
            } else if (entry.key == key) {
                return entry;
            }
            index = (index + 1) & (capacity - 1);
        }
    }

    pub fn get(self: *Table, key: *obj.ObjString, value: *Value) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, self.capacity, key);
        if (entry.key == null) return false;
        value.* = entry.value;
        return true;
    }

    fn adjustCapacity(self: *Table, capacity: usize) void {
        const entries = common.allocator.alloc(Entry, capacity) catch unreachable;
        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.nil();
        }

        self.count = 0;
        for (0..self.capacity) |i| {
            const entry = &self.entries[i];
            if (entry.key == null) {
                continue;
            }
            var dest = findEntry(entries.ptr, capacity, entry.key.?);
            dest.key = entry.key;
            dest.value = entry.value;
            self.count += 1;
        }
        if (self.capacity > 0) { // capacity==0 only when entries is unallocated.
            common.allocator.free(self.entries[0..self.capacity]);
        }
        self.entries = entries.ptr;
        self.capacity = capacity;
        self.load_capacity = @intFromFloat(@as(f32, @floatFromInt(capacity)) * TABLE_MAX_LOAD);
    }

    pub fn set(self: *Table, key: *obj.ObjString, value: Value) bool {
        if (self.count + 1 > self.load_capacity) {
            const capacity = @max(8, self.capacity * 2);
            self.adjustCapacity(capacity);
        }

        const entry = findEntry(self.entries, self.capacity, key);
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value.is_nil()) {
            self.count += 1;
        }
        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn delete(self: *Table, key: *obj.ObjString) bool {
        if (self.count == 0) return false;
        const entry = findEntry(self.entries, self.capacity, key);
        if (entry.key == null) return false;
        entry.key = null;
        entry.value = Value.boolean(true);
        return true;
    }

    pub fn add_all(to: *Table, from: *Table) void {
        for (0..from.capacity) |i| {
            const entry = &from.entries[i];
            if (entry.key != null)
                _ = to.set(entry.key.?, entry.value);
        }
    }

    pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*obj.ObjString {
        if (self.count == 0) return null;

        var index = hash & (self.capacity - 1);
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null) {
                if (entry.value.is_nil()) {
                    return null;
                }
            } else if (chars.len == entry.key.?.chars.len and entry.key.?.hash == hash and std.mem.eql(u8, chars, entry.key.?.chars)) {
                return entry.key;
            }
            index = (index + 1) & (self.capacity - 1);
        }
    }

    pub fn removeWhite(self: *Table) void {
        for (0..self.capacity) |i| {
            const entry = &self.entries[i];
            if (entry.key != null and !entry.key.?.obj.is_marked) {
                _ = self.delete(entry.key.?);
            }
        }
    }
};
