const std = @import("std");

const object = @import("object.zig");
const Obj = object.Obj;

pub const Value = union(enum) {
    const Self = @This();

    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn print(self: *const Value, writer: anytype, comptime newline: bool) void {
        if (newline) {
            switch (self.*) {
                .Number => |n| writer.print("{d}\n", .{n}) catch {},
                .Bool => |b| writer.print("{}\n", .{b}) catch {},
                .Nil => writer.print("nil\n", .{}) catch {},
                .Obj => |obj| obj.print(writer, newline) catch {},
            }
        } else {
            switch (self.*) {
                .Number => |n| writer.print("{d}", .{n}) catch {},
                .Bool => |b| writer.print("{}", .{b}) catch {},
                .Nil => writer.print("nil", .{}) catch {},
                .Obj => |obj| obj.print(writer, newline) catch {},
            }
        }
    }

    pub inline fn boolean(val: bool) Value {
        return .{ .Bool = val };
    }

    pub inline fn nil() Value {
        return Value.Nil;
    }

    pub inline fn number(num: f64) Value {
        return .{ .Number = num };
    }

    pub inline fn is_falsy(self: Value) bool {
        return (self == .Nil or (self == Value.Bool and !self.Bool));
    }

    pub inline fn values_equal(a: Value, b: Value) bool {
        if (@intFromEnum(a) != @intFromEnum(b))
            return false;

        return switch (a) {
            .Number => a.Number == b.Number,
            .Bool => a.Bool == b.Bool,
            .Nil => true,
            .Obj => {
                const a_str = @fieldParentPtr(object.ObjString, "obj", a.Obj);
                const b_str = @fieldParentPtr(object.ObjString, "obj", b.Obj);
                return a_str == b_str;
            }, // for now we only have string objects. Will need to revisit this in future.
        };
    }

    pub inline fn as(self: *const Self, comptime T: type) *T {
        return self.Obj.downcast(T);
    }

    pub inline fn is(self: *const Self, typ: object.ObjType) bool {
        return (self.* == .Obj and self.Obj.type == typ);
    }
};
