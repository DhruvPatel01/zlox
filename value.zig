const std = @import("std");

const object = @import("object.zig");
const Obj = object.Obj;

pub const Value = union(enum) {
    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn print(self: *const Value) void {
        switch (self.*) {
            .Number => |n| std.debug.print("{d}", .{n}),
            .Bool => |b| std.debug.print("{}", .{b}),
            .Nil => std.debug.print("Nil", .{}),
            else => unreachable,
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
        if (@enumToInt(a) != @enumToInt(b))
            return false;

        return switch (a) {
            .Number => a.Number == b.Number,
            .Bool => a.Bool == b.Bool,
            .Nil => true,
            else => false,
        };
    }
};
