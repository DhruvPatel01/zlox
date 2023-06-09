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
            .Number => std.debug.print("{d}", .{self.Number}),
            else => unreachable,
        }
    }

    pub inline fn boolean(val: bool) Value {
        return .{ .Bool = val };
    }

    pub inline fn nil() Value {
        return .{.Nil};
    }

    pub inline fn number(num: f64) Value {
        return .{ .Number = num };
    }

    pub inline fn as_bool(self: Value) bool {
        return self.Bool;
    }

    pub inline fn as_nil(self: Value) void {
        _ = self;
        return;
    }

    pub inline fn as_number(self: Value) f64 {
        return self.Number;
    }
};
