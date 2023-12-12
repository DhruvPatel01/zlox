const std = @import("std");

const object = @import("object.zig");
const Obj = object.Obj;
const common = @import("common.zig");

pub const Value = if (common.nan_boxing) ValuePacked else ValueUnPacked;

pub const ValuePacked = struct {
    const Self = @This();
    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;
    const NIL_TAG: u64 = 1;
    const FALSE_TAG: u64 = 2;
    const TRUE_TAG: u64 = 3;

    p: u64,

    const NIL_VAL: Self = .{ .p = QNAN | NIL_TAG };
    const TRUE_VAL: Self = .{ .p = QNAN | TRUE_TAG };
    const FALSE_VAL: Self = .{ .p = QNAN | FALSE_TAG };

    pub inline fn is_number(self: Self) bool {
        return (self.p & QNAN) != QNAN;
    }

    pub fn number(num: f64) Self {
        var x: u64 = undefined;
        @memcpy(std.mem.asBytes(&x), std.mem.asBytes(&num));
        return .{ .p = x };
    }

    pub fn as_number(self: Self) f64 {
        var x: f64 = undefined;
        @memcpy(std.mem.asBytes(&x), std.mem.asBytes(&self.p));
        return x;
    }

    pub inline fn is_nil(self: Self) bool {
        return self.p == NIL_VAL.p;
    }

    pub inline fn nil() Self {
        return NIL_VAL;
    }

    pub inline fn is_boolean(self: Self) bool {
        return self.p | 1 == TRUE_VAL.p;
    }

    pub inline fn boolean(b: bool) Self {
        if (b) return TRUE_VAL else return FALSE_VAL;
    }

    pub inline fn as_boolean(self: Self) bool {
        return self.p == TRUE_VAL.p;
    }

    pub inline fn is_falsy(self: Self) bool {
        return (self.is_nil() or (self.is_boolean() and !self.as_boolean()));
    }

    pub inline fn obj(obj_: *object.Obj) Self {
        return .{ .p = SIGN_BIT | QNAN | @intFromPtr(obj_) };
    }

    pub inline fn as_obj(self: Self) *object.Obj {
        return @ptrFromInt(self.p & ~(SIGN_BIT | QNAN));
    }

    pub inline fn is_obj(self: Self) bool {
        return self.p & (QNAN | SIGN_BIT) == (QNAN | SIGN_BIT);
    }

    pub inline fn as(self: Self, comptime T: type) *T {
        return self.as_obj().downcast(T);
    }

    pub inline fn is(self: Self, typ: object.ObjType) bool {
        return self.is_obj() and self.as_obj().type == typ;
    }

    pub fn print(self: Self, writer: anytype, comptime newline: bool) void {
        if (newline) {
            if (self.is_number()) {
                writer.print("{d}\n", .{self.as_number()}) catch {};
            } else if (self.is_boolean()) {
                writer.print("{}\n", .{self.as_boolean()}) catch {};
            } else if (self.is_nil()) {
                writer.print("nil\n", .{}) catch {};
            } else {
                self.as_obj().print(writer, newline) catch {};
            }
        } else {
            if (self.is_number()) {
                writer.print("{d}", .{self.as_number()}) catch {};
            } else if (self.is_boolean()) {
                writer.print("{}", .{self.as_boolean()}) catch {};
            } else if (self.is_nil()) {
                writer.print("nil", .{}) catch {};
            } else {
                self.as_obj().print(writer, newline) catch {};
            }
        }
    }

    pub inline fn values_equal(a: Self, b: Self) bool {
        if (a.is_number() and b.is_number()) {
            return a.as_number() == b.as_number();
        }

        return a.p == b.p;
    }
};

pub const ValueUnPacked = union(enum) {
    const Self = @This();

    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn print(self: *const Self, writer: anytype, comptime newline: bool) void {
        if (newline) {
            switch (self.*) {
                .Number => |n| writer.print("{d}\n", .{n}) catch {},
                .Bool => |b| writer.print("{}\n", .{b}) catch {},
                .Nil => writer.print("nil\n", .{}) catch {},
                .Obj => |o| o.print(writer, newline) catch {},
            }
        } else {
            switch (self.*) {
                .Number => |n| writer.print("{d}", .{n}) catch {},
                .Bool => |b| writer.print("{}", .{b}) catch {},
                .Nil => writer.print("nil", .{}) catch {},
                .Obj => |o| o.print(writer, newline) catch {},
            }
        }
    }

    pub inline fn boolean(val: bool) Self {
        return .{ .Bool = val };
    }

    pub inline fn is_nil(self: *const Self) bool {
        return self.* == Self.Nil;
    }

    pub inline fn nil() Self {
        return Self.Nil;
    }

    pub inline fn number(num: f64) Self {
        return .{ .Number = num };
    }

    pub inline fn obj(obj_: *object.Obj) Self {
        return .{ .Obj = obj_ };
    }

    pub inline fn as_obj(self: *const Self) *object.Obj {
        return self.Obj;
    }

    pub inline fn is_falsy(self: Self) bool {
        return (self == .Nil or (self == Self.Bool and !self.Bool));
    }

    pub inline fn is_number(self: *const Self) bool {
        return self.* == .Number;
    }

    pub inline fn as_number(self: Self) f64 {
        return self.Number;
    }

    pub inline fn values_equal(a: Self, b: Self) bool {
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

    pub inline fn is_obj(self: *const Self) bool {
        return self.* == .Obj;
    }
};
