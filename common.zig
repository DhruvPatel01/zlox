const std = @import("std");

pub const allocator = std.heap.c_allocator;
// pub const general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
// // defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
// var allocator = null;

pub const debug_print_code = false;
pub const debug_trace_execution = false;
