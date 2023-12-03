const std = @import("std");

const _gc = @import("gc.zig");

const c_allocator = std.heap.c_allocator;
var gc = _gc.GCAllocator.init(c_allocator);
const gc_allocator = gc.allocator();

// var logging_allocator = std.heap.loggingAllocator(gc_allocator);
// pub const allocator = logging_allocator.allocator();

pub const allocator = gc.allocator();

pub const debug_print_code = false;
pub const debug_trace_execution = false;
pub const debug_stress_gc = false;
pub const debug_log_gc = false;
