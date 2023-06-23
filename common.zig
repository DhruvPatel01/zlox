const std = @import("std");

pub const allocator = std.heap.page_allocator;

pub const debug_print_code = true;
pub const debug_trace_execution = false;
