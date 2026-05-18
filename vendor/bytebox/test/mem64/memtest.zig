const std = @import("std");

const KB = 1024;
const MB = 1024 * KB;
const GB = 1024 * MB;

const PAGE_SIZE = 64 * KB;
const PAGES_PER_GB = GB / PAGE_SIZE;

const GLOBAL_DATA: []const volatile u8 = "YNDKMI*#"; // tests if data segments use index type

fn assert(cond: bool) !void {
    if (!cond) {
        return error.Failed;
    }
}

fn alignPtr(mem: [*]volatile u8, alignment: usize) [*]volatile u8 {
    return @ptrFromInt(std.mem.alignForward(usize, @intFromPtr(mem), alignment)); // volatile?
}

fn alignToSinglePtr(comptime T: type, mem: [*]volatile u8) *volatile T {
    const mem_aligned = alignPtr(mem, @alignOf(T));
    return @ptrCast(@alignCast(mem_aligned));
}

export fn memtest(val_i32: i32, val_i64: i64, val_f32: f32, val_f64: f64) i32 {
    testInternal(val_i32, val_i64, val_f32, val_f64) catch {
        return 1;
    };
    return 0;
}

fn testInternal(val_i32: i32, val_i64: i64, val_f32: f32, val_f64: f64) !void {
    const grow_value: isize = @wasmMemoryGrow(0, PAGES_PER_GB * 6); // memory.grow
    try assert(grow_value != -1);

    // volatile pointers ensure the loads and stores don't get optimized away
    const start_page: [*]volatile u8 = @ptrFromInt(@as(usize, @intCast(grow_value)));

    const mem = start_page + (GB * 4);
    const mem_loads: [*]volatile u8 = mem + MB * 2;
    const mem_stores: [*]volatile u8 = mem + MB * 1;

    const num_pages: usize = @wasmMemorySize(0);
    try assert(num_pages >= PAGES_PER_GB * 6);

    const ptr_load_i32 = alignToSinglePtr(i32, mem_loads + 0);
    const ptr_load_i64 = alignToSinglePtr(i64, mem_loads + 64);
    const ptr_load_f32 = alignToSinglePtr(f32, mem_loads + 128);
    const ptr_load_f64 = alignToSinglePtr(f64, mem_loads + 192);

    ptr_load_i32.* = val_i32; // i32.store
    ptr_load_i64.* = val_i64; // i64.store
    ptr_load_f32.* = val_f32; // f32.store
    ptr_load_f64.* = val_f64; // f64.store

    try assert(ptr_load_i32.* == val_i32);
    try assert(ptr_load_i64.* == val_i64);
    try assert(ptr_load_f32.* == val_f32);
    try assert(ptr_load_f64.* == val_f64);

    const ptr_store_i32 = alignToSinglePtr(i32, mem_stores + 0);
    const ptr_store_i64 = alignToSinglePtr(i64, mem_stores + 64);
    const ptr_store_f32 = alignToSinglePtr(f32, mem_stores + 128);
    const ptr_store_f64 = alignToSinglePtr(f64, mem_stores + 192);

    ptr_store_i32.* = ptr_load_i32.*; // i32.load && i32.store
    ptr_store_i64.* = ptr_load_i64.*; // i64.load && i64.store
    ptr_store_f32.* = ptr_load_f32.*; // f32.load && f32.store
    ptr_store_f64.* = ptr_load_f64.*; // f64.load && f64.store

    try assert(ptr_store_i32.* == ptr_load_i32.*);
    try assert(ptr_store_i64.* == ptr_load_i64.*);
    try assert(ptr_store_f32.* == ptr_load_f32.*);
    try assert(ptr_store_f64.* == ptr_load_f64.*);

    var load32: i32 = 0;
    ptr_load_i32.* = 0x7F;
    load32 = @as(*volatile i8, @ptrCast(@alignCast(ptr_load_i32))).*; // i32.load8_s
    try assert(load32 == 0x7F);
    ptr_load_i32.* = 0xFF;
    load32 = @as(*volatile u8, @ptrCast(@alignCast(ptr_load_i32))).*; // i32.load8_u
    try assert(load32 == 0xFF);
    ptr_load_i32.* = 0x7FFF;
    load32 = @as(*volatile i16, @ptrCast(@alignCast(ptr_load_i32))).*; // i32.load16_s
    try assert(load32 == 0x7FFF);
    ptr_load_i32.* = 0xFFFF;
    load32 = @as(*volatile u16, @ptrCast(@alignCast(ptr_load_i32))).*; // i32.load16_s
    try assert(load32 == 0xFFFF);

    var load64: i64 = 0;
    ptr_load_i64.* = 0x7F;
    load64 = @as(*volatile i8, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load8_s
    try assert(load64 == 0x7F);
    ptr_load_i64.* = 0xFF;
    load64 = @as(*volatile u8, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load8_u
    try assert(load64 == 0xFF);
    ptr_load_i64.* = 0x7FFF;
    load64 = @as(*volatile i16, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load16_s
    try assert(load64 == 0x7FFF);
    ptr_load_i64.* = 0xFFFF;
    load64 = @as(*volatile u16, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load16_u
    try assert(load64 == 0xFFFF);
    ptr_load_i64.* = 0x7FFFFFFF;
    load64 = @as(*volatile i32, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load32_s
    try assert(load64 == 0x7FFFFFFF);
    ptr_load_i64.* = 0xFFFFFFFF;
    load64 = @as(*volatile u32, @ptrCast(@alignCast(ptr_load_i64))).*; // i64.load32_u
    try assert(load64 == 0xFFFFFFFF);

    const memset_dest = (mem + KB)[0..KB];
    const memcpy_dest = (mem + KB * 2)[0..KB];
    @memset(memset_dest, 0xFF); // memory.fill
    @memcpy(memcpy_dest, memset_dest); // memory.copy

    try assert(memset_dest[0] == 0xFF);
    try assert(memset_dest[KB - 1] == 0xFF);
    try assert(memcpy_dest[0] == 0xFF);
    try assert(memcpy_dest[KB - 1] == 0xFF);

    // forces data segment to be generated
    @memcpy(memcpy_dest[0..GLOBAL_DATA.len], GLOBAL_DATA);

    try assert(memcpy_dest[0] == 'Y');
    try assert(memcpy_dest[1] == 'N');
    try assert(memcpy_dest[2] == 'D');
    try assert(memcpy_dest[3] == 'K');
    try assert(memcpy_dest[4] == 'M');
    try assert(memcpy_dest[5] == 'I');
    try assert(memcpy_dest[6] == '*');
    try assert(memcpy_dest[7] == '#');
}
