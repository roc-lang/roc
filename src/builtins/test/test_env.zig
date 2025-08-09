const std = @import("std");
const builtins = @import("builtins");
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

/// Tracks allocations for testing purposes with C ABI compatibility. Uses a single global testing allocator to track allocations. If we need multiple independent allocators we will need to modify this and use comptime.
pub const TestEnv = struct {
    const AllocationInfo = struct {
        size: usize,
        alignment: usize,
    };
    const AllocationMap = std.HashMap(*anyopaque, AllocationInfo, std.hash_map.AutoContext(*anyopaque), std.hash_map.default_max_load_percentage);

    allocation_map: AllocationMap,
    allocator: std.mem.Allocator,
    ops: ?RocOps,

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return TestEnv{
            .allocation_map = AllocationMap.init(allocator),
            .allocator = allocator,
            .ops = null,
        };
    }

    pub fn getOps(self: *TestEnv) *RocOps {
        if (self.ops == null) {
            self.ops = RocOps{
                .env = @as(*anyopaque, @ptrCast(self)),
                .roc_alloc = rocAllocFn,
                .roc_dealloc = rocDeallocFn,
                .roc_realloc = rocReallocFn,
                .roc_dbg = rocDbgFn,
                .roc_expect_failed = rocExpectFailedFn,
                .roc_crashed = rocCrashedFn,
                .host_fns = undefined, // No host functions in tests
            };
        }
        return &self.ops.?;
    }

    pub fn deinit(self: *TestEnv) void {
        // Free any remaining allocations
        var iterator = self.allocation_map.iterator();
        while (iterator.next()) |entry| {
            const bytes: [*]u8 = @ptrCast(@alignCast(entry.key_ptr.*));
            const slice = bytes[0..entry.value_ptr.size];
            // For aligned allocations, we need to free them properly
            switch (entry.value_ptr.alignment) {
                1 => self.allocator.free(slice),
                2 => self.allocator.free(@as([]align(2) u8, @alignCast(slice))),
                4 => self.allocator.free(@as([]align(4) u8, @alignCast(slice))),
                8 => self.allocator.free(@as([]align(8) u8, @alignCast(slice))),
                16 => self.allocator.free(@as([]align(16) u8, @alignCast(slice))),
                else => @panic("Unsupported alignment in test deallocator cleanup"),
            }
        }

        self.allocation_map.deinit();
    }

    pub fn getAllocationCount(self: *const TestEnv) usize {
        return self.allocation_map.count();
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.C) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        // Allocate memory using the testing allocator with comptime alignment
        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, 1, roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, 2, roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, 4, roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, 8, roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, 16, roc_alloc.length),
            else => @panic("Unsupported alignment in test allocator"),
        } catch {
            @panic("Test allocation failed");
        };

        // Cast the pointer to *anyopaque
        const result: *anyopaque = @ptrCast(ptr.ptr);

        // Save the allocation in the map
        self.allocation_map.put(result, AllocationInfo{
            .size = roc_alloc.length,
            .alignment = roc_alloc.alignment,
        }) catch {
            self.allocator.free(ptr);
            @panic("Failed to track test allocation");
        };

        roc_alloc.answer = result;
    }

    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.C) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        if (self.allocation_map.fetchRemove(roc_dealloc.ptr)) |entry| {
            const bytes: [*]u8 = @ptrCast(@alignCast(roc_dealloc.ptr));
            const slice = bytes[0..entry.value.size];
            // For aligned allocations, we need to free them properly
            switch (entry.value.alignment) {
                1 => self.allocator.free(slice),
                2 => self.allocator.free(@as([]align(2) u8, @alignCast(slice))),
                4 => self.allocator.free(@as([]align(4) u8, @alignCast(slice))),
                8 => self.allocator.free(@as([]align(8) u8, @alignCast(slice))),
                16 => self.allocator.free(@as([]align(16) u8, @alignCast(slice))),
                else => @panic("Unsupported alignment in test deallocator"),
            }
        }
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.C) void {
        _ = env;
        _ = roc_realloc;
        @panic("Test realloc not implemented yet");
    }

    fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.C) void {
        _ = env;
        const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
        std.debug.print("DBG: {s}\n", .{message});
    }

    fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
        _ = env;
        const message = @as([*]u8, @ptrCast(roc_expect.utf8_bytes))[0..roc_expect.len];
        std.debug.print("EXPECT FAILED: {s}\n", .{message});
    }

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.C) noreturn {
        _ = env;
        const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
        @panic(message);
    }
};
