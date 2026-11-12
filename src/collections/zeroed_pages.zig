//! Fresh zero pages and virtual reservations for lifecycle-aware tables.
//! Mapping policy is centralized here so table owners only choose density.
//!
//! `map` returns pages that read as zero, with no fill. `reserve` returns a
//! virtual range for a growing column whose pages are committed as the column
//! reaches them, so growth never copies. Every host has an explicit
//! implementation of the same contract, chosen at compile time:
//!
//! - Linux: anonymous private mappings, `MADV_HUGEPAGE` for dense tables and
//!   `MADV_NOHUGEPAGE` for sparse ones, `MADV_DONTNEED` to make pages zero
//!   again, `MAP_NORESERVE` reservations.
//! - macOS and the BSDs: anonymous private mappings with no huge-page advice;
//!   a reset maps fresh anonymous pages over the range.
//! - Windows: `MEM_RESERVE` ranges committed in chunks ahead of the length;
//!   trimming and resetting decommit.
//! - The freestanding wasm host: pages from `memory.grow` are zero by
//!   specification but cannot be returned, so freed blocks are kept on a free
//!   list keyed by size and cleared before they are handed out again, and a
//!   reservation grows by relocation.
//!
//! Mapping failure is `error.OutOfMemory`, the same user-facing failure as an
//! allocation returning null.

const std = @import("std");
const builtin = @import("builtin");
const storage_work = @import("storage_work.zig");

const Allocator = std.mem.Allocator;

/// How a table's pages are touched, which decides the huge-page advice.
pub const Density = enum {
    /// Most pages are touched: huge pages cut the fault count by 512x.
    dense,
    /// Few pages are touched: a touched entry costs one small page.
    sparse,
};

/// Sparse columns no larger than this stay on the recycled allocator.
pub const small_table_bytes: usize = 32 * 1024;

/// The huge-page size reservations are aligned to.
pub const huge_page_size: usize = 2 * 1024 * 1024;

const Host = enum { linux, posix, windows, wasm };

const host: Host = switch (builtin.os.tag) {
    .linux => .linux,
    .macos, .ios, .tvos, .watchos, .visionos, .maccatalyst, .driverkit, .freebsd, .netbsd, .openbsd, .dragonfly, .illumos, .haiku => .posix,
    .windows => .windows,
    .freestanding, .wasi => .wasm,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .managarm,
    .hurd,
    .plan9,
    .rtems,
    .serenity,
    .uefi,
    .@"3ds",
    .wiiu,
    .@"switch",
    .gba,
    .psx,
    .tios,
    .ashetos,
    .ps3,
    .ps4,
    .ps5,
    .psp,
    .vita,
    .emscripten,
    .amdhsa,
    .amdpal,
    .cuda,
    .mesa3d,
    .nvcl,
    .opencl,
    .opengl,
    .vulkan,
    => @compileError("the compiler's page primitive has no implementation for this host"),
};

/// The granularity of every mapping on this host.
pub fn pageSize() usize {
    return switch (host) {
        .wasm => std.wasm.page_size,
        .linux, .posix, .windows => std.heap.pageSize(),
    };
}

/// Whether the host can move a reservation by moving page tables.
pub const can_remap_reservations = host == .linux;

const page_align = std.heap.page_size_min;
const PageSlice = []align(page_align) u8;

/// Whether a reservation pre-faults the chunk ahead of its length on Linux.
/// Disabled until corpus measurements justify the extra commit calls.
pub const linux_commit_ahead = false;
/// The bytes a reservation commits ahead of its length at a time.
pub const commit_chunk_bytes: usize = huge_page_size;

/// Fresh pages that read as zero.
pub const Pages = struct {
    bytes: PageSlice,
    density: Density = .sparse,

    /// Release the pages. Never touches their contents.
    pub fn unmap(self: Pages) void {
        if (self.bytes.len == 0) return;
        unmapRange(self.bytes);
    }

    /// Make every page read as zero again. This is a guaranteed-zero reset,
    /// never an advisory one: its cost is proportional to the pages touched
    /// since the mapping or the last reset, and the next touch of a page
    /// faults again.
    pub fn reset(self: Pages) Allocator.Error!void {
        if (self.bytes.len == 0) return;
        storage_work.add(.zero_resets, 1);
        switch (host) {
            .linux => {
                const rc = std.os.linux.madvise(self.bytes.ptr, self.bytes.len, std.os.linux.MADV.DONTNEED);
                // DONTNEED on a private anonymous mapping fails only for an
                // address range that is not such a mapping.
                std.debug.assert(std.os.linux.errno(rc) == .SUCCESS);
            },
            .posix => {
                _ = std.posix.mmap(
                    self.bytes.ptr,
                    self.bytes.len,
                    .{ .READ = true, .WRITE = true },
                    .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .FIXED = true },
                    -1,
                    0,
                ) catch return error.OutOfMemory;
                switch (self.density) {
                    .dense => advise(self.bytes, .dense),
                    .sparse => advise(self.bytes, .sparse),
                }
            },
            .windows => {
                windowsFree(self.bytes.ptr, self.bytes.len, .{ .DECOMMIT = true });
                try windowsCommit(self.bytes.ptr, self.bytes.len);
            },
            .wasm => {
                @memset(self.bytes, 0);
                storage_work.add(.bytes_filled, self.bytes.len);
            },
        }
    }
};

/// Map `len` bytes of fresh pages that read as zero. `len` is rounded up to
/// the page size. A zero length maps nothing.
pub fn map(len: usize, comptime density: Density) Allocator.Error!Pages {
    if (len == 0) return .{ .bytes = &.{} };
    const rounded = try roundToPages(len);
    const bytes: PageSlice = switch (host) {
        .linux, .posix => try posixMap(rounded, false),
        .windows => blk: {
            const ptr = try windowsReserve(rounded);
            windowsCommit(ptr, rounded) catch |err| {
                windowsFree(ptr, 0, .{ .RELEASE = true });
                return err;
            };
            break :blk ptr[0..rounded];
        },
        .wasm => try wasm_pool.take(rounded),
    };
    advise(bytes, density);
    storage_work.add(.mappings, 1);
    storage_work.add(.mapped_bytes, rounded);
    return .{ .bytes = bytes, .density = density };
}

/// A zero-initialized sparse column. Small columns use the owner's allocator;
/// large columns leave untouched pages nonresident. `T` must have no padding
/// and must admit the all-zero value.
pub fn SparseSlice(comptime T: type) type {
    if (!std.meta.hasUniqueRepresentation(T)) @compileError("sparse zero storage requires a padding-free element");
    if (@alignOf(T) > std.heap.page_size_min) @compileError("sparse zero elements must fit the host page alignment");
    const zero = std.mem.zeroes(T);
    return struct {
        const Self = @This();
        /// The logical entries.
        items: []T = &.{},
        pages: Pages = .{ .bytes = &.{} },

        /// Fresh zero entries without a fill for large domains.
        pub fn init(gpa: Allocator, len: usize) Allocator.Error!Self {
            const bytes = std.math.mul(usize, len, @sizeOf(T)) catch return error.OutOfMemory;
            if (bytes <= small_table_bytes) {
                const items = try gpa.alloc(T, len);
                @memset(items, zero);
                storage_work.add(.bytes_filled, bytes);
                return .{ .items = items };
            }
            const pages = try map(bytes, .sparse);
            return .{ .items = @as([*]T, @ptrCast(@alignCast(pages.bytes.ptr)))[0..len], .pages = pages };
        }

        /// Release owned storage without inspecting its entries.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (self.pages.bytes.len != 0) self.pages.unmap() else gpa.free(self.items);
            self.* = .{};
        }
    };
}

/// A virtual range for a column that grows by appending. The column commits
/// pages ahead of its length with `commitTo`, and its base address stays put
/// for as long as its length fits the reservation, on every host with
/// virtual memory. The wasm host has no reservation: `commitTo` past the
/// current block relocates it, which is why every reader of a reservation
/// takes its base after `commitTo`.
pub const Reservation = struct {
    /// The usable range, aligned to `huge_page_size` on hosts with virtual
    /// memory. Its length is the reservation's size.
    bytes: PageSlice = &.{},
    /// Bytes from the start of `bytes` that may be read and written.
    committed: usize = 0,
    /// The whole mapping owned by this reservation, which can begin before
    /// `bytes` (alignment) and is shortened by `trim`.
    mapping: PageSlice = &.{},
    density: Density = .dense,

    /// Make the first `len` bytes accessible. Pages beyond the previously
    /// committed length read as zero. Fails with `error.OutOfMemory` when
    /// the commit fails, or when `len` passes the reservation on hosts with
    /// virtual memory, where the owner moves the column (see
    /// `ReservedList`).
    pub fn commitTo(self: *Reservation, len: usize) Allocator.Error!void {
        if (len <= self.committed) return;
        switch (host) {
            .linux => {
                if (len > self.bytes.len) return error.OutOfMemory;
                const target = @min(self.bytes.len, std.mem.alignForward(usize, len, commit_chunk_bytes));
                if (linux_commit_ahead) {
                    storage_work.add(.commit_calls, 1);
                    const populate_write = 23;
                    const rc = std.os.linux.madvise(self.bytes.ptr + self.committed, target - self.committed, populate_write);
                    // Commit failures, including lack of kernel support for
                    // prefaulting, propagate rather than silently changing policy.
                    if (std.os.linux.errno(rc) != .SUCCESS) return error.OutOfMemory;
                }
                self.committed = target;
            },
            .posix => {
                if (len > self.bytes.len) return error.OutOfMemory;
                self.committed = @min(self.bytes.len, std.mem.alignForward(usize, len, commit_chunk_bytes));
            },
            .windows => {
                if (len > self.bytes.len) return error.OutOfMemory;
                const target = @min(self.bytes.len, std.mem.alignForward(usize, len, commit_chunk_bytes));
                storage_work.add(.commit_calls, 1);
                try windowsCommit(@alignCast(self.bytes.ptr + self.committed), target - self.committed);
                self.committed = target;
            },
            .wasm => {
                var capacity = @max(self.bytes.len, pageSize());
                while (capacity < len) capacity = std.math.mul(usize, capacity, 2) catch return error.OutOfMemory;
                const moved = try wasm_pool.take(capacity);
                @memcpy(moved[0..self.committed], self.bytes[0..self.committed]);
                storage_work.add(.bytes_copied, self.committed);
                if (self.bytes.len != 0) wasm_pool.give(self.bytes);
                self.bytes = moved;
                self.mapping = moved;
                self.committed = moved.len;
            },
        }
    }

    /// Move a Linux reservation to a larger huge-page-aligned range without
    /// copying its contents. Failure leaves the original mapping owned here.
    pub fn relocateLinux(self: *Reservation, max_len: usize, comptime new_density: Density) Allocator.Error!void {
        if (comptime host != .linux) @compileError("reservation remapping requires Linux");
        std.debug.assert(self.mapping.ptr == self.bytes.ptr);
        std.debug.assert(max_len > self.bytes.len);
        var next = try reserve(max_len, new_density);
        errdefer next.release();
        // Advice applies to the usable range only. Its alignment tail can
        // therefore be a separate VMA, which mremap must not span.
        const tail = self.mapping[self.bytes.len..];
        _ = std.posix.mremap(
            self.bytes.ptr,
            self.bytes.len,
            next.bytes.len,
            .{ .MAYMOVE = true, .FIXED = true },
            next.bytes.ptr,
        ) catch return error.OutOfMemory;
        if (tail.len != 0) std.posix.munmap(@alignCast(tail));
        next.committed = self.committed;
        self.* = next;
        storage_work.add(.relocations, 1);
    }

    /// Return the pages past `live_len` to the kernel. The reservation can
    /// no longer grow afterwards. On Windows the address range stays
    /// reserved and only its commit is released.
    pub fn trim(self: *Reservation, live_len: usize) void {
        std.debug.assert(live_len <= self.committed);
        const page = pageSize();
        const keep = std.mem.alignForward(usize, live_len, page);
        switch (host) {
            .linux, .posix => {
                const mapping_end = @intFromPtr(self.mapping.ptr) + self.mapping.len;
                const keep_end = @intFromPtr(self.bytes.ptr) + keep;
                if (keep_end < mapping_end) {
                    const tail: PageSlice = @as([*]align(page_align) u8, @ptrFromInt(keep_end))[0 .. mapping_end - keep_end];
                    storage_work.add(.pages_trimmed, tail.len / page);
                    std.posix.munmap(tail);
                    self.mapping.len -= tail.len;
                }
                self.bytes.len = keep;
                self.committed = @min(self.committed, keep);
            },
            .windows => {
                if (keep < self.committed) {
                    storage_work.add(.pages_trimmed, (self.committed - keep) / page);
                    windowsFree(@alignCast(self.bytes.ptr + keep), self.committed - keep, .{ .DECOMMIT = true });
                }
                self.bytes.len = keep;
                self.committed = @min(self.committed, keep);
            },
            .wasm => {},
        }
    }

    /// Release the whole reservation.
    pub fn release(self: *Reservation) void {
        if (self.mapping.len != 0) {
            switch (host) {
                .linux, .posix => std.posix.munmap(self.mapping),
                .windows => windowsFree(self.mapping.ptr, 0, .{ .RELEASE = true }),
                .wasm => wasm_pool.give(self.mapping),
            }
        }
        self.* = .{};
    }
};

/// Reserve `max_len` bytes of address space for a growing column. Nothing is
/// committed until `commitTo`. On hosts with virtual memory the range is
/// aligned to `huge_page_size`, so huge-page backing applies from its first
/// entry.
pub fn reserve(max_len: usize, comptime density: Density) Allocator.Error!Reservation {
    if (max_len == 0) return .{ .density = density };
    const usable = try roundTo(max_len, huge_page_size);
    const result: Reservation = switch (host) {
        .linux, .posix => blk: {
            const over = std.math.add(usize, usable, huge_page_size) catch return error.OutOfMemory;
            var mapping = try posixMap(over, true);
            const start = std.mem.alignForward(usize, @intFromPtr(mapping.ptr), huge_page_size);
            const lead = start - @intFromPtr(mapping.ptr);
            if (lead != 0) std.posix.munmap(mapping[0..lead]);
            mapping = @as([*]align(page_align) u8, @ptrFromInt(start))[0 .. over - lead];
            const bytes = mapping[0..usable];
            advise(bytes, density);
            break :blk .{ .bytes = bytes, .mapping = mapping, .density = density };
        },
        .windows => blk: {
            const over = std.math.add(usize, usable, huge_page_size) catch return error.OutOfMemory;
            const ptr = try windowsReserve(over);
            const start = std.mem.alignForward(usize, @intFromPtr(ptr), huge_page_size);
            const bytes = @as([*]align(page_align) u8, @ptrFromInt(start))[0..usable];
            break :blk .{ .bytes = bytes, .mapping = ptr[0..over], .density = density };
        },
        .wasm => .{ .density = density },
    };
    if (result.mapping.len != 0) {
        storage_work.add(.reservations, 1);
        storage_work.add(.reserved_bytes, result.mapping.len);
    }
    return result;
}

fn roundTo(len: usize, unit: usize) Allocator.Error!usize {
    const padded = std.math.add(usize, len, unit - 1) catch return error.OutOfMemory;
    return padded & ~(unit - 1);
}

fn roundToPages(len: usize) Allocator.Error!usize {
    return roundTo(len, pageSize());
}

fn advise(bytes: PageSlice, comptime density: Density) void {
    if (host != .linux) return;
    const advice = switch (density) {
        .dense => std.os.linux.MADV.HUGEPAGE,
        .sparse => std.os.linux.MADV.NOHUGEPAGE,
    };
    // Huge-page advice is a performance hint that has no effect on contents;
    // a kernel built without transparent huge pages rejects it.
    _ = std.os.linux.madvise(bytes.ptr, bytes.len, advice);
}

fn posixMap(len: usize, comptime no_reserve: bool) Allocator.Error!PageSlice {
    var flags: std.posix.MAP = .{ .TYPE = .PRIVATE, .ANONYMOUS = true };
    if (no_reserve and @hasField(std.posix.MAP, "NORESERVE")) flags.NORESERVE = true;
    return std.posix.mmap(null, len, .{ .READ = true, .WRITE = true }, flags, -1, 0) catch error.OutOfMemory;
}

fn unmapRange(bytes: PageSlice) void {
    switch (host) {
        .linux, .posix => std.posix.munmap(bytes),
        .windows => windowsFree(bytes.ptr, 0, .{ .RELEASE = true }),
        .wasm => wasm_pool.give(bytes),
    }
}

const windows = if (host == .windows) std.os.windows else struct {};

fn windowsReserve(len: usize) Allocator.Error![*]align(page_align) u8 {
    var base_addr: ?*anyopaque = null;
    var size: windows.SIZE_T = len;
    const status = windows.ntdll.NtAllocateVirtualMemory(windows.GetCurrentProcess(), @ptrCast(&base_addr), 0, &size, .{ .RESERVE = true }, .{ .NOACCESS = true });
    if (status != .SUCCESS) return error.OutOfMemory;
    return @ptrCast(@alignCast(base_addr));
}

fn windowsCommit(ptr: [*]align(page_align) u8, len: usize) Allocator.Error!void {
    if (len == 0) return;
    var base_addr: ?*anyopaque = ptr;
    var size: windows.SIZE_T = len;
    const status = windows.ntdll.NtAllocateVirtualMemory(windows.GetCurrentProcess(), @ptrCast(&base_addr), 0, &size, .{ .COMMIT = true }, .{ .READWRITE = true });
    if (status != .SUCCESS) return error.OutOfMemory;
}

fn windowsFree(ptr: [*]align(page_align) u8, len: usize, free_type: windows.MEM.FREE) void {
    var base_addr: ?*anyopaque = ptr;
    var size: windows.SIZE_T = len;
    const status = windows.ntdll.NtFreeVirtualMemory(windows.GetCurrentProcess(), @ptrCast(&base_addr), &size, free_type);
    std.debug.assert(status == .SUCCESS);
}

/// The wasm host's pages: `memory.grow` returns zero pages that can never be
/// returned, so freed blocks are kept by size and cleared before reuse. The
/// wasm host has no threads.
const wasm_pool = struct {
    const FreeBlock = struct { next: ?*FreeBlock, len: usize };
    var head: ?*FreeBlock = null;

    fn take(len: usize) Allocator.Error!PageSlice {
        var link = &head;
        while (link.*) |block| : (link = &block.next) {
            if (block.len == len) {
                link.* = block.next;
                const bytes: PageSlice = @as([*]align(page_align) u8, @ptrCast(@alignCast(block)))[0..len];
                @memset(bytes, 0);
                storage_work.add(.bytes_filled, len);
                return bytes;
            }
        }
        const pages = len / std.wasm.page_size;
        const previous = @wasmMemoryGrow(0, pages);
        if (previous < 0) return error.OutOfMemory;
        const start: usize = @as(usize, @intCast(previous)) * std.wasm.page_size;
        return @as([*]align(page_align) u8, @ptrFromInt(start))[0..len];
    }

    fn give(bytes: PageSlice) void {
        const block: *FreeBlock = @ptrCast(bytes.ptr);
        block.* = .{ .next = head, .len = bytes.len };
        head = block;
    }
};

test "mapped pages read as zero and reset makes them zero again" {
    const pages = try map(3 * pageSize() + 1, .sparse);
    defer pages.unmap();
    try std.testing.expectEqual(4 * pageSize(), pages.bytes.len);
    for (pages.bytes) |byte| try std.testing.expectEqual(@as(u8, 0), byte);
    pages.bytes[5] = 7;
    pages.bytes[pageSize() * 2 + 3] = 9;
    try pages.reset();
    for (pages.bytes) |byte| try std.testing.expectEqual(@as(u8, 0), byte);
}

test "a zero-length mapping maps nothing" {
    const pages = try map(0, .dense);
    defer pages.unmap();
    try std.testing.expectEqual(@as(usize, 0), pages.bytes.len);
}

test "a reservation commits ahead, stays put while it fits, and trims its tail" {
    var reservation = try reserve(8 * huge_page_size, .dense);
    defer reservation.release();
    try reservation.commitTo(100);
    const base_address = reservation.bytes.ptr;
    reservation.bytes[0] = 1;
    reservation.bytes[99] = 2;
    try reservation.commitTo(3 * huge_page_size + 5);
    if (host != .wasm) {
        try std.testing.expectEqual(base_address, reservation.bytes.ptr);
        try std.testing.expect(std.mem.isAligned(@intFromPtr(base_address), huge_page_size));
    }
    try std.testing.expectEqual(@as(u8, 1), reservation.bytes[0]);
    try std.testing.expectEqual(@as(u8, 2), reservation.bytes[99]);
    try std.testing.expectEqual(@as(u8, 0), reservation.bytes[3 * huge_page_size + 4]);
    reservation.trim(200);
    try std.testing.expectEqual(@as(u8, 2), reservation.bytes[99]);
    if (host != .wasm) {
        try std.testing.expectEqual(pageSize(), reservation.bytes.len);
        try std.testing.expectError(error.OutOfMemory, reservation.commitTo(9 * huge_page_size));
    }
}

test "counters observe mappings, reservations and trims" {
    if (!storage_work.enabled) return error.SkipZigTest;
    const mappings = storage_work.get(.mappings);
    const reservations = storage_work.get(.reservations);
    const trimmed = storage_work.get(.pages_trimmed);
    const pages = try map(1, .dense);
    pages.unmap();
    var reservation = try reserve(huge_page_size, .sparse);
    try reservation.commitTo(1);
    reservation.trim(1);
    reservation.release();
    try std.testing.expectEqual(mappings + 1, storage_work.get(.mappings));
    try std.testing.expectEqual(reservations + @intFromBool(host != .wasm), storage_work.get(.reservations));
    if (host != .wasm) try std.testing.expect(storage_work.get(.pages_trimmed) > trimmed);
}

test "Linux reservation growth moves page tables without copying bytes" {
    if (!can_remap_reservations) return error.SkipZigTest;
    var reservation = try reserve(huge_page_size, .dense);
    defer reservation.release();
    try reservation.commitTo(100);
    reservation.bytes[0] = 17;
    reservation.bytes[99] = 19;
    const copied = storage_work.get(.bytes_copied);
    try reservation.relocateLinux(4 * huge_page_size, .dense);
    try reservation.commitTo(3 * huge_page_size);
    try std.testing.expect(std.mem.isAligned(@intFromPtr(reservation.bytes.ptr), huge_page_size));
    try std.testing.expectEqual(@as(u8, 17), reservation.bytes[0]);
    try std.testing.expectEqual(@as(u8, 19), reservation.bytes[99]);
    try std.testing.expectEqual(@as(u8, 0), reservation.bytes[2 * huge_page_size]);
    try std.testing.expectEqual(copied, storage_work.get(.bytes_copied));
}

test "sparse atomic byte columns start unknown without filling large domains" {
    const Byte = std.atomic.Value(u8);
    const before = storage_work.get(.bytes_filled);
    var column = try SparseSlice(Byte).init(std.testing.allocator, 100000);
    defer column.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(u8, 0), column.items[0].load(.acquire));
    try std.testing.expectEqual(@as(u8, 0), column.items[99999].load(.acquire));
    column.items[99999].store(2, .release);
    try std.testing.expectEqual(@as(u8, 2), column.items[99999].load(.acquire));
    if (host != .wasm) try std.testing.expectEqual(before, storage_work.get(.bytes_filled));
}
