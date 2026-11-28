//! A Debug-build allocator wrapper that makes stale borrows fail.
//!
//! A slice or pointer into a growable list is valid only until the list grows.
//! Whether a growth moves the list's backing is the allocator's choice, so a
//! borrow held across a growth usually keeps working and fails only when the
//! allocator happens to move the backing. This wrapper takes that choice away:
//!
//! - It never resizes or remaps an allocation to a different length, so every
//!   growth or shrink of a list moves it to a fresh backing.
//! - A freed allocation is quarantined instead of being returned to the
//!   backing allocator, so its address is not handed out again soon. An
//!   allocation of at least `protect_min_pages` pages is allocated on whole
//!   pages of its own; when it is freed its pages are made inaccessible, so
//!   any access through a stale borrow faults. A smaller allocation is filled
//!   with `poison_byte`, which makes every integer index, length and pointer
//!   read from it far out of range, and is checked to be still poisoned when
//!   it leaves quarantine, so a write through a stale borrow panics then.
//! - The quarantine is bounded in entries and bytes. The oldest entry leaves
//!   first and is freed to the backing allocator.
//!
//! Page protection is available on Linux, macOS and the BSDs. Elsewhere every
//! allocation takes the poison form.
//!
//! Only Debug compilers use this wrapper; `available` is false in every other
//! build mode, and the type refuses to be instantiated there.

const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const StaleBorrowAllocator = @This();

/// Whether this build may use the wrapper.
pub const available = builtin.mode == .debug;

/// The byte every quarantined small allocation is filled with. As a `u32` it
/// is above three billion, as a signed integer it is negative, and as a
/// pointer it is not a canonical address on x86_64 or aarch64.
pub const poison_byte: u8 = 0xBD;

const Host = enum { linux, libc_posix, none };

const host: Host = switch (builtin.os.tag) {
    .linux => .linux,
    .macos, .ios, .tvos, .watchos, .visionos, .maccatalyst, .driverkit, .freebsd, .netbsd, .openbsd, .dragonfly => if (builtin.link_libc) .libc_posix else .none,
    .illumos,
    .haiku,
    .windows,
    .freestanding,
    .wasi,
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
    => .none,
};

/// Whether freed large allocations can be made inaccessible on this host.
pub const page_protection_available = host != .none;

/// The most small allocations the quarantine holds.
pub const poisoned_capacity: usize = 16 * 1024;
/// The most page-protected allocations the quarantine holds. Each one can
/// split a kernel memory map entry in three, and the kernel bounds how many a
/// process may have.
pub const protected_capacity: usize = 4 * 1024;

/// The default limit on page-protected allocations whose pages are discarded,
/// which cost address space but no memory: 4 GiB, or a sixteenth of the
/// address space where that is smaller.
pub const discarded_protected_bytes: usize = @min(4 * 1024 * 1024 * 1024, (std.math.maxInt(usize) + 1) / 16);

/// Limits of a wrapper. Every field has a usable default.
pub const Config = struct {
    /// Whether allocations of at least `protect_min_pages` pages get pages of
    /// their own that are made inaccessible when freed.
    protect_pages: bool = page_protection_available,
    /// The smallest allocation, in pages, that takes the protected form.
    protect_min_pages: usize = 4,
    /// The most small allocations held in quarantine, at most `poisoned_capacity`.
    max_poisoned: usize = poisoned_capacity,
    /// The most bytes of small allocations held in quarantine.
    max_poisoned_bytes: usize = 64 * 1024 * 1024,
    /// The most page-protected allocations held, at most `protected_capacity`.
    max_protected: usize = protected_capacity,
    /// The most bytes of page-protected allocations held. On Linux their pages
    /// are also discarded, so they cost address space but no memory, and the
    /// limit is `discarded_protected_bytes`.
    max_protected_bytes: usize = if (host == .linux) discarded_protected_bytes else 256 * 1024 * 1024,
};

const Entry = struct {
    /// For a protected entry, the whole pages it owns.
    bytes: []u8,
    alignment: Alignment,
};

fn Ring(comptime capacity: usize) type {
    return struct {
        const Self = @This();

        entries: [capacity]Entry = undefined,
        head: usize = 0,
        count: usize = 0,
        bytes: usize = 0,

        fn push(self: *Self, entry: Entry) void {
            std.debug.assert(self.count < capacity);
            self.entries[(self.head + self.count) % capacity] = entry;
            self.count += 1;
            self.bytes += entry.bytes.len;
        }

        fn pop(self: *Self) ?Entry {
            if (self.count == 0) return null;
            const entry = self.entries[self.head];
            self.head = (self.head + 1) % capacity;
            self.count -= 1;
            self.bytes -= entry.bytes.len;
            return entry;
        }
    };
}

backing: Allocator,
config: Config,
mutex: std.atomic.Mutex = .unlocked,
poisoned: Ring(poisoned_capacity) = .{},
protected: Ring(protected_capacity) = .{},

/// A wrapper around `backing`, which must be thread-safe if the wrapper is
/// used from several threads.
pub fn init(backing: Allocator, config: Config) StaleBorrowAllocator {
    if (comptime !available) @compileError("StaleBorrowAllocator exists only in Debug builds");
    std.debug.assert(config.max_poisoned <= poisoned_capacity);
    std.debug.assert(config.max_protected <= protected_capacity);
    std.debug.assert(!config.protect_pages or page_protection_available);
    std.debug.assert(config.protect_min_pages != 0);
    return .{ .backing = backing, .config = config };
}

/// The `Allocator` interface of this wrapper.
pub fn allocator(self: *StaleBorrowAllocator) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

/// Free every quarantined allocation to the backing allocator, so that a
/// leak check of the backing allocator sees only real leaks.
pub fn flush(self: *StaleBorrowAllocator) void {
    while (true) {
        self.lock();
        const entry = self.poisoned.pop() orelse self.protected.pop();
        self.unlock();
        self.release(entry orelse return);
    }
}

/// How many allocations the quarantine holds.
pub fn quarantinedCount(self: *StaleBorrowAllocator) usize {
    self.lock();
    defer self.unlock();
    return self.poisoned.count + self.protected.count;
}

/// How many bytes the quarantine holds, counting whole pages for protected
/// allocations.
pub fn quarantinedBytes(self: *StaleBorrowAllocator) usize {
    self.lock();
    defer self.unlock();
    return self.poisoned.bytes + self.protected.bytes;
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn lock(self: *StaleBorrowAllocator) void {
    while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
}

fn unlock(self: *StaleBorrowAllocator) void {
    self.mutex.unlock();
}

fn isProtected(self: *const StaleBorrowAllocator, len: usize) bool {
    return self.config.protect_pages and len >= self.config.protect_min_pages * std.heap.pageSize();
}

fn pageAlignment(alignment: Alignment) Alignment {
    return alignment.max(.fromByteUnits(std.heap.pageSize()));
}

fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
    const self: *StaleBorrowAllocator = @ptrCast(@alignCast(ctx));
    if (!self.isProtected(len)) return self.backing.rawAlloc(len, alignment, ret_addr);
    const pages_len = std.mem.alignForward(usize, len, std.heap.pageSize());
    return self.backing.rawAlloc(pages_len, pageAlignment(alignment), ret_addr);
}

/// Changing an allocation's length in place is refused, so that every growth
/// and shrink moves.
fn resize(_: *anyopaque, memory: []u8, _: Alignment, new_len: usize, _: usize) bool {
    return new_len == memory.len;
}

fn remap(_: *anyopaque, memory: []u8, _: Alignment, new_len: usize, _: usize) ?[*]u8 {
    return if (new_len == memory.len) memory.ptr else null;
}

fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, _: usize) void {
    const self: *StaleBorrowAllocator = @ptrCast(@alignCast(ctx));
    if (self.isProtected(memory.len)) {
        const pages_len = std.mem.alignForward(usize, memory.len, std.heap.pageSize());
        const entry: Entry = .{ .bytes = memory.ptr[0..pages_len], .alignment = pageAlignment(alignment) };
        protect(entry.bytes);
        self.lock();
        const evicted = if (self.protected.count == self.protected.entries.len) self.protected.pop() else null;
        self.protected.push(entry);
        self.unlock();
        if (evicted) |victim| self.release(victim);
    } else {
        @memset(memory, poison_byte);
        self.lock();
        const evicted = if (self.poisoned.count == self.poisoned.entries.len) self.poisoned.pop() else null;
        self.poisoned.push(.{ .bytes = memory, .alignment = alignment });
        self.unlock();
        if (evicted) |victim| self.release(victim);
    }
    self.evictOverLimits();
}

fn evictOverLimits(self: *StaleBorrowAllocator) void {
    while (true) {
        self.lock();
        const config = self.config;
        const victim = if (self.poisoned.count > config.max_poisoned or
            (self.poisoned.bytes > config.max_poisoned_bytes and self.poisoned.count > 1))
            self.poisoned.pop()
        else if (self.protected.count > config.max_protected or
            (self.protected.bytes > config.max_protected_bytes and self.protected.count > 1))
            self.protected.pop()
        else
            null;
        self.unlock();
        self.release(victim orelse return);
    }
}

/// Free a quarantined allocation to the backing allocator, after checking
/// that a poisoned one was not written while it was quarantined.
fn release(self: *StaleBorrowAllocator, entry: Entry) void {
    if (self.isProtectedEntry(entry)) {
        unprotect(entry.bytes);
    } else if (std.mem.findNone(u8, entry.bytes, &.{poison_byte})) |offset| {
        std.debug.panic(
            "stale borrow: byte {d} of a freed {d}-byte allocation at 0x{x} was written after it was freed",
            .{ offset, entry.bytes.len, @intFromPtr(entry.bytes.ptr) },
        );
    }
    self.backing.rawFree(entry.bytes, entry.alignment, @returnAddress());
}

fn isProtectedEntry(self: *const StaleBorrowAllocator, entry: Entry) bool {
    return self.isProtected(entry.bytes.len);
}

fn protect(bytes: []u8) void {
    const pages: []align(std.heap.page_size_min) u8 = @alignCast(bytes);
    switch (host) {
        .linux => {
            const linux = std.os.linux;
            // Discarding the pages makes the quarantine cost address space only.
            const discard = linux.madvise(pages.ptr, pages.len, linux.MADV.DONTNEED);
            if (linux.errno(discard) != .SUCCESS) std.debug.panic("stale borrow quarantine: madvise failed on 0x{x}", .{@intFromPtr(pages.ptr)});
            const rc = linux.mprotect(pages.ptr, pages.len, .{});
            if (linux.errno(rc) != .SUCCESS) std.debug.panic("stale borrow quarantine: mprotect failed on 0x{x}", .{@intFromPtr(pages.ptr)});
        },
        .libc_posix => {
            if (std.c.mprotect(@ptrCast(pages.ptr), pages.len, .{}) != 0) {
                std.debug.panic("stale borrow quarantine: mprotect failed on 0x{x}", .{@intFromPtr(pages.ptr)});
            }
        },
        .none => unreachable, // `Config.protect_pages` is false on hosts without page protection
    }
}

fn unprotect(bytes: []u8) void {
    const pages: []align(std.heap.page_size_min) u8 = @alignCast(bytes);
    switch (host) {
        .linux => {
            const linux = std.os.linux;
            const rc = linux.mprotect(pages.ptr, pages.len, .{ .READ = true, .WRITE = true });
            if (linux.errno(rc) != .SUCCESS) std.debug.panic("stale borrow quarantine: mprotect failed on 0x{x}", .{@intFromPtr(pages.ptr)});
        },
        .libc_posix => {
            if (std.c.mprotect(@ptrCast(pages.ptr), pages.len, .{ .READ = true, .WRITE = true }) != 0) {
                std.debug.panic("stale borrow quarantine: mprotect failed on 0x{x}", .{@intFromPtr(pages.ptr)});
            }
        },
        .none => unreachable, // `Config.protect_pages` is false on hosts without page protection
    }
}
