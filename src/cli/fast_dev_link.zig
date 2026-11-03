//! A fast dev link: `roc build --opt=dev --specialize=no` of a Linux x86_64
//! executable, linked by copying the pre-linked host (`host_prelink.zig`)
//! and patching the program's unit images in after it (`patch_link.zig`)
//! instead of composing a program object and running LLD.
//!
//! The output file is created when the build starts, and a background job
//! hashes the platform's host inputs, computes the host's key, and on a
//! cache hit copies the host into the output while the program compiles.
//! On a miss the link step prelinks the host with LLD, files it, and writes
//! it into the output.
const std = @import("std");
const builtin = @import("builtin");
const compile = @import("compile");
const linker = @import("linker.zig");
const host_prelink = @import("host_prelink.zig");
const patch_link = @import("patch_link.zig");
const CliCtx = @import("CliCtx.zig").CliCtx;
const RocTarget = @import("target.zig").RocTarget;

const Allocator = std.mem.Allocator;
const CacheManager = compile.CacheManager;
const linux = std.os.linux;

pub const OutputFile = patch_link.OutputFile;

/// The cache scope host records are filed under.
const host_scope = "prelinked-host";

/// What a platform's host is linked from and must keep.
pub const HostSpec = struct {
    target: RocTarget,
    link_abi: linker.TargetAbi,
    platform_files_pre: []const []const u8,
    platform_files_post: []const []const u8,
    platform_files_dir: []const u8,
    /// The platform's link items as its header names them, with the app's
    /// position marked.
    link_items: []const []const u8,
    /// The platform's verified bundle hash, for a downloaded platform.
    bundle_hash: ?[]const u8,
    /// The `provides` symbols, sorted and distinct.
    provides: []const []const u8,
    /// The symbols the app region may import from the host, sorted and
    /// distinct: the hosted symbols, the runtime set, the support imports.
    keep: []const []const u8,
    /// The symbols a missing host definition of is the user's error, rather
    /// than a compiler invariant: every symbol in `keep`. Sorted and
    /// distinct.
    host_required: []const []const u8,
};

/// The background job that finds the host and copies it into the output.
pub const HostJob = struct {
    gpa: Allocator,
    std_io: std.Io,
    manager: *CacheManager,
    spec: *const HostSpec,
    output: *OutputFile,
    thread: ?std.Thread = null,
    key: [32]u8 = undefined,
    outcome: Outcome = .pending,
    hash_ns: u64 = 0,
    lookup_ns: u64 = 0,
    copy_ns: u64 = 0,

    pub const Outcome = union(enum) {
        pending,
        /// The cached record, mapped, with the host already in the output.
        hit: Hit,
        miss,
        /// A host input could not be read; the link reports it.
        unreadable_input: []const u8,
        failed: patch_link.OutputFile.Error,
    };

    pub const Hit = struct {
        artifact: compile.manager.AcquiredArtifact,
        mapping: []align(std.heap.page_size_min) const u8,
    };

    /// Start the job on its own thread.
    pub fn start(self: *HostJob) std.Thread.SpawnError!void {
        self.thread = try std.Thread.spawn(.{}, run, .{self});
    }

    /// Wait for the job to finish.
    pub fn wait(self: *HostJob) void {
        if (self.thread) |thread| thread.join();
        self.thread = null;
    }

    pub fn deinit(self: *HostJob) void {
        self.wait();
        switch (self.outcome) {
            .hit => |*hit| {
                if (comptime builtin.os.tag != .linux) unreachable; // only a Linux host's job finds a pre-linked host
                std.posix.munmap(hit.mapping);
                hit.artifact.deinit();
            },
            .pending, .miss, .unreadable_input, .failed => {},
        }
    }

    fn fail(self: *HostJob, err: patch_link.OutputFile.Error) void {
        self.outcome = .{ .failed = err };
    }

    fn run(self: *HostJob) void {
        if (comptime builtin.os.tag != .linux) unreachable; // fastDevLinkApplies selects the fast dev link only on Linux hosts
        const hash_started = now(self.std_io);
        var digests: std.ArrayList([32]u8) = .empty;
        defer digests.deinit(self.gpa);
        if (self.spec.bundle_hash == null) {
            var paths: std.ArrayList([]const u8) = .empty;
            defer paths.deinit(self.gpa);
            paths.appendSlice(self.gpa, self.spec.platform_files_pre) catch return self.fail(error.OutOfMemory);
            paths.appendSlice(self.gpa, self.spec.platform_files_post) catch return self.fail(error.OutOfMemory);
            digests.resize(self.gpa, paths.items.len) catch return self.fail(error.OutOfMemory);
            const hashed = contentDigests(self.gpa, self.std_io, paths.items, digests.items) catch return self.fail(error.OutOfMemory);
            if (hashed) |unreadable| {
                self.outcome = .{ .unreadable_input = paths.items[unreadable] };
                return;
            }
        }
        self.key = host_prelink.key(.{
            .target = @tagName(self.spec.target),
            .link_abi = @tagName(self.spec.link_abi),
            .provides = self.spec.provides,
            .keep = self.spec.keep,
            .link_items = self.spec.link_items,
            .bundle_hash = self.spec.bundle_hash,
            .input_digests = digests.items,
        });
        self.hash_ns = elapsed(self.std_io, hash_started);

        const lookup_started = now(self.std_io);
        var artifact = self.manager.acquireFamily(.prelinked_host, self.key, host_scope) orelse {
            self.outcome = .miss;
            return;
        };
        const size = std.math.cast(usize, artifact.size) orelse {
            artifact.deinit();
            self.outcome = .miss;
            return;
        };
        const mapping = mapReadOnly(artifact.file.handle, size) catch {
            artifact.deinit();
            self.outcome = .miss;
            return;
        };
        // A record this compiler did not write is a miss.
        const view = host_prelink.View.init(mapping) catch {
            std.posix.munmap(mapping);
            artifact.deinit();
            self.outcome = .miss;
            return;
        };
        self.lookup_ns = elapsed(self.std_io, lookup_started);
        const copy_started = now(self.std_io);
        self.output.copyFrom(artifact.file.handle, host_prelink.host_offset, view.host()) catch |err| {
            std.posix.munmap(mapping);
            artifact.deinit();
            self.outcome = .{ .failed = err };
            return;
        };
        self.copy_ns = elapsed(self.std_io, copy_started);
        self.outcome = .{ .hit = .{ .artifact = artifact, .mapping = mapping } };
    }
};

/// Map `size` bytes of a file read-only. Only a compiler running on Linux
/// makes a fast dev link (`fastDevLinkApplies`).
fn mapReadOnly(handle: std.Io.File.Handle, size: usize) error{MapFailed}![]align(std.heap.page_size_min) u8 {
    if (comptime builtin.os.tag != .linux) {
        unreachable; // fastDevLinkApplies selects the fast dev link only on Linux hosts
    } else {
        return std.posix.mmap(null, size, .{ .READ = true }, .{ .TYPE = .PRIVATE }, handle, 0) catch error.MapFailed;
    }
}

/// The bytes hashed as one piece of a host input; the pieces of all inputs
/// are hashed in parallel.
const hash_chunk: usize = 4 * 1024 * 1024;

/// Fill `digests` with the content digest of each file in `paths`: the
/// BLAKE3 hash of its length and of the BLAKE3 hashes of its
/// `hash_chunk`-sized pieces, in order. Returns the index of a file that
/// cannot be read, if any.
fn contentDigests(gpa: Allocator, std_io: std.Io, paths: []const []const u8, digests: [][32]u8) Allocator.Error!?usize {
    const Input = struct { file: ?std.Io.File = null, bytes: []align(std.heap.page_size_min) const u8 = &.{}, first_chunk: usize = 0 };
    const inputs = try gpa.alloc(Input, paths.len);
    defer gpa.free(inputs);
    @memset(inputs, .{});
    defer for (inputs) |input| {
        if (input.bytes.len != 0) std.posix.munmap(input.bytes);
        if (input.file) |file| file.close(std_io);
    };
    var chunk_count: usize = 0;
    for (paths, inputs, 0..) |path, *input, index| {
        const file = std.Io.Dir.cwd().openFile(std_io, path, .{}) catch return index;
        input.file = file;
        const stat = file.stat(std_io) catch return index;
        const size = std.math.cast(usize, stat.size) orelse return index;
        if (size != 0) input.bytes = mapReadOnly(file.handle, size) catch return index;
        input.first_chunk = chunk_count;
        chunk_count += std.math.divCeil(usize, size, hash_chunk) catch unreachable; // hash_chunk is nonzero
    }
    const Chunk = struct { bytes: []const u8 };
    const chunks = try gpa.alloc(Chunk, chunk_count);
    defer gpa.free(chunks);
    for (inputs) |input| {
        var at: usize = 0;
        var slot = input.first_chunk;
        while (at < input.bytes.len) : ({
            at += hash_chunk;
            slot += 1;
        }) chunks[slot] = .{ .bytes = input.bytes[at..@min(input.bytes.len, at + hash_chunk)] };
    }
    const chunk_digests = try gpa.alloc([32]u8, chunk_count);
    defer gpa.free(chunk_digests);
    const Hasher = struct {
        fn run(all: []const Chunk, out: [][32]u8, next: *std.atomic.Value(usize)) void {
            while (true) {
                const index = next.fetchAdd(1, .monotonic);
                if (index >= all.len) return;
                std.crypto.hash.Blake3.hash(all[index].bytes, &out[index], .{});
            }
        }
    };
    var next = std.atomic.Value(usize).init(0);
    var threads: [8]std.Thread = undefined;
    const wanted = @min(threads.len, chunk_count, std.Thread.getCpuCount() catch 1);
    var started: usize = 0;
    // A thread that cannot start leaves its share to the others and to
    // this one; the digests do not depend on how many threads ran.
    while (started < wanted) : (started += 1) {
        threads[started] = std.Thread.spawn(.{}, Hasher.run, .{ chunks, chunk_digests, &next }) catch break;
    }
    Hasher.run(chunks, chunk_digests, &next);
    for (threads[0..started]) |thread| thread.join();
    for (inputs, digests, 0..) |input, *digest, index| {
        const end = if (index + 1 < inputs.len) inputs[index + 1].first_chunk else chunk_count;
        var hasher = std.crypto.hash.Blake3.init(.{});
        var size_bytes: [8]u8 = undefined;
        std.mem.writeInt(u64, &size_bytes, input.bytes.len, .little);
        hasher.update(&size_bytes);
        for (chunk_digests[input.first_chunk..end]) |chunk_digest| hasher.update(&chunk_digest);
        hasher.final(digest);
    }
    return null;
}

/// Link the platform's host with LLD into a host record (cold path) and
/// file it in the cache. The caller owns the record.
pub fn prelink(ctx: *CliCtx, spec: *const HostSpec, key: [32]u8, manager: *CacheManager, scratch_dir: []const u8) (linker.LinkError || host_prelink.FinishError || error{ReadFailed})![]align(16) u8 {
    const object = try host_prelink.linkObject(ctx.gpa, spec.provides);
    defer ctx.gpa.free(object);
    const object_path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, "roc_fast_dev_host.o" });
    const exe_path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, "roc_fast_dev_host" });
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = object_path, .data = object }) catch return error.LinkFailed;
    try linker.link(ctx, .{
        .target_format = .elf,
        .target_abi = spec.link_abi,
        .target_os = .linux,
        .target_arch = .x86_64,
        .output_path = exe_path,
        .output_kind = .exe,
        .lazy_platform_archives = true,
        .object_files = &.{object_path},
        .platform_files_pre = spec.platform_files_pre,
        .platform_files_post = spec.platform_files_post,
        .force_undefined_symbols = spec.keep,
        .platform_files_dir = spec.platform_files_dir,
        .scratch_dir = scratch_dir,
    });
    const exe = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, exe_path, ctx.gpa, .unlimited) catch return error.ReadFailed;
    defer ctx.gpa.free(exe);
    const record = try host_prelink.finish(ctx.gpa, exe, spec.provides, spec.keep);
    manager.storeFamilyBytes(.prelinked_host, key, record, host_scope, "prelinked host");
    return record;
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

/// Sorted, distinct copies of the union of `lists`, allocated from `arena`.
pub fn sortedUnion(arena: Allocator, lists: []const []const []const u8) Allocator.Error![]const []const u8 {
    var seen: std.StringArrayHashMapUnmanaged(void) = .empty;
    for (lists) |list| for (list) |name| try seen.put(arena, name, {});
    const names = try arena.dupe([]const u8, seen.keys());
    std.mem.sort([]const u8, names, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);
    return names;
}
