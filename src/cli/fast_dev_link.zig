//! A fast dev link: `roc build --opt=dev --specialize=no` of an executable
//! for any 64-bit x86 or ARM target (ELF for Linux and the BSDs, Mach-O,
//! COFF), linked by copying the
//! pre-linked host (`host_prelink.zig`, `macho/HostPrelink.zig`,
//! `coff/HostPrelink.zig`) and patching the program's unit images in after
//! it (`patch_link.zig`, `macho/PatchLink.zig`, `coff/PatchLink.zig`)
//! instead of composing a program object and running LLD. The link is a
//! function of the target's format alone; the compiler host only supplies
//! the system calls the output file is written with (`link_output.zig`).
//!
//! The output file is created when the build starts, and a background job
//! hashes the platform's host inputs. For an ELF host it then computes the
//! host's key and on a cache hit copies the host into the output while the
//! program compiles. A Mach-O or COFF host's key also covers the kinds and
//! sizes of the `provides` symbols, which the program's images tell, so its
//! lookup waits for them; on macOS the copy is a clone where the filesystem
//! has them. On a miss the link step prelinks the host with LLD, files it,
//! and writes it into the output.
const std = @import("std");
const compile = @import("compile");
const linker = @import("linker.zig");
const host_prelink = @import("host_prelink.zig");
const HostPrelink = @import("macho/HostPrelink.zig");
const CoffHostPrelink = @import("coff/HostPrelink.zig");
const link_output = @import("link_output.zig");
const CliCtx = @import("CliCtx.zig").CliCtx;
const RocTarget = @import("target.zig").RocTarget;

const Allocator = std.mem.Allocator;
const CacheManager = compile.CacheManager;

pub const OutputFile = link_output.OutputFile;

/// The cache scope host records are filed under.
const host_scope = "prelinked-host";

/// The executable format of a fast dev link's host.
pub const Format = enum { elf, macho, coff };

/// What a platform's host is linked from and must keep.
pub const HostSpec = struct {
    format: Format,
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
    /// Whether the output is a shared library rather than an executable.
    shared: bool,
    /// For a shared library, the symbols it exports: the provides and the
    /// host inputs' own exports. Sorted and distinct; empty for an
    /// executable.
    exports: []const []const u8,
};

/// The background job that hashes the host inputs and, for an ELF host,
/// finds the host and copies it into the output.
pub const HostJob = struct {
    gpa: Allocator,
    std_io: std.Io,
    /// Null for a build without a cache, whose lookup misses and whose
    /// prelink files nothing.
    manager: ?*CacheManager,
    spec: *const HostSpec,
    output: *OutputFile,
    thread: ?std.Thread = null,
    key: [32]u8 = undefined,
    /// The content digests of the host inputs, for a key computed after
    /// the job ran.
    digests: std.ArrayList([32]u8) = .empty,
    outcome: Outcome = .pending,
    hash_ns: u64 = 0,
    lookup_ns: u64 = 0,
    copy_ns: u64 = 0,

    pub const Outcome = union(enum) {
        pending,
        /// The inputs are hashed; the lookup waits for the provides'
        /// layouts (`lookupWithLayouts`).
        hashed,
        /// The cached record, mapped, with the host already in the output.
        hit: Hit,
        miss,
        /// A host input could not be read; the link reports it.
        unreadable_input: []const u8,
        failed: OutputFile.Error,
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
                link_output.unmapReadOnly(hit.mapping);
                hit.artifact.deinit();
            },
            .pending, .hashed, .miss, .unreadable_input, .failed => {},
        }
        self.digests.deinit(self.gpa);
    }

    fn fail(self: *HostJob, err: OutputFile.Error) void {
        self.outcome = .{ .failed = err };
    }

    fn run(self: *HostJob) void {
        const hash_started = now(self.std_io);
        if (self.spec.bundle_hash == null) {
            var paths: std.ArrayList([]const u8) = .empty;
            defer paths.deinit(self.gpa);
            paths.appendSlice(self.gpa, self.spec.platform_files_pre) catch return self.fail(error.OutOfMemory);
            paths.appendSlice(self.gpa, self.spec.platform_files_post) catch return self.fail(error.OutOfMemory);
            self.digests.resize(self.gpa, paths.items.len) catch return self.fail(error.OutOfMemory);
            const hashed = contentDigests(self.gpa, self.std_io, paths.items, self.digests.items) catch return self.fail(error.OutOfMemory);
            if (hashed) |unreadable| {
                self.outcome = .{ .unreadable_input = paths.items[unreadable] };
                return;
            }
        }
        self.hash_ns = elapsed(self.std_io, hash_started);
        // An ELF executable's windows are fixed; every other host's are
        // sized by the provides' layouts, which its key waits for.
        if (self.spec.format == .elf and !self.spec.shared) {
            self.key = self.keyFor(&.{});
            self.lookup();
        } else {
            self.outcome = .hashed;
        }
    }

    /// The host's key, from the hashed inputs and the provides' layouts.
    fn keyFor(self: *HostJob, provides: []const host_prelink.Provide) [32]u8 {
        return host_prelink.key(.{
            .record_version = switch (self.spec.format) {
                .elf => host_prelink.format_version,
                .macho => HostPrelink.format_version,
                .coff => CoffHostPrelink.format_version,
            },
            .target = @tagName(self.spec.target),
            .link_abi = @tagName(self.spec.link_abi),
            .provides = self.spec.provides,
            .provide_layouts = provides,
            .keep = self.spec.keep,
            .shared = self.spec.shared,
            .exports = self.spec.exports,
            .link_items = self.spec.link_items,
            .bundle_hash = self.spec.bundle_hash,
            .input_digests = self.digests.items,
        });
    }

    /// Look a host whose windows are sized by layouts up once the provides'
    /// layouts are known, after the job ran. The host goes into the output
    /// on a hit.
    pub fn lookupWithLayouts(self: *HostJob, provides: []const host_prelink.Provide) void {
        std.debug.assert(self.thread == null and self.outcome == .hashed);
        self.key = self.keyFor(provides);
        self.lookup();
    }

    /// Find the host under `key` and put it into the output.
    fn lookup(self: *HostJob) void {
        const lookup_started = now(self.std_io);
        const manager = self.manager orelse {
            self.outcome = .miss;
            return;
        };
        var artifact = manager.acquireFamily(.prelinked_host, self.key, host_scope) orelse {
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
        // A record this compiler did not write is a miss. The host's bytes
        // the output begins with: an ELF record holds the host after its
        // header page, a Mach-O record's host leads up to its signature, a
        // COFF record's up to its symbol table.
        const host_start: ?HostStart = switch (self.spec.format) {
            .elf => if (host_prelink.View.init(mapping)) |view| .{ .offset = host_prelink.host_offset, .bytes = view.host() } else |_| null,
            .macho => if (HostPrelink.View.init(mapping)) |view| .{ .offset = 0, .bytes = view.segments() } else |_| null,
            .coff => if (CoffHostPrelink.View.init(mapping)) |view| .{ .offset = 0, .bytes = view.sections() } else |_| null,
        };
        const host_bytes = host_start orelse {
            link_output.unmapReadOnly(mapping);
            artifact.deinit();
            self.outcome = .miss;
            return;
        };
        self.lookup_ns = elapsed(self.std_io, lookup_started);
        const copy_started = now(self.std_io);
        self.output.copyHost(self.gpa, .{ .handle = artifact.file.handle, .path = artifact.path, .offset = host_bytes.offset }, host_bytes.bytes) catch |err| {
            link_output.unmapReadOnly(mapping);
            artifact.deinit();
            self.outcome = .{ .failed = err };
            return;
        };
        self.copy_ns = elapsed(self.std_io, copy_started);
        self.outcome = .{ .hit = .{ .artifact = artifact, .mapping = mapping } };
    }
};

/// The host's bytes a record holds, where the output begins.
const HostStart = struct { offset: u64, bytes: []const u8 };

/// Map `size` bytes of a file read-only.
pub fn mapReadOnly(handle: std.Io.File.Handle, size: usize) error{MapFailed}![]align(std.heap.page_size_min) u8 {
    return link_output.mapReadOnly(handle, size);
}

/// The bytes hashed as one piece of a host input; the pieces of all inputs
/// are hashed in parallel.
const hash_chunk: usize = 4 * 1024 * 1024;

/// Fill `digests` with the content digest of each file in `paths`: the
/// BLAKE3 hash of its length and of the BLAKE3 hashes of its
/// `hash_chunk`-sized pieces, in order. Returns the index of a file that
/// cannot be read, if any.
pub fn contentDigests(gpa: Allocator, std_io: std.Io, paths: []const []const u8, digests: [][32]u8) Allocator.Error!?usize {
    const Input = struct { file: ?std.Io.File = null, bytes: []align(std.heap.page_size_min) const u8 = &.{}, first_chunk: usize = 0 };
    const inputs = try gpa.alloc(Input, paths.len);
    defer gpa.free(inputs);
    @memset(inputs, .{});
    defer for (inputs) |input| {
        if (input.bytes.len != 0) link_output.unmapReadOnly(input.bytes);
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

pub const PrelinkError = linker.LinkError || host_prelink.FinishError || HostPrelink.FinishError || CoffHostPrelink.FinishError || error{ReadFailed};

/// Link the platform's host with LLD into a host record (cold path) and
/// file it in the cache, when there is one. The caller owns the record.
/// `provides` carries the provides' layouts, which a Mach-O or COFF host's
/// windows are sized by.
pub fn prelink(ctx: *CliCtx, spec: *const HostSpec, key: [32]u8, manager: ?*CacheManager, scratch_dir: []const u8, provides: []const host_prelink.Provide) PrelinkError![]align(16) u8 {
    const arm64 = spec.target.toCpuArch() == .aarch64;
    const output_kind: linker.OutputKind = if (spec.shared) .shared_lib else .exe;
    const object = switch (spec.format) {
        .elf => if (spec.shared) try host_prelink.linkSharedObject(ctx.gpa, arm64, provides) else try host_prelink.linkObject(ctx.gpa, arm64, spec.provides),
        .macho => try HostPrelink.linkObject(ctx.gpa, arm64, provides, spec.keep),
        .coff => try CoffHostPrelink.linkObject(ctx.gpa, arm64, provides, spec.keep),
    };
    defer ctx.gpa.free(object);
    const object_path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, if (spec.format == .coff) "roc_fast_dev_host.obj" else "roc_fast_dev_host.o" });
    const exe_path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, "roc_fast_dev_host" });
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = object_path, .data = object }) catch return error.LinkFailed;
    switch (spec.format) {
        .elf => try linker.link(ctx, .{
            .target_format = .elf,
            .target_abi = spec.link_abi,
            .target_os = spec.target.toOsTag(),
            .target_arch = spec.target.toCpuArch(),
            .output_path = exe_path,
            .output_kind = output_kind,
            .export_symbols = spec.exports,
            .lazy_platform_archives = true,
            .object_files = &.{object_path},
            .platform_files_pre = spec.platform_files_pre,
            .platform_files_post = spec.platform_files_post,
            .force_undefined_symbols = spec.keep,
            .platform_files_dir = spec.platform_files_dir,
            .scratch_dir = scratch_dir,
        }),
        .macho => {
            // Every kept symbol is left to dynamic lookup when nothing
            // defines it, so a host lacking one still links; the patch link
            // reports it when the app imports it.
            var extra_args: std.ArrayList([]const u8) = .empty;
            defer extra_args.deinit(ctx.gpa);
            try extra_args.appendSlice(ctx.gpa, &.{ "-headerpad", headerpad_arg, "-S", "-no_fixup_chains", "-adhoc_codesign" });
            for (spec.keep) |name| {
                try extra_args.append(ctx.gpa, "-U");
                try extra_args.append(ctx.gpa, try std.fmt.allocPrint(ctx.arena, "_{s}", .{name}));
            }
            try linker.link(ctx, .{
                .target_format = .macho,
                .target_abi = spec.link_abi,
                .target_os = .macos,
                .target_arch = spec.target.toCpuArch(),
                .output_path = exe_path,
                .output_kind = output_kind,
                .export_symbols = spec.exports,
                .lazy_platform_archives = true,
                .object_files = &.{object_path},
                .platform_files_pre = spec.platform_files_pre,
                .platform_files_post = spec.platform_files_post,
                .extra_args = extra_args.items,
                .platform_files_dir = spec.platform_files_dir,
                .scratch_dir = scratch_dir,
            });
        },
        .coff => {
            // Every kept symbol and every provide is named to the link
            // (`/include`), which keeps the provides' windows and gives a
            // DLL's symbol its import thunk; a kept symbol nothing defines
            // is left unresolved, so a host lacking one still links, and
            // the patch link reports it when the app imports it. The page
            // file alignment leaves the header page room for the section
            // headers the patch link appends.
            const included = try sortedUnion(ctx.arena, &.{ spec.keep, spec.provides });
            try linker.link(ctx, .{
                .target_format = .coff,
                .target_abi = spec.link_abi,
                .target_os = .windows,
                .target_arch = spec.target.toCpuArch(),
                .output_path = exe_path,
                .output_kind = output_kind,
                .export_symbols = spec.exports,
                .lazy_platform_archives = true,
                .object_files = &.{object_path},
                .platform_files_pre = spec.platform_files_pre,
                .platform_files_post = spec.platform_files_post,
                .force_undefined_symbols = included,
                .extra_args = &.{ "/filealign:4096", "/force:unresolved" },
                .platform_files_dir = spec.platform_files_dir,
                .scratch_dir = scratch_dir,
            });
        },
    }
    const exe = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, exe_path, ctx.gpa, .unlimited) catch return error.ReadFailed;
    defer ctx.gpa.free(exe);
    const record = switch (spec.format) {
        .elf => try host_prelink.finish(ctx.gpa, exe, spec.provides, spec.keep, if (spec.shared) provides else null),
        .macho => try HostPrelink.finish(ctx.gpa, exe, provides, spec.keep),
        .coff => try CoffHostPrelink.finish(ctx.gpa, exe, provides, spec.keep),
    };
    if (manager) |cache| cache.storeFamilyBytes(.prelinked_host, key, record, host_scope, "prelinked host");
    return record;
}

const headerpad_arg = std.fmt.comptimePrint("0x{x}", .{HostPrelink.headerpad});

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
