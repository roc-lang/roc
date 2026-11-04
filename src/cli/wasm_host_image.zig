//! The images a wasm fast dev link (`wasm_link.zig`) composes with the
//! program's units, each converted once from a relocatable object and filed
//! in the cache under the `prelinked_host` family as a wasm record (an
//! image in the `WasmUnitImage` format, whose "RBWU" tag tells it apart from
//! a Linux host record, filed under its own scope):
//!
//! - The support image: the compiler's objects for the target, composed by
//!   `wasm-ld -r` (compiler-rt, the builtins, the Boxy runtime, each in its
//!   plain or stack-checked variant, and the stack support functions when
//!   the target or the platform needs them). Its key is the target, the
//!   stack-check mode and the embedded objects' content digests; the stack
//!   support object is generated from the target and the mode, which the
//!   key holds.
//! - The host image: the platform's link inputs, composed by `wasm-ld -r`
//!   the way the final link includes them (every archive member). Its key
//!   is the target, the platform's link items, and its inputs' content
//!   digests (or its verified bundle hash). The link configuration the
//!   platform declares is not part of it: the image is a function of the
//!   inputs alone, and the composer applies the configuration, which
//!   compile-time evaluation may only settle after checking.
//!
//! No key names a path. A background job (`ImageJob`) hashes the inputs and
//! looks the records up while the program compiles, as the Linux fast dev
//! link's `fast_dev_link.HostJob` does for its host: the host image and
//! both stack-check variants of the support image, since the mode is
//! settled only after checking. A miss converts on the main thread when the
//! link needs the image.
const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const compile = @import("compile");
const backend = @import("backend");
const linker = @import("linker.zig");
const fast_dev_link = @import("fast_dev_link.zig");
const CliCtx = @import("CliCtx.zig").CliCtx;
const RocTarget = @import("target.zig").RocTarget;

const Allocator = std.mem.Allocator;
const CacheManager = compile.CacheManager;
const WasmUnitImage = backend.wasm.WasmUnitImage;
const ObjectImage = backend.wasm.ObjectImage;

/// The cache scope the wasm records are filed under.
const scope = "prelinked-host-wasm";

/// The record format; bumped whenever a conversion's output changes.
const record_version: u32 = 1;

/// One of the compiler's objects in the support image.
pub const SupportObject = struct {
    /// The file name it is written under for `wasm-ld -r`.
    name: []const u8,
    bytes: []const u8,
};

/// What the images are made from.
pub const Spec = struct {
    target: RocTarget,
    /// The embedded objects of each support image variant: without stack
    /// checks, then with them.
    support_objects: [2][]const SupportObject,
    /// The platform's host inputs, in link order.
    platform_files: []const []const u8,
    /// The platform's link items as its header names them, with the app's
    /// position marked.
    link_items: []const []const u8,
    /// The platform's verified bundle hash, for a downloaded platform.
    bundle_hash: ?[]const u8,
    /// Whether the link needs the host image: a finished module does, an
    /// object for the platform's own linker does not.
    needs_host: bool,
};

/// The support image's cache key.
pub fn supportKey(target: RocTarget, stack_checks: bool, object_digests: []const [32]u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-wasm-support-image-v1");
    hasher.update(&build_options.compiler_compatibility_hash);
    hashU64(&hasher, record_version);
    hashU64(&hasher, WasmUnitImage.format_version);
    hashBytes(&hasher, @tagName(target));
    hasher.update(&.{@intFromBool(stack_checks)});
    hashU64(&hasher, object_digests.len);
    for (object_digests) |digest| hasher.update(&digest);
    return hasher.finalResult();
}

/// The host image's cache key.
pub fn hostKey(spec: *const Spec, input_digests: []const [32]u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-wasm-host-image-v1");
    hasher.update(&build_options.compiler_compatibility_hash);
    hashU64(&hasher, record_version);
    hashU64(&hasher, WasmUnitImage.format_version);
    hashBytes(&hasher, @tagName(spec.target));
    hashList(&hasher, spec.link_items);
    if (spec.bundle_hash) |bundle| {
        hasher.update(&.{1});
        hashBytes(&hasher, bundle);
    } else {
        hasher.update(&.{0});
        hashU64(&hasher, input_digests.len);
        for (input_digests) |digest| hasher.update(&digest);
    }
    return hasher.finalResult();
}

fn hashU64(hasher: *std.crypto.hash.sha2.Sha256, value: u64) void {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    hashU64(hasher, bytes.len);
    hasher.update(bytes);
}

fn hashList(hasher: *std.crypto.hash.sha2.Sha256, list: []const []const u8) void {
    hashU64(hasher, list.len);
    for (list) |item| hashBytes(hasher, item);
}

/// A record found in the cache, mapped.
pub const Hit = struct {
    artifact: compile.manager.AcquiredArtifact,
    mapping: []align(std.heap.page_size_min) const u8,

    fn deinit(self: *Hit) void {
        if (comptime builtin.os.tag != .linux) unreachable; // only a Linux host's job maps a record
        std.posix.munmap(self.mapping);
        self.artifact.deinit();
    }
};

/// What looking one record up found.
pub const Lookup = union(enum) {
    pending,
    hit: Hit,
    miss,
};

/// The background job that hashes the inputs and looks both images up.
pub const ImageJob = struct {
    gpa: Allocator,
    std_io: std.Io,
    manager: *CacheManager,
    spec: *const Spec,
    thread: ?std.Thread = null,
    /// By stack-check mode: without checks, then with them.
    support_keys: [2][32]u8 = undefined,
    host_key: [32]u8 = undefined,
    support: [2]Lookup = .{ .pending, .pending },
    host: Lookup = .pending,
    /// A platform input that could not be read; the link reports it.
    unreadable_input: ?[]const u8 = null,
    failed: ?Allocator.Error = null,
    hash_ns: u64 = 0,
    lookup_ns: u64 = 0,

    /// Start the job on its own thread.
    pub fn start(self: *ImageJob) std.Thread.SpawnError!void {
        self.thread = try std.Thread.spawn(.{}, run, .{self});
    }

    /// Wait for the job to finish.
    pub fn wait(self: *ImageJob) void {
        if (self.thread) |thread| thread.join();
        self.thread = null;
    }

    /// Wait for the job, then release the records it found.
    pub fn deinit(self: *ImageJob) void {
        self.wait();
        for ([_]*Lookup{ &self.support[0], &self.support[1], &self.host }) |found| switch (found.*) {
            .hit => |*hit| hit.deinit(),
            .pending, .miss => {},
        };
    }

    fn run(self: *ImageJob) void {
        if (comptime builtin.os.tag != .linux) unreachable; // wasmFastLinkApplies selects the wasm fast dev link only on Linux hosts
        const hash_started = now(self.std_io);
        for (self.spec.support_objects, &self.support_keys, [_]bool{ false, true }) |objects, *key, stack_checks| {
            const object_digests = self.gpa.alloc([32]u8, objects.len) catch return self.fail(error.OutOfMemory);
            defer self.gpa.free(object_digests);
            for (objects, object_digests) |object, *digest| std.crypto.hash.Blake3.hash(object.bytes, digest, .{});
            key.* = supportKey(self.spec.target, stack_checks, object_digests);
        }
        if (self.spec.needs_host) {
            var digests: []const [32]u8 = &.{};
            var owned: ?[][32]u8 = null;
            defer if (owned) |list| self.gpa.free(list);
            if (self.spec.bundle_hash == null) {
                const list = self.gpa.alloc([32]u8, self.spec.platform_files.len) catch return self.fail(error.OutOfMemory);
                owned = list;
                const unreadable = fast_dev_link.contentDigests(self.gpa, self.std_io, self.spec.platform_files, list) catch return self.fail(error.OutOfMemory);
                if (unreadable) |index| {
                    self.unreadable_input = self.spec.platform_files[index];
                    return;
                }
                digests = list;
            }
            self.host_key = hostKey(self.spec, digests);
        }
        self.hash_ns = elapsed(self.std_io, hash_started);

        const lookup_started = now(self.std_io);
        for (&self.support, self.support_keys) |*found, key| found.* = self.lookup(key);
        if (self.spec.needs_host) self.host = self.lookup(self.host_key);
        self.lookup_ns = elapsed(self.std_io, lookup_started);
    }

    fn fail(self: *ImageJob, err: Allocator.Error) void {
        self.failed = err;
    }

    /// The record filed under `key`, mapped: a miss when there is none or
    /// it does not read as an image this compiler wrote.
    fn lookup(self: *ImageJob, key: [32]u8) Lookup {
        var artifact = self.manager.acquireFamily(.prelinked_host, key, scope) orelse return .miss;
        const size = std.math.cast(usize, artifact.size) orelse {
            artifact.deinit();
            return .miss;
        };
        const mapping = fast_dev_link.mapReadOnly(artifact.file.handle, size) catch {
            artifact.deinit();
            return .miss;
        };
        _ = WasmUnitImage.View.init(mapping) catch {
            if (comptime builtin.os.tag != .linux) unreachable; // only a Linux host's job maps a record
            std.posix.munmap(mapping);
            artifact.deinit();
            return .miss;
        };
        return .{ .hit = .{ .artifact = artifact, .mapping = mapping } };
    }
};

/// Why an image could not be made.
pub const ConvertError = linker.LinkError || ObjectImage.Error || error{ReadFailed};

/// Convert the compiler's support objects, `objects`, to the support image
/// for `stack_checks` and file it under `key`. The caller owns the result.
pub fn convertSupport(ctx: *CliCtx, objects: []const SupportObject, stack_checks: bool, key: [32]u8, manager: *CacheManager, scratch_dir: []const u8, unsupported: *?ObjectImage.Unsupported) ConvertError![]align(WasmUnitImage.alignment.toByteUnits()) u8 {
    var paths: std.ArrayList([]const u8) = .empty;
    for (objects) |object| {
        const path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, object.name });
        std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = path, .data = object.bytes }) catch return error.ReadFailed;
        try paths.append(ctx.arena, path);
    }
    const image = try composeAndConvert(ctx, paths.items, scratch_dir, "roc_wasm_support.o", .{ .whole_archive = false, .strip_debug = true }, .{ .stack_checks = stack_checks, .retain = false }, unsupported);
    manager.storeFamilyBytes(.prelinked_host, key, image, scope, "wasm support image");
    return image;
}

/// Convert the platform's host inputs to the host image and file it under
/// `key`. The caller owns the result.
pub fn convertHost(ctx: *CliCtx, spec: *const Spec, key: [32]u8, manager: *CacheManager, scratch_dir: []const u8, unsupported: *?ObjectImage.Unsupported) ConvertError![]align(WasmUnitImage.alignment.toByteUnits()) u8 {
    const image = try composeAndConvert(ctx, spec.platform_files, scratch_dir, "roc_wasm_host.o", .{ .whole_archive = true, .strip_debug = true }, .{ .stack_checks = false, .retain = true }, unsupported);
    manager.storeFamilyBytes(.prelinked_host, key, image, scope, "wasm host image");
    return image;
}

fn composeAndConvert(
    ctx: *CliCtx,
    inputs: []const []const u8,
    scratch_dir: []const u8,
    output_name: []const u8,
    link_options: linker.RelocatableWasmOptions,
    options: ObjectImage.Options,
    unsupported: *?ObjectImage.Unsupported,
) ConvertError![]align(WasmUnitImage.alignment.toByteUnits()) u8 {
    const output_path = try std.fs.path.join(ctx.arena, &.{ scratch_dir, output_name });
    try linker.linkWasmObjectsRelocatable(ctx, output_path, inputs, link_options);
    const bytes = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, output_path, ctx.gpa, .unlimited) catch return error.ReadFailed;
    defer ctx.gpa.free(bytes);
    return ObjectImage.convert(ctx.gpa, bytes, options, unsupported);
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}
