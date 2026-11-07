//! Compiler-only units demanded by a function-valued CTFE program's emitted
//! references. Ordinary object images provide the exact dependency edges; no
//! source scan or caller-dependent cache payload participates in selection.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const eval = @import("eval");
const unit = @import("boxy_unit.zig");
const Allocator = std.mem.Allocator;
const Ctfe = eval.CompileTimeFinalization;

/// One invocation's immutable snapshot of the available producer units.
/// Built on the coordinator; workers never consult its mutable module maps.
pub const Plan = struct {
    cache: *Cache,
    units: []unit.Unit,
    objects: []const Ctfe.BoxyObject,
    views: []const @import("check").CheckedArtifact.ImportedModuleView,

    pub fn deinit(self: *Plan, allocator: Allocator) void {
        for (self.units) |producer| allocator.free(producer.views);
    }

    pub fn source(self: *Plan) Ctfe.FunctionObjectSource {
        return .{ .context = self, .prepare = prepare, .views = self.views };
    }

    fn prepare(raw: *anyopaque, allocator: Allocator, names: []const []const u8) Ctfe.FinalizeError![]const Ctfe.BoxyObject {
        const self: *Plan = @ptrCast(@alignCast(raw));
        var providers: std.StringHashMapUnmanaged(usize) = .empty;
        defer providers.deinit(allocator);
        const images = try allocator.alloc(backend.dev.UnitImage.View, self.objects.len);
        defer allocator.free(images);
        for (self.objects, images, 0..) |object, *image, index| {
            image.* = backend.dev.UnitImage.View.init(object.code.image.bytes) catch unreachable; // admitted ordinary image
            const symbols = image.symbols();
            for (image.exports()) |symbol| {
                const entry = try providers.getOrPut(allocator, image.name(symbols[symbol].name));
                if (!entry.found_existing) entry.value_ptr.* = index;
            }
        }
        const needed = try allocator.alloc(bool, images.len);
        defer allocator.free(needed);
        @memset(needed, false);
        for (names) |name| if (providers.get(name)) |index| {
            needed[index] = true;
        };
        // Producers are in imports-first order. A reverse visit propagates
        // demand to all code/data dependencies, including callable pointers.
        var index = images.len;
        while (index != 0) {
            index -= 1;
            if (!needed[index]) continue;
            const image = images[index];
            for (image.imports()) |import| {
                if (backend.dev.HostSplice.hostFunction(image.name(import.name)) != null) continue;
                if (providers.get(image.name(import.name))) |dependency| {
                    std.debug.assert(dependency < index);
                    needed[dependency] = true;
                }
            }
        }
        var objects: std.ArrayList(Ctfe.BoxyObject) = .empty;
        errdefer objects.deinit(allocator);
        for (needed, self.units) |demanded, producer| {
            if (!demanded) continue;
            try objects.append(allocator, try self.cache.get(producer));
        }
        return try objects.toOwnedSlice(allocator);
    }
};

/// Deduplicates requested units across concurrent evaluation programs. A short
/// map lock publishes entries; each unit has its own completion lock so unrelated
/// units can compile concurrently. The post-check executor runs nested backend
/// work on the waiting worker too, including on a one-worker compiler.
pub const Cache = struct {
    allocator: Allocator,
    io: std.Io,
    mutex: std.Io.Mutex = .init,
    entries: std.AutoHashMapUnmanaged([32]u8, *Entry) = .empty,

    const Entry = struct {
        mutex: std.Io.Mutex = .init,
        outcome: union(enum) { pending, failed: Ctfe.FinalizeError, done: unit.Built } = .pending,
    };

    pub fn deinit(self: *Cache) void {
        var entries = self.entries.valueIterator();
        while (entries.next()) |entry| {
            switch (entry.*.outcome) {
                .pending, .failed => {},
                .done => |built| built.deinit(self.allocator),
            }
            self.allocator.destroy(entry.*);
        }
        self.entries.deinit(self.allocator);
    }

    fn entryFor(self: *Cache, key: [32]u8) Allocator.Error!*Entry {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        if (self.entries.get(key)) |entry| return entry;
        const entry = try self.allocator.create(Entry);
        errdefer self.allocator.destroy(entry);
        entry.* = .{};
        try self.entries.put(self.allocator, key, entry);
        return entry;
    }

    fn get(self: *Cache, producer: unit.Unit) Ctfe.FinalizeError!Ctfe.BoxyObject {
        const entry = try self.entryFor(producer.key.bytes);
        entry.mutex.lockUncancelable(self.io);
        defer entry.mutex.unlock(self.io);
        if (entry.outcome == .pending) {
            const built = unit.restoreOrBuild(producer) catch |err| {
                switch (err) {
                    error.NativeCompilationFailed, error.UnsupportedTarget, error.HostedFunctionNotBound => base.invariant("a native function unit with admitted host bindings failed to compile: {s}", .{@errorName(err)}),
                    else => |failure| {
                        entry.outcome = .{ .failed = failure };
                        return failure;
                    },
                }
            };
            entry.outcome = .{ .done = built };
            base.dev_diagnostics.countTestWork(if (built.restored) .ctfe_function_unit_hits else .ctfe_function_unit_builds, 1);
            base.dev_diagnostics.countTestWork(.ctfe_function_metadata_bytes, built.function_metadata.len);
        }
        const built = switch (entry.outcome) {
            .pending => unreachable, // the pending branch above either stores a completed outcome or returns its failure
            .failed => |err| return err,
            .done => |built| built,
        };
        const image = built.image orelse unreachable; // this consumer is native-unit-image CTFE
        return .{
            .module = producer.artifact.key.bytes,
            .code = .{ .image = .{ .bytes = image.bytes, .code_locations = image.code_locations } },
            .hosted_symbols = built.hosted_symbols,
            .function_owner = producer.key.bytes,
            .function_metadata = built.function_metadata,
        };
    }
};
