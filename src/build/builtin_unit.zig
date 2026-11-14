//! Build-time production of the Builtin module's Boxy unit (design.md
//! "Caching Under `.boxy`", "Embedded units").
//!
//! A compiler running on a target with a dev backend builds the Builtin's
//! unit for that target at dev opt the first time a cache needs it: the unit
//! compile-time evaluation links against and a native dev build links. Its
//! inputs are all fixed when the compiler is built: the Builtin imports
//! nothing and binds no hosted functions, and its link name is a function of
//! its module name and checking interface. So the compiler's build runs the
//! compiler's own unit build here, on the baked checked Builtin the compiler
//! loads, and links the resulting table (`compile.boxy_embedded_units`) into
//! the compiler, which serves the unit from it whenever it computes the same
//! key.
//!
//! This tool is the compiler's own modules compiled exactly as the compiler
//! is: for the compiler's target, in the compiler's optimize mode, so it has
//! the compiler's compatibility identity (`build_options.compiler_compatibility_hash`),
//! which the table records. The compiler's output depends on that identity
//! (a Debug compiler's dev backend emits runtime checks into the code it
//! generates that an optimized compiler's does not), so no
//! other executable could build the entry the compiler files. The build
//! therefore runs it only for a compiler whose target this machine runs; any
//! other compiler embeds no entries. A target without a dev backend gets an
//! empty table.
//!
//! Usage:
//!
//!     builtin_unit <output table> <roc target name>
//!     builtin_unit --verify <table> <roc target name> <empty cache directory>
//!
//! `--verify` is the build's test that the table holds exactly what the
//! compiler files for the same key: it keys and builds the Builtin's unit
//! through `boxy_unit.restoreOrBuild`, the run-time path, into a cache of its
//! own, and requires the filed entry to be byte-identical to the table's
//! entry for that key, and the table to serve that key.

const std = @import("std");
const backend = @import("backend");
const build_options = @import("build_options");
const compile = @import("compile");
const eval = @import("eval");
const lowering_policy = @import("lowering_policy");
const RocTarget = @import("roc_target").RocTarget;

const boxy_unit = compile.boxy_unit;
const embedded_units = compile.boxy_embedded_units;
const object_cache = compile.boxy_object_cache;

const MainError = std.process.Args.ToSliceError || std.Io.Dir.WriteFileError || std.Io.Dir.ReadFileAllocError ||
    eval.BuiltinModules.InitError || boxy_unit.BuildError || error{ ExpectedArguments, UnknownTarget, EmbeddedEntryDiffers };

/// Write the table, or verify one (`--verify`).
pub fn main(init: std.process.Init) MainError!void {
    const gpa = init.gpa;
    const io = init.io;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len == 5 and std.mem.eql(u8, args[1], "--verify")) {
        return verify(gpa, io, args[2], try parseTarget(args[3]), args[4]);
    }
    if (args.len != 3) return error.ExpectedArguments;
    const target = try parseTarget(args[2]);
    const table = if (backend.dev.isa.isaOfOrNull(target) == null)
        try embedded_units.encode(gpa, build_options.compiler_compatibility_hash, &.{})
    else
        try builtinUnitTable(gpa, io, target);
    defer gpa.free(table);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = args[1], .data = table });
}

fn parseTarget(name: []const u8) error{UnknownTarget}!RocTarget {
    return std.meta.stringToEnum(RocTarget, name) orelse {
        std.log.err("unknown Roc target {s}", .{name});
        return error.UnknownTarget;
    };
}

/// The Builtin's unit as a compiler running on `target` prepares it for
/// compile-time evaluation (`compile.coordinator.BoxyObjects`), whose key
/// a native dev build's unit of the Builtin shares.
const BuiltinUnit = struct {
    builtins: *eval.BuiltinModules,
    digests: compile.boxy_interface.Digests,
    interfaces: eval.CompileTimeFinalization.ExportInterfaces,
    lowering: compile.coordinator.BoxyObjectLowering,

    fn init(self: *BuiltinUnit, gpa: std.mem.Allocator, io: std.Io, builtins: *eval.BuiltinModules, target: RocTarget) std.mem.Allocator.Error!void {
        // The Builtin's link name is its stable name unless a module of the
        // link shares that name, in which case the compiler keys it
        // differently and the table does not serve it.
        self.* = .{
            .builtins = builtins,
            .digests = try compile.boxy_interface.Digests.init(gpa, io, &.{&builtins.checked_artifact}),
            .interfaces = eval.CompileTimeFinalization.ExportInterfaces.init(gpa, io),
            .lowering = lowering_policy.compileTimeObjectLowering(target),
        };
        try self.digests.ensurePrepared(&builtins.checked_artifact);
    }

    fn deinit(self: *BuiltinUnit) void {
        self.interfaces.deinit();
        self.digests.deinit();
    }

    fn keyInput(self: *BuiltinUnit, gpa: std.mem.Allocator) std.mem.Allocator.Error!object_cache.KeyInput {
        return try boxy_unit.objectKeyInput(gpa, &self.digests, &self.builtins.checked_artifact, null, self.lowering.opt, self.lowering.inline_expects, self.lowering.target, &.{}, false, false, false);
    }

    fn unit(self: *BuiltinUnit, gpa: std.mem.Allocator, key: object_cache.Key) boxy_unit.Unit {
        var config = self.lowering.config;
        config.boxy_unit = .{ .imports = &self.interfaces, .root = false, .names = &self.digests.names };
        config.checked_module_state = .complete;
        return .{
            .gpa = gpa,
            .artifact = &self.builtins.checked_artifact,
            .platform = null,
            .views = &.{},
            .target = self.lowering.target,
            .config = config,
            .interfaces = &self.interfaces,
            .cache_manager = null,
            .key = key,
        };
    }
};

/// The table holding the Builtin's unit for `target`.
fn builtinUnitTable(gpa: std.mem.Allocator, io: std.Io, target: RocTarget) MainError![]u8 {
    var builtins = try eval.BuiltinModules.init(gpa, io);
    defer builtins.deinit();
    var builtin_unit: BuiltinUnit = undefined;
    try builtin_unit.init(gpa, io, &builtins, target);
    defer builtin_unit.deinit();
    const input = try builtin_unit.keyInput(gpa);
    var unit = builtin_unit.unit(gpa, object_cache.key(input));
    unit.want_pieces = false;
    const result = try boxy_unit.buildEntry(unit);
    defer result.built.deinit(gpa);
    defer gpa.free(result.bytes);
    return try embedded_units.encode(gpa, build_options.compiler_compatibility_hash, &.{.{ .input = input, .entry = result.bytes }});
}

/// Build the Builtin's unit for `target` through the run-time path into a
/// cache at `cache_dir`, and require the entry it files to be the entry the
/// table at `table_path` serves for the same key.
fn verify(gpa: std.mem.Allocator, io: std.Io, table_path: []const u8, target: RocTarget, cache_dir: []const u8) MainError!void {
    const read = try std.Io.Dir.cwd().readFileAlloc(io, table_path, gpa, .unlimited);
    defer gpa.free(read);
    const table = try gpa.alignedAlloc(u8, .fromByteUnits(embedded_units.alignment), read.len);
    defer gpa.free(table);
    @memcpy(table, read);
    const units: embedded_units.Units = .{ .table = table };

    var builtins = try eval.BuiltinModules.init(gpa, io);
    defer builtins.deinit();
    var builtin_unit: BuiltinUnit = undefined;
    try builtin_unit.init(gpa, io, &builtins, target);
    defer builtin_unit.deinit();
    const key = try boxy_unit.objectKey(gpa, &builtin_unit.digests, &builtins.checked_artifact, null, builtin_unit.lowering.opt, builtin_unit.lowering.inline_expects, target, &.{}, false, false, false);
    const embedded = units.find(key) orelse {
        if (backend.dev.isa.isaOfOrNull(target) == null) return;
        std.log.err("the table holds no entry for the Builtin unit's key", .{});
        return error.EmbeddedEntryDiffers;
    };

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var manager = compile.CacheManager.init(gpa, .{ .enabled = true, .cache_dir = cache_dir }, compile.CoreCtx.os(gpa, arena.allocator(), io));
    defer manager.deinit();
    var unit = builtin_unit.unit(gpa, key);
    unit.cache_manager = &manager;
    const built = try boxy_unit.restoreOrBuild(unit);
    defer built.deinit(gpa);
    if (built.restored) {
        std.log.err("the verification cache at {s} already held the Builtin unit", .{cache_dir});
        return error.EmbeddedEntryDiffers;
    }
    const filed = manager.loadFamilyBytes(.boxy_object, key.bytes, object_cache.scope) orelse {
        std.log.err("the run-time path filed no entry for the Builtin unit", .{});
        return error.EmbeddedEntryDiffers;
    };
    defer manager.allocator.free(filed.bytes);
    if (!std.mem.eql(u8, filed.bytes, embedded)) {
        std.log.err("the embedded Builtin unit ({d} bytes) differs from the entry the run-time path filed ({d} bytes)", .{ embedded.len, filed.bytes.len });
        return error.EmbeddedEntryDiffers;
    }

    // The table serves the key in place of the cache.
    var served_unit = builtin_unit.unit(gpa, key);
    served_unit.embedded = units;
    const served = try boxy_unit.restoreOrBuild(served_unit);
    defer served.deinit(gpa);
    if (!served.restored or !std.mem.eql(u8, &served.exports_digest, &built.exports_digest) or served.image == null or served.image.?.owner != .embedded) {
        std.log.err("the table does not serve the Builtin unit's key", .{});
        return error.EmbeddedEntryDiffers;
    }
}
