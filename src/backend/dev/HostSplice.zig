//! Links object-cache entries spliced into the compile-time evaluator's
//! image. Compile-time hooks remain symbolic until executable linking;
//! a spliced entry was compiled as object
//! code and reaches everything by name: the host's runtime symbols, the
//! builtins, the boxy runtime, compiler-rt and the C memory routines, hosted
//! functions, static data and literal backings, and other procedures and
//! refcount helpers. This binds those names to the evaluator's process,
//! defines the data items the entries carry, and stands in for hosted
//! functions, which no compile-time evaluation may call.
//!
//! The image is one mapping: the generated code, then a jump stub for every
//! function of the compiler the code calls, then the carried data. A call or
//! data reference in object code is PC-relative, and the compiler's own
//! functions and heap can be anywhere in the address space, so every such
//! reference resolves to something inside the mapping.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const lir = @import("lir");
const LirCodeGenMod = @import("LirCodeGen.zig");
const ProcArtifact = @import("ProcArtifact.zig");
const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
const StaticDataImage = @import("StaticDataImage.zig").StaticDataImage;
const relocation_mod = @import("Relocation.zig");
const ExecutableMemory = @import("ExecutableMemory.zig").ExecutableMemory;
const SpliceSource = @import("ObjectFileCompiler.zig").SpliceSource;
const spliceExternalProcs = @import("ObjectFileCompiler.zig").spliceExternalProcs;

const Allocator = std.mem.Allocator;
const HostLirCodeGen = LirCodeGenMod.HostLirCodeGen;
const BoxyBuiltinFn = LirCodeGenMod.BoxyBuiltinFn;
const BoxyNativeFnTable = LirCodeGenMod.BoxyNativeFnTable;
const BuiltinFn = builtins.builtin_registry.BuiltinFn;

/// Why an image could not be linked.
pub const LinkError = Allocator.Error || error{
    /// A relocation names something neither the compiler nor the image defines.
    UnresolvedSymbol,
    /// A relocation's site or encoding cannot be patched.
    InvalidRelocation,
    /// The executable mapping could not be made.
    MappingFailed,
};

const stub_size = 16;

/// Everything spliced into one image, until it is linked.
pub const HostSplice = struct {
    allocator: Allocator,
    /// The carried data items in placement order, one per name.
    data: std.ArrayList(ProcArtifact.DataItem) = .empty,
    data_names: std.StringHashMap(void),
    /// Each data item's offset in the mapping once `link` has laid it out.
    data_offsets: std.StringHashMap(usize),
    /// Hosted functions spliced code calls, by name, with the stub's code
    /// offset.
    hosted_stubs: std.StringHashMap(usize),
    /// Procedures spliced so far.
    spliced_procs: usize = 0,
    /// The name of the last relocation `link` could not bind.
    unresolved: ?[]const u8 = null,
    /// Process-local bindings, never carried by reusable code or artifacts.
    comptime_hooks: ?LirCodeGenMod.ComptimeHooks = null,
    /// Owned names for borrowed process data; mutable slots must not be copied.
    static_data_bindings: std.StringHashMapUnmanaged(usize) = .empty,

    pub fn init(allocator: Allocator) HostSplice {
        return .{
            .allocator = allocator,
            .data_names = std.StringHashMap(void).init(allocator),
            .data_offsets = std.StringHashMap(usize).init(allocator),
            .hosted_stubs = std.StringHashMap(usize).init(allocator),
        };
    }

    /// The callbacks must remain callable for the linked executable's lifetime.
    /// Runtime object generation leaves these bindings unset.
    pub fn setComptimeHooks(self: *HostSplice, hooks: ?LirCodeGenMod.ComptimeHooks) void {
        self.comptime_hooks = hooks;
    }

    /// Copy explicit symbol bindings, not data. The image allocation must outlive
    /// the executable, but neither this image struct nor its export names must.
    /// Range-limited code references reach these addresses through carried pointer
    /// cells, whose absolute data relocations are patched only while linking.
    pub fn bindStaticDataSymbols(self: *HostSplice, exports: []const StaticDataExport, image: *const StaticDataImage) (Allocator.Error || error{ MissingStaticDataSymbol, DuplicateStaticDataSymbol })!void {
        for (exports) |static_export| {
            const address = image.symbolAddress(static_export.symbol_name) orelse return error.MissingStaticDataSymbol;
            if (self.static_data_bindings.get(static_export.symbol_name)) |existing| {
                if (existing != address) return error.DuplicateStaticDataSymbol;
                continue;
            }
            const name = try self.allocator.dupe(u8, static_export.symbol_name);
            errdefer self.allocator.free(name);
            try self.static_data_bindings.putNoClobber(self.allocator, name, address);
        }
    }

    pub fn deinit(self: *HostSplice) void {
        self.data.deinit(self.allocator);
        self.data_names.deinit();
        self.data_offsets.deinit();
        var bindings = self.static_data_bindings.keyIterator();
        while (bindings.next()) |name| self.allocator.free(name.*);
        self.static_data_bindings.deinit(self.allocator);
        var stubs = self.hosted_stubs.keyIterator();
        while (stubs.next()) |name| self.allocator.free(name.*);
        self.hosted_stubs.deinit();
        self.* = undefined;
    }

    /// Place the object-cache entry of every external procedure in `demand`
    /// into the open code generator, before the program's own procedures
    /// compile.
    pub fn spliceExternal(self: *HostSplice, codegen: *HostLirCodeGen, demand: []const lir.LIR.LirProcSpecId, source: SpliceSource) Allocator.Error!void {
        var external = std.ArrayList(lir.LIR.LirProcSpecId).empty;
        defer external.deinit(self.allocator);
        for (demand) |proc_id| {
            if (codegen.store.getProcSpec(proc_id).external) try external.append(self.allocator, proc_id);
        }
        if (external.items.len == 0) return;
        var carried = std.ArrayList(ProcArtifact.DataItem).empty;
        defer carried.deinit(self.allocator);
        try spliceExternalProcs(HostLirCodeGen, self.allocator, codegen, codegen.store.getProcSpecs(), external.items, source, &carried);
        try self.addDataItems(carried.items);
        self.spliced_procs += external.items.len;
    }

    /// Carry artifact data into the executable mapping, once per exact name.
    /// Records are copied; their names, bytes, and relocations remain borrowed
    /// and must outlive this splice and its `link` call.
    pub fn addDataItems(self: *HostSplice, items: []const ProcArtifact.DataItem) Allocator.Error!void {
        for (items) |item| {
            const gop = try self.data_names.getOrPut(item.name);
            if (gop.found_existing) continue;
            errdefer _ = self.data_names.remove(item.name);
            try self.data.append(self.allocator, item);
        }
    }

    /// Emit a stub for every hosted function the image names: a name that
    /// neither the compiler nor the image defines can only be the
    /// platform's, since object code is undefined in nothing else. The
    /// evaluator's own code names one where it takes a hosted function as a
    /// value. Runs after every procedure is compiled and before the image
    /// is finished.
    pub fn generateHostedStubs(self: *HostSplice, codegen: *HostLirCodeGen, boxy_native_fns: *const BoxyNativeFnTable) Allocator.Error!void {
        var code_symbols = std.StringHashMap(usize).init(self.allocator);
        defer {
            var keys = code_symbols.keyIterator();
            while (keys.next()) |name| self.allocator.free(name.*);
            code_symbols.deinit();
        }
        try collectCodeSymbols(self.allocator, codegen, &code_symbols);
        var needed = std.ArrayList([]const u8).empty;
        defer needed.deinit(self.allocator);
        const names = codegen.getSymbolNames();
        for (codegen.codegen.relocations.items) |relocation| {
            const name = switch (relocation) {
                .linked_function => |function| names[@intFromEnum(function.symbol)],
                .linked_data => |data| blk: {
                    const name = names[@intFromEnum(data.symbol)];
                    if (self.static_data_bindings.contains(name)) continue;
                    break :blk name;
                },
                .local_data, .jmp_to_return, .retired => continue,
            };
            if (ComptimeHook.fromName(name) != null) continue;
            if (compilerFunction(name, boxy_native_fns) != null or code_symbols.contains(name) or self.data_names.contains(name) or self.hosted_stubs.contains(name)) continue;
            if (!containsName(needed.items, name)) try needed.append(self.allocator, name);
        }
        // A carried constant can hold a hosted function as a value.
        for (self.data.items) |item| for (item.relocations) |relocation| {
            // Data declarations are not hosted functions, including unbound
            // process slots reached through symbolic pointer cells.
            if (!relocation.function) continue;
            const name = relocation.name;
            if (ComptimeHook.fromName(name) != null) continue;
            if (compilerFunction(name, boxy_native_fns) != null or code_symbols.contains(name) or self.data_names.contains(name) or self.hosted_stubs.contains(name)) continue;
            if (!containsName(needed.items, name)) try needed.append(self.allocator, name);
        };
        for (needed.items) |name| {
            const owned = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(owned);
            const offset = try codegen.generateHostedStub(name);
            try self.hosted_stubs.putNoClobber(owned, offset);
        }
    }

    /// Map the finished image executable: copy the code, give every function
    /// of the compiler it calls a stub, define the carried data, and patch
    /// every relocation.
    pub fn link(self: *HostSplice, codegen: *HostLirCodeGen, boxy_native_fns: *const BoxyNativeFnTable) LinkError!ExecutableMemory {
        const code = codegen.getGeneratedCode();
        const names = codegen.getSymbolNames();

        var relocations = std.ArrayList(relocation_mod.Relocation).empty;
        defer relocations.deinit(self.allocator);
        for (codegen.getRelocations()) |indexed| {
            try relocations.append(self.allocator, switch (indexed) {
                .retired => continue,
                .linked_function => |function| .{ .linked_function = .{ .offset = function.offset, .name = names[@intFromEnum(function.symbol)] } },
                .linked_data => |data| .{ .linked_data = .{ .offset = data.offset, .name = names[@intFromEnum(data.symbol)], .kind = data.kind } },
                .local_data => |data| .{ .local_data = data },
                .jmp_to_return => |jump| .{ .jmp_to_return = jump },
            });
        }

        var code_symbols = std.StringHashMap(usize).init(self.allocator);
        defer {
            var keys = code_symbols.keyIterator();
            while (keys.next()) |name| self.allocator.free(name.*);
            code_symbols.deinit();
        }
        try collectCodeSymbols(self.allocator, codegen, &code_symbols);

        // Every compiler function the image reaches, in first-use order,
        // gets one stub.
        var stub_targets: std.AutoArrayHashMapUnmanaged(usize, void) = .empty;
        defer stub_targets.deinit(self.allocator);
        var binder = Binder{
            .splice = self,
            .boxy_native_fns = boxy_native_fns,
            .code_symbols = &code_symbols,
            .stub_targets = &stub_targets,
            .allocator = self.allocator,
        };
        for (relocations.items) |relocation| {
            const name = switch (relocation) {
                .linked_function => |function| function.name,
                .linked_data => |data| data.name,
                .local_data, .jmp_to_return => continue,
            };
            const binding = if (relocation == .linked_data) try binder.classifyData(name) else try binder.classify(name);
            if (binding == .unresolved) {
                self.unresolved = name;
                return error.UnresolvedSymbol;
            }
        }
        for (self.data.items) |item| for (item.relocations) |relocation| {
            const binding = if (relocation.function) try binder.classify(relocation.name) else try binder.classifyData(relocation.name);
            if (binding == .unresolved) {
                self.unresolved = relocation.name;
                return error.UnresolvedSymbol;
            }
        };

        const stubs_start = std.mem.alignForward(usize, code.len, stub_size);
        const data_start = std.mem.alignForward(usize, stubs_start + stub_targets.count() * stub_size, 16);
        var total = data_start;
        for (self.data.items) |item| {
            total = std.mem.alignForward(usize, total, @max(item.alignment, 1));
            try self.data_offsets.put(item.name, total);
            total += @max(item.bytes.len, 1);
        }

        var executable = ExecutableMemory.initWritable(total, code.len, 0) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.EmptyCode, error.MmapFailed, error.VirtualAllocFailed, error.UnsupportedPlatform => return error.MappingFailed,
        };
        errdefer executable.deinit();
        const image = executable.memory;
        @memcpy(image[0..code.len], code);
        binder.image_base = @intFromPtr(image.ptr);
        binder.stubs_start = stubs_start;
        for (stub_targets.keys(), 0..) |target, index| {
            writeStub(image[stubs_start + index * stub_size ..][0..stub_size], target);
        }
        for (self.data.items) |item| {
            const offset = self.data_offsets.get(item.name) orelse unreachable;
            @memcpy(image[offset..][0..item.bytes.len], item.bytes);
        }

        for (relocations.items) |relocation| {
            const resolver: relocation_mod.SymbolResolverContext = if (relocation == .linked_data) Binder.resolveData else Binder.resolve;
            relocation_mod.applyRelocationsWithContext(image[0..code.len], binder.image_base, &.{relocation}, &binder, resolver) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedSymbol => return error.UnresolvedSymbol,
                error.InvalidOffset, error.UnsupportedRelocationEncoding, error.MisalignedBranchTarget, error.BranchOutOfRange => return error.InvalidRelocation,
            };
        }
        for (self.data.items) |item| {
            const offset = self.data_offsets.get(item.name) orelse unreachable;
            for (item.relocations) |relocation| {
                const target = (if (relocation.function) binder.address(relocation.name) else binder.dataAddress(relocation.name)) orelse return error.UnresolvedSymbol;
                const field: usize = relocation.offset;
                if (field + @sizeOf(usize) > item.bytes.len) return error.InvalidRelocation;
                const value: usize = @intCast(@as(i128, target) + relocation.addend);
                std.mem.writeInt(usize, image[offset + field ..][0..@sizeOf(usize)], value, .little);
            }
        }

        executable.finishWrite() catch return error.MappingFailed;
        return executable;
    }
};

fn containsName(names: []const []const u8, name: []const u8) bool {
    for (names) |candidate| if (std.mem.eql(u8, candidate, name)) return true;
    return false;
}

const Binding = enum { compiler_function, image, process_data, unresolved };

/// These private ABI names cannot be supplied by a platform or an artifact.
const ComptimeHook = enum {
    ensure_static_value,
    branch_taken,
    exhaustiveness_failed,
    failure_region,
    call_enter,
    call_exit,

    fn fromName(name: []const u8) ?ComptimeHook {
        return std.StaticStringMap(ComptimeHook).initComptime(.{
            .{ "roc__comptime_ensure_static_value", .ensure_static_value },
            .{ "roc__comptime_branch_taken", .branch_taken },
            .{ "roc__comptime_exhaustiveness_failed", .exhaustiveness_failed },
            .{ "roc__comptime_failure_region", .failure_region },
            .{ "roc__comptime_call_enter", .call_enter },
            .{ "roc__comptime_call_exit", .call_exit },
        }).get(name);
    }

    fn address(self: ComptimeHook, hooks: LirCodeGenMod.ComptimeHooks) usize {
        return switch (self) {
            inline .ensure_static_value, .branch_taken, .exhaustiveness_failed, .failure_region, .call_enter, .call_exit => |hook| @intFromPtr(@field(hooks, @tagName(hook))),
        };
    }
};

/// Resolves relocation names against the compiler and the image.
const Binder = struct {
    splice: *HostSplice,
    boxy_native_fns: *const BoxyNativeFnTable,
    code_symbols: *const std.StringHashMap(usize),
    stub_targets: *std.AutoArrayHashMapUnmanaged(usize, void),
    allocator: Allocator,
    image_base: usize = 0,
    stubs_start: usize = 0,

    /// Bind `name` before the image exists, registering a stub for a
    /// function of the compiler.
    fn classify(self: *Binder, name: []const u8) Allocator.Error!Binding {
        if (ComptimeHook.fromName(name)) |hook| {
            const hooks = self.splice.comptime_hooks orelse return .unresolved;
            try self.stub_targets.put(self.allocator, hook.address(hooks), {});
            return .compiler_function;
        }
        if (compilerFunction(name, self.boxy_native_fns)) |target| {
            try self.stub_targets.put(self.allocator, target, {});
            return .compiler_function;
        }
        if (self.splice.hosted_stubs.contains(name) or self.code_symbols.contains(name) or self.splice.data_names.contains(name)) return .image;
        return .unresolved;
    }

    fn address(self: *const Binder, name: []const u8) ?usize {
        if (ComptimeHook.fromName(name)) |hook| {
            const hooks = self.splice.comptime_hooks orelse return null;
            const index = self.stub_targets.getIndex(hook.address(hooks)) orelse return null;
            return self.image_base + self.stubs_start + index * stub_size;
        }
        if (compilerFunction(name, self.boxy_native_fns)) |target| {
            const index = self.stub_targets.getIndex(target) orelse return null;
            return self.image_base + self.stubs_start + index * stub_size;
        }
        if (self.splice.hosted_stubs.get(name)) |offset| return self.image_base + offset;
        if (self.code_symbols.get(name)) |offset| return self.image_base + offset;
        if (self.splice.data_offsets.get(name)) |offset| {
            for (self.splice.data.items) |item| {
                if (std.mem.eql(u8, item.name, name)) return self.image_base + offset + item.symbol_offset;
            }
        }
        return null;
    }

    fn resolve(context: *const anyopaque, name: []const u8) ?usize {
        const self: *const Binder = @ptrCast(@alignCast(context));
        return self.address(name);
    }

    fn classifyData(self: *Binder, name: []const u8) Allocator.Error!Binding {
        // Private hooks cannot be supplied as data by a caller, either.
        if (ComptimeHook.fromName(name) == null and self.splice.static_data_bindings.contains(name)) return .process_data;
        return self.classify(name);
    }

    fn dataAddress(self: *const Binder, name: []const u8) ?usize {
        if (ComptimeHook.fromName(name) == null) {
            if (self.splice.static_data_bindings.get(name)) |address_value| return address_value;
        }
        return self.address(name);
    }

    fn resolveData(context: *const anyopaque, name: []const u8) ?usize {
        const self: *const Binder = @ptrCast(@alignCast(context));
        return self.dataAddress(name);
    }
};

/// The compiler's own function `name` binds to: a builtin, a runtime symbol
/// of the in-process host, a boxy runtime function, or a compiler-rt or C
/// routine. The dict seed is the compile-time evaluator's fixed zero, as it
/// is in the evaluator's own code.
fn compilerFunction(name: []const u8, boxy_native_fns: *const BoxyNativeFnTable) ?usize {
    if (BuiltinFn.fromSymbolName(name)) |builtin_fn| {
        if (builtin_fn == .dict_pseudo_seed) return @intFromPtr(&comptimeDictSeed);
        return builtin_fn.wrapperAddress();
    }
    if (builtins.in_process_host.Symbol.fromName(name)) |symbol| return symbol.address();
    if (BoxyBuiltinFn.fromSymbolName(name)) |boxy_fn| return boxy_native_fns[@intFromEnum(boxy_fn)];
    return builtins.native_runtime_libcalls.resolve(name);
}

fn comptimeDictSeed() callconv(.c) u64 {
    return 0;
}

/// Every procedure and refcount helper of the image by the name object code
/// gives it: the program's own, and the spliced ones.
fn collectCodeSymbols(allocator: Allocator, codegen: *const HostLirCodeGen, out: *std.StringHashMap(usize)) Allocator.Error!void {
    for (codegen.codeRegions()) |region| {
        const name: []u8 = switch (region.kind) {
            .proc => |proc_id| try codegen.store.getProcSpec(proc_id).identity.symbolName(allocator),
            .spliced_proc => |identity| try identity.symbolName(allocator),
            .rc_helper => |key| try LirCodeGenMod.compiledRcHelperSymbolName(allocator, codegen.layout_store, key),
            .spliced_helper => try allocator.dupe(u8, codegen.splicedHelperName(region.start + region.entry) orelse continue),
            .boxy_thunk, .spliced_boxy_thunk, .entrypoint, .message_pool_run, .branch_island, .hosted_stub => continue,
        };
        errdefer allocator.free(name);
        const gop = try out.getOrPut(name);
        if (gop.found_existing) {
            allocator.free(name);
            continue;
        }
        gop.value_ptr.* = region.start + region.entry;
    }
}

/// An absolute jump to `target`: x86_64 `jmp [rip]` over the address, or
/// AArch64 `ldr x16, #8; br x16` over it.
fn writeStub(stub: *[stub_size]u8, target: usize) void {
    @memset(stub, 0);
    if (builtin.cpu.arch == .x86_64) {
        stub[0..6].* = .{ 0xFF, 0x25, 0x00, 0x00, 0x00, 0x00 };
        std.mem.writeInt(u64, stub[6..14], target, .little);
    } else if (builtin.cpu.arch == .aarch64) {
        std.mem.writeInt(u32, stub[0..4], 0x58000050, .little);
        std.mem.writeInt(u32, stub[4..8], 0xD61F0200, .little);
        std.mem.writeInt(u64, stub[8..16], target, .little);
    } else unreachable;
}

test "compiler functions resolve and the dict seed is the evaluator's zero" {
    var table: BoxyNativeFnTable = undefined;
    @memset(&table, 0);
    try std.testing.expect(compilerFunction("roc_alloc", &table) != null);
    try std.testing.expect(compilerFunction(BuiltinFn.crash_str.symbolName(), &table) != null);
    try std.testing.expectEqual(@intFromPtr(&comptimeDictSeed), compilerFunction(BuiltinFn.dict_pseudo_seed.symbolName(), &table).?);
    try std.testing.expect(compilerFunction("memcpy", &table) != null);
    try std.testing.expect(compilerFunction("roc__p00", &table) == null);
}

test "a stub jumps to its target" {
    if (builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    const Target = struct {
        fn answer() callconv(.c) u64 {
            return 42;
        }
    };
    var executable = try ExecutableMemory.initWritable(stub_size, stub_size, 0);
    defer executable.deinit();
    writeStub(executable.memory[0..stub_size], @intFromPtr(&Target.answer));
    try executable.finishWrite();
    const stub: *const fn () callconv(.c) u64 = @ptrCast(@alignCast(executable.codePtr()));
    try std.testing.expectEqual(@as(u64, 42), stub());
}

test "comptime hook bindings are explicit for every private ABI symbol" {
    const Hooks = struct {
        var exited: bool = false;
        fn ensure(_: u32) callconv(.c) void {}
        fn branch(_: u32, _: u32) callconv(.c) void {}
        fn exhaustive(_: u32) callconv(.c) void {}
        fn region(_: u32, _: u32, _: u32, _: u32, _: u32, _: u32) callconv(.c) void {}
        fn enter(_: u32, _: u32, _: u32, _: u32, _: u32) callconv(.c) void {}
        fn exit() callconv(.c) void {
            exited = true;
        }
    };
    const hooks: LirCodeGenMod.ComptimeHooks = .{
        .ensure_static_value = &Hooks.ensure,
        .branch_taken = &Hooks.branch,
        .exhaustiveness_failed = &Hooks.exhaustive,
        .failure_region = &Hooks.region,
        .call_enter = &Hooks.enter,
        .call_exit = &Hooks.exit,
    };
    const allocator = std.testing.allocator;
    var splice = HostSplice.init(allocator);
    defer splice.deinit();
    var table: BoxyNativeFnTable = undefined;
    @memset(&table, 0);
    var symbols = std.StringHashMap(usize).init(allocator);
    defer symbols.deinit();
    var targets: std.AutoArrayHashMapUnmanaged(usize, void) = .empty;
    defer targets.deinit(allocator);
    var binder = Binder{
        .splice = &splice,
        .boxy_native_fns = &table,
        .code_symbols = &symbols,
        .stub_targets = &targets,
        .allocator = allocator,
        .image_base = 4096,
        .stubs_start = 32,
    };
    inline for (@typeInfo(ComptimeHook).@"enum".fields) |field| {
        const name = "roc__comptime_" ++ field.name;
        // Even an image definition cannot mask a missing private binding.
        try symbols.put(name, 8);
        try std.testing.expectEqual(Binding.unresolved, try binder.classify(name));
        try std.testing.expectEqual(null, binder.address(name));
        try std.testing.expectEqual(@as(usize, 0), targets.count());
    }
    splice.setComptimeHooks(hooks);
    inline for (@typeInfo(ComptimeHook).@"enum".fields) |field| {
        const name = "roc__comptime_" ++ field.name;
        const target = @intFromPtr(@field(hooks, field.name));
        try std.testing.expectEqual(Binding.compiler_function, try binder.classify(name));
        const index = targets.getIndex(target).?;
        try std.testing.expectEqual(@as(?usize, 4096 + 32 + index * stub_size), binder.address(name));
    }
    try std.testing.expectEqual(null, ComptimeHook.fromName("roc__comptime_call_exit_extra"));
    try std.testing.expectEqual(null, ComptimeHook.fromName("roc__comptime_unknown"));
    try std.testing.expectEqual(Binding.unresolved, try binder.classify("roc__comptime_unknown"));
    // Ordinary compiler symbols retain precedence over image definitions.
    try symbols.put("memcpy", 8);
    try std.testing.expectEqual(Binding.compiler_function, try binder.classify("memcpy"));
    try symbols.put("roc__pexample", 8);
    try std.testing.expectEqual(Binding.image, try binder.classify("roc__pexample"));
    try std.testing.expectEqual(@as(?usize, 4104), binder.address("roc__pexample"));

    if (builtin.cpu.arch == .x86_64 or builtin.cpu.arch == .aarch64) {
        var executable = try ExecutableMemory.initWritable(targets.count() * stub_size, stub_size, 0);
        defer executable.deinit();
        binder.image_base = @intFromPtr(executable.memory.ptr);
        binder.stubs_start = 0;
        for (targets.keys(), 0..) |target, index| {
            writeStub(executable.memory[index * stub_size ..][0..stub_size], target);
        }
        try executable.finishWrite();
        const exit: @TypeOf(hooks.call_exit) = @ptrFromInt(binder.address("roc__comptime_call_exit").?);
        Hooks.exited = false;
        exit();
        try std.testing.expect(Hooks.exited);
    }
    splice.setComptimeHooks(null);
    inline for (@typeInfo(ComptimeHook).@"enum".fields) |field| {
        const name = "roc__comptime_" ++ field.name;
        try std.testing.expectEqual(Binding.unresolved, try binder.classify(name));
        try std.testing.expectEqual(null, binder.address(name));
    }
}

test "static bindings link pointer cells without copying mutable slots" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try @import("layout").Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer codegen.deinit();
    _ = try codegen.generateHostedStub("test_unused_entry");

    const exports = [_]StaticDataExport{.{
        .symbol_name = "test_mutable_slot",
        .bytes = &([_]u8{0} ** 16),
        .symbol_offset = 8,
        .alignment = 8,
    }};
    var slots = try StaticDataImage.init(allocator, &exports);
    defer slots.deinit();
    var splice = HostSplice.init(allocator);
    defer splice.deinit();
    const cells = [_]ProcArtifact.DataItem{.{
        .name = "test_slot_pointer",
        .bytes = &([_]u8{0} ** 8),
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 0, .name = "test_mutable_slot", .addend = 0, .function = false }},
    }};
    try splice.addDataItems(&cells);
    try splice.addDataItems(&cells);
    try std.testing.expectEqual(@as(usize, 1), splice.data.items.len);
    var table: BoxyNativeFnTable = undefined;
    @memset(&table, 0);
    try splice.generateHostedStubs(&codegen, &table);
    try std.testing.expect(!splice.hosted_stubs.contains("test_mutable_slot"));
    try codegen.finishImage();
    try std.testing.expectError(error.UnresolvedSymbol, splice.link(&codegen, &table));
    try std.testing.expectEqualStrings("test_mutable_slot", splice.unresolved.?);
    try splice.bindStaticDataSymbols(&exports, &slots);
    // Repeated registration is idempotent and does not change the owned name.
    try splice.bindStaticDataSymbols(&exports, &slots);
    var different_slots = try StaticDataImage.init(allocator, &exports);
    defer different_slots.deinit();
    try std.testing.expectError(error.DuplicateStaticDataSymbol, splice.bindStaticDataSymbols(&exports, &different_slots));
    try std.testing.expectError(error.MissingStaticDataSymbol, splice.bindStaticDataSymbols(&.{.{
        .symbol_name = "missing_slot",
        .bytes = &.{},
        .alignment = 1,
    }}, &slots));
    const owned_name = splice.static_data_bindings.getKey("test_mutable_slot").?;
    try std.testing.expect(owned_name.ptr != exports[0].symbol_name.ptr);

    var symbols = std.StringHashMap(usize).init(allocator);
    defer symbols.deinit();
    var targets: std.AutoArrayHashMapUnmanaged(usize, void) = .empty;
    defer targets.deinit(allocator);
    var binder = Binder{
        .splice = &splice,
        .boxy_native_fns = &table,
        .code_symbols = &symbols,
        .stub_targets = &targets,
        .allocator = allocator,
    };
    try std.testing.expectEqual(Binding.process_data, try binder.classifyData("test_mutable_slot"));
    try std.testing.expectEqual(Binding.unresolved, try binder.classify("test_mutable_slot"));
    try std.testing.expectEqual(null, binder.address("test_mutable_slot"));
    try symbols.put("test_mutable_slot", 8);
    try std.testing.expectEqual(Binding.image, try binder.classify("test_mutable_slot"));
    try std.testing.expectEqual(@as(?usize, 8), binder.address("test_mutable_slot"));
    try std.testing.expectEqual(slots.symbolAddress("test_mutable_slot"), binder.dataAddress("test_mutable_slot"));

    var executable = try splice.link(&codegen, &table);
    defer executable.deinit();
    const offset = splice.data_offsets.get("test_slot_pointer").?;
    const address = std.mem.readInt(usize, executable.memory[offset..][0..@sizeOf(usize)], .little);
    try std.testing.expectEqual(slots.symbolAddress("test_mutable_slot").?, address);
    const slot: *u64 = @ptrFromInt(address);
    slot.* = 42;
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, slots.allocation[8..16], .little));
    // Linking must not bake the process address into retained source data.
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, splice.data.items[0].bytes[0..@sizeOf(usize)], .little));
}
