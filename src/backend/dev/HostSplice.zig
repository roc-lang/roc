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
//! function of the compiler the code calls, then the carried data, then the
//! program's static data, which stays writable so evaluation can publish
//! completed values into it. A call or data reference in object code is
//! PC-relative, and the compiler's own functions and heap can be anywhere in
//! the address space, so every such reference resolves to something inside
//! the mapping.

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
    /// Each carried data item's index in `data`, by name.
    data_names: std.StringHashMap(u32),
    /// Each data item's offset in the mapping once `link` has laid it out.
    data_offsets: std.StringHashMap(usize),
    /// Hosted functions spliced code calls, by name, with the stub's code
    /// offset.
    hosted_stubs: std.StringHashMap(usize),
    /// Procedures spliced so far.
    spliced_procs: usize = 0,
    /// The name of the last relocation `link` could not bind.
    unresolved: ?[]const u8 = null,
    /// The program's static data, placed after the carried data.
    static_data: ?*const StaticDataImage.Layout = null,
    /// Where `link` placed the static data in the mapping.
    static_data_offset: usize = 0,

    pub fn init(allocator: Allocator) HostSplice {
        return .{
            .allocator = allocator,
            .data_names = std.StringHashMap(u32).init(allocator),
            .data_offsets = std.StringHashMap(usize).init(allocator),
            .hosted_stubs = std.StringHashMap(usize).init(allocator),
        };
    }

    /// Reserve the program's static data at the end of the mapping, laid
    /// out by `layout`, which must outlive `link`. Reserve it before
    /// generating hosted stubs, which no static data needs. The caller places
    /// the image in `staticDataMemory` once the mapping exists.
    pub fn reserveStaticData(self: *HostSplice, layout: *const StaticDataImage.Layout) void {
        std.debug.assert(layout.alignment.toByteUnits() <= std.heap.page_size_min);
        self.static_data = layout;
    }

    /// The memory `link` reserved for the static data in `executable`.
    pub fn staticDataMemory(self: *const HostSplice, executable: *ExecutableMemory) []u8 {
        const layout = self.static_data orelse unreachable;
        return executable.memory[self.static_data_offset..][0..layout.size];
    }

    pub fn deinit(self: *HostSplice) void {
        self.data.deinit(self.allocator);
        self.data_names.deinit();
        self.data_offsets.deinit();
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
            const proc = codegen.store.getProcSpecImmediate(proc_id);
            if (!proc.external) continue;
            // In-process code splices; it never links object-cache objects.
            if (builtin.mode == .debug and proc.c_abi_entry) @import("base").invariant("{s}", .{"in-process code reached a linked object-cache entry"});
            try external.append(self.allocator, proc_id);
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
            gop.value_ptr.* = @intCast(self.data.items.len);
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
                .linked_function => |function| names[@backingInt(function.symbol)],
                .linked_data => |data| blk: {
                    const name = names[@backingInt(data.symbol)];
                    if (self.static_data) |layout| if (layout.symbolOffset(name) != null) continue;
                    break :blk name;
                },
                .local_data, .jmp_to_return, .retired => continue,
            };
            if (compilerFunction(name, boxy_native_fns) != null or code_symbols.contains(name) or self.data_names.contains(name) or self.hosted_stubs.contains(name)) continue;
            if (!containsName(needed.items, name)) try needed.append(self.allocator, name);
        }
        // A carried constant can hold a hosted function as a value.
        for (self.data.items) |item| for (item.relocations) |relocation| {
            // Data declarations are not hosted functions.
            if (!relocation.function) continue;
            const name = relocation.name;
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
                .linked_function => |function| .{ .linked_function = .{ .offset = function.offset, .name = names[@backingInt(function.symbol)] } },
                .linked_data => |data| .{ .linked_data = .{ .offset = data.offset, .name = names[@backingInt(data.symbol)], .kind = data.kind } },
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
        // Static data starts on its own page, so it stays writable while
        // everything before it becomes executable.
        const executable_len = std.mem.alignForward(usize, total, std.heap.page_size_min);
        if (self.static_data) |layout| {
            self.static_data_offset = executable_len;
            binder.static_data_start = executable_len;
            total = executable_len + layout.size;
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

        if (self.static_data != null) {
            executable.finishWriteExecutablePrefix(executable_len) catch return error.MappingFailed;
        } else {
            executable.finishWrite() catch return error.MappingFailed;
        }
        return executable;
    }
};

fn containsName(names: []const []const u8, name: []const u8) bool {
    for (names) |candidate| if (std.mem.eql(u8, candidate, name)) return true;
    return false;
}

const Binding = enum { compiler_function, image, unresolved };

/// Resolves relocation names against the compiler and the image.
const Binder = struct {
    splice: *HostSplice,
    boxy_native_fns: *const BoxyNativeFnTable,
    code_symbols: *const std.StringHashMap(usize),
    stub_targets: *std.AutoArrayHashMapUnmanaged(usize, void),
    allocator: Allocator,
    image_base: usize = 0,
    stubs_start: usize = 0,
    static_data_start: usize = 0,

    /// Bind `name` before the image exists, registering a stub for a
    /// function of the compiler.
    fn classify(self: *Binder, name: []const u8) Allocator.Error!Binding {
        if (compilerFunction(name, self.boxy_native_fns)) |target| {
            try self.stub_targets.put(self.allocator, target, {});
            return .compiler_function;
        }
        if (self.splice.hosted_stubs.contains(name) or self.code_symbols.contains(name) or self.splice.data_names.contains(name)) return .image;
        return .unresolved;
    }

    fn address(self: *const Binder, name: []const u8) ?usize {
        if (compilerFunction(name, self.boxy_native_fns)) |target| {
            const index = self.stub_targets.getIndex(target) orelse return null;
            return self.image_base + self.stubs_start + index * stub_size;
        }
        if (self.splice.hosted_stubs.get(name)) |offset| return self.image_base + offset;
        if (self.code_symbols.get(name)) |offset| return self.image_base + offset;
        if (self.splice.data_offsets.get(name)) |offset| {
            const index = self.splice.data_names.get(name) orelse unreachable;
            return self.image_base + offset + self.splice.data.items[index].symbol_offset;
        }
        return null;
    }

    fn resolve(context: *const anyopaque, name: []const u8) ?usize {
        const self: *const Binder = @ptrCast(@alignCast(context));
        return self.address(name);
    }

    fn classifyData(self: *Binder, name: []const u8) Allocator.Error!Binding {
        if (self.splice.static_data) |layout| if (layout.symbolOffset(name) != null) return .image;
        return self.classify(name);
    }

    fn dataAddress(self: *const Binder, name: []const u8) ?usize {
        if (self.splice.static_data) |layout| if (layout.symbolOffset(name)) |offset| {
            return self.image_base + self.static_data_start + offset;
        };
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
    if (BoxyBuiltinFn.fromSymbolName(name)) |boxy_fn| return boxy_native_fns[@backingInt(boxy_fn)];
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
            .proc => |proc_id| try codegen.store.getProcSpecImmediate(proc_id).identity.symbolName(allocator),
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

test "compiler symbols keep precedence over image definitions" {
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
    try std.testing.expectEqual(Binding.unresolved, try binder.classify("roc__comptime_unknown"));
    try symbols.put("memcpy", 8);
    try std.testing.expectEqual(Binding.compiler_function, try binder.classify("memcpy"));
    try symbols.put("roc__pexample", 8);
    try std.testing.expectEqual(Binding.image, try binder.classify("roc__pexample"));
    try std.testing.expectEqual(@as(?usize, 4104), binder.address("roc__pexample"));
}

test "static data is placed in the mapping and stays writable" {
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
        .bytes = &(@as([16]u8, @splat(0))),
        .symbol_offset = 8,
        .alignment = 8,
    }};
    var layout = try StaticDataImage.Layout.plan(allocator, &exports, .{});
    var layout_live = true;
    defer if (layout_live) layout.deinit(allocator);
    var splice = HostSplice.init(allocator);
    defer splice.deinit();
    // A carried item that points at the slot.
    const pointers = [_]ProcArtifact.DataItem{.{
        .name = "test_slot_pointer",
        .bytes = &(@as([8]u8, @splat(0))),
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 0, .name = "test_mutable_slot", .addend = 0, .function = false }},
    }};
    try splice.addDataItems(&pointers);
    splice.reserveStaticData(&layout);
    var table: BoxyNativeFnTable = undefined;
    @memset(&table, 0);
    try splice.generateHostedStubs(&codegen, &table);
    try codegen.finishImage();

    var executable = try splice.link(&codegen, &table);
    defer executable.deinit();
    layout_live = false;
    var slots = try layout.place(allocator, splice.staticDataMemory(&executable));
    defer slots.deinit();
    const offset = splice.data_offsets.get("test_slot_pointer").?;
    const address = std.mem.readInt(usize, executable.memory[offset..][0..@sizeOf(usize)], .little);
    try std.testing.expectEqual(slots.symbolAddress("test_mutable_slot").?, address);
    const base_address = @intFromPtr(executable.memory.ptr);
    try std.testing.expect(address >= base_address and address < base_address + executable.memory.len);
    const slot: *u64 = @ptrFromInt(address);
    slot.* = 42;
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, slots.allocation[8..16], .little));
    // Linking must not bake the mapping's address into retained source data.
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, splice.data.items[0].bytes[0..@sizeOf(usize)], .little));
}

test "linking many data items resolves each pointer through its target's own symbol offset" {
    // Every item points at the next one. Finding a target by scanning all
    // items makes linking quadratic, which at this count runs for minutes.
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try @import("layout").Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    var codegen = try HostLirCodeGen.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer codegen.deinit();
    _ = try codegen.generateHostedStub("test_unused_entry");

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const count = 100_000;
    const items = try arena.allocator().alloc(ProcArtifact.DataItem, count);
    const names = try arena.allocator().alloc([]const u8, count);
    for (names, 0..) |*name, index| name.* = try std.fmt.allocPrint(arena.allocator(), "test_item_{d}", .{index});
    for (items, 0..) |*item, index| {
        const bytes = try arena.allocator().alloc(u8, 2 * @sizeOf(usize));
        @memset(bytes, 0);
        const relocations = try arena.allocator().alloc(ProcArtifact.DataRelocation, 1);
        relocations[0] = .{ .offset = @sizeOf(usize), .name = names[(index + 1) % count], .addend = 0, .function = false };
        item.* = .{
            .name = names[index],
            .bytes = bytes,
            .alignment = @alignOf(usize),
            .symbol_offset = @sizeOf(usize),
            .relocations = relocations,
        };
    }
    var splice = HostSplice.init(allocator);
    defer splice.deinit();
    try splice.addDataItems(items);
    var table: BoxyNativeFnTable = undefined;
    @memset(&table, 0);
    try splice.generateHostedStubs(&codegen, &table);
    try codegen.finishImage();
    var executable = try splice.link(&codegen, &table);
    defer executable.deinit();
    const base_address = @intFromPtr(executable.memory.ptr);
    for (names, 0..) |name, index| {
        const offset = splice.data_offsets.get(name).?;
        const pointer = std.mem.readInt(usize, executable.memory[offset + @sizeOf(usize) ..][0..@sizeOf(usize)], .little);
        const target = splice.data_offsets.get(names[(index + 1) % count]).?;
        try std.testing.expectEqual(base_address + target + @sizeOf(usize), pointer);
    }
}
