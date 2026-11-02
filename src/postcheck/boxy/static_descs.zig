//! Static descriptors and names (design.md "Type Descriptors").
//!
//! Every closed type a body needs a descriptor for gets one static
//! descriptor, interned by the type's join-set representative, emitted as
//! readonly static data in the runtime's descriptor format at the target's
//! pointer width. Each descriptor (and each name) is a static-data slot whose
//! value is the pointer to the descriptor (or `Name`) node, so a body reads it
//! into an `opaque_ptr` local with an ordinary `static_data` literal. Nodes
//! reference one another through relocations; recursive types make the node
//! graph cyclic, which needs no special treatment because every node is
//! reserved before any is filled.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const layout = @import("layout");
const lir_core = @import("lir_core");
const boxy_runtime = @import("boxy_runtime");
const types = @import("types.zig");
const Layouts = @import("layouts.zig").Layouts;
const lower = @import("lower.zig");

const Allocator = std.mem.Allocator;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const desc_format = boxy_runtime.desc;
const TypeId = types.TypeId;
const NameId = types.NameId;
const WorkerId = lower.WorkerId;

const StaticDataExport = LirProgram.StaticDataExport;
const StaticDataRelocation = LirProgram.StaticDataRelocation;
const ExportIndex = u32;

/// The static descriptors and dictionaries the program carries as static data.
pub const StaticDescs = struct {
    allocator: Allocator,
    store: *types.Store,
    layouts: *Layouts,
    result: *LirProgram.Result,
    width: desc_format.PointerWidth,
    exports: std.ArrayList(StaticDataExport) = .empty,
    /// Descriptor slot and node of each closed type, by representative.
    desc_slots: std.AutoHashMapUnmanaged(Key, Reserved) = .empty,
    name_slots: collections.DenseMapUnmanaged(NameId, Reserved) = .empty,
    pending: std.ArrayList(Pending) = .empty,
    opaque_word: ?LIR.StaticDataId = null,
    null_word: ?LIR.StaticDataId = null,
    /// How many dictionary callables static callables have captured, which
    /// names each one's node.
    extra_nodes: u32 = 0,
    /// The key word of each recursive member type composed at runtime.
    recursion_keys: collections.DenseMapUnmanaged(TypeId, LIR.StaticDataId) = .empty,
    /// Each static dictionary, by its procedure and capture words.
    dictionaries: std.StringHashMapUnmanaged(Reserved) = .empty,

    /// A descriptor's identity: a type, or the recursion box around one.
    const Key = struct { ty: TypeId, recursive_box: bool };
    pub const Reserved = struct { slot: LIR.StaticDataId, node: ExportIndex };
    const Pending = struct { key: Key, node: ExportIndex };

    pub fn init(allocator: Allocator, store: *types.Store, layouts: *Layouts, result: *LirProgram.Result, target_usize: base.target.TargetUsize) StaticDescs {
        return .{
            .allocator = allocator,
            .store = store,
            .layouts = layouts,
            .result = result,
            .width = switch (target_usize) {
                .u32 => .u32,
                .u64 => .u64,
            },
        };
    }

    pub fn deinit(self: *StaticDescs) void {
        for (self.exports.items) |item| {
            self.allocator.free(item.symbol_name);
            self.allocator.free(item.bytes);
            for (item.relocations) |relocation| {
                if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
            }
            self.allocator.free(item.relocations);
        }
        self.exports.deinit(self.allocator);
        self.desc_slots.deinit(self.allocator);
        self.name_slots.deinit(self.allocator);
        self.recursion_keys.deinit(self.allocator);
        var dictionary_keys = self.dictionaries.keyIterator();
        while (dictionary_keys.next()) |key| self.allocator.free(key.*);
        self.dictionaries.deinit(self.allocator);
        self.pending.deinit(self.allocator);
    }

    /// Hand the emitted exports to the program; the caller owns them after.
    pub fn takeExports(self: *StaticDescs) Allocator.Error![]StaticDataExport {
        return try self.exports.toOwnedSlice(self.allocator);
    }

    /// The static-data slot holding the descriptor of closed type `ty`.
    pub fn descriptorSlot(self: *StaticDescs, ty: TypeId) Allocator.Error!LIR.StaticDataId {
        const reserved = try self.reserveDescriptor(.{ .ty = ty, .recursive_box = false });
        try self.drain();
        return reserved.slot;
    }

    /// The static-data slot holding the descriptor of a pointer-sized word
    /// with no reference-counted content (a descriptor, or an immortal
    /// dictionary, stored in a dynamic capture record).
    pub fn opaqueWordSlot(self: *StaticDescs) Allocator.Error!LIR.StaticDataId {
        if (self.opaque_word) |slot| return slot;
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const reserved = try self.addSlotWithNode(desc_layout.size, desc_layout.alignment);
        const bytes = @constCast(self.exports.items[reserved.node].bytes);
        writeU32(bytes, desc_layout.of(.size), self.ptrSize());
        writeU32(bytes, desc_layout.of(.alignment), self.ptrSize());
        bytes[desc_layout.of(.kind)] = @backingInt(desc_format.Kind.scalar);
        bytes[desc_layout.of(.scalar)] = @backingInt(desc_format.Scalar.opaque_ptr);
        bytes[desc_layout.of(.sort_key)] = @backingInt(layout.SortKey.pointer);
        self.opaque_word = reserved.slot;
        return reserved.slot;
    }

    /// The static-data slot pointing at the word that names the open
    /// recursive member type `ty` in the runtime's interning of its composed
    /// descriptors. The word holds `ty`'s id, so no two members' words are
    /// identical data.
    pub fn recursionKeySlot(self: *StaticDescs, ty: TypeId) Allocator.Error!LIR.StaticDataId {
        if (self.recursion_keys.get(ty)) |slot| return slot;
        const reserved = try self.addSlotWithNode(8, 4);
        const bytes = @constCast(self.exports.items[reserved.node].bytes);
        writeU32(bytes, 0, recursion_key_mark);
        writeU32(bytes, 4, @backingInt(ty));
        try self.recursion_keys.put(self.allocator, ty, reserved.slot);
        return reserved.slot;
    }

    const recursion_key_mark: u32 = 0x5245_4359;

    /// A fresh program-owned composition slot, named by worker and site rather
    /// than by its initial zero bytes. No two sites share mutable state.
    pub fn compositionSlot(self: *StaticDescs, identity: LIR.ProcIdentity, site: u32) Allocator.Error!LIR.StaticDataId {
        const slot = try self.addSlot();
        const name = try std.fmt.allocPrint(self.allocator, "roc__boxy_site_{s}_{d}", .{ &std.fmt.bytesToHex(identity.bytes, .lower), site });
        const node = try self.addExport(name, self.ptrSize(), self.ptrSize(), slot);
        self.exports.items[node].is_mutable = true;
        return slot;
    }

    /// The static dictionary of `proc` capturing `words`: an immortal callable
    /// node, and the slot whose value points at its payload as a composed
    /// dictionary's word does.
    pub fn dictionary(self: *StaticDescs, proc: LIR.LirProcSpecId, words: []const CaptureWord) Allocator.Error!Reserved {
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try key.appendSlice(self.allocator, std.mem.asBytes(&@backingInt(proc)));
        for (words) |word| {
            try key.append(self.allocator, @backingInt(std.meta.activeTag(word)));
            const id: u32 = switch (word) {
                .desc => |ty| @backingInt(ty),
                .callable => |node| node,
                .zero => 0,
            };
            try key.appendSlice(self.allocator, std.mem.asBytes(&id));
        }
        if (self.dictionaries.get(key.items)) |existing| return existing;

        const slot = try self.addSlot();
        const slot_export = try self.addExport(try LirProgram.staticDataSymbolName(self.allocator, slot), self.ptrSize(), self.ptrSize(), slot);
        const node = try self.staticCallableOf(@backingInt(slot), 1, proc, words);
        try self.setRelocations(slot_export, &.{.{ .offset = 0, .target = node, .addend = static_callable_header }});
        try self.drain();
        const reserved: Reserved = .{ .slot = slot, .node = node };
        const owned_key = try self.allocator.dupe(u8, key.items);
        errdefer self.allocator.free(owned_key);
        try self.dictionaries.put(self.allocator, owned_key, reserved);
        return reserved;
    }

    /// The static-data slot holding a null pointer.
    pub fn nullWordSlot(self: *StaticDescs) Allocator.Error!LIR.StaticDataId {
        if (self.null_word) |slot| return slot;
        const slot = try self.addSlot();
        _ = try self.addExport(try LirProgram.staticDataSymbolName(self.allocator, slot), self.ptrSize(), self.ptrSize(), slot);
        self.null_word = slot;
        return slot;
    }

    /// The static-data slot holding the `Name` of `name`.
    pub fn nameSlot(self: *StaticDescs, name: NameId) Allocator.Error!LIR.StaticDataId {
        return (try self.reserveName(name)).slot;
    }

    fn ptrSize(self: *const StaticDescs) u32 {
        return self.width.bytes();
    }

    fn addSlot(self: *StaticDescs) Allocator.Error!LIR.StaticDataId {
        const id: LIR.StaticDataId = @fromBackingInt(@intCast(self.result.static_data_values.items.len));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = .opaque_ptr,
        });
        return id;
    }

    /// Append an export of `size` zero bytes and return its index.
    fn addExport(self: *StaticDescs, name: []u8, size: u32, alignment: u32, value_id: ?LIR.StaticDataId) Allocator.Error!ExportIndex {
        errdefer self.allocator.free(name);
        const bytes = try self.allocator.alloc(u8, size);
        @memset(bytes, 0);
        const index: ExportIndex = @intCast(self.exports.items.len);
        try self.exports.append(self.allocator, .{
            .symbol_name = name,
            .value_id = value_id,
            .bytes = bytes,
            .alignment = alignment,
            .is_global = false,
            .is_exported = false,
        });
        return index;
    }

    /// A slot whose value points at a fresh node of `node_size` bytes.
    fn addSlotWithNode(self: *StaticDescs, node_size: u32, node_alignment: u32) Allocator.Error!Reserved {
        const slot = try self.addSlot();
        const slot_export = try self.addExport(try LirProgram.staticDataSymbolName(self.allocator, slot), self.ptrSize(), self.ptrSize(), slot);
        const node = try self.addExport(try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(slot), 1), node_size, node_alignment, null);
        try self.setRelocations(slot_export, &.{.{ .offset = 0, .target = node }});
        return .{ .slot = slot, .node = node };
    }

    /// One pointer inside a node: at `offset`, the address of `target`.
    const Pointer = struct { offset: u32, target: ExportIndex, addend: u32 = 0 };

    fn setRelocations(self: *StaticDescs, owner: ExportIndex, pointers: []const Pointer) Allocator.Error!void {
        const relocations = try self.allocator.alloc(StaticDataRelocation, pointers.len);
        errdefer self.allocator.free(relocations);
        for (pointers, relocations) |pointer, *relocation| {
            relocation.* = .{
                .offset = pointer.offset,
                .target_symbol_name = self.exports.items[pointer.target].symbol_name,
                .target = .{ .data_symbol = @fromBackingInt(pointer.target) },
                .addend = pointer.addend,
            };
        }
        const item = &self.exports.items[owner];
        self.allocator.free(item.relocations);
        item.relocations = relocations;
    }

    fn program(self: *StaticDescs) *lower.Program {
        return @alignCast(@fieldParentPtr("static_descs", self));
    }

    /// The `Methods` node of the closed nominal type `ty`'s overrides, or
    /// null when it has none: each override an immortal static callable of
    /// the method's dictionary entry, capturing the descriptors of the types
    /// the method's scheme takes.
    fn methodsNode(self: *StaticDescs, ty: TypeId) Allocator.Error!?ExportIndex {
        const overrides = try self.program().nominalOverrides(ty);
        if (overrides[0] == null and overrides[1] == null and overrides[2] == null) return null;
        const owner = @backingInt(self.desc_slots.get(.{ .ty = ty, .recursive_box = false }).?.slot);
        const methods_layout = desc_format.Methods.layout(self.width);
        const methods = try self.addExport(try LirProgram.staticDataNodeSymbolName(self.allocator, owner, 3), methods_layout.size, self.ptrSize(), null);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);
        const fields = [_]std.meta.FieldEnum(desc_format.Methods){ .is_eq, .to_hash, .to_inspect };
        inline for (fields, 0..) |field, index| if (overrides[index]) |override| {
            const callable = switch (override) {
                .procedure => |procedure| try self.staticCallable(owner, @intCast(4 + index), procedure),
                .local => types.invariant("a static descriptor's type holds a local-procedure override, which only a body composes"),
            };
            try pointers.append(self.allocator, .{ .offset = methods_layout.of(field), .target = callable, .addend = static_callable_header });
        };
        try self.setRelocations(methods, pointers.items);
        return methods;
    }

    /// Bytes before a static callable's payload: alignment padding, then the
    /// zero (static) refcount word immediately before the payload.
    const static_callable_header: u32 = @intCast(boxy_runtime.compiled.static_callable_header);

    /// One word a static callable captures: a descriptor, another static
    /// callable (a dictionary), or a word no body reads.
    pub const CaptureWord = union(enum) { desc: TypeId, callable: ExportIndex, zero };

    /// An immortal erased callable of `override`'s entry whose capture is
    /// the descriptors of its scheme's types then its dictionaries, one word
    /// each. Each dictionary is resolved from its dispatcher's closed type
    /// (`Program.dictionaryTree`) into a static callable of its own. Every
    /// callable is reserved before any is filled, so a recursive requirement's
    /// callables can capture one another.
    fn staticCallable(self: *StaticDescs, owner: u32, index: u32, override: @FieldType(lower.NominalOverride, "procedure")) Allocator.Error!ExportIndex {
        const program_ = self.program();
        const tree = try program_.dictionaryTree(.{ .target = .{
            .target = override.target,
            .instance = .{ .bound = override.bound, .requirements = override.dicts },
        } });
        const nodes = try self.allocator.alloc(ExportIndex, tree.len);
        defer self.allocator.free(nodes);
        const thunks = try self.allocator.alloc([]const WorkerId, tree.len);
        @memset(thunks, &.{});
        defer {
            for (thunks) |worker_thunks| self.allocator.free(worker_thunks);
            self.allocator.free(thunks);
        }
        // Parents precede their children, so each node's module is known by
        // the time its requirements resolve.
        const modules = try self.allocator.alloc(types.ModuleIndex, tree.len);
        defer self.allocator.free(modules);
        for (tree, 0..) |node, position| {
            switch (node.kind) {
                .target => |target| {
                    const entry = if (position == 0) program_.worker(override.entry).* else program_.worker(try program_.methodEntryWorker(target.target)).*;
                    modules[position] = entry.module;
                    for (target.children) |child| modules[child] = entry.module;
                    // Literal arguments follow the dictionaries: a static
                    // callable per literal thunk, capturing nothing
                    // (design.md "Literal Arguments").
                    thunks[position] = try program_.literalThunks(self.allocator, target.target, target.instance.bound);
                    const words = target.instance.bound.len + target.children.len + thunks[position].len;
                    // Each node index past the slot's own names one callable.
                    const node_index: u32 = if (position == 0) index else blk: {
                        self.extra_nodes += 1;
                        break :blk 16 + self.extra_nodes;
                    };
                    nodes[position] = try self.reserveStaticCallable(owner, node_index, @intCast(words));
                },
                .resolved => |resolution| switch (resolution) {
                    .structural => |structural| {
                        self.extra_nodes += 1;
                        const worker = program_.worker(try program_.structuralWorker(modules[position], structural.method)).*;
                        nodes[position] = try self.staticCallableOf(owner, 16 + self.extra_nodes, worker.proc, &.{.{ .desc = structural.over }});
                    },
                    .crash => |kind| {
                        self.extra_nodes += 1;
                        const worker = program_.worker(try program_.crashDictionaryWorker(modules[position], kind)).*;
                        nodes[position] = try self.staticCallableOf(owner, 16 + self.extra_nodes, worker.proc, &.{});
                    },
                    .derived => types.invariant("a closed nominal's override requirement names a derived codec or map, which checking records with its plan at the dispatch it serves"),
                    .scope_variable => types.invariant("a closed nominal's override names a scope variable"),
                    .target => types.invariant("a dictionary's target requirement is not a resolved node"),
                },
                .back_edge => |head| nodes[position] = nodes[head],
            }
        }
        for (tree, 0..) |node, position| {
            const target = switch (node.kind) {
                .target => |target| target,
                .resolved, .back_edge => continue,
            };
            var words: std.ArrayList(CaptureWord) = .empty;
            defer words.deinit(self.allocator);
            for (target.instance.bound) |ty| try words.append(self.allocator, .{ .desc = ty });
            for (target.children) |child| try words.append(self.allocator, .{ .callable = nodes[child] });
            for (thunks[position]) |thunk| {
                self.extra_nodes += 1;
                const callable = try self.staticCallableOf(owner, 16 + self.extra_nodes, program_.worker(thunk).proc, &.{});
                try words.append(self.allocator, .{ .callable = callable });
            }
            const proc = if (position == 0) program_.worker(override.entry).proc else program_.worker(try program_.methodEntryWorker(target.target)).proc;
            try self.fillStaticCallable(nodes[position], proc, words.items);
        }
        return nodes[0];
    }

    /// A static callable's export, sized for `word_count` capture words.
    fn reserveStaticCallable(self: *StaticDescs, owner: u32, index: u32, word_count: u32) Allocator.Error!ExportIndex {
        const size = static_callable_header + self.staticCallableCaptureOffset() + word_count * self.ptrSize();
        return try self.addExport(try LirProgram.staticDataNodeSymbolName(self.allocator, owner, index), size, static_callable_header, null);
    }

    fn staticCallableCaptureOffset(self: *const StaticDescs) u32 {
        return std.mem.alignForward(u32, 2 * self.ptrSize(), static_callable_header);
    }

    fn staticCallableOf(self: *StaticDescs, owner: u32, index: u32, proc: LIR.LirProcSpecId, words: []const CaptureWord) Allocator.Error!ExportIndex {
        const node = try self.reserveStaticCallable(owner, index, @intCast(words.len));
        try self.fillStaticCallable(node, proc, words);
        return node;
    }

    fn fillStaticCallable(self: *StaticDescs, node: ExportIndex, proc: LIR.LirProcSpecId, words: []const CaptureWord) Allocator.Error!void {
        const ptr = self.ptrSize();
        const capture_offset = self.staticCallableCaptureOffset();
        var relocation_count: usize = 1;
        for (words) |word| relocation_count += @intFromBool(word != .zero);
        const relocations = try self.allocator.alloc(StaticDataRelocation, relocation_count);
        errdefer self.allocator.free(relocations);
        relocations[0] = .{
            .offset = static_callable_header,
            // The procedure's identity names it, whatever id compaction
            // later gives it.
            .target_symbol_name = try self.result.store.getProcSpec(proc).identity.symbolName(self.allocator),
            .owns_target_symbol_name = true,
            .kind = .function_pointer,
            .procedure = proc,
            .callable_capture_offset = capture_offset,
        };
        var next: usize = 1;
        for (words, 0..) |word, position| {
            const target, const addend = switch (word) {
                .desc => |ty| .{ (try self.reserveDescriptor(.{ .ty = ty, .recursive_box = false })).node, 0 },
                .callable => |callable| .{ callable, static_callable_header },
                .zero => continue,
            };
            relocations[next] = .{
                .offset = static_callable_header + capture_offset + @as(u32, @intCast(position)) * ptr,
                .target_symbol_name = self.exports.items[target].symbol_name,
                .target = .{ .data_symbol = @fromBackingInt(target) },
                .addend = addend,
            };
            next += 1;
        }
        self.exports.items[node].relocations = relocations;
    }

    fn reserveName(self: *StaticDescs, name: NameId) Allocator.Error!Reserved {
        if (self.name_slots.get(name)) |existing| return existing;
        const name_layout = desc_format.externLayout(desc_format.Name, self.width);
        const reserved = try self.addSlotWithNode(name_layout.size, name_layout.alignment);
        const text = self.store.nameText(name);
        const text_node = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(reserved.slot), 2),
            @intCast(@max(text.len, 1)),
            1,
            null,
        );
        @memcpy(@constCast(self.exports.items[text_node].bytes)[0..text.len], text);
        const bytes = @constCast(self.exports.items[reserved.node].bytes);
        @memcpy(bytes[name_layout.of(.id)..][0..32], &desc_format.nameIdDigest(self.store.nameContentId(name)));
        writeU32(bytes, name_layout.of(.len), @intCast(text.len));
        try self.setRelocations(reserved.node, &.{.{ .offset = name_layout.of(.bytes), .target = text_node }});
        try self.name_slots.put(self.allocator, name, reserved);
        return reserved;
    }

    fn reserveDescriptor(self: *StaticDescs, key: Key) Allocator.Error!Reserved {
        if (self.desc_slots.get(key)) |existing| return existing;
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const reserved = try self.addSlotWithNode(desc_layout.size, desc_layout.alignment);
        try self.desc_slots.put(self.allocator, key, reserved);
        try self.pending.append(self.allocator, .{ .key = key, .node = reserved.node });
        return reserved;
    }

    fn drain(self: *StaticDescs) Allocator.Error!void {
        while (self.pending.pop()) |item| try self.fill(item);
    }

    /// The descriptor node of a component stored at `committed`: the
    /// component type's own descriptor, or the recursion box around it when
    /// the committed layout boxed a component the type does not box.
    fn componentNode(self: *StaticDescs, ty: TypeId, committed: layout.Idx) Allocator.Error!ExportIndex {
        const boxed_by_layout = self.layouts.layouts.getLayout(committed).tag == .box and self.store.get(ty) != .box;
        return (try self.reserveDescriptor(.{ .ty = ty, .recursive_box = boxed_by_layout })).node;
    }

    fn fill(self: *StaticDescs, item: Pending) Allocator.Error!void {
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);

        if (item.key.recursive_box) {
            const payload = (try self.reserveDescriptor(.{ .ty = item.key.ty, .recursive_box = false })).node;
            const bytes = @constCast(self.exports.items[item.node].bytes);
            writeU32(bytes, desc_layout.of(.size), self.ptrSize());
            writeU32(bytes, desc_layout.of(.alignment), self.ptrSize());
            bytes[desc_layout.of(.kind)] = @backingInt(desc_format.Kind.recursive_box);
            bytes[desc_layout.of(.sort_key)] = @backingInt(layout.SortKey.pointer);
            bytes[desc_layout.of(.flags)] = @bitCast(desc_format.Flags{ .contains_refcounted = true, .recursive = true });
            try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = payload });
            return try self.setRelocations(item.node, pointers.items);
        }

        const ty = item.key.ty;
        const committed = try self.layouts.layoutOf(ty);
        const layouts = self.layouts.layouts;
        const committed_layout = layouts.getLayout(committed);
        const size_align = layouts.layoutSizeAlign(committed_layout);
        var kind: desc_format.Kind = undefined;
        var scalar: desc_format.Scalar = .u8;
        var count: u32 = 0;
        var discriminant_offset: u32 = 0;
        var discriminant_size: u32 = 0;
        var flags: desc_format.Flags = .{
            .contains_refcounted = layouts.layoutContainsRefcounted(committed_layout),
            .recursive = try self.layouts.onSizeCycle(ty),
        };

        switch (self.store.get(ty)) {
            .primitive => |primitive| switch (primitive) {
                .bool => {
                    kind = .bool;
                    discriminant_size = 1;
                },
                .str => kind = .str,
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => {
                    kind = .scalar;
                    scalar = scalarOf(primitive);
                    if (scalar.isVector()) {
                        if (try self.methodsNode(ty)) |methods| try pointers.append(self.allocator, .{ .offset = desc_layout.of(.methods), .target = methods });
                    }
                },
            },
            .empty_record => kind = .record,
            .function => kind = .erased_callable,
            .list => |elem| {
                kind = .list;
                const elem_layout = switch (committed_layout.tag) {
                    .list => committed_layout.getIdx(),
                    .scalar, .box, .box_of_zst, .list_of_zst, .struct_, .closure, .erased_callable, .zst, .tag_union, .ptr, .hole, .dynamic => try self.layouts.layoutOf(elem),
                };
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(elem, elem_layout) });
            },
            // `Box(function)` is the function's erased callable, with the
            // function as its child so the value still inspects as a box.
            .box => |payload| if (self.store.get(payload) == .function) {
                kind = .erased_callable;
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = (try self.reserveDescriptor(.{ .ty = payload, .recursive_box = false })).node });
            } else {
                kind = .box;
                const payload_layout = switch (committed_layout.tag) {
                    .box => committed_layout.getIdx(),
                    .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .erased_callable, .zst, .tag_union, .ptr, .hole, .dynamic => try self.layouts.layoutOf(payload),
                };
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(payload, payload_layout) });
            },
            .record => |record| {
                kind = .record;
                flags.declared_order = record.declared != null;
                const fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                const storage = try self.layouts.recordStorage(record);
                defer self.allocator.free(storage);
                count = @intCast(storage.len);
                try self.fillFields(item.node, &pointers, committed_layout, storage, fields, if (record.declared) |declared| self.store.declaredSlots(declared) else null);
            },
            .tuple => |items_span| {
                kind = .tuple;
                const items = try self.allocator.dupe(TypeId, self.store.span(items_span));
                defer self.allocator.free(items);
                count = @intCast(items.len);
                try self.fillFields(item.node, &pointers, committed_layout, items, null, null);
            },
            .tag_union => |tag_union| {
                kind = .tag_union;
                flags.presence_slot = self.layouts.presenceSlotPayload(ty) != null;
                const tags = try self.allocator.dupe(types.Tag, self.store.tagSpan(tag_union.tags));
                defer self.allocator.free(tags);
                count = @intCast(tags.len);
                if (committed_layout.tag == .tag_union) {
                    const info = layouts.getTagUnionInfo(committed_layout);
                    discriminant_offset = info.discriminant_offset;
                    discriminant_size = info.data.discriminant_size;
                }
                try self.fillVariants(item.node, &pointers, committed_layout, tags);
            },
            .nominal => |nominal| {
                if (nominal.builtin == .bool) {
                    kind = .bool;
                    discriminant_size = 1;
                } else {
                    kind = .nominal;
                    flags.inspect_opaque = nominal.is_opaque;
                    const backing = try self.layouts.converter.backing(ty);
                    const backing_node = (try self.reserveDescriptor(.{ .ty = backing, .recursive_box = false })).node;
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = backing_node });
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.name), .target = (try self.reserveName(nominal.name)).node });
                    if (try self.methodsNode(ty)) |methods| try pointers.append(self.allocator, .{ .offset = desc_layout.of(.methods), .target = methods });
                }
            },
            .variable => types.invariant("a type variable reached static descriptor emission"),
        }

        const bytes = @constCast(self.exports.items[item.node].bytes);
        writeU32(bytes, desc_layout.of(.size), size_align.size);
        writeU32(bytes, desc_layout.of(.alignment), @intCast(size_align.alignment.toByteUnits()));
        bytes[desc_layout.of(.kind)] = @backingInt(kind);
        bytes[desc_layout.of(.scalar)] = @backingInt(scalar);
        bytes[desc_layout.of(.sort_key)] = @backingInt(committed_layout.sortKey());
        bytes[desc_layout.of(.flags)] = @bitCast(flags);
        writeU32(bytes, desc_layout.of(.count), count);
        writeU32(bytes, desc_layout.of(.discriminant_offset), discriminant_offset);
        writeU32(bytes, desc_layout.of(.discriminant_size), discriminant_size);
        try self.setRelocations(item.node, pointers.items);
    }

    /// The `Field` entries of a record or tuple, in the committed layout's
    /// memory order. `names` is null for a tuple; `storage` holds a slot for
    /// each of them by storage index, and past them the slots of a padded
    /// record's unnamed fields, which `declared` orders.
    fn fillFields(
        self: *StaticDescs,
        owner: ExportIndex,
        pointers: *std.ArrayList(Pointer),
        committed_layout: layout.Layout,
        storage: []const TypeId,
        names: ?[]const types.Field,
        declared: ?[]const u32,
    ) Allocator.Error!void {
        if (storage.len == 0) return;
        const layouts = self.layouts.layouts;
        const field_layout = desc_format.Field.layout(self.width);
        var refcounted_count: u32 = 0;
        if (committed_layout.tag == .struct_) {
            const struct_idx = committed_layout.getStruct().idx;
            const memory_count = layouts.getStructData(struct_idx).fields.count;
            var position: u32 = 0;
            while (position < memory_count) : (position += 1) {
                const field = layouts.getStructField(struct_idx, position);
                if (!field.is_padding and layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) refcounted_count += 1;
            }
        }
        const fields_size = field_layout.size * @as(u32, @intCast(storage.len));
        const trailer_size = if (refcounted_count == 0) 0 else (refcounted_count + 1) * @sizeOf(u32);
        const owner_slot = self.slotOfNode(owner);
        const entries = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(owner_slot), 2),
            fields_size + trailer_size,
            field_layout.alignment,
            null,
        );
        var entry_pointers: std.ArrayList(Pointer) = .empty;
        defer entry_pointers.deinit(self.allocator);
        if (refcounted_count != 0) writeU32(@constCast(self.exports.items[entries].bytes), fields_size, refcounted_count);
        var next_refcounted: u32 = 1;
        if (committed_layout.tag == .struct_) {
            const struct_idx = committed_layout.getStruct().idx;
            const memory_count = layouts.getStructData(struct_idx).fields.count;
            if (memory_count != storage.len) types.invariant("a record's committed layout disagrees with its field count");
            var position: u32 = 0;
            while (position < memory_count) : (position += 1) {
                const field = layouts.getStructField(struct_idx, position);
                const base_offset = position * field_layout.size;
                const bytes = @constCast(self.exports.items[entries].bytes);
                writeU32(bytes, base_offset + field_layout.of(.field_index), field.index);
                writeU32(bytes, base_offset + field_layout.of(.offset), layouts.getStructFieldOffset(struct_idx, position));
                writeU32(bytes, base_offset + field_layout.of(.is_padding), @intFromBool(field.is_padding));
                if (!field.is_padding and layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) {
                    writeU32(bytes, fields_size + next_refcounted * @sizeOf(u32), position);
                    next_refcounted += 1;
                }
                // The entry at memory position `field.index` names this slot.
                writeU32(bytes, field.index * field_layout.size + field_layout.of(.field_slot), position);
                const component = storage[field.index];
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentNode(component, field.layout) });
                if (names) |record_fields| if (!field.is_padding) {
                    try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.name), .target = (try self.reserveName(record_fields[field.index].name)).node });
                };
            }
        } else {
            // Every field is zero-sized: each sits at offset 0, in checked
            // field order or in the order its declaration gives.
            for (storage, 0..) |_, position| {
                const index: usize = if (declared) |slots| slots[position] else position;
                const base_offset: u32 = @as(u32, @intCast(position)) * field_layout.size;
                const bytes = @constCast(self.exports.items[entries].bytes);
                writeU32(bytes, base_offset + field_layout.of(.field_index), @intCast(index));
                writeU32(bytes, @as(u32, @intCast(index)) * field_layout.size + field_layout.of(.field_slot), @intCast(position));
                const is_padding = if (names) |record_fields| index >= record_fields.len else false;
                writeU32(bytes, base_offset + field_layout.of(.is_padding), @intFromBool(is_padding));
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentNode(storage[index], try self.layouts.layoutOf(storage[index])) });
                if (names) |record_fields| if (!is_padding) {
                    try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.name), .target = (try self.reserveName(record_fields[index].name)).node });
                };
            }
        }
        try self.setRelocations(entries, entry_pointers.items);
        try pointers.append(self.allocator, .{ .offset = desc_format.TypeDesc.layout(self.width).of(.entries), .target = entries });
    }

    fn fillVariants(
        self: *StaticDescs,
        owner: ExportIndex,
        pointers: *std.ArrayList(Pointer),
        committed_layout: layout.Layout,
        tags: []const types.Tag,
    ) Allocator.Error!void {
        if (tags.len == 0) return;
        const layouts = self.layouts.layouts;
        const variant_layout = desc_format.Variant.layout(self.width);
        const owner_slot = self.slotOfNode(owner);
        const entries = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(owner_slot), 2),
            variant_layout.size * @as(u32, @intCast(tags.len)),
            variant_layout.alignment,
            null,
        );
        var entry_pointers: std.ArrayList(Pointer) = .empty;
        defer entry_pointers.deinit(self.allocator);
        const zst = try self.store.emptyRecord();
        for (tags, 0..) |tag, index| {
            const payloads = try self.allocator.dupe(TypeId, self.store.span(tag.payloads));
            defer self.allocator.free(payloads);
            const payload_ty = (try self.layouts.tagPayload(payloads)) orelse zst;
            const payload_layout = if (committed_layout.tag == .tag_union)
                layouts.getTagUnionInfo(committed_layout).variants.get(index).payload_layout
            else
                try self.layouts.layoutOf(payload_ty);
            const base_offset: u32 = @as(u32, @intCast(index)) * variant_layout.size;
            const bytes = @constCast(self.exports.items[entries].bytes);
            writeU32(bytes, base_offset + variant_layout.of(.discriminant), @intCast(index));
            writeU32(bytes, base_offset + variant_layout.of(.payload_count), @intCast(payloads.len));
            try entry_pointers.append(self.allocator, .{ .offset = base_offset + variant_layout.of(.name), .target = (try self.reserveName(tag.name)).node });
            try entry_pointers.append(self.allocator, .{ .offset = base_offset + variant_layout.of(.payload), .target = try self.componentNode(payload_ty, payload_layout) });
        }
        try self.setRelocations(entries, entry_pointers.items);
        try pointers.append(self.allocator, .{ .offset = desc_format.TypeDesc.layout(self.width).of(.entries), .target = entries });
    }

    /// The slot a node export was created for: node `roc__d{slot}_1`
    /// directly follows its slot's export.
    fn slotOfNode(self: *const StaticDescs, node: ExportIndex) LIR.StaticDataId {
        return self.exports.items[node - 1].value_id orelse types.invariant("a descriptor node did not follow its slot");
    }
};

fn writeU32(bytes: []u8, offset: u32, value: u32) void {
    std.mem.writeInt(u32, bytes[offset..][0..4], value, .little);
}

fn scalarOf(primitive: types.Primitive) desc_format.Scalar {
    return switch (primitive) {
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
        .bool, .str => unreachable, // `fill` gives Bool and Str their own kinds
    };
}

test "static reference field trailers use target width and physical field order" {
    const allocator = std.testing.allocator;
    for ([_]base.target.TargetUsize{ .u32, .u64 }) |width| {
        var store = types.Store.init(allocator);
        defer store.deinit();
        var modules: types.Modules = .{ .allocator = allocator };
        defer modules.deinit();
        var result = try LirProgram.Result.init(allocator, width);
        defer result.deinit();
        var layouts: Layouts = .{
            .allocator = allocator,
            .store = &store,
            .converter = .{ .store = &store, .modules = &modules },
            .layouts = &result.layouts,
        };
        defer layouts.deinit();
        var descs = StaticDescs.init(allocator, &store, &layouts, &result, width);
        defer descs.deinit();
        const text = try store.primitive(.str);
        const number = try store.primitive(.u64);
        for ([_]bool{ false, true }) |has_refs| {
            const ty = try store.intern(.{ .tuple = if (has_refs) &.{ text, number, text } else &.{ number, number, number } });
            _ = try descs.descriptorSlot(ty);
            const descriptor = descs.exports.items[descs.desc_slots.get(.{ .ty = ty, .recursive_box = false }).?.node];
            const desc_layout = desc_format.TypeDesc.layout(descs.width);
            const fields_node = for (descriptor.relocations) |relocation| {
                if (relocation.offset == desc_layout.of(.entries)) break @backingInt(relocation.target.data_symbol);
            } else return error.TestUnexpectedResult;
            const fields = descs.exports.items[fields_node];
            const fields_size = 3 * desc_format.Field.layout(descs.width).size;
            try std.testing.expectEqual(@as(usize, fields_size + @as(u32, if (has_refs) 12 else 0)), fields.bytes.len);
            try std.testing.expectEqual(@as(usize, 3), fields.relocations.len);
            if (has_refs) {
                try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, fields.bytes[fields_size..][0..4], .little));
                try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, fields.bytes[fields_size + 4 ..][0..4], .little));
                try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, fields.bytes[fields_size + 8 ..][0..4], .little));
            }
        }
    }
}
