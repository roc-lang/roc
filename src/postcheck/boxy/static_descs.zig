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
//!
//! The same reserve-then-fill holds for the other static data of `.boxy`:
//! descriptors of erased-call argument records, immortal static callables
//! (dictionaries and literal thunks), and the type tables of generic workers
//! (`tables.zig`), which address descriptors, callables, and one another.

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
    opaque_word: ?Reserved = null,
    null_word: ?LIR.StaticDataId = null,
    /// How many dictionary callables static callables have captured, which
    /// names each one's node.
    extra_nodes: u32 = 0,
    /// Each static dictionary, by its procedure and capture words.
    dictionaries: std.StringHashMapUnmanaged(Reserved) = .empty,
    /// The override scopes descriptors are emitted in (`ScopeId`); scope
    /// `none` is the empty one.
    scopes: std.ArrayList([]const ScopedMethods) = .empty,
    scope_ids: std.StringHashMapUnmanaged(ScopeId) = .empty,
    /// The part of each scope the reach of a type names (`restrictScope`).
    restricted_scopes: std.AutoHashMapUnmanaged(struct { scope: ScopeId, ty: TypeId }, ScopeId) = .empty,
    /// Each argument-record descriptor, by its words and element types.
    args_descs: std.StringHashMapUnmanaged(Reserved) = .empty,
    /// Each type table, by its worker and inputs.
    tables: std.StringHashMapUnmanaged(Reserved) = .empty,
    /// The tables reserved, in reservation order; those from `next_unfilled`
    /// on have not been handed out for filling.
    unfilled_tables: std.ArrayList(UnfilledTable) = .empty,
    next_unfilled: usize = 0,
    /// The root callable of each static dictionary tree, by its root.
    dictionary_trees: std.StringHashMapUnmanaged(ExportIndex) = .empty,
    /// Whether each type table ends with its position among the reserved
    /// tables, which reading a stored function value's table names
    /// (`StoredFn.Retained`), in a program that stores function values.
    table_identities: bool = false,
    /// The words of each cyclic recipe group a table evaluated, by the
    /// group's content (`tables.Evaluation`), in the group's own order.
    groups: std.StringHashMapUnmanaged([]const CaptureWord) = .empty,
    /// The static-data slot whose value is each word (`wordSlot`).
    word_slots: std.HashMapUnmanaged(CaptureWord, LIR.StaticDataId, WordContext, std.hash_map.default_max_load_percentage) = .empty,

    /// A descriptor's identity: a type, or the recursion box around one, and
    /// the override scope that supplies the overrides of the nominal types
    /// in its reach that no static descriptor holds on its own.
    const Key = struct { ty: TypeId, recursive_box: bool, scope: ScopeId = .none };
    pub const Reserved = struct { slot: LIR.StaticDataId, node: ExportIndex };

    /// Names an override scope: the override callables of nominal types
    /// whose overrides a body supplies (a method declared in a function body,
    /// or one whose requirements the body's own dictionaries discharge).
    /// Scopes are interned by content and restricted to the nominal types a
    /// descriptor's reach names, so equal descriptors share one key.
    pub const ScopeId = enum(u32) { none = 0, _ };

    /// The override callables a scope gives the closed nominal type
    /// `nominal`, in `Methods` order; null where it has none.
    pub const ScopedMethods = struct { nominal: TypeId, methods: [3]?CaptureWord };

    /// A table reserved by `table` whose words its worker's needs give.
    /// `index` is its position among the reserved tables.
    pub const UnfilledTable = struct { worker: WorkerId, inputs: []const CaptureWord, reserved: Reserved, index: u32 };

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
        freeKeys(self.allocator, &self.dictionaries);
        for (self.scopes.items) |entries| self.allocator.free(entries);
        self.scopes.deinit(self.allocator);
        freeKeys(self.allocator, &self.scope_ids);
        self.restricted_scopes.deinit(self.allocator);
        freeKeys(self.allocator, &self.args_descs);
        freeKeys(self.allocator, &self.tables);
        for (self.unfilled_tables.items) |unfilled| self.allocator.free(unfilled.inputs);
        self.unfilled_tables.deinit(self.allocator);
        freeKeys(self.allocator, &self.dictionary_trees);
        var groups = self.groups.iterator();
        while (groups.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.groups.deinit(self.allocator);
        self.word_slots.deinit(self.allocator);
        self.pending.deinit(self.allocator);
    }

    const WordContext = struct {
        pub fn hash(_: WordContext, word: CaptureWord) u64 {
            var hasher = std.hash.Wyhash.init(0);
            hashCaptureWord(&hasher, word);
            return hasher.final();
        }

        pub fn eql(_: WordContext, a: CaptureWord, b: CaptureWord) bool {
            return captureWordsEqual(a, b);
        }
    };

    /// The static-data slot whose value is `word`, which a body reads
    /// into a local with a `static_data` literal.
    pub fn wordSlot(self: *StaticDescs, word: CaptureWord) Allocator.Error!LIR.StaticDataId {
        if (self.word_slots.get(word)) |known| return known;
        const slot = try self.addSlot();
        const slot_export = try self.addExport(try LirProgram.staticDataSymbolName(self.allocator, slot), self.ptrSize(), self.ptrSize(), slot);
        if (try self.wordTarget(word)) |target| {
            try self.setRelocations(slot_export, &.{.{ .offset = 0, .target = target.node, .addend = target.addend }});
        }
        try self.drain();
        try self.word_slots.put(self.allocator, word, slot);
        return slot;
    }

    /// The words of the cyclic recipe group whose content is `key`, if a
    /// table evaluated it before.
    pub fn group(self: *const StaticDescs, key: []const u8) ?[]const CaptureWord {
        return self.groups.get(key);
    }

    /// Record `words` as the words of the cyclic recipe group `key`.
    pub fn putGroup(self: *StaticDescs, key: []const u8, words: []const CaptureWord) Allocator.Error!void {
        const owned_key = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(owned_key);
        const owned_words = try self.allocator.dupe(CaptureWord, words);
        errdefer self.allocator.free(owned_words);
        try self.groups.put(self.allocator, owned_key, owned_words);
    }

    /// The entries of override scope `scope_id`.
    pub fn scopeEntries(self: *const StaticDescs, scope_id: ScopeId) []const ScopedMethods {
        if (scope_id == .none) return &.{};
        return self.scopes.items[@backingInt(scope_id)];
    }

    /// An immortal callable of `proc` with `word_count` capture words,
    /// reserved: `fillCallable` gives its words once the callables it
    /// captures, which may capture it, are reserved too.
    pub fn reserveCallable(self: *StaticDescs, word_count: u32) Allocator.Error!ExportIndex {
        self.extra_nodes += 1;
        return try self.reserveStaticCallable(0, 16 + self.extra_nodes, word_count);
    }

    /// Fill the callable `reserveCallable` reserved.
    pub fn fillCallable(self: *StaticDescs, node: ExportIndex, proc: LIR.LirProcSpecId, words: []const CaptureWord) Allocator.Error!void {
        try self.fillStaticCallable(node, proc, words);
        try self.drain();
    }

    /// Free the owned byte keys of `map` and the map.
    fn freeKeys(allocator: Allocator, map: anytype) void {
        var keys = map.keyIterator();
        while (keys.next()) |key| allocator.free(key.*);
        map.deinit(allocator);
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

    fn opaqueWord(self: *StaticDescs) Allocator.Error!Reserved {
        if (self.opaque_word) |reserved| return reserved;
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const reserved = try self.addSlotWithNode(desc_layout.size, desc_layout.alignment);
        const bytes = @constCast(self.exports.items[reserved.node].bytes);
        writeU32(bytes, desc_layout.of(.size), self.ptrSize());
        writeU32(bytes, desc_layout.of(.alignment), self.ptrSize());
        bytes[desc_layout.of(.kind)] = @backingInt(desc_format.Kind.scalar);
        bytes[desc_layout.of(.scalar)] = @backingInt(desc_format.Scalar.opaque_ptr);
        bytes[desc_layout.of(.sort_key)] = @backingInt(layout.SortKey.pointer);
        self.opaque_word = reserved;
        return reserved;
    }

    /// The override scope of `entries`, which name distinct nominal types.
    pub fn internScope(self: *StaticDescs, entries: []const ScopedMethods) Allocator.Error!ScopeId {
        if (entries.len == 0) return .none;
        const sorted = try self.allocator.dupe(ScopedMethods, entries);
        errdefer self.allocator.free(sorted);
        std.mem.sort(ScopedMethods, sorted, {}, struct {
            fn lessThan(_: void, a: ScopedMethods, b: ScopedMethods) bool {
                return @backingInt(a.nominal) < @backingInt(b.nominal);
            }
        }.lessThan);
        for (sorted[1..], sorted[0 .. sorted.len - 1]) |entry, previous| {
            if (entry.nominal == previous.nominal) types.invariant("an override scope names one nominal type twice");
        }
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        for (sorted) |entry| {
            try appendKeyWord(self.allocator, &key, @backingInt(entry.nominal));
            for (entry.methods) |method| {
                if (method) |word| {
                    try key.append(self.allocator, 1);
                    try appendCaptureWordKey(self.allocator, &key, word);
                } else try key.append(self.allocator, 0);
            }
        }
        if (self.scope_ids.get(key.items)) |existing| {
            self.allocator.free(sorted);
            return existing;
        }
        // Scope `none` is the first entry.
        if (self.scopes.items.len == 0) try self.scopes.append(self.allocator, &.{});
        try self.scopes.ensureUnusedCapacity(self.allocator, 1);
        const id: ScopeId = @fromBackingInt(@intCast(self.scopes.items.len));
        const owned_key = try self.allocator.dupe(u8, key.items);
        errdefer self.allocator.free(owned_key);
        try self.scope_ids.put(self.allocator, owned_key, id);
        self.scopes.appendAssumeCapacity(sorted);
        return id;
    }

    /// The part of `scope_id` that names a nominal type in the reach of
    /// `root`: its structure, its nominal types' arguments, and their
    /// backings. Descriptors of types whose reach names no scoped nominal
    /// type share the scope-less key.
    pub fn restrictScope(self: *StaticDescs, scope_id: ScopeId, root: TypeId) Allocator.Error!ScopeId {
        if (scope_id == .none) return .none;
        if (self.restricted_scopes.get(.{ .scope = scope_id, .ty = root })) |known| return known;
        const entries = self.scopes.items[@backingInt(scope_id)];
        var kept: std.ArrayList(ScopedMethods) = .empty;
        defer kept.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, root);
        const store = self.store;
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (store.get(ty)) {
                .primitive, .empty_record, .variable, .function => {},
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try pending.append(self.allocator, field.ty);
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                    try pending.appendSlice(self.allocator, store.paddingTypes(record));
                },
                .tuple => |items| try pending.appendSlice(self.allocator, store.span(items)),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(self.allocator, store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                },
                .list, .box => |child| try pending.append(self.allocator, child),
                .nominal => |nominal| {
                    for (entries) |entry| {
                        if (entry.nominal == ty) try kept.append(self.allocator, entry);
                    }
                    try pending.appendSlice(self.allocator, store.nominalArgs(nominal));
                    if (nominal.has_backing) try pending.append(self.allocator, try self.layouts.converter.backing(ty));
                },
            }
        }
        const restricted = if (kept.items.len == entries.len) scope_id else try self.internScope(kept.items);
        try self.restricted_scopes.put(self.allocator, .{ .scope = scope_id, .ty = root }, restricted);
        return restricted;
    }

    /// The overrides scope `scope_id` gives the nominal type `ty`, if any.
    fn scopedMethods(self: *const StaticDescs, scope_id: ScopeId, ty: TypeId) ?[3]?CaptureWord {
        if (scope_id == .none) return null;
        for (self.scopes.items[@backingInt(scope_id)]) |entry| {
            if (entry.nominal == ty) return entry.methods;
        }
        return null;
    }

    /// The descriptor of an erased call's packed argument record
    /// (`layout.erased_call_abi`): `leading` pointer-sized words that no
    /// reference counting reads, then values of the closed types `elems`, in
    /// declared order. `scope_id` supplies the overrides of the nominal types
    /// in the elements' reach that no static descriptor holds on its own.
    pub fn argsDescriptor(self: *StaticDescs, leading: u32, elems: []const TypeId, scope_id: ScopeId) Allocator.Error!Reserved {
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try appendKeyWord(self.allocator, &key, leading);
        try appendKeyWord(self.allocator, &key, @backingInt(scope_id));
        for (elems) |ty| try appendKeyWord(self.allocator, &key, @backingInt(ty));
        if (self.args_descs.get(key.items)) |existing| return existing;

        const count = leading + @as(u32, @intCast(elems.len));
        const fields = try self.allocator.alloc(layout.StructField, count);
        defer self.allocator.free(fields);
        for (fields, 0..) |*field, index| field.* = .{
            .index = @intCast(index),
            .layout = if (index < leading) .opaque_ptr else try self.layouts.layoutOf(elems[index - leading]),
        };
        const committed = try self.result.layouts.putNominalStructFields(fields);
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const reserved = try self.addSlotWithNode(desc_layout.size, desc_layout.alignment);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);
        var recursive = false;
        for (elems) |ty| recursive = recursive or try self.layouts.onSizeCycle(ty);
        const committed_layout = self.layouts.layouts.getLayout(committed);
        try self.fillFields(reserved.node, &pointers, committed_layout, .{ .leading = leading, .types = elems, .scope = scope_id }, null, null);
        self.writeHeader(reserved.node, committed_layout, .{
            .kind = .tuple,
            .count = count,
            .flags = .{
                .contains_refcounted = self.layouts.layouts.layoutContainsRefcounted(committed_layout),
                .recursive = recursive,
                .declared_order = true,
            },
        });
        try self.setRelocations(reserved.node, pointers.items);
        try self.drain();
        const owned_key = try self.allocator.dupe(u8, key.items);
        errdefer self.allocator.free(owned_key);
        try self.args_descs.put(self.allocator, owned_key, reserved);
        return reserved;
    }

    /// The type table of `worker` at the inputs `inputs`, reserved on first
    /// request: its words are filled once its worker's needs are known
    /// (`fillTable`), and until then the table is one of the unfilled ones
    /// (`nextUnfilledTable`). Tables address one another and the
    /// dictionaries that capture them, so a table is reserved before any
    /// table it reaches is filled.
    pub fn table(self: *StaticDescs, worker: WorkerId, inputs: []const CaptureWord) Allocator.Error!Reserved {
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try appendKeyWord(self.allocator, &key, @backingInt(worker));
        for (inputs) |word| try appendCaptureWordKey(self.allocator, &key, word);
        if (self.tables.get(key.items)) |existing| return existing;
        const reserved = try self.addSlotWithNode(0, self.ptrSize());
        const owned_inputs = try self.allocator.dupe(CaptureWord, inputs);
        errdefer self.allocator.free(owned_inputs);
        try self.unfilled_tables.ensureUnusedCapacity(self.allocator, 1);
        const owned_key = try self.allocator.dupe(u8, key.items);
        errdefer self.allocator.free(owned_key);
        try self.tables.put(self.allocator, owned_key, reserved);
        self.unfilled_tables.appendAssumeCapacity(.{ .worker = worker, .inputs = owned_inputs, .reserved = reserved, .index = @intCast(self.unfilled_tables.items.len) });
        return reserved;
    }

    /// The oldest reserved table not yet handed out for filling, or null.
    pub fn nextUnfilledTable(self: *StaticDescs) ?UnfilledTable {
        if (self.next_unfilled == self.unfilled_tables.items.len) return null;
        defer self.next_unfilled += 1;
        return self.unfilled_tables.items[self.next_unfilled];
    }

    /// Fill the reserved table `unfilled` with `words`, word `index` at
    /// `tables.entryOffset(index, pointer width)`. With `table_identities`,
    /// a word past them holds the table's position among the reserved
    /// tables.
    pub fn fillTable(self: *StaticDescs, unfilled: UnfilledTable, words: []const CaptureWord) Allocator.Error!void {
        const node = unfilled.reserved.node;
        const item = &self.exports.items[node];
        if (item.bytes.len != 0) types.invariant("a type table was filled twice");
        const word_count = words.len + @intFromBool(self.table_identities);
        const bytes = try self.allocator.alloc(u8, word_count * self.ptrSize());
        @memset(bytes, 0);
        if (self.table_identities) writeU32(bytes, LIR.tableEntryOffset(@intCast(words.len), self.ptrSize()), unfilled.index);
        self.allocator.free(item.bytes);
        item.bytes = bytes;
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);
        for (words, 0..) |word, index| {
            const target = try self.wordTarget(word) orelse continue;
            try pointers.append(self.allocator, .{ .offset = LIR.tableEntryOffset(@intCast(index), self.ptrSize()), .target = target.node, .addend = target.addend });
        }
        try self.setRelocations(node, pointers.items);
        try self.drain();
    }

    /// The node a word addresses and the offset into it, or null for a
    /// null word.
    fn wordTarget(self: *StaticDescs, word: CaptureWord) Allocator.Error!?struct { node: ExportIndex, addend: u32 } {
        return switch (word) {
            .desc => |ty| .{ .node = (try self.reserveDescriptor(.{ .ty = ty, .recursive_box = false })).node, .addend = 0 },
            .scoped_desc => |scoped| .{ .node = (try self.reserveDescriptor(.{ .ty = scoped.ty, .recursive_box = false, .scope = try self.restrictScope(scoped.scope, scoped.ty) })).node, .addend = 0 },
            .callable => |callable| .{ .node = callable, .addend = static_callable_header },
            .table, .node => |node| .{ .node = node, .addend = 0 },
            .zero => null,
        };
    }

    /// The static dictionary of `proc` capturing `words`: an immortal callable
    /// node, and the slot whose value points at its payload as a type table's
    /// dictionary word does.
    pub fn dictionary(self: *StaticDescs, proc: LIR.LirProcSpecId, words: []const CaptureWord) Allocator.Error!Reserved {
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try appendKeyWord(self.allocator, &key, @backingInt(proc));
        for (words) |word| try appendCaptureWordKey(self.allocator, &key, word);
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
    /// null when it has none. Where the descriptor's override scope gives
    /// `ty` its overrides, the node holds those callables; otherwise each
    /// override is an immortal static callable of the method's dictionary
    /// entry, capturing the descriptors of the types the method's scheme
    /// takes.
    fn methodsNode(self: *StaticDescs, key: Key) Allocator.Error!?ExportIndex {
        const ty = key.ty;
        const scoped = self.scopedMethods(key.scope, ty);
        const overrides = try self.program().nominalOverrides(ty);
        if (scoped == null and overrides[0] == null and overrides[1] == null and overrides[2] == null) return null;
        const owner = @backingInt(self.desc_slots.get(key).?.slot);
        const methods_layout = desc_format.Methods.layout(self.width);
        const methods = try self.addExport(try LirProgram.staticDataNodeSymbolName(self.allocator, owner, 3), methods_layout.size, self.ptrSize(), null);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);
        const fields = [_]std.meta.FieldEnum(desc_format.Methods){ .is_eq, .to_hash, .to_inspect };
        inline for (fields, 0..) |field, index| {
            const callable: ?ExportIndex = if (scoped) |methods_words|
                if (methods_words[index]) |word| switch (word) {
                    .callable => |node| node,
                    .desc, .scoped_desc, .table, .node, .zero => types.invariant("an override scope's method is not a callable"),
                } else null
            else if (overrides[index]) |override| switch (override) {
                .procedure => |procedure| try self.staticCallable(owner, @intCast(4 + index), procedure),
                .local => types.invariant("a static descriptor's type holds a local-procedure override outside the override scope that supplies it"),
            } else null;
            if (callable) |node| try pointers.append(self.allocator, .{ .offset = methods_layout.of(field), .target = node, .addend = static_callable_header });
        }
        try self.setRelocations(methods, pointers.items);
        return methods;
    }

    /// Bytes before a static callable's payload: alignment padding, then the
    /// zero (static) refcount word immediately before the payload.
    const static_callable_header: u32 = @intCast(boxy_runtime.compiled.static_callable_header);

    /// One word a static callable or a type table holds: a descriptor of a
    /// closed type, one in an override scope, another static callable (a
    /// dictionary or literal thunk), a type table, an argument-record
    /// descriptor, or a word no body reads.
    pub const CaptureWord = union(enum) {
        desc: TypeId,
        scoped_desc: struct { ty: TypeId, scope: ScopeId },
        callable: ExportIndex,
        table: ExportIndex,
        node: ExportIndex,
        zero,
    };

    /// Mix `word` into `hasher`, as `captureWordsEqual` compares words.
    pub fn hashCaptureWord(hasher: *std.hash.Wyhash, word: CaptureWord) void {
        const tag: u32 = @backingInt(std.meta.activeTag(word));
        hasher.update(std.mem.asBytes(&tag));
        const operands: [2]u32 = switch (word) {
            .desc => |ty| .{ @backingInt(ty), 0 },
            .scoped_desc => |scoped| .{ @backingInt(scoped.ty), @backingInt(scoped.scope) },
            .callable, .table, .node => |node| .{ node, 0 },
            .zero => .{ 0, 0 },
        };
        hasher.update(std.mem.asBytes(&operands));
    }

    /// Whether two words are the same word.
    pub fn captureWordsEqual(a: CaptureWord, b: CaptureWord) bool {
        return std.meta.eql(a, b);
    }

    /// An immortal erased callable of `override`'s entry, through
    /// `staticDictionaryTree`.
    fn staticCallable(self: *StaticDescs, owner: u32, index: u32, override: @FieldType(lower.NominalOverride, "procedure")) Allocator.Error!ExportIndex {
        return try self.staticDictionaryTree(owner, index, .{ .target = .{
            .target = override.target,
            .instance = .{ .bound = override.bound, .requirements = override.dicts },
        } }, override.entry);
    }

    /// The static dictionary of the closed requirement tree `root`
    /// (`Program.dictionaryTree`): each target an immortal erased callable of
    /// its dictionary entry (`root_entry` for a target root, which may name
    /// another entry than the target's own), each resolved requirement a
    /// static callable of its structural or crashing method. A target's
    /// callable captures the type table of its entry at the descriptors of
    /// its scheme's types, then its dictionaries, then its literal thunks.
    ///
    /// Every callable is reserved before any is filled, so the dictionaries
    /// of a requirement that reaches itself capture one another. The tree is
    /// a function of its closed root, so it is emitted once per root: a
    /// dictionary capturing itself is the same static data wherever its root
    /// is asked for, and so are the tables keyed by it.
    pub fn staticDictionaryTree(self: *StaticDescs, owner: u32, index: u32, root: lower.DictionaryRoot, root_entry: ?WorkerId) Allocator.Error!ExportIndex {
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try appendKeyWord(self.allocator, &key, if (root_entry) |entry| @backingInt(entry) + 1 else 0);
        switch (root) {
            .requirement => |requirement| {
                try key.append(self.allocator, 0);
                try appendRequirementKey(self.allocator, &key, requirement);
            },
            .target => |target| {
                try key.append(self.allocator, 1);
                try appendKeyWord(self.allocator, &key, @backingInt(target.target));
                try appendKeyWord(self.allocator, &key, @intCast(target.instance.bound.len));
                for (target.instance.bound) |ty| try appendKeyWord(self.allocator, &key, @backingInt(ty));
                try appendKeyWord(self.allocator, &key, @intCast(target.instance.requirements.len));
                for (target.instance.requirements) |requirement| try appendRequirementKey(self.allocator, &key, requirement);
            },
        }
        if (self.dictionary_trees.get(key.items)) |existing| return existing;

        const program_ = self.program();
        const tree = try program_.dictionaryTree(root);
        const nodes = try self.allocator.alloc(ExportIndex, tree.len);
        defer self.allocator.free(nodes);
        const thunks = try self.allocator.alloc([]const WorkerId, tree.len);
        @memset(thunks, &.{});
        defer {
            for (thunks) |worker_thunks| self.allocator.free(worker_thunks);
            self.allocator.free(thunks);
        }
        const entries = try self.allocator.alloc(WorkerId, tree.len);
        defer self.allocator.free(entries);
        // Parents precede their children, so each node's module is known by
        // the time its requirements resolve; a requirement root's is its own.
        const modules = try self.allocator.alloc(types.ModuleIndex, tree.len);
        defer self.allocator.free(modules);
        switch (root) {
            .requirement => |requirement| modules[0] = requirement.module,
            .target => {},
        }
        for (tree, 0..) |node, position| {
            switch (node.kind) {
                .target => |target| {
                    entries[position] = if (position == 0 and root_entry != null) root_entry.? else try program_.methodEntryWorker(target.target);
                    const entry = program_.worker(entries[position]).*;
                    modules[position] = entry.module;
                    for (target.children) |child| modules[child] = entry.module;
                    // Literal arguments follow the dictionaries: a static
                    // callable per literal thunk, capturing nothing
                    // (design.md "Literal Arguments").
                    thunks[position] = try program_.literalThunks(self.allocator, target.target, target.instance.bound);
                    const words: u32 = if (program_.inputCount(entries[position]) == 0) 0 else 1;
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
                        const structural_id = try program_.structuralWorker(modules[position], structural.method);
                        const worker = program_.worker(structural_id).*;
                        nodes[position] = try self.staticCallableOf(owner, 16 + self.extra_nodes, worker.proc, &.{.{ .table = (try self.table(structural_id, &.{.{ .desc = structural.over }})).node }});
                    },
                    .crash => |kind| {
                        self.extra_nodes += 1;
                        const worker = program_.worker(try program_.crashDictionaryWorker(modules[position], kind)).*;
                        nodes[position] = try self.staticCallableOf(owner, 16 + self.extra_nodes, worker.proc, &.{});
                    },
                    .derived => types.invariant("a closed requirement tree names a derived codec or map, which checking records with its plan at the dispatch it serves"),
                    .scope_variable => types.invariant("a closed requirement tree names a scope variable"),
                    .target => types.invariant("a dictionary's target requirement is not a resolved node"),
                },
                .back_edge => |head| nodes[position] = nodes[head],
            }
        }
        // A target's dictionary, at the callable type its requirement names:
        // an adapter around the target's entry where the two store the
        // method's signature differently (`Program.storageDifference`), as
        // a body's dictionary of the same requirement is adapted.
        const adapted = try self.allocator.alloc(ExportIndex, tree.len);
        defer self.allocator.free(adapted);
        for (tree, adapted, 0..) |node, *adapted_node, position| {
            adapted_node.* = nodes[position];
            const requirement = node.requirement orelse continue;
            const target = switch (node.kind) {
                .target => |target| target,
                .back_edge => |head| tree[head].kind.target,
                .resolved => continue,
            };
            const target_worker = program_.worker(target.target).*;
            const own_fn = try program_.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
            const own_ty = try program_.substitute(self.allocator, own_fn, target.instance.bound);
            switch (try program_.storageDifference(own_ty, requirement.callable)) {
                .none => {},
                .function => {
                    // The adapter is in the target's variables: it captures
                    // the dictionary, then its table at their descriptors.
                    const adapter = try program_.adapterWorker(target.target, own_ty, requirement.callable);
                    const descs = try self.allocator.alloc(CaptureWord, target.instance.bound.len);
                    defer self.allocator.free(descs);
                    for (descs, target.instance.bound) |*desc, ty| desc.* = .{ .desc = ty };
                    var words: std.ArrayList(CaptureWord) = .empty;
                    defer words.deinit(self.allocator);
                    try words.append(self.allocator, .{ .callable = nodes[position] });
                    if (descs.len != 0) try words.append(self.allocator, .{ .table = (try self.table(adapter, descs)).node });
                    self.extra_nodes += 1;
                    adapted_node.* = try self.staticCallableOf(owner, 16 + self.extra_nodes, program_.worker(adapter).proc, words.items);
                },
                .rows, .record_order => types.invariant("two callable types of a dictionary differ outside the signature of the function it is"),
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
            for (target.children) |child| try words.append(self.allocator, .{ .callable = adapted[child] });
            for (thunks[position]) |thunk| {
                self.extra_nodes += 1;
                const callable = try self.staticCallableOf(owner, 16 + self.extra_nodes, program_.worker(thunk).proc, &.{});
                try words.append(self.allocator, .{ .callable = callable });
            }
            const proc = program_.worker(entries[position]).proc;
            if (words.items.len == 0) {
                try self.fillStaticCallable(nodes[position], proc, &.{});
            } else {
                const entry_table = try self.table(entries[position], words.items);
                try self.fillStaticCallable(nodes[position], proc, &.{.{ .table = entry_table.node }});
            }
        }
        const owned_key = try self.allocator.dupe(u8, key.items);
        errdefer self.allocator.free(owned_key);
        try self.dictionary_trees.put(self.allocator, owned_key, adapted[0]);
        return adapted[0];
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
            const target = try self.wordTarget(word) orelse continue;
            relocations[next] = .{
                .offset = static_callable_header + capture_offset + @as(u32, @intCast(position)) * ptr,
                .target_symbol_name = self.exports.items[target.node].symbol_name,
                .target = .{ .data_symbol = @fromBackingInt(target.node) },
                .addend = target.addend,
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

    /// The descriptor node of a component stored at `committed`, in the
    /// part of the containing descriptor's override scope `scope_id` its
    /// reach names: the component type's own descriptor, or the recursion
    /// box around it when the committed layout boxed a component the type
    /// does not box.
    fn componentNode(self: *StaticDescs, ty: TypeId, committed: layout.Idx, scope_id: ScopeId) Allocator.Error!ExportIndex {
        return (try self.reserveDescriptor(.{
            .ty = ty,
            .recursive_box = try self.layouts.isRecursionBox(committed, ty),
            .scope = try self.restrictScope(scope_id, ty),
        })).node;
    }

    /// The header fields of a descriptor `writeHeader` writes besides those
    /// its committed layout gives.
    const Header = struct {
        kind: desc_format.Kind,
        scalar: desc_format.Scalar = .u8,
        flags: desc_format.Flags,
        count: u32 = 0,
        discriminant_offset: u32 = 0,
        discriminant_size: u32 = 0,
    };

    /// Write the header of descriptor `node`, whose committed layout is
    /// `committed_layout`.
    fn writeHeader(self: *StaticDescs, node: ExportIndex, committed_layout: layout.Layout, header: Header) void {
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const size_align = self.layouts.layouts.layoutSizeAlign(committed_layout);
        const bytes = @constCast(self.exports.items[node].bytes);
        writeU32(bytes, desc_layout.of(.size), size_align.size);
        writeU32(bytes, desc_layout.of(.alignment), @intCast(size_align.alignment.toByteUnits()));
        bytes[desc_layout.of(.kind)] = @backingInt(header.kind);
        bytes[desc_layout.of(.scalar)] = @backingInt(header.scalar);
        bytes[desc_layout.of(.sort_key)] = @backingInt(committed_layout.sortKey());
        bytes[desc_layout.of(.flags)] = @bitCast(header.flags);
        writeU32(bytes, desc_layout.of(.count), header.count);
        writeU32(bytes, desc_layout.of(.discriminant_offset), header.discriminant_offset);
        writeU32(bytes, desc_layout.of(.discriminant_size), header.discriminant_size);
    }

    fn fill(self: *StaticDescs, item: Pending) Allocator.Error!void {
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);

        if (item.key.recursive_box) {
            const payload = (try self.reserveDescriptor(.{ .ty = item.key.ty, .recursive_box = false, .scope = item.key.scope })).node;
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
        const scope_id = item.key.scope;
        const committed = try self.layouts.layoutOf(ty);
        const layouts = self.layouts.layouts;
        const committed_layout = layouts.getLayout(committed);
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
                        if (try self.methodsNode(item.key)) |methods| try pointers.append(self.allocator, .{ .offset = desc_layout.of(.methods), .target = methods });
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
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(elem, elem_layout, scope_id) });
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
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(payload, payload_layout, scope_id) });
            },
            .record => |record| {
                kind = .record;
                flags.declared_order = record.declared != null;
                const fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                const storage = try self.layouts.recordStorage(record);
                defer self.allocator.free(storage);
                count = @intCast(storage.len);
                try self.fillFields(item.node, &pointers, committed_layout, .{ .types = storage, .scope = scope_id }, fields, if (record.declared) |declared| self.store.declaredSlots(declared) else null);
            },
            .tuple => |items_span| {
                kind = .tuple;
                const items = try self.allocator.dupe(TypeId, self.store.span(items_span));
                defer self.allocator.free(items);
                count = @intCast(items.len);
                try self.fillFields(item.node, &pointers, committed_layout, .{ .types = items, .scope = scope_id }, null, null);
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
                try self.fillVariants(item.node, &pointers, committed_layout, tags, scope_id);
            },
            .nominal => |nominal| {
                if (nominal.builtin == .bool) {
                    kind = .bool;
                    discriminant_size = 1;
                } else {
                    kind = .nominal;
                    flags.inspect_opaque = nominal.is_opaque;
                    const backing = try self.layouts.converter.backing(ty);
                    const backing_node = (try self.reserveDescriptor(.{ .ty = backing, .recursive_box = false, .scope = try self.restrictScope(scope_id, backing) })).node;
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = backing_node });
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.name), .target = (try self.reserveName(nominal.name)).node });
                    if (try self.methodsNode(item.key)) |methods| try pointers.append(self.allocator, .{ .offset = desc_layout.of(.methods), .target = methods });
                }
            },
            .variable => types.invariant("a type variable reached static descriptor emission"),
        }

        self.writeHeader(item.node, committed_layout, .{
            .kind = kind,
            .scalar = scalar,
            .flags = flags,
            .count = count,
            .discriminant_offset = discriminant_offset,
            .discriminant_size = discriminant_size,
        });
        try self.setRelocations(item.node, pointers.items);
    }

    /// The components of a record, tuple, or argument record, by storage
    /// index: `leading` pointer-sized words no reference counting reads,
    /// then a slot of each of `types`, in the override scope `scope`.
    const Components = struct {
        leading: u32 = 0,
        types: []const TypeId,
        scope: ScopeId,

        fn len(self: Components) usize {
            return self.leading + self.types.len;
        }
    };

    /// The descriptor node of component `index` of `components`, stored at
    /// `committed`.
    fn componentAt(self: *StaticDescs, components: Components, index: usize, committed: layout.Idx) Allocator.Error!ExportIndex {
        if (index < components.leading) return (try self.opaqueWord()).node;
        return try self.componentNode(components.types[index - components.leading], committed, components.scope);
    }

    /// The `Field` entries of a record or tuple, in the committed layout's
    /// memory order. `names` is null for a tuple or an argument record;
    /// `components` holds a slot for each field by storage index, and past
    /// them the slots of a padded record's unnamed fields, which `declared`
    /// orders.
    fn fillFields(
        self: *StaticDescs,
        owner: ExportIndex,
        pointers: *std.ArrayList(Pointer),
        committed_layout: layout.Layout,
        components: Components,
        names: ?[]const types.Field,
        declared: ?[]const u32,
    ) Allocator.Error!void {
        const storage_len = components.len();
        if (storage_len == 0) return;
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
        const fields_size = field_layout.size * @as(u32, @intCast(storage_len));
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
            if (memory_count != storage_len) types.invariant("a record's committed layout disagrees with its field count");
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
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentAt(components, field.index, field.layout) });
                if (names) |record_fields| if (!field.is_padding) {
                    try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.name), .target = (try self.reserveName(record_fields[field.index].name)).node });
                };
            }
        } else {
            // Every field is zero-sized: each sits at offset 0, in checked
            // field order or in the order its declaration gives.
            for (0..storage_len) |position| {
                const index: usize = if (declared) |slots| slots[position] else position;
                const base_offset: u32 = @as(u32, @intCast(position)) * field_layout.size;
                const bytes = @constCast(self.exports.items[entries].bytes);
                writeU32(bytes, base_offset + field_layout.of(.field_index), @intCast(index));
                writeU32(bytes, @as(u32, @intCast(index)) * field_layout.size + field_layout.of(.field_slot), @intCast(position));
                const is_padding = if (names) |record_fields| index >= record_fields.len else false;
                writeU32(bytes, base_offset + field_layout.of(.is_padding), @intFromBool(is_padding));
                const committed: layout.Idx = if (index < components.leading) .opaque_ptr else try self.layouts.layoutOf(components.types[index - components.leading]);
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentAt(components, index, committed) });
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
        scope_id: ScopeId,
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
            try entry_pointers.append(self.allocator, .{ .offset = base_offset + variant_layout.of(.payload), .target = try self.componentNode(payload_ty, payload_layout, scope_id) });
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

/// Append `value` to an interning key.
pub fn appendKeyWord(allocator: Allocator, key: *std.ArrayList(u8), value: u32) Allocator.Error!void {
    try key.appendSlice(allocator, std.mem.asBytes(&value));
}

/// Append `word` to an interning key.
pub fn appendCaptureWordKey(allocator: Allocator, key: *std.ArrayList(u8), word: StaticDescs.CaptureWord) Allocator.Error!void {
    try key.append(allocator, @backingInt(std.meta.activeTag(word)));
    switch (word) {
        .desc => |ty| try appendKeyWord(allocator, key, @backingInt(ty)),
        .scoped_desc => |scoped| {
            try appendKeyWord(allocator, key, @backingInt(scoped.ty));
            try appendKeyWord(allocator, key, @backingInt(scoped.scope));
        },
        .callable, .table, .node => |node| try appendKeyWord(allocator, key, node),
        .zero => {},
    }
}

/// Append the requirement `requirement` to an interning key.
fn appendRequirementKey(allocator: Allocator, key: *std.ArrayList(u8), requirement: lower.DictRequirement) Allocator.Error!void {
    try appendKeyWord(allocator, key, @backingInt(requirement.module));
    try appendKeyWord(allocator, key, @backingInt(requirement.method));
    try appendKeyWord(allocator, key, if (requirement.structural) |kind| @as(u32, @backingInt(kind)) + 1 else 0);
    try appendKeyWord(allocator, key, @backingInt(requirement.dispatcher));
    try appendKeyWord(allocator, key, @backingInt(requirement.callable));
}

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

/// A `StaticDescs` over a fresh type store at one pointer width, for tests
/// that need no lowering.
const TestFixture = struct {
    store: types.Store,
    modules: types.Modules,
    result: LirProgram.Result,
    layouts: Layouts,
    descs: StaticDescs,

    /// Initialize in place: the parts point at one another.
    fn init(self: *TestFixture, width: base.target.TargetUsize) Allocator.Error!void {
        const allocator = std.testing.allocator;
        self.store = types.Store.init(allocator);
        self.modules = .{ .allocator = allocator };
        self.result = try LirProgram.Result.init(allocator, width);
        self.layouts = .{
            .allocator = allocator,
            .store = &self.store,
            .converter = .{ .store = &self.store, .modules = &self.modules },
            .layouts = &self.result.layouts,
        };
        self.descs = StaticDescs.init(allocator, &self.store, &self.layouts, &self.result, width);
    }

    fn deinit(self: *TestFixture) void {
        self.descs.deinit();
        self.layouts.deinit();
        self.result.deinit();
        self.modules.deinit();
        self.store.deinit();
    }

    /// The `Field` entries of descriptor node `node`, and its bytes.
    fn fieldsOf(self: *TestFixture, node: ExportIndex) error{TestUnexpectedResult}!StaticDataExport {
        const desc_layout = desc_format.TypeDesc.layout(self.descs.width);
        for (self.descs.exports.items[node].relocations) |relocation| {
            if (relocation.offset == desc_layout.of(.entries)) return self.descs.exports.items[@backingInt(relocation.target.data_symbol)];
        }
        return error.TestUnexpectedResult;
    }

    fn headerWord(self: *TestFixture, node: ExportIndex, comptime field: std.meta.FieldEnum(desc_format.TypeDesc)) u32 {
        const desc_layout = desc_format.TypeDesc.layout(self.descs.width);
        return std.mem.readInt(u32, self.descs.exports.items[node].bytes[desc_layout.of(field)..][0..4], .little);
    }

    fn headerByte(self: *TestFixture, node: ExportIndex, comptime field: std.meta.FieldEnum(desc_format.TypeDesc)) u8 {
        const desc_layout = desc_format.TypeDesc.layout(self.descs.width);
        return self.descs.exports.items[node].bytes[desc_layout.of(field)];
    }
};

test "argument record descriptors keep declared order at natural alignment" {
    var fixture: TestFixture = undefined;
    try fixture.init(.u64);
    defer fixture.deinit();
    const byte = try fixture.store.primitive(.u8);
    const word = try fixture.store.primitive(.u64);
    const args = try fixture.descs.argsDescriptor(0, &.{ byte, word, byte }, .none);
    try std.testing.expectEqual(args, try fixture.descs.argsDescriptor(0, &.{ byte, word, byte }, .none));
    try std.testing.expectEqual(@as(u32, 24), fixture.headerWord(args.node, .size));
    try std.testing.expectEqual(@as(u32, 3), fixture.headerWord(args.node, .count));
    try std.testing.expectEqual(@as(u8, @backingInt(desc_format.Kind.tuple)), fixture.headerByte(args.node, .kind));
    const flags: desc_format.Flags = @bitCast(fixture.headerByte(args.node, .flags));
    try std.testing.expect(flags.declared_order);
    try std.testing.expect(!flags.contains_refcounted);
    const fields = try fixture.fieldsOf(args.node);
    const field_layout = desc_format.Field.layout(fixture.descs.width);
    for ([_]u32{ 0, 8, 16 }, 0..) |offset, slot| {
        const base_offset: u32 = @as(u32, @intCast(slot)) * field_layout.size;
        try std.testing.expectEqual(@as(u32, @intCast(slot)), std.mem.readInt(u32, fields.bytes[base_offset + field_layout.of(.field_index) ..][0..4], .little));
        try std.testing.expectEqual(offset, std.mem.readInt(u32, fields.bytes[base_offset + field_layout.of(.offset) ..][0..4], .little));
    }
}

test "argument record descriptors lead with words no reference counting reads" {
    for ([_]base.target.TargetUsize{ .u32, .u64 }) |width| {
        var fixture: TestFixture = undefined;
        try fixture.init(width);
        defer fixture.deinit();
        const text = try fixture.store.primitive(.str);
        const args = try fixture.descs.argsDescriptor(2, &.{text}, .none);
        const ptr = width.size();
        try std.testing.expectEqual(@as(u32, 2 * ptr + 3 * ptr), fixture.headerWord(args.node, .size));
        try std.testing.expectEqual(@as(u32, 3), fixture.headerWord(args.node, .count));
        const fields = try fixture.fieldsOf(args.node);
        const fields_size = 3 * desc_format.Field.layout(fixture.descs.width).size;
        // One reference-counted slot, the string's, at memory position 2.
        try std.testing.expectEqual(@as(usize, fields_size + 8), fields.bytes.len);
        try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, fields.bytes[fields_size..][0..4], .little));
        try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, fields.bytes[fields_size + 4 ..][0..4], .little));
        // The leading words share the opaque word's descriptor.
        const opaque_node = (try fixture.descs.opaqueWord()).node;
        var opaque_words: u32 = 0;
        for (fields.relocations) |relocation| {
            if (@backingInt(relocation.target.data_symbol) == opaque_node) opaque_words += 1;
        }
        try std.testing.expectEqual(@as(u32, 2), opaque_words);
    }
}

test "type tables are reserved once per worker and inputs, then filled" {
    for ([_]base.target.TargetUsize{ .u32, .u64 }) |width| {
        var fixture: TestFixture = undefined;
        try fixture.init(width);
        defer fixture.deinit();
        const number = try fixture.store.primitive(.u64);
        const worker: WorkerId = @fromBackingInt(4);
        const inputs = [_]StaticDescs.CaptureWord{ .{ .desc = number }, .zero };
        const reserved = try fixture.descs.table(worker, &inputs);
        try std.testing.expectEqual(reserved, try fixture.descs.table(worker, &inputs));
        try std.testing.expect(!std.meta.eql(reserved, try fixture.descs.table(@fromBackingInt(5), &inputs)));
        const first = fixture.descs.nextUnfilledTable() orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(reserved, first.reserved);
        for (inputs, first.inputs) |expected, actual| try std.testing.expect(StaticDescs.captureWordsEqual(expected, actual));
        _ = fixture.descs.nextUnfilledTable() orelse return error.TestUnexpectedResult;
        try std.testing.expect(fixture.descs.nextUnfilledTable() == null);

        // A table reaching itself: its second word addresses it.
        const args = try fixture.descs.argsDescriptor(1, &.{number}, .none);
        try fixture.descs.fillTable(first, &.{ .{ .desc = number }, .zero, .{ .table = reserved.node }, .{ .node = args.node } });
        const filled = fixture.descs.exports.items[reserved.node];
        const ptr = width.size();
        try std.testing.expectEqual(@as(usize, 4 * ptr), filled.bytes.len);
        try std.testing.expectEqual(@as(usize, 3), filled.relocations.len);
        try std.testing.expectEqual(@as(u32, 0), filled.relocations[0].offset);
        try std.testing.expectEqual(fixture.descs.desc_slots.get(.{ .ty = number, .recursive_box = false }).?.node, @backingInt(filled.relocations[0].target.data_symbol));
        try std.testing.expectEqual(LIR.tableEntryOffset(2, ptr), filled.relocations[1].offset);
        try std.testing.expectEqual(reserved.node, @backingInt(filled.relocations[1].target.data_symbol));
        try std.testing.expectEqual(LIR.tableEntryOffset(3, ptr), filled.relocations[2].offset);
        try std.testing.expectEqual(args.node, @backingInt(filled.relocations[2].target.data_symbol));
    }
}

test "override scopes are interned by content and restricted to a type's reach" {
    var fixture: TestFixture = undefined;
    try fixture.init(.u64);
    defer fixture.deinit();
    const store = &fixture.store;
    const name = try store.internName("Local");
    const local = try store.intern(.{ .nominal = .{
        .decl = .{ .module = @fromBackingInt(0), .decl = @fromBackingInt(0) },
        .args = &.{},
        .name = name,
        .is_opaque = true,
        .builtin = null,
        .has_backing = false,
    } });
    const other = try store.intern(.{ .nominal = .{
        .decl = .{ .module = @fromBackingInt(0), .decl = @fromBackingInt(1) },
        .args = &.{},
        .name = name,
        .is_opaque = true,
        .builtin = null,
        .has_backing = false,
    } });
    const local_methods: StaticDescs.ScopedMethods = .{ .nominal = local, .methods = .{ null, null, .{ .callable = 9 } } };
    const other_methods: StaticDescs.ScopedMethods = .{ .nominal = other, .methods = .{ .{ .callable = 7 }, null, null } };
    const both = try fixture.descs.internScope(&.{ local_methods, other_methods });
    try std.testing.expectEqual(both, try fixture.descs.internScope(&.{ other_methods, local_methods }));
    try std.testing.expectEqual(StaticDescs.ScopeId.none, try fixture.descs.internScope(&.{}));

    const number = try store.primitive(.u64);
    const holds_local = try store.intern(.{ .list = try store.intern(.{ .tuple = &.{ number, local } }) });
    try std.testing.expectEqual(StaticDescs.ScopeId.none, try fixture.descs.restrictScope(both, number));
    try std.testing.expectEqual(try fixture.descs.internScope(&.{local_methods}), try fixture.descs.restrictScope(both, holds_local));
    const holds_both = try store.intern(.{ .tuple = &.{ other, holds_local } });
    try std.testing.expectEqual(both, try fixture.descs.restrictScope(both, holds_both));
    try std.testing.expect(std.meta.eql(local_methods.methods, fixture.descs.scopedMethods(both, local).?));
    try std.testing.expect(fixture.descs.scopedMethods(both, number) == null);
}
