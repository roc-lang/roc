//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");

const collections = @import("../collections.zig");
const types = @import("./types.zig");

const exitOnOutOfMemory = collections.utils.exitOnOom;

const Desc = types.Descriptor;
const Var = types.Var;
const VarSafeList = types.VarSafeList;
const Content = types.Content;
const Rank = types.Rank;
const RecordField = types.RecordField;
const Tag = types.Tag;

const RecordFieldSafeList = types.RecordFieldSafeList;
const TagSafeList = types.TagSafeList;

/// A variable & it's descriptor info
pub const ResolvedVarDesc = struct { var_: Var, desc_idx: DescStore.Idx, desc: Desc };

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

// Reperents either type data *or* a symlink to another type variable
pub const Slot = union(enum) {
    root: DescStore.Idx,
    redirect: Var,

    const ArrayList = std.ArrayList(Slot);
};

/// The store of all type variables and their descriptors
///
/// Each type variables (`Var`) points to a Slot.
/// A Slot either redirects to a different slot or contains type `Content`
///
/// Var maps to a SlotStore.Idx internally
pub const Store = struct {
    const Self = @This();

    gpa: std.mem.Allocator,

    // Slots & descriptors
    slots: SlotStore,
    descs: DescStore,

    // Small strings
    flex_rigid_var_names: collections.SmallStringInterner,
    record_field_names: collections.SmallStringInterner,
    tag_names: collections.SmallStringInterner,

    // Everything else
    alias_args: VarSafeList,
    tuple_elems: VarSafeList,
    type_apply_args: VarSafeList,
    func_args: VarSafeList,
    record_fields: RecordFieldSafeList,
    tags: TagSafeList,
    tag_args: VarSafeList,

    /// Init the unification table
    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: eventually use herusitics here to determin sensible defaults
        return .{
            .gpa = gpa,

            // slots & descriptors
            .descs = DescStore.init(gpa, 64),
            .slots = SlotStore.init(gpa, 64),

            // strs
            .flex_rigid_var_names = collections.SmallStringInterner.initCapacity(gpa, 64),
            .record_field_names = collections.SmallStringInterner.initCapacity(gpa, 64),
            .tag_names = collections.SmallStringInterner.initCapacity(gpa, 64),

            // everything else
            .alias_args = VarSafeList.initCapacity(gpa, 64),
            .tuple_elems = VarSafeList.initCapacity(gpa, 64),
            .type_apply_args = VarSafeList.initCapacity(gpa, 64),
            .func_args = VarSafeList.initCapacity(gpa, 64),
            .record_fields = RecordFieldSafeList.initCapacity(gpa, 64),
            .tags = TagSafeList.initCapacity(gpa, 64),
            .tag_args = VarSafeList.initCapacity(gpa, 64),
        };
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        // slots & descriptors
        self.descs.deinit();
        self.slots.deinit();

        // strs
        self.flex_rigid_var_names.deinit(self.gpa);
        self.record_field_names.deinit(self.gpa);
        self.tag_names.deinit(self.gpa);

        // everything else
        self.alias_args.deinit(self.gpa);
        self.tuple_elems.deinit(self.gpa);
        self.type_apply_args.deinit(self.gpa);
        self.func_args.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.tag_args.deinit(self.gpa);
    }

    // fresh variables //

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn fresh(self: *Self) Var {
        return self.freshFromContent(Content{ .flex_var = null });
    }

    /// Create a new variable with the given descriptor
    pub fn register(self: *Self, desc: Desc) Var {
        const desc_idx = self.descs.insert(desc);
        const slot_idx = self.slots.insert(.{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the provided desc at the top level
    /// Used in tests
    pub fn freshFromContent(self: *Self, content: Content) Var {
        const desc_idx = self.descs.insert(.{ .content = content, .rank = Rank.top_level });
        const slot_idx = self.slots.insert(.{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a variable redirecting to the provided var
    /// Used in tests
    pub fn freshRedirect(self: *Self, var_: Var) Var {
        const slot_idx = self.slots.insert(.{ .redirect = var_ });
        return Self.slotIdxToVar(slot_idx);
    }

    // sub list setters //

    /// Append a slice of alias args to the backing list, returning the range
    pub fn appendAliasArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.alias_args.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tuple elems to the backing list, returning the range
    pub fn appendTupleElems(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.tuple_elems.appendSlice(self.gpa, slice);
    }

    /// Append a slice of type apply args to the backing list, returning the range
    pub fn appendTypeApplyArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.type_apply_args.appendSlice(self.gpa, slice);
    }

    /// Append a slice of func args to the backing list, returning the range
    pub fn appendFuncArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.func_args.appendSlice(self.gpa, slice);
    }

    /// Append a slice of record fields to the backing list, returning the range
    pub fn appendRecordFields(self: *Self, slice: []const RecordField) RecordFieldSafeList.Range {
        return self.record_fields.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tags to the backing list, returning the range
    pub fn appendTags(self: *Self, slice: []const Tag) TagSafeList.Range {
        return self.tags.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tag args to the backing list, returning the range
    pub fn appendTagArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.tag_args.appendSlice(self.gpa, slice);
    }

    // sub list getters //

    /// Given a range, get a slice of record fields from the backing array
    pub fn getRecordFieldsSlice(self: *Self, range: RecordFieldSafeList.Range) []const RecordField {
        return self.record_fields.rangeToSlice(range);
    }

    // resolvers //

    /// Given a type var, follow all redirects until finding the root descriptor
    ///
    /// Will mutate the DescStore in place to compress the path
    pub fn resolveVarAndCompressPath(self: *Self, initial_var: Var) ResolvedVarDesc {
        // Resolve the variable
        const redirected = self.resolveVar(initial_var);
        const redirected_root_var = redirected.var_;

        // then follow the chain again, but compressing each to the root
        if (initial_var != redirected_root_var) {
            var compressed_slot_idx = Self.varToSlotIdx(initial_var);
            var compressed_slot: Slot = self.slots.get(compressed_slot_idx);
            while (true) {
                switch (compressed_slot) {
                    .redirect => |next_redirect_var| {
                        self.slots.set(compressed_slot_idx, Slot{ .redirect = redirected_root_var });
                        compressed_slot_idx = Self.varToSlotIdx(next_redirect_var);
                        compressed_slot = self.slots.get(compressed_slot_idx);
                    },
                    .root => |_| break,
                }
            }
        }

        // Compress the path
        return redirected;
    }

    /// Given a type var, follow all redirects until finding the root descriptor
    pub fn resolveVar(self: *const Self, initial_var: Var) ResolvedVarDesc {
        var redirected_slot_idx = Self.varToSlotIdx(initial_var);
        var redirected_slot: Slot = self.slots.get(redirected_slot_idx);
        while (true) {
            switch (redirected_slot) {
                .redirect => |next_redirect_var| {
                    redirected_slot_idx = Self.varToSlotIdx(next_redirect_var);
                    redirected_slot = self.slots.get(redirected_slot_idx);
                },
                .root => |_| break,
            }
        }
        const redirected_root_var = Self.slotIdxToVar(redirected_slot_idx);

        // TODO: refactor to remove panic?
        switch (redirected_slot) {
            .redirect => |_| @panic("redirected slot was still redirect after following chain"),
            .root => |desc_idx| {
                const desc = self.descs.get(desc_idx);
                return .{
                    .var_ = redirected_root_var,
                    .desc_idx = desc_idx,
                    .desc = desc,
                };
            },
        }
    }

    // equivalance //

    /// The result of checking for equivalance
    pub const VarEquivResult = union(enum) { equiv, not_equiv: ResolvedVarDescs };

    /// Check if two variables are equivalant
    /// This will follow all redirects and compress the path
    ///
    /// If the vars are *not equivalant, then return the resolved vars & descs
    pub fn checkVarsEquiv(self: *Self, a_var: Var, b_var: Var) VarEquivResult {
        const a = self.resolveVarAndCompressPath(a_var);
        const b = self.resolveVarAndCompressPath(b_var);
        if (a.desc_idx == b.desc_idx) {
            return .equiv;
        } else {
            return .{ .not_equiv = .{ .a = a, .b = b } };
        }
    }

    // union //

    /// Link the variables & updated the content in the unification table
    /// * update b to to the new desc value
    /// * redirect a -> b
    ///
    // NOTE: The elm & the roc compiler this step differently
    // * The elm compiler sets b to redirect to a
    // * The roc compiler sets a to redirect to b (based on the `ena` compiler
    // See the `union` function in subs.rs for details
    pub fn union_(self: *Self, a_var: Var, b_var: Var, new_desc: Desc) void {
        const b_data = self.resolveVarAndCompressPath(b_var);

        // Update b to be the new desc
        self.descs.set(b_data.desc_idx, new_desc);

        // Update a to point to b
        self.slots.set(Self.varToSlotIdx(a_var), .{ .redirect = b_var });
    }

    // test helpers //

    /// Get the slot for the provided var
    /// Used in tests
    /// If you're reaching for this in non-test code, you probably want
    /// resolveVar or resolveVarAndCompressPath instead
    pub fn getSlot(self: *Self, var_: Var) Slot {
        return self.slots.get(Self.varToSlotIdx(var_));
    }

    /// Get the descriptor for the provided idx
    /// Used in tests
    pub fn getDesc(self: *Self, desc_idx: DescStore.Idx) Desc {
        return self.descs.get(desc_idx);
    }

    // helpers //

    fn varToSlotIdx(var_: Var) SlotStore.Idx {
        return @enumFromInt(@intFromEnum(var_));
    }

    fn slotIdxToVar(slot_idx: SlotStore.Idx) Var {
        return @enumFromInt(@intFromEnum(slot_idx));
    }
};

/// Represents a store of slots
const SlotStore = struct {
    const Self = @This();

    backing: Slot.ArrayList,

    fn init(gpa: std.mem.Allocator, capacity: usize) Self {
        const arr_list = Slot.ArrayList.initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
        return .{ .backing = arr_list };
    }

    fn deinit(self: *Self) void {
        self.backing.deinit();
    }

    /// Insert a new slot into the store
    fn insert(self: *Self, typ: Slot) Idx {
        const idx: Idx = @enumFromInt(self.backing.items.len);
        self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Idx, val: Slot) void {
        self.backing.items[@intFromEnum(idx)] = val;
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Slot {
        return self.backing.items[@intFromEnum(idx)];
    }

    /// A type-safe index into the store
    const Idx = enum(u32) { _ };
};

/// Represents a store of descriptors
///
/// Indexes into the list are typesafe
const DescStore = struct {
    const Self = @This();

    backing: std.ArrayList(Desc),

    /// Init & allocated memory
    fn init(gpa: std.mem.Allocator, capacity: usize) Self {
        const arr_list = std.ArrayList(Desc).initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
        return .{ .backing = arr_list };
    }

    /// Deinit & free allocated memory
    fn deinit(self: *Self) void {
        self.backing.deinit();
    }

    /// Insert a value into the store
    fn insert(self: *Self, typ: Desc) Idx {
        const idx: Idx = @enumFromInt(self.backing.items.len);
        self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Idx, val: Desc) void {
        self.backing.items[@intFromEnum(idx)] = val;
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Desc {
        return self.backing.items[@intFromEnum(idx)];
    }

    /// A type-safe index into the store
    const Idx = enum(u32) { _ };
};

// path compression

test "resolveVarAndCompressPath - flattens redirect chain to flex_var" {
    const gpa = std.testing.allocator;

    var store = Store.init(gpa);
    defer store.deinit();

    const c = store.fresh();
    const b = store.freshRedirect(c);
    const a = store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(Content{ .flex_var = null }, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}

test "resolveVarAndCompressPath - no-op on already root" {
    const gpa = std.testing.allocator;

    var store = Store.init(gpa);
    defer store.deinit();

    const num = types.Content{ .structure = types.num_flex_var };
    const num_var = store.freshFromContent(num);

    const result = store.resolveVarAndCompressPath(num_var);

    try std.testing.expectEqual(num, result.desc.content);
    try std.testing.expectEqual(num_var, result.var_);
    // try std.testing.expectEqual(store.getSlot(num_var), Slot{ .root = num_desc_idx });
}

test "resolveVarAndCompressPath - flattens redirect chain to structure" {
    const gpa = std.testing.allocator;

    var store = Store.init(gpa);
    defer store.deinit();

    const num = types.Content{ .structure = types.num_flex_var };
    const c = store.freshFromContent(num);
    const b = store.freshRedirect(c);
    const a = store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(num, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}
