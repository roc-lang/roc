//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");

const base = @import("../base.zig");
const collections = @import("../collections.zig");
const types = @import("./types.zig");

const exitOnOutOfMemory = collections.utils.exitOnOom;

const Allocator = std.mem.Allocator;
const Desc = types.Descriptor;
const Var = types.Var;
const Content = types.Content;
const Rank = types.Rank;
const Mark = types.Mark;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;

/// A variable & its descriptor info
pub const ResolvedVarDesc = struct { var_: Var, desc_idx: DescStore.Idx, desc: Desc };

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

/// Reperents either type data *or* a symlink to another type variable
pub const Slot = union(enum) {
    root: DescStore.Idx,
    redirect: Var,

    const ArrayList = std.ArrayListUnmanaged(Slot);
};

/// The store of all type variables and their descriptors
///
/// Each type variables (`Var`) points to a Slot.
/// A Slot either redirects to a different slot or contains type `Content`
///
/// Var maps to a SlotStore.Idx internally
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    /// Type variable storage
    slots: SlotStore,
    descs: DescStore,

    /// Storage for compound type parts
    tuple_elems: VarSafeList,
    func_args: VarSafeList,
    record_fields: RecordFieldSafeMultiList,
    tags: TagSafeMultiList,
    tag_args: VarSafeList,

    /// Init the unification table
    pub fn init(gpa: Allocator) Self {
        // TODO: eventually use herusitics here to determine sensible defaults
        return Self.initCapacity(gpa, 1024, 512);
    }

    /// Init the unification table
    pub fn initCapacity(gpa: Allocator, root_capacity: usize, child_capacity: usize) Self {
        return .{
            .gpa = gpa,

            // slots & descriptors
            .descs = DescStore.init(gpa, root_capacity),
            .slots = SlotStore.init(gpa, root_capacity),

            // everything else
            .tuple_elems = VarSafeList.initCapacity(gpa, child_capacity),
            .func_args = VarSafeList.initCapacity(gpa, child_capacity),
            .record_fields = RecordFieldSafeMultiList.initCapacity(gpa, child_capacity),
            .tags = TagSafeMultiList.initCapacity(gpa, child_capacity),
            .tag_args = VarSafeList.initCapacity(gpa, child_capacity),
        };
    }

    /// Ensure that slots & descriptor arrays have at least the provided capacity
    pub fn ensureTotalCapacity(self: *const Self, capacity: usize) Allocator.Error!void {
        try self.descs.backing.ensureTotalCapacity(self.gpa, capacity);
        try self.slots.backing.ensureTotalCapacity(self.gpa, capacity);
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        // slots & descriptors
        self.descs.deinit(self.gpa);
        self.slots.deinit(self.gpa);

        // everything else
        self.tuple_elems.deinit(self.gpa);
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

    /// Create a new variable with the provided desc
    /// Used in tests
    pub fn freshFromContent(self: *Self, content: Content) Var {
        const desc_idx = self.descs.insert(self.gpa, .{ .content = content, .rank = Rank.top_level, .mark = Mark.none });
        const slot_idx = self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a variable redirecting to the provided var
    /// Used in tests
    pub fn freshRedirect(self: *Self, var_: Var) Var {
        const slot_idx = self.slots.insert(self.gpa, .{ .redirect = var_ });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given descriptor
    pub fn register(self: *Self, desc: Desc) Var {
        const desc_idx = self.descs.insert(self.gpa, desc);
        const slot_idx = self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    // setting variables //

    pub const SetVarError = error{VarNotInitialized};

    /// Create a new unbound, flexible type variable without a name at the given idx
    ///
    /// This function may allocate if the provided index is greater than the
    /// current capacities of descs or slots.
    ///
    /// Used in canonicalization when creating type slots
    pub fn setVarFlex(self: *Self, target_var: Var) SetVarError!Var {
        return self.setVarContent(target_var, Content{ .flex_var = null });
    }

    /// Create a new variable with the provided desc at the given index
    ///
    /// This function may allocate if the provided index is greater than the
    /// current capacities of descs or slots.
    ///
    /// Used in canonicalization when creating type slots
    ///
    /// If the store size is less than the specified target_var, fill in slots
    /// thru that index
    pub fn setVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        try self.fillInSlotsThru(target_var);

        const resolved = self.resolveVar(target_var);
        var desc = resolved.desc;
        desc.content = content;
        self.descs.set(resolved.desc_idx, desc);
    }

    /// Create a new variable with the provided desc at the given index
    ///
    /// This function may allocate if the provided index is greater than the
    /// current capacities of descs or slots.
    ///
    /// Used in canonicalization when creating type slots
    ///
    /// If the store size is less than the specified target_var, fill in slots
    /// thru that index
    pub fn setVarRedirect(self: *Self, target_var: Var, redirect_to: Var) Allocator.Error!void {
        try self.fillInSlotsThru(target_var);
        const slot_idx = Self.varToSlotIdx(target_var);
        self.slots.set(slot_idx, .{ .redirect = redirect_to });
    }

    /// Given a target variable, check that the var is in bounds
    /// If it is, do nothing
    /// If it's not, then fill in the types store with flex vars for all missing
    /// intervening vars, *up to and including* the provided var
    pub fn fillInSlotsThru(self: *Self, target_var: Var) Allocator.Error!void {
        const idx = @intFromEnum(target_var);

        if (idx > self.descs.backing.len) {
            try self.descs.backing.ensureTotalCapacity(
                self.gpa,
                idx - self.descs.backing.len + 1,
            );
        }
        if (idx > self.slots.backing.items.len) {
            try self.slots.backing.ensureTotalCapacity(
                self.gpa,
                idx - self.slots.backing.items.len + 1,
            );
        }

        while (self.slots.backing.items.len <= idx) {
            const desc_idx = self.descs.insert(
                self.gpa,
                .{ .content = .{ .flex_var = null }, .rank = Rank.top_level, .mark = Mark.none },
            );
            _ = self.slots.insert(self.gpa, .{ .root = desc_idx });
        }
    }

    // make builtin types //

    pub fn mkBool(self: *Self, gpa: Allocator, idents: *base.Ident.Store, ext_var: Var) Content {
        const true_ident = idents.insert(gpa, base.Ident.for_text("True"), base.Region.zero());
        const false_ident = idents.insert(gpa, base.Ident.for_text("False"), base.Region.zero());

        const true_tag = self.mkTag(true_ident, &[_]Var{});
        const false_tag = self.mkTag(false_ident, &[_]Var{});
        return self.mkTagUnion(&[_]Tag{ true_tag, false_tag }, ext_var);
    }

    // make content types //

    /// Make a tag union data type
    /// Does not insert content into the types store
    pub fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) Content {
        const tags_range = self.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return Content{ .structure = .{ .tag_union = tag_union } };
    }

    /// Make a tag data type
    /// Does not insert content into the types store
    pub fn mkTag(self: *Self, name: base.Ident.Idx, args: []const Var) Tag {
        const args_range = self.appendTagArgs(args);
        return Tag{ .name = name, .args = args_range };
    }

    // Make a function data type with unbound effectfulness
    // Does not insert content into the types store.
    pub fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) Content {
        const args_range = self.appendFuncArgs(args);
        return Content{ .structure = .{ .fn_unbound = .{ .args = args_range, .ret = ret } } };
    }

    // Make a pure function data type (as opposed to an effectful or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncPure(self: *Self, args: []const Var, ret: Var) Content {
        const args_range = self.appendFuncArgs(args);
        return Content{ .structure = .{ .fn_pure = .{ .args = args_range, .ret = ret } } };
    }

    // Make an effectful function data type (as opposed to a pure or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) Content {
        const args_range = self.appendFuncArgs(args);
        return Content{ .structure = .{ .fn_effectful = .{ .args = args_range, .ret = ret } } };
    }

    // sub list setters //

    /// Append a tuple elem to the backing list, returning the idx
    pub fn appendTupleElem(self: *Self, v: Var) VarSafeList.Idx {
        return self.tuple_elems.append(self.gpa, v);
    }

    /// Append a slice of tuple elems to the backing list, returning the range
    pub fn appendTupleElems(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.tuple_elems.appendSlice(self.gpa, slice);
    }

    /// Append a slice of func args to the backing list, returning the range
    pub fn appendFuncArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.func_args.appendSlice(self.gpa, slice);
    }

    /// Append a record field to the backing list, returning the idx
    pub fn appendRecordField(self: *Self, field: RecordField) RecordFieldSafeMultiList.Idx {
        return self.record_fields.append(self.gpa, field);
    }

    /// Append a slice of record fields to the backing list, returning the range
    pub fn appendRecordFields(self: *Self, slice: []const RecordField) RecordFieldSafeMultiList.Range {
        return self.record_fields.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tags to the backing list, returning the range
    pub fn appendTags(self: *Self, slice: []const Tag) TagSafeMultiList.Range {
        return self.tags.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tag args to the backing list, returning the range
    pub fn appendTagArgs(self: *Self, slice: []const Var) VarSafeList.Range {
        return self.tag_args.appendSlice(self.gpa, slice);
    }

    // sub list getters //

    /// Given a range, get a slice of tuple from the backing list
    pub fn getTupleElemsSlice(self: *const Self, range: VarSafeList.Range) VarSafeList.Slice {
        return self.tuple_elems.rangeToSlice(range);
    }

    /// Given a range, get a slice of func from the backing list
    pub fn getFuncArgsSlice(self: *const Self, range: VarSafeList.Range) VarSafeList.Slice {
        return self.func_args.rangeToSlice(range);
    }

    /// Given a range, get a slice of record fields from the backing array
    pub fn getRecordFieldsSlice(self: *const Self, range: RecordFieldSafeMultiList.Range) RecordFieldSafeMultiList.Slice {
        return self.record_fields.rangeToSlice(range);
    }

    /// Given a range, get a slice of tags from the backing array
    pub fn getTagsSlice(self: *const Self, range: TagSafeMultiList.Range) TagSafeMultiList.Slice {
        return self.tags.rangeToSlice(range);
    }

    /// Given a range, get a slice of tag args from the backing list
    pub fn getTagArgsSlice(self: *const Self, range: VarSafeList.Range) VarSafeList.Slice {
        return self.tag_args.rangeToSlice(range);
    }

    // mark & rank //

    /// Set the mark for a descriptor
    pub fn setDescMark(self: *Self, desc_idx: DescStore.Idx, mark: Mark) void {
        var desc = self.descs.get(desc_idx);
        desc.mark = mark;
        self.descs.set(desc_idx, desc);
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
                .root => |desc_idx| {
                    const redirected_root_var = Self.slotIdxToVar(redirected_slot_idx);
                    const desc = self.descs.get(desc_idx);
                    return .{
                        .var_ = redirected_root_var,
                        .desc_idx = desc_idx,
                        .desc = desc,
                    };
                },
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

    // equivalence //

    /// The result of checking for equivalence
    pub const VarEquivResult = union(enum) { equiv, not_equiv: ResolvedVarDescs };

    /// Check if two variables are equivalent
    /// This will follow all redirects and compress the path
    ///
    /// If the vars are *not equivalent, then return the resolved vars & descs
    pub fn checkVarsEquiv(self: *Self, a_var: Var, b_var: Var) VarEquivResult {
        // Ensure both variables are in bounds before resolving
        self.fillInSlotsThru(a_var) catch unreachable;
        self.fillInSlotsThru(b_var) catch unreachable;

        const a = self.resolveVarAndCompressPath(a_var);
        const b = self.resolveVarAndCompressPath(b_var);
        if (a.desc_idx == b.desc_idx) {
            return .equiv;
        } else {
            return .{ .not_equiv = .{ .a = a, .b = b } };
        }
    }

    /// Ensure that all slots required for an alias or nominal type are allocated.
    /// This includes:
    /// - The type variable itself
    /// - The backing variable at +1
    /// - All argument variables starting at +2
    pub fn ensureAliasSlots(self: *Self, alias_var: Var, num_args: u32) Allocator.Error!void {
        // The highest index we need is alias_var + 1 + num_args
        // (alias var, backing var, then each arg)
        const max_idx = @intFromEnum(alias_var) + 1 + num_args;
        try self.fillInSlotsThru(@enumFromInt(max_idx));
    }

    // union //

    /// Link the variables & updated the content in the unification table
    /// * update b to to the new desc value
    /// * redirect a -> b
    ///
    // NOTE: The elm & the roc compiler this step differently
    // * The elm compiler sets b to redirect to a
    // * The roc compiler sets a to redirect to b
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

    const Error = error{VarNotRoot};

    /// Set a root var to be the specified content
    /// Used in tests
    pub fn setRootVarContent(self: *Self, var_: Var, content: Content) error{VarNotRoot}!void {
        const slot = self.slots.get(Self.varToSlotIdx(var_));
        switch (slot) {
            .root => |desc_idx| {
                var desc = self.descs.get(desc_idx);
                desc.content = content;
                self.descs.set(desc_idx, desc);
            },
            .redirect => {
                return error.VarNotRoot;
            },
        }
    }

    // helpers //

    pub fn varToSlotIdx(var_: Var) SlotStore.Idx {
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

    fn init(gpa: Allocator, capacity: usize) Self {
        const arr_list = Slot.ArrayList.initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
        return .{ .backing = arr_list };
    }

    fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Insert a new slot into the store
    fn insert(self: *Self, gpa: Allocator, typ: Slot) Idx {
        const idx: Idx = @enumFromInt(self.backing.items.len);
        self.backing.append(gpa, typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Insert a value into the store
    fn appendAssumeCapacity(self: *Self, gpa: Allocator, typ: Desc) Idx {
        const idx: Idx = @enumFromInt(self.backing.len);
        self.backing.appendAssumeCapacity(gpa, typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    pub fn set(self: *Self, idx: Idx, val: Slot) void {
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

    backing: std.MultiArrayList(Desc),

    /// Init & allocated memory
    fn init(gpa: Allocator, capacity: usize) Self {
        var arr = std.MultiArrayList(Desc){};
        arr.ensureUnusedCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
        return .{ .backing = arr };
    }

    /// Deinit & free allocated memory
    fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Insert a value into the store
    fn insert(self: *Self, gpa: Allocator, typ: Desc) Idx {
        const idx: Idx = @enumFromInt(self.backing.len);
        self.backing.append(gpa, typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Idx, val: Desc) void {
        self.backing.set(@intFromEnum(idx), val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Desc {
        return self.backing.get(@intFromEnum(idx));
    }

    /// A type-safe index into the store
    /// This type is made public below
    const Idx = enum(u32) { _ };
};

/// An index into the desc store
pub const DescStoreIdx = DescStore.Idx;

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

    const num_flex = store.fresh();
    const requirements = types.Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = 0,
    };
    const num = types.Content{ .structure = .{ .num = .{ .num_poly = .{ .var_ = num_flex, .requirements = requirements } } } };
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

    const num_flex = store.fresh();
    const requirements = types.Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = 0,
    };
    const num = types.Content{ .structure = .{ .num = .{ .num_poly = .{ .var_ = num_flex, .requirements = requirements } } } };
    const c = store.freshFromContent(num);
    const b = store.freshRedirect(c);
    const a = store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(num, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}
