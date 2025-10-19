//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const serialization = @import("serialization");

const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Desc = types.Descriptor;
const Var = types.Var;
const Content = types.Content;
const Rank = types.Rank;
const Mark = types.Mark;
const Flex = types.Flex;
const Rigid = types.Rigid;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;
const Descriptor = types.Descriptor;
const TypeIdent = types.TypeIdent;
const Alias = types.Alias;
const FlatType = types.FlatType;
const NominalType = types.NominalType;
const Record = types.Record;
const Num = types.Num;
const StaticDispatchConstraint = types.StaticDispatchConstraint;

const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

/// A variable & its descriptor info
pub const ResolvedVarDesc = struct { var_: Var, desc_idx: DescStore.Idx, desc: Desc };

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

/// Reperents either type data *or* a symlink to another type variable
pub const Slot = union(enum) {
    root: DescStore.Idx,
    redirect: Var,

    /// Calculate the size needed to serialize this Slot
    pub fn serializedSize(self: *const Slot) usize {
        _ = self;
        return @sizeOf(u8) + @sizeOf(u32); // tag + data
    }

    /// Deserialize a Slot from the provided buffer
    pub fn deserializeFrom(buffer: []const u8) !Slot {
        if (buffer.len < @sizeOf(u8) + @sizeOf(u32)) return error.BufferTooSmall;

        const tag = buffer[0];
        const data = std.mem.readInt(u32, buffer[1..5], .little);

        switch (tag) {
            0 => return Slot{ .root = @enumFromInt(data) },
            1 => return Slot{ .redirect = @enumFromInt(data) },
            else => return error.InvalidTag,
        }
    }
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
    vars: VarSafeList,
    record_fields: RecordFieldSafeMultiList,
    tags: TagSafeMultiList,
    static_dispatch_constraints: StaticDispatchConstraint.SafeList,

    /// Init the unification table
    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        // TODO: eventually use herusitics here to determine sensible defaults
        return try Self.initCapacity(gpa, 1024, 512);
    }

    /// Init the unification table
    pub fn initCapacity(gpa: Allocator, root_capacity: usize, child_capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,

            // slots & descriptors
            .descs = try DescStore.init(gpa, root_capacity),
            .slots = try SlotStore.init(gpa, root_capacity),

            // everything else
            .vars = try VarSafeList.initCapacity(gpa, child_capacity),
            .record_fields = try RecordFieldSafeMultiList.initCapacity(gpa, child_capacity),
            .tags = try TagSafeMultiList.initCapacity(gpa, child_capacity),
            .static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, child_capacity),
        };
    }

    /// Ensure that slots & descriptor arrays have at least the provided capacity
    pub fn ensureTotalCapacity(self: *Self, capacity: usize) Allocator.Error!void {
        try self.descs.backing.ensureTotalCapacity(self.gpa, capacity);
        try self.slots.backing.items.ensureTotalCapacity(self.gpa, capacity);
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        // slots & descriptors
        self.descs.deinit(self.gpa);
        self.slots.deinit(self.gpa);

        // everything else
        self.vars.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.static_dispatch_constraints.deinit(self.gpa);
    }

    /// Return the number of type variables in the store.
    pub fn len(self: *const Self) u64 {
        return self.slots.backing.len();
    }

    // fresh variables //

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn fresh(self: *Self) std.mem.Allocator.Error!Var {
        return try self.freshFromContent(Content{ .flex = Flex.init() });
    }

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn freshWithRank(self: *Self, rank: Rank) std.mem.Allocator.Error!Var {
        return try self.freshFromContentWithRank(Content{ .flex = Flex.init() }, rank);
    }

    /// Create a new variable with the provided desc
    /// Used in tests
    pub fn freshFromContent(self: *Self, content: Content) std.mem.Allocator.Error!Var {
        const desc_idx = try self.descs.insert(self.gpa, .{ .content = content, .rank = Rank.top_level, .mark = Mark.none });
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given content and rank
    pub fn freshFromContentWithRank(self: *Self, content: Content, rank: Rank) std.mem.Allocator.Error!Var {
        const desc_idx = try self.descs.insert(self.gpa, .{ .content = content, .rank = rank, .mark = Mark.none });
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a variable redirecting to the provided var
    /// Used in tests
    pub fn freshRedirect(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        const slot_idx = try self.slots.insert(self.gpa, .{ .redirect = var_ });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given descriptor
    pub fn register(self: *Self, desc: Desc) std.mem.Allocator.Error!Var {
        const desc_idx = try self.descs.insert(self.gpa, desc);
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Check if a variable is a rediret
    pub fn isRedirect(self: *const Self, var_: Var) bool {
        switch (self.slots.get(Self.varToSlotIdx(var_))) {
            .redirect => return true,
            .root => return false,
        }
    }

    // setting variables //

    /// Set a type variable to the provided content
    pub fn setVarDesc(self: *Self, target_var: Var, desc: Desc) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        const resolved = self.resolveVar(target_var);
        self.descs.set(resolved.desc_idx, desc);
    }

    /// Set a type variable to the provided content
    pub fn setVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        const resolved = self.resolveVar(target_var);
        var desc = resolved.desc;
        desc.content = content;
        self.descs.set(resolved.desc_idx, desc);
    }

    /// Set a type variable to redirect to the provided redirect
    pub fn setVarRedirect(self: *Self, target_var: Var, redirect_to: Var) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        std.debug.assert(@intFromEnum(redirect_to) < self.len());
        const slot_idx = Self.varToSlotIdx(target_var);
        self.slots.set(slot_idx, .{ .redirect = redirect_to });
    }

    // make builtin types //

    pub fn mkBool(self: *Self, gpa: Allocator, idents: *base.Ident.Store, ext_var: Var) std.mem.Allocator.Error!Content {
        // TODO: Hardcode idents once in store, do no create fn anno
        const false_ident = try idents.insert(gpa, base.Ident.for_text("False"));
        const true_ident = try idents.insert(gpa, base.Ident.for_text("True"));

        const false_tag = try self.mkTag(false_ident, &[_]Var{});
        const true_tag = try self.mkTag(true_ident, &[_]Var{});
        return try self.mkTagUnion(&[_]Tag{ false_tag, true_tag }, ext_var);
    }

    pub fn mkResult(
        self: *Self,
        gpa: Allocator,
        idents: *base.Ident.Store,
        ok_var: Var,
        err_var: Var,
        ext_var: Var,
    ) std.mem.Allocator.Error!Content {
        // TODO: Hardcode idents once in store, do no create every fn call
        const true_ident = try idents.insert(gpa, base.Ident.for_text("Ok"));
        const false_ident = try idents.insert(gpa, base.Ident.for_text("Err"));

        const ok_tag = try self.mkTag(true_ident, &[_]Var{ok_var});
        const err_tag = try self.mkTag(false_ident, &[_]Var{err_var});
        return try self.mkTagUnion(&[_]Tag{ ok_tag, err_tag }, ext_var);
    }

    // make content types //

    /// Make a tag union data type
    /// Does not insert content into the types store
    pub fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!Content {
        const tags_range = try self.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return Content{ .structure = .{ .tag_union = tag_union } };
    }

    /// Make a tag data type
    /// Does not insert content into the types store
    pub fn mkTag(self: *Self, name: base.Ident.Idx, args: []const Var) std.mem.Allocator.Error!Tag {
        const args_range = try self.appendVars(args);
        return Tag{ .name = name, .args = args_range };
    }

    /// Make alias data type
    /// Does not insert content into the types store
    pub fn mkAlias(self: *Self, ident: TypeIdent, backing_var: Var, args: []const Var) std.mem.Allocator.Error!Content {
        const backing_idx = try self.appendVar(backing_var);
        var span = try self.appendVars(args);

        // Adjust args span to include backing  var
        span.start = backing_idx;
        span.count = span.count + 1;

        return Content{
            .alias = Alias{
                .ident = ident,
                .vars = .{ .nonempty = span },
            },
        };
    }

    /// Make nominal data type
    /// Does not insert content into the types store
    pub fn mkNominal(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.Ident.Idx,
    ) std.mem.Allocator.Error!Content {
        const backing_idx = try self.appendVar(backing_var);
        var span = try self.appendVars(args);

        // Adjust args span to include backing  var
        span.start = backing_idx;
        span.count = span.count + 1;

        return Content{ .structure = FlatType{
            .nominal_type = NominalType{
                .ident = ident,
                .vars = .{ .nonempty = span },
                .origin_module = origin_module,
            },
        } };
    }

    // Make a function data type with unbound effectfulness
    // Does not insert content into the types store.
    pub fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_unbound = .{
            .args = args_range,
            .ret = ret,
            .needs_instantiation = needs_inst,
        } } };
    }

    // Make a pure function data type (as opposed to an effectful or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_pure = .{ .args = args_range, .ret = ret, .needs_instantiation = needs_inst } } };
    }

    // Make an effectful function data type (as opposed to a pure or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_effectful = .{ .args = args_range, .ret = ret, .needs_instantiation = needs_inst } } };
    }

    // Helper to check if a type variable needs instantiation
    pub fn needsInstantiation(self: *const Self, var_: Var) bool {
        const resolved = self.resolveVar(var_);

        // Generalized variables (rank 0) always need instantiation
        if (resolved.desc.rank == Rank.generalized) {
            return true;
        }

        return self.needsInstantiationContent(resolved.desc.content);
    }

    pub fn needsInstantiationContent(self: *const Self, content: Content) bool {
        return switch (content) {
            .flex => true, // Flexible variables need instantiation
            .rigid => true, // Rigid variables need instantiation when used outside their defining scope
            .alias => true, // Aliases may contain type variables, so assume they need instantiation
            .structure => |flat_type| self.needsInstantiationFlatType(flat_type),
            .err => false,
        };
    }

    pub fn needsInstantiationFlatType(self: *const Self, flat_type: FlatType) bool {
        return switch (flat_type) {
            .str => false,
            .box => |box_var| self.needsInstantiation(box_var),
            .list => |list_var| self.needsInstantiation(list_var),
            .list_unbound => true,
            .tuple => |tuple| blk: {
                const elems_slice = self.sliceVars(tuple.elems);
                for (elems_slice) |elem_var| {
                    if (self.needsInstantiation(elem_var)) break :blk true;
                }
                break :blk false;
            },
            .num => |num| switch (num) {
                .num_poly => |poly_var| self.needsInstantiation(poly_var),
                .num_unbound => true,
                .int_poly => |poly_var| self.needsInstantiation(poly_var),
                .int_unbound => true,
                .frac_poly => |poly_var| self.needsInstantiation(poly_var),
                .frac_unbound => true,
                else => false, // Concrete numeric types don't need instantiation
            },
            .nominal_type => false, // Nominal types are concrete
            .fn_pure => |func| func.needs_instantiation,
            .fn_effectful => |func| func.needs_instantiation,
            .fn_unbound => |func| func.needs_instantiation,
            .record => |record| self.needsInstantiationRecord(record),
            .record_unbound => |fields| self.needsInstantiationRecordFields(fields),
            .empty_record => false,
            .tag_union => |tag_union| self.needsInstantiationTagUnion(tag_union),
            .empty_tag_union => false,
        };
    }

    pub fn needsInstantiationRecord(self: *const Self, record: Record) bool {
        const fields_slice = self.getRecordFieldsSlice(record.fields);
        for (fields_slice.items(.var_)) |type_var| {
            if (self.needsInstantiation(type_var)) return true;
        }
        return self.needsInstantiation(record.ext);
    }

    pub fn needsInstantiationRecordFields(self: *const Self, fields: RecordField.SafeMultiList.Range) bool {
        const fields_slice = self.getRecordFieldsSlice(fields);
        for (fields_slice.items(.var_)) |type_var| {
            if (self.needsInstantiation(type_var)) return true;
        }
        return false;
    }

    pub fn needsInstantiationTagUnion(self: *const Self, tag_union: TagUnion) bool {
        const tags_slice = self.getTagsSlice(tag_union.tags);
        for (tags_slice.items(.args)) |tag_args| {
            const args_slice = self.sliceVars(tag_args);
            for (args_slice) |arg_var| {
                if (self.needsInstantiation(arg_var)) return true;
            }
        }
        return self.needsInstantiation(tag_union.ext);
    }

    // sub list setters //

    /// Append a var to the backing list, returning the idx
    pub fn appendVar(self: *Self, v: Var) std.mem.Allocator.Error!VarSafeList.Idx {
        return try self.vars.append(self.gpa, v);
    }

    /// Append a var to the backing list, returning the idx
    pub fn appendVars(self: *Self, s: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        return try self.vars.appendSlice(self.gpa, s);
    }

    /// Append a record field to the backing list, returning the idx
    pub fn appendRecordField(self: *Self, field: RecordField) std.mem.Allocator.Error!RecordFieldSafeMultiList.Idx {
        return try self.record_fields.append(self.gpa, field);
    }

    /// Append a slice of record fields to the backing list, returning the range
    pub fn appendRecordFields(self: *Self, slice: []const RecordField) std.mem.Allocator.Error!RecordFieldSafeMultiList.Range {
        return try self.record_fields.appendSlice(self.gpa, slice);
    }

    /// Append a tag to the backing list, returning the idx
    pub fn appendTag(self: *Self, tag: Tag) Allocator.Error!TagSafeMultiList.Idx {
        return try self.tags.append(self.gpa, tag);
    }

    /// Append a slice of tags to the backing list, returning the range
    pub fn appendTags(self: *Self, slice: []const Tag) std.mem.Allocator.Error!TagSafeMultiList.Range {
        return try self.tags.appendSlice(self.gpa, slice);
    }

    /// Append static dispatch constraints to the backing list, returning the range
    pub fn appendStaticDispatchConstraints(self: *Self, s: []const StaticDispatchConstraint) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        return try self.static_dispatch_constraints.appendSlice(self.gpa, s);
    }

    // sub list getters //

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceVars(self: *const Self, range: VarSafeList.Range) []Var {
        return self.vars.sliceRange(range);
    }

    /// Given a range, get a slice of record fields from the backing array
    pub fn getRecordFieldsSlice(self: *const Self, range: RecordFieldSafeMultiList.Range) RecordFieldSafeMultiList.Slice {
        return self.record_fields.sliceRange(range);
    }

    /// Given a range, get a slice of tags from the backing array
    pub fn getTagsSlice(self: *const Self, range: TagSafeMultiList.Range) TagSafeMultiList.Slice {
        return self.tags.sliceRange(range);
    }

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceStaticDispatchConstraints(self: *const Self, range: StaticDispatchConstraint.SafeList.Range) []StaticDispatchConstraint {
        return self.static_dispatch_constraints.sliceRange(range);
    }

    // helpers - alias types //

    // Alias types contain a span of variables. In this span, the 1st element
    // is the backing variable, and the remainder are the arguments

    /// Get the backing var for this alias type
    pub fn getAliasBackingVar(self: *const Self, alias: Alias) Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        return self.vars.get(alias.vars.nonempty.start).*;
    }

    /// Get the arg vars for this alias type
    pub fn sliceAliasArgs(self: *const Self, alias: Alias) []Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        const slice = self.vars.sliceRange(alias.vars.nonempty);
        return slice[1..];
    }

    /// Get the an iterator arg vars for this alias type
    pub fn iterAliasArgs(self: *const Self, alias: Alias) VarSafeList.Iterator {
        std.debug.assert(alias.vars.nonempty.count > 0);
        var span = alias.vars.nonempty;
        span.dropFirstElem();
        return self.vars.iterRange(span);
    }

    // helpers - nominal types //

    // Nominal types contain a span of variables. In this span, the 1st element
    // is the backing variable, and the remainder are the arguments

    /// Get the backing var for this nominal type
    pub fn getNominalBackingVar(self: *const Self, nominal: NominalType) Var {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        return self.vars.get(nominal.vars.nonempty.start).*;
    }

    /// Get the arg vars for this nominal type
    pub fn sliceNominalArgs(self: *const Self, nominal: NominalType) []Var {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        const slice = self.vars.sliceRange(nominal.vars.nonempty);
        return slice[1..];
    }

    /// Get the an iterator arg vars for this nominal type
    pub fn iterNominalArgs(self: *const Self, nominal: NominalType) VarSafeList.Iterator {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        var span = nominal.vars.nonempty;
        span.dropFirstElem();
        return self.vars.iterRange(span);
    }

    // mark & rank //

    /// Set the mark for a descriptor
    pub fn setDescMark(self: *Self, desc_idx: DescStore.Idx, mark: Mark) void {
        var desc = self.descs.get(desc_idx);
        desc.mark = mark;
        self.descs.set(desc_idx, desc);
    }

    /// Set the rank for a descriptor
    pub fn setDescRank(self: *Self, desc_idx: DescStore.Idx, rank: Rank) void {
        var desc = self.descs.get(desc_idx);
        desc.rank = rank;
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
    /// CRITICAL: The merge direction (a -> b) is load-bearing and must not be changed!
    /// Multiple parts of the unification algorithm depend on this specific order:
    /// - When unifying aliases with structures, we rely on this order to ensure
    ///   that we don't loose alias context
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

    // serialization //

    /// Serialized representation of types store
    pub const Serialized = struct {
        gpa: [2]u64, // Reserve space for allocator (vtable ptr + context ptr), provided during deserialization
        slots: SlotStore.Serialized,
        descs: DescStore.Serialized,
        vars: VarSafeList.Serialized,
        record_fields: RecordFieldSafeMultiList.Serialized,
        tags: TagSafeMultiList.Serialized,
        static_dispatch_constraints: StaticDispatchConstraint.SafeList.Serialized,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            // Serialize each component
            try self.slots.serialize(&store.slots, allocator, writer);
            try self.descs.serialize(&store.descs, allocator, writer);
            try self.vars.serialize(&store.vars, allocator, writer);
            try self.record_fields.serialize(&store.record_fields, allocator, writer);
            try self.tags.serialize(&store.tags, allocator, writer);
            try self.static_dispatch_constraints.serialize(&store.static_dispatch_constraints, allocator, writer);

            // Set gpa to all zeros; the space needs to be here,
            // but the value will be set separately during deserialization.
            self.gpa = .{ 0, 0 };
        }

        /// Deserialize this Serialized struct into a Store
        pub fn deserialize(self: *Serialized, offset: i64, gpa: Allocator) *Store {
            // Note: Serialized may be smaller than the runtime struct because:
            // - Uses i64 offsets instead of usize pointers
            // - Omits runtime-only fields like the allocator
            // We deserialize by overwriting the Serialized memory with the runtime struct.
            const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

            store.* = Store{
                .gpa = gpa,
                .slots = self.slots.deserialize(offset).*,
                .descs = self.descs.deserialize(offset).*,
                .vars = self.vars.deserialize(offset).*,
                .record_fields = self.record_fields.deserialize(offset).*,
                .tags = self.tags.deserialize(offset).*,
                .static_dispatch_constraints = self.static_dispatch_constraints.deserialize(offset).*,
            };

            return store;
        }
    };

    /// Serialize this Store to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Self);

        // Then serialize each component and update the struct
        offset_self.* = .{
            .gpa = allocator,
            .slots = (try self.slots.serialize(allocator, writer)).*,
            .descs = (try self.descs.serialize(allocator, writer)).*,
            .vars = (try self.vars.serialize(allocator, writer)).*,
            .record_fields = (try self.record_fields.serialize(allocator, writer)).*,
            .tags = (try self.tags.serialize(allocator, writer)).*,
            .static_dispatch_constraints = (try self.static_dispatch_constraints.serialize(allocator, writer)).*,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.slots.relocate(offset);
        self.descs.relocate(offset);
        self.vars.relocate(offset);
        self.record_fields.relocate(offset);
        self.tags.relocate(offset);
        self.static_dispatch_constraints.relocate(offset);
    }

    /// Calculate the size needed to serialize this Store
    pub fn serializedSize(self: *const Self) usize {
        const slots_size = self.slots.serializedSize();
        const descs_size = self.descs.serializedSize();
        const record_fields_size = self.record_fields.serializedSize();
        const tags_size = self.tags.serializedSize();
        const vars_size = self.vars.serializedSize();
        const static_dispatch_constraints_size = self.static_dispatch_constraints.serializedSize();

        // Add alignment padding for each component
        var total_size: usize = @sizeOf(u32) * 6; // size headers
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += slots_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += descs_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += record_fields_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += tags_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += vars_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += static_dispatch_constraints_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);
    }

    /// Deserialize a Store from the provided buffer
    pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
        if (buffer.len < @sizeOf(u32) * 6) return error.BufferTooSmall;

        var offset: usize = 0;

        // Read sizes
        const slots_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const descs_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const record_fields_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const tags_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const vars_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const static_dispatch_constraints_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        // Deserialize data
        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const slots_buffer = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(buffer[offset .. offset + slots_size]));
        const slots = try SlotStore.deserializeFrom(slots_buffer, allocator);
        offset += slots_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const descs_buffer = @as([]align(@alignOf(Desc)) const u8, @alignCast(buffer[offset .. offset + descs_size]));
        const descs = try DescStore.deserializeFrom(descs_buffer, allocator);
        offset += descs_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const record_fields_buffer = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(buffer[offset .. offset + record_fields_size]));
        const record_fields = try RecordFieldSafeMultiList.deserializeFrom(record_fields_buffer, allocator);
        offset += record_fields_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const tags_buffer = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(buffer[offset .. offset + tags_size]));
        const tags = try TagSafeMultiList.deserializeFrom(tags_buffer, allocator);
        offset += tags_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const vars_buffer = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(buffer[offset .. offset + vars_size]));
        const vars = try VarSafeList.deserializeFrom(vars_buffer, allocator);
        offset += vars_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT);
        const static_dispatch_constraints_buffer = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(buffer[offset .. offset + static_dispatch_constraints_size]));
        const static_dispatch_constraints = try StaticDispatchConstraint.SafeList.deserializeFrom(static_dispatch_constraints_buffer, allocator);
        offset += static_dispatch_constraints_size;

        return Self{
            .gpa = allocator,
            .slots = slots,
            .descs = descs,
            .record_fields = record_fields,
            .tags = tags,
            .vars = vars,
            .static_dispatch_constraints = static_dispatch_constraints,
        };
    }
};

/// Represents a store of slots
const SlotStore = struct {
    const Self = @This();

    backing: collections.SafeList(Slot),

    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .backing = try collections.SafeList(Slot).initCapacity(gpa, capacity) };
    }

    fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Serialized representation of SlotStore
    pub const Serialized = struct {
        backing: collections.SafeList(Slot).Serialized,

        /// Serialize a SlotStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            slot_store: *const SlotStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.backing.serialize(&slot_store.backing, allocator, writer);
        }

        /// Deserialize this Serialized struct into a SlotStore
        pub fn deserialize(self: *Serialized, offset: i64) *SlotStore {
            // Note: Serialized may be smaller than the runtime struct.
            // We deserialize by overwriting the Serialized memory with the runtime struct.
            const slot_store = @as(*SlotStore, @ptrFromInt(@intFromPtr(self)));

            slot_store.* = SlotStore{
                .backing = self.backing.deserialize(offset).*,
            };

            return slot_store;
        }
    };

    /// Insert a new slot into the store
    fn insert(self: *Self, gpa: Allocator, typ: Slot) std.mem.Allocator.Error!Idx {
        const safe_idx = try self.backing.append(gpa, typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Insert a value into the store
    fn appendAssumeCapacity(self: *Self, gpa: Allocator, typ: Slot) std.mem.Allocator.Error!Idx {
        const safe_idx = try self.backing.append(gpa, typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Set a value in the store
    pub fn set(self: *Self, idx: Idx, val: Slot) void {
        self.backing.set(@enumFromInt(@intFromEnum(idx)), val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Slot {
        return self.backing.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    /// Serialize this SlotStore to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // Since SlotStore is just a wrapper around SafeList, serialize the backing directly
        const serialized_backing = try self.backing.serialize(allocator, writer);
        // Cast the serialized SafeList pointer to a SlotStore pointer
        return @ptrCast(serialized_backing);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.backing.relocate(offset);
    }

    /// Calculate the size needed to serialize this SlotStore
    fn serializedSize(self: *const Self) usize {
        return self.backing.serializedSize();
    }

    /// Deserialize a SlotStore from the provided buffer
    fn deserializeFrom(buffer: []align(@alignOf(Slot)) const u8, allocator: Allocator) !Self {
        return .{
            .backing = try collections.SafeList(Slot).deserializeFrom(buffer, allocator),
        };
    }

    /// A type-safe index into the store
    const Idx = enum(u32) { _ };
};

/// Represents a store of descriptors
///
/// Indexes into the list are typesafe
const DescStore = struct {
    const Self = @This();
    const DescSafeMultiList = collections.SafeMultiList(Desc);

    backing: DescSafeMultiList,

    /// Init & allocated memory
    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .backing = try DescSafeMultiList.initCapacity(gpa, capacity) };
    }

    /// Deinit & free allocated memory
    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Serialized representation of DescStore
    pub const Serialized = struct {
        backing: DescSafeMultiList.Serialized,

        /// Serialize a DescStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            desc_store: *const DescStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.backing.serialize(&desc_store.backing, allocator, writer);
        }

        /// Deserialize this Serialized struct into a DescStore
        pub fn deserialize(self: *Serialized, offset: i64) *DescStore {
            // Note: Serialized may be smaller than the runtime struct.
            // We deserialize by overwriting the Serialized memory with the runtime struct.
            const desc_store = @as(*DescStore, @ptrFromInt(@intFromPtr(self)));

            desc_store.* = DescStore{
                .backing = self.backing.deserialize(offset).*,
            };

            return desc_store;
        }
    };

    /// Insert a value into the store
    fn insert(self: *Self, gpa: Allocator, typ: Desc) std.mem.Allocator.Error!Idx {
        const safe_idx = try self.backing.append(gpa, typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Idx, val: Desc) void {
        self.backing.set(@enumFromInt(@intFromEnum(idx)), val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Desc {
        return self.backing.get(@enumFromInt(@intFromEnum(idx)));
    }

    /// Serialize this DescStore to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // Since DescStore is just a wrapper around SafeMultiList, serialize the backing directly
        const serialized_backing = try self.backing.serialize(allocator, writer);
        // Cast the serialized SafeMultiList pointer to a DescStore pointer
        return @ptrCast(serialized_backing);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.backing.relocate(offset);
    }

    /// Calculate the size needed to serialize this DescStore
    pub fn serializedSize(self: *const Self) usize {
        return self.backing.serializedSize();
    }

    /// Deserialize a DescStore from the provided buffer
    pub fn deserializeFrom(buffer: []align(@alignOf(Desc)) const u8, allocator: Allocator) !Self {
        const backing = try DescSafeMultiList.deserializeFrom(buffer, allocator);
        return Self{ .backing = backing };
    }

    /// A type-safe index into the store
    /// This type is made public below
    const Idx = enum(u32) { _ };
};

/// An index into the desc store
pub const DescStoreIdx = DescStore.Idx;

// path compression

test "resolveVarAndCompressPath - flattens redirect chain to flex" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const c = try store.fresh();
    const b = try store.freshRedirect(c);
    const a = try store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}

test "resolveVarAndCompressPath - no-op on already root" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const num_flex = try store.fresh();
    const num = Content{ .structure = .{ .num = .{ .num_poly = num_flex } } };
    const num_var = try store.freshFromContent(num);

    const result = store.resolveVarAndCompressPath(num_var);

    try std.testing.expectEqual(num, result.desc.content);
    try std.testing.expectEqual(num_var, result.var_);
    // try std.testing.expectEqual(store.getSlot(num_var), Slot{ .root = num_desc_idx });
}

test "resolveVarAndCompressPath - flattens redirect chain to structure" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const num_flex = try store.fresh();
    const num = Content{ .structure = .{ .num = .{ .num_poly = num_flex } } };
    const c = try store.freshFromContent(num);
    const b = try store.freshRedirect(c);
    const a = try store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(num, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}

test "Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    // Create an empty Store
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.len());
}

test "Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    // Create original Store and add some types
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create some type variables
    const flex = try original.fresh();
    const rigid = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) });

    // Create a redirect
    const redirect_var = try original.freshRedirect(flex);

    // Verify original values
    const flex_resolved = original.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const rigid_resolved = original.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, rigid_resolved.desc.content);

    const redirect_resolved = original.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the types are accessible
    try std.testing.expectEqual(@as(usize, 3), deserialized.len());

    const deser_flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, deser_flex_resolved.desc.content);

    const deser_rigid_resolved = deserialized.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, deser_rigid_resolved.desc.content);

    const deser_redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(deser_flex_resolved.desc_idx, deser_redirect_resolved.desc_idx);
}

test "Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;
    var idents = try base.Ident.Store.initCapacity(gpa, 10);
    defer idents.deinit(gpa);

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create various types
    const flex = try original.fresh();
    const str_var = try original.freshFromContent(Content{ .structure = .{ .str = {} } });
    const list_elem = try original.fresh();
    const list_var = try original.freshFromContent(Content{ .structure = .{ .list = list_elem } });

    // Create a function type
    const arg1 = try original.fresh();
    const arg2 = try original.fresh();
    const ret = try original.fresh();
    const func_content = try original.mkFuncPure(&[_]Var{ arg1, arg2 }, ret);
    const func_var = try original.freshFromContent(func_content);

    // Create a record type
    const field1_var = try original.fresh();
    const field2_var = try original.fresh();
    const record_fields = try original.appendRecordFields(&[_]RecordField{
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 100 }, .var_ = field1_var },
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 200 }, .var_ = field2_var },
    });
    const record_ext = try original.fresh();
    const record_content = Content{ .structure = .{ .record = .{ .fields = record_fields, .ext = record_ext } } };
    const record_var = try original.freshFromContent(record_content);

    // Create a tag union
    const tag1 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 300 }, &[_]Var{flex});
    const tag2 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 400 }, &[_]Var{ arg1, arg2 });
    const tag_union_ext = try original.fresh();
    const tag_union_content = try original.mkTagUnion(&[_]Tag{ tag1, tag2 }, tag_union_ext);
    const tag_union_var = try original.freshFromContent(tag_union_content);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - Store is at the beginning of the buffer
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all types
    const deser_str = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .{ .str = {} } }, deser_str.desc.content);

    const deser_list = deserialized.resolveVar(list_var);
    try std.testing.expectEqual(FlatType{ .list = list_elem }, deser_list.desc.content.structure);

    const deser_func = deserialized.resolveVar(func_var);
    switch (deser_func.desc.content.structure) {
        .fn_pure => |func| {
            const args = deserialized.sliceVars(func.args);
            try std.testing.expectEqual(@as(usize, 2), args.len);
            try std.testing.expectEqual(arg1, args[0]);
            try std.testing.expectEqual(arg2, args[1]);
            try std.testing.expectEqual(ret, func.ret);
        },
        else => unreachable,
    }

    const deser_record = deserialized.resolveVar(record_var);
    switch (deser_record.desc.content.structure) {
        .record => |record| {
            const fields_slice = deserialized.getRecordFieldsSlice(record.fields);
            try std.testing.expectEqual(@as(usize, 2), fields_slice.len);
            try std.testing.expectEqual(@as(u29, 100), fields_slice.items(.name)[0].idx);
            try std.testing.expectEqual(@as(u29, 200), fields_slice.items(.name)[1].idx);
            try std.testing.expectEqual(field1_var, fields_slice.items(.var_)[0]);
            try std.testing.expectEqual(field2_var, fields_slice.items(.var_)[1]);
            try std.testing.expectEqual(record_ext, record.ext);
        },
        else => unreachable,
    }

    const deser_tag_union = deserialized.resolveVar(tag_union_var);
    switch (deser_tag_union.desc.content.structure) {
        .tag_union => |tag_union| {
            const tags_slice = deserialized.getTagsSlice(tag_union.tags);
            try std.testing.expectEqual(@as(usize, 2), tags_slice.len);
            try std.testing.expectEqual(@as(u29, 300), tags_slice.items(.name)[0].idx);
            try std.testing.expectEqual(@as(u29, 400), tags_slice.items(.name)[1].idx);

            const tag1_args = deserialized.sliceVars(tags_slice.items(.args)[0]);
            try std.testing.expectEqual(@as(usize, 1), tag1_args.len);
            try std.testing.expectEqual(flex, tag1_args[0]);

            const tag2_args = deserialized.sliceVars(tags_slice.items(.args)[1]);
            try std.testing.expectEqual(@as(usize, 2), tag2_args.len);
            try std.testing.expectEqual(arg1, tag2_args[0]);
            try std.testing.expectEqual(arg2, tag2_args[1]);

            try std.testing.expectEqual(tag_union_ext, tag_union.ext);
        },
        else => unreachable,
    }
}

test "SlotStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    var slot_store = try SlotStore.init(gpa, 4);
    defer slot_store.deinit(gpa);

    // Add some slots
    _ = try slot_store.insert(gpa, .{ .root = @enumFromInt(100) });
    _ = try slot_store.insert(gpa, .{ .redirect = @enumFromInt(0) });
    _ = try slot_store.insert(gpa, .{ .root = @enumFromInt(200) });

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile("test_slot_store_serialized.dat", .{ .read = true });
    defer file.close();

    // Serialize using SlotStore.Serialized with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, SlotStore.Serialized);
    try serialized_ptr.serialize(&slot_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.read(buffer);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*SlotStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify
    try std.testing.expectEqual(@as(u64, 3), deserialized.backing.len());
    try std.testing.expectEqual(Slot{ .root = @enumFromInt(100) }, deserialized.get(@enumFromInt(0)));
    try std.testing.expectEqual(Slot{ .redirect = @enumFromInt(0) }, deserialized.get(@enumFromInt(1)));
    try std.testing.expectEqual(Slot{ .root = @enumFromInt(200) }, deserialized.get(@enumFromInt(2)));
}

test "DescStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    var desc_store = try DescStore.init(gpa, 4);
    defer desc_store.deinit(gpa);

    // Add some descriptors
    const desc1 = Descriptor{
        .content = Content{ .flex = Flex.init() },
        .rank = Rank.generalized,
        .mark = Mark.none,
    };
    const desc2 = Descriptor{
        .content = Content{ .structure = .{ .str = {} } },
        .rank = Rank.top_level,
        .mark = Mark.visited,
    };

    _ = try desc_store.insert(gpa, desc1);
    _ = try desc_store.insert(gpa, desc2);

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile("test_desc_store_serialized.dat", .{ .read = true });
    defer file.close();

    // Serialize using DescStore.Serialized with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, DescStore.Serialized);
    try serialized_ptr.serialize(&desc_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.read(buffer);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*DescStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));
    // Note: deserialize already handles relocation, don't call relocate again

    // Verify
    try std.testing.expectEqual(@as(usize, 2), deserialized.backing.items.len);
    try std.testing.expectEqual(desc1, deserialized.get(@enumFromInt(0)));
    try std.testing.expectEqual(desc2, deserialized.get(@enumFromInt(1)));
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    var store = try Store.init(gpa);
    defer store.deinit();

    // Create some type variables
    const flex = try store.fresh();
    const str_var = try store.freshFromContent(Content{ .structure = .{ .str = {} } });
    const redirect_var = try store.freshRedirect(flex);

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile("test_store_serialized.dat", .{ .read = true });
    defer file.close();

    // Serialize using Store.Serialized
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(gpa);

    const serialized_ptr = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized_ptr.serialize(&store, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.read(buffer);

    // Deserialize - Store.Serialized is at the beginning of the buffer
    const deser_ptr = @as(*Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify the store was deserialized correctly
    try std.testing.expectEqual(@as(usize, 3), deserialized.len());

    const flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const str_resolved = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .{ .str = {} } }, str_resolved.desc.content);

    const redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);
}

test "Store multiple instances CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    // Create multiple stores
    var store1 = try Store.init(gpa);
    defer store1.deinit();

    var store2 = try Store.init(gpa);
    defer store2.deinit();

    var store3 = try Store.init(gpa);
    defer store3.deinit();

    // Populate differently
    const var1_1 = try store1.fresh();
    const var1_2 = try store1.freshFromContent(Content{ .structure = .{ .str = {} } });
    _ = try store1.freshRedirect(var1_1);

    const var2_1 = try store2.fresh();
    const var2_2 = try store2.fresh();
    const func_content = try store2.mkFuncEffectful(&[_]Var{var2_1}, var2_2);
    _ = try store2.freshFromContent(func_content);

    // store3 left empty

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_stores.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(gpa);

    const offset1 = writer.total_bytes; // Store1 starts at current position
    _ = try store1.serialize(gpa, &writer);

    const offset2 = writer.total_bytes; // Store2 starts at current position
    _ = try store2.serialize(gpa, &writer);

    const offset3 = writer.total_bytes; // Store3 starts at current position
    _ = try store3.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify store 1
    try std.testing.expectEqual(@as(usize, 3), deserialized1.len());
    const deser1_var2 = deserialized1.resolveVar(var1_2);
    try std.testing.expectEqual(Content{ .structure = .{ .str = {} } }, deser1_var2.desc.content);

    // Verify store 2
    try std.testing.expectEqual(@as(usize, 3), deserialized2.len());

    // Verify store 3 (empty)
    try std.testing.expectEqual(@as(usize, 0), deserialized3.len());
}

test "SlotStore and DescStore serialization and deserialization" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create several variables to populate SlotStore with roots
    const var1 = try original.freshFromContent(Content{ .flex = Flex.init() });
    const var2 = try original.freshFromContent(Content{ .structure = .{ .str = {} } });
    const var3 = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 123))) });

    // Create redirects to populate SlotStore with redirects
    const redirect1 = try original.freshRedirect(var1);
    _ = try original.freshRedirect(var2);
    const redirect3 = try original.freshRedirect(redirect1); // Chain of redirects

    // Verify SlotStore has both root and redirect entries
    try std.testing.expectEqual(@as(usize, 6), original.slots.backing.len());

    // Verify DescStore has the descriptors
    try std.testing.expectEqual(@as(usize, 3), original.descs.backing.items.len);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_explicit_stores.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - Store struct is at the beginning of the buffer
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify SlotStore was correctly deserialized
    try std.testing.expectEqual(@as(usize, 6), deserialized.slots.backing.len());

    // Verify DescStore was correctly deserialized
    try std.testing.expectEqual(@as(usize, 3), deserialized.descs.backing.items.len);

    // Verify we can resolve variables correctly
    const resolved1 = deserialized.resolveVar(var1);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, resolved1.desc.content);

    const resolved2 = deserialized.resolveVar(var2);
    try std.testing.expectEqual(Content{ .structure = .{ .str = {} } }, resolved2.desc.content);

    const resolved3 = deserialized.resolveVar(var3);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 123))) }, resolved3.desc.content);

    // Verify redirects work
    const resolved_redirect1 = deserialized.resolveVar(redirect1);
    try std.testing.expectEqual(resolved1.desc_idx, resolved_redirect1.desc_idx);

    const resolved_redirect3 = deserialized.resolveVar(redirect3);
    try std.testing.expectEqual(resolved1.desc_idx, resolved_redirect3.desc_idx);
}

test "Store with path compression CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create a redirect chain
    const c = try original.fresh();
    const b = try original.freshRedirect(c);
    const a = try original.freshRedirect(b);

    // Compress the path
    _ = original.resolveVarAndCompressPath(a);

    // Verify path is compressed
    try std.testing.expectEqual(Slot{ .redirect = c }, original.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, original.getSlot(b));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_compressed_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - Store is at the beginning of the buffer
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify compressed paths are preserved
    try std.testing.expectEqual(Slot{ .redirect = c }, deserialized.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, deserialized.getSlot(b));
}
