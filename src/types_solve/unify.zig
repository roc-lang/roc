const std = @import("std");

const exitOnOutOfMemory = @import("../collections.zig").utils.exitOnOom;
const Region = @import("../base/Region.zig");
const Ident = @import("../base/Ident.zig");
const types = @import("./types.zig");

const TypeIdent = types.TypeIdent;
const Var = types.Var;
const Desc = types.Descriptor;
const Rank = types.Rank;
const Content = types.Content;
const Alias = types.Alias;
const FlatType = types.FlatType;
const TypeApply = types.TypeApply;
const Tuple = types.Tuple;
const Num = types.Num;
const Func = types.Func;

/// unify
pub fn unify(table: *UnificationTable, vars: *VarArray, a: Var, b: Var) Result {
    var unify_pool = UnifyPool.init(table, vars);
    unify_pool.unifyGuarded(a, b) catch |err| switch (err) {
        error.TypeMismatch => {
            table.union_(a, b, .{
                .content = .err,
                .rank = Rank.generalized,
            });
            return Result{ .err = .{ .a = a, .b = b } };
        },
    };
    return .ok;
}

/// The result of unification
pub const Result = union(enum) {
    const Self = @This();

    ok,
    err: Err,

    /// The error types that occurred during unification
    /// TODO: Make these full error types
    pub const Err = struct { a: Var, b: Var };

    pub fn isOk(self: Self) bool {
        return self == .ok;
    }
};

/// A bounded array of variables
const VarArray = std.BoundedArray(Var, 16);

const UnifyPool = struct {
    const Self = @This();

    table: *UnificationTable,
    vars: *VarArray,

    /// Init a unify_pool
    /// Caller owns the memory of the provided values
    fn init(table: *UnificationTable, vars: *VarArray) Self {
        return .{ .table = table, .vars = vars };
    }

    // merge helpers

    /// Link the variables & updated the content in the unification table
    /// In the old compiler, this function was called "merge"
    fn merge(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) void {
        self.table.union_(vars.a.var_, vars.b.var_, .{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
    }

    // unification

    /// Error thrown during unification when there's a type mismatch
    const Error = error{TypeMismatch};

    /// Unify checking for equivalance
    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) error{TypeMismatch}!void {
        switch (self.table.checkVarsEquiv(a_var, b_var)) {
            .equiv => {
                // this means that the vars are identitcal, so nothing needs to happen
            },
            .not_equiv => |vars| {
                switch (vars.a.desc.content) {
                    .flex_var => |mb_a_ident| {
                        self.unifyFlex(&vars, mb_a_ident, vars.b.desc.content);
                    },
                    .structural_alias => |a_alias| {
                        try self.unifyStructuralAlias(&vars, a_alias, vars.b.desc.content);
                    },
                    .opaque_alias => |a_alias| {
                        try self.unifyOpaqueAlias(&vars, a_alias, vars.b.desc.content);
                    },
                    .effectful => {
                        try self.unifyEffectful(&vars, vars.b.desc.content);
                    },
                    .pure => {
                        try self.unifyPure(&vars, vars.b.desc.content);
                    },
                    .concrete => |a_flat_type| {
                        try self.unifyConcrete(&vars, a_flat_type, vars.b.desc.content);
                    },
                    .err => return error.TypeMismatch,
                }
            },
        }
    }

    // Unify flex //

    /// Unify when `a` was a flex
    fn unifyFlex(self: *Self, vars: *const ResolvedVarDescs, mb_a_ident: ?Ident.Idx, b_content: Content) void {
        switch (b_content) {
            .flex_var => |mb_b_ident| {
                if (mb_a_ident) |a_ident| {
                    self.merge(vars, Content{ .flex_var = a_ident });
                } else {
                    self.merge(vars, Content{ .flex_var = mb_b_ident });
                }
            },
            .structural_alias => self.merge(vars, b_content),
            .opaque_alias => self.merge(vars, b_content),
            .effectful => self.merge(vars, b_content),
            .pure => self.merge(vars, b_content),
            .concrete => self.merge(vars, b_content),
            .err => self.merge(vars, .err),
        }
    }

    // Unify structural alias //

    /// Unify when `a` was a structural alias
    fn unifyStructuralAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .structural_alias = a_alias });
            },
            .structural_alias => |b_alias| {
                if (TypeIdent.eql(a_alias.ident, b_alias.ident)) {
                    try self.unifyTwoAliases(vars, a_alias, b_alias);
                } else {
                    try self.unifyGuarded(a_alias.backing_var, b_alias.backing_var);
                }
            },
            .opaque_alias => try self.unifyGuarded(a_alias.backing_var, vars.b.var_),
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .concrete => try self.unifyGuarded(a_alias.backing_var, vars.b.var_),
            .err => self.merge(vars, .err),
        }
    }

    /// Unify when `a` was a structural alias
    fn unifyOpaqueAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .structural_alias = a_alias });
            },
            .structural_alias => |b_alias| {
                try self.unifyGuarded(vars.a.var_, b_alias.backing_var);
            },
            .opaque_alias => |b_alias| {
                if (TypeIdent.eql(a_alias.ident, b_alias.ident)) {
                    try self.unifyTwoAliases(vars, a_alias, b_alias);
                } else {
                    return error.TypeMismatch;
                }
            },
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .concrete => return error.TypeMismatch,
            .err => self.merge(vars, .err),
        }
    }

    /// Unify two aliases, either structural or opaque
    ///
    /// This function assumes the caller has already checked that the alias names match
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    ///
    /// NOTE: the rust version of this function `unify_two_aliases` is *signifgantly* more
    /// complicated than the version here
    fn unifyTwoAliases(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_alias: Alias) error{TypeMismatch}!void {
        if (a_alias.args.len != b_alias.args.len) {
            return error.TypeMismatch;
        }

        for (0..a_alias.args.len) |i| {
            try self.unifyGuarded(a_alias.args.buffer[i], b_alias.args.buffer[i]);
        }

        // TODO: Here, we'll need some special handling for recursion variables
        // For each argument variable, we need to prefer recursive vars. Then
        // we'll build a new structurla alias and pass that to self.merge
        // See unify_two_aliases and choose_merged_var in the rust compiler for details

        // The rust compiler doesn't report this error, should we?
        self.unifyGuarded(a_alias.backing_var, b_alias.backing_var) catch {};

        self.merge(vars, vars.b.desc.content);
    }

    // Unify effectful //

    /// Unify when `a` was a effectful
    fn unifyEffectful(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
        _ = self;
        _ = vars;
        _ = b_content;
        @panic("Unimplemetend");
    }

    // Unify pure //

    /// Unify when `a` was a pure
    fn unifyPure(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
        _ = self;
        _ = vars;
        _ = b_content;
        @panic("Unimplemetend");
    }

    // Unify concrete //

    /// Unify when `a` is a concrete type
    fn unifyConcrete(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_content: Content,
    ) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .concrete = a_flat_type });
            },
            .structural_alias => |alias| {
                try self.unifyGuarded(vars.a.var_, alias.backing_var);
            },
            .opaque_alias => |_| return error.TypeMismatch,
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .concrete => |b_flat_type| {
                try self.unifyFlatType(vars, a_flat_type, b_flat_type);
            },
            .err => self.merge(vars, .err),
        }
    }

    /// Unify when `a` is a concrete type
    fn unifyFlatType(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_flat_type: FlatType,
    ) error{TypeMismatch}!void {
        switch (a_flat_type) {
            .type_apply => |a_type_apply| {
                switch (b_flat_type) {
                    .type_apply => |b_type_apply| {
                        try self.unify_type_apply(vars, a_type_apply, b_type_apply);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        try self.unify_tuple(vars, a_tuple, b_tuple);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .num => |a_num| {
                switch (b_flat_type) {
                    .num => |b_num| {
                        try self.unify_num(vars, a_num, b_num);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .func => |a_func| {
                switch (b_flat_type) {
                    .func => |b_func| {
                        try self.unify_func(vars, a_func, b_func);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record => |a_record| {
                switch (b_flat_type) {
                    .record => |b_record| {
                        _ = a_record;
                        _ = b_record;
                        // try self.unify_record(vars, a_record, b_record);
                        @panic("Unimplemented");
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_record => {
                switch (b_flat_type) {
                    .empty_record => {
                        // try self.unify_empty_record(vars, a_empty_record, b_empty_record);
                        @panic("Unimplemented");
                    },
                    else => return error.TypeMismatch,
                }
            },
        }
    }

    /// unify type application (both with args like 'List Str' and without like 'Bool')
    ///
    /// this checks:
    /// * that the type names match
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_type_apply(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_type_apply: TypeApply,
        b_type_apply: TypeApply,
    ) error{TypeMismatch}!void {
        if (!TypeIdent.eql(a_type_apply.ident, b_type_apply.ident)) {
            return error.TypeMismatch;
        }
        if (a_type_apply.args.len != b_type_apply.args.len) {
            return error.TypeMismatch;
        }

        for (0..a_type_apply.args.len) |i| {
            try self.unifyGuarded(a_type_apply.args.buffer[i], b_type_apply.args.buffer[i]);
        }

        // TODO: Here, we'll need some special handling for recursion variables
        // For each argument variable, we need to prefer recursive vars. Then
        // we'll build a new flat_type.type_apply with those aruguments and pass
        // that to self.merge
        // See unify_flat_type and choose_merged_var in the rust compiler for details

        self.merge(vars, vars.b.desc.content);
    }

    /// unify tuples
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_tuple(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tuple: Tuple,
        b_tuple: Tuple,
    ) error{TypeMismatch}!void {
        if (a_tuple.elems.len != b_tuple.elems.len) {
            return error.TypeMismatch;
        }

        for (0..a_tuple.elems.len) |i| {
            try self.unifyGuarded(a_tuple.elems.buffer[i], b_tuple.elems.buffer[i]);
        }

        self.merge(vars, vars.b.desc.content);
    }

    /// unify numbers
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_num(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num: Num,
        b_num: Num,
    ) error{TypeMismatch}!void {
        switch (a_num) {
            .flex_var => {
                // if a is flex, update to b's desc
                self.merge(vars, vars.b.desc.content);
            },
            .int => |a_int| switch (b_num) {
                .flex_var => {
                    // if b is flex, update to a's desc
                    self.merge(vars, vars.a.desc.content);
                },
                .frac => return error.TypeMismatch,
                .int => |b_int| {
                    if (a_int == .flex_var) {
                        // if a is flex, update to b's desc
                        self.merge(vars, vars.b.desc.content);
                    } else if (b_int == .flex_var) {
                        // if b is flex, update to a's desc
                        self.merge(vars, vars.a.desc.content);
                    } else if (a_int == b_int) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    }
                },
            },

            .frac => |a_frac| switch (b_num) {
                .flex_var => {
                    // if b is flex, update to a's desc
                    self.merge(vars, vars.a.desc.content);
                },
                .int => return error.TypeMismatch,
                .frac => |b_frac| {
                    if (a_frac == .flex_var) {
                        // if a is flex, update to b's desc
                        self.merge(vars, vars.b.desc.content);
                    } else if (b_frac == .flex_var) {
                        // if b is flex, update to a's desc
                        self.merge(vars, vars.a.desc.content);
                    } else if (a_frac == b_frac) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    }
                },
            },
        }
    }

    /// unify func
    ///
    /// this checks:
    /// * that the arg arities are the same
    /// * that parallel args unify
    /// * that ret unifies
    fn unify_func(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_func: Func,
        b_func: Func,
    ) error{TypeMismatch}!void {
        if (a_func.args.len != b_func.args.len) {
            return error.TypeMismatch;
        }

        for (0..a_func.args.len) |i| {
            try self.unifyGuarded(a_func.args.buffer[i], b_func.args.buffer[i]);
        }

        try self.unifyGuarded(a_func.ret, b_func.ret);

        self.merge(vars, vars.b.desc.content);
    }
};

/// A variable & it's descriptor info
const ResolvedVarDesc = struct { var_: Var, desc_idx: DescStore.Idx, desc: Desc };

/// Two variables & descs
const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

/// Type solver
///
/// You use a Var to lookup a Slot, then a Slot to lookup a Decriptor
const UnificationTable = struct {
    slot_store: SlotStore,
    desc_store: DescStore,

    const Self = @This();

    /// Init the unification table
    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: eventually use herusitics here to determin sensible defaults
        return .{
            .desc_store = DescStore.init(gpa, 64),
            .slot_store = SlotStore.init(gpa, 64),
        };
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        self.desc_store.deinit();
        self.slot_store.deinit();
    }

    /// Create a new unbound, flexible type variable without a name
    pub fn freshFlexVar(self: *Self) Var {
        return self.freshFromContent(Content{ .flex_var = null });
    }

    /// Create a new variable with the provided desc at the top level
    pub fn freshFromContent(self: *Self, content: Content) Var {
        const desc_idx = self.desc_store.insert(.{ .content = content, .rank = Rank.top_level });
        const slot_var = self.slot_store.insert(.{ .root = desc_idx });
        return slot_var;
    }

    /// Create a variable redirecting to the provided var
    /// Used for test
    pub fn freshRedirect(self: *Self, var_: Var) Var {
        const slot_var = self.slot_store.insert(.{ .redirect = var_ });
        return slot_var;
    }

    /// Link the variables & updated the content in the unification table
    /// * update b to to the new desc value
    /// * redirect a -> b
    ///
    // NOTE: The elm & the roc compiler this step differently
    // * The elm compiler sets b to redirect to a
    // * The roc compiler sets a to redirect to b (based on the `ena` compiler
    // See the `union` function in subs.rs for details
    pub fn union_(self: *Self, a_var: Var, b_var: Var, new_desc: Desc) void {
        const b_data = self.resolveAndCompressVar(b_var);

        // Update b to be the new desc
        self.desc_store.set(b_data.desc_idx, new_desc);

        // Update a to point to b
        self.slot_store.set(a_var, .{ .redirect = b_var });
    }

    /// The result of checking for equivalance
    const VarEquivResult = union(enum) { equiv, not_equiv: ResolvedVarDescs };

    /// Check if two variables are equivalant
    /// This will follow all redirects and compress the path
    ///
    /// If the vars are *not equivalant, then return the resolved vars & descs
    fn checkVarsEquiv(self: *Self, a_var: Var, b_var: Var) VarEquivResult {
        const a = self.resolveAndCompressVar(a_var);
        const b = self.resolveAndCompressVar(b_var);
        if (a.desc_idx == b.desc_idx) {
            return .equiv;
        } else {
            return .{ .not_equiv = .{ .a = a, .b = b } };
        }
    }

    /// Given a type var, follow all redirects until finding the root descriptor
    /// Will mutate the DescStore in place to compress the path
    /// If the type is not a redirect, the var return will match the one passed in
    pub fn resolveAndCompressVar(self: *Self, initial_var: Var) ResolvedVarDesc {
        // First, we follow the chain down to the concrete type
        var redirected_var = initial_var;
        var redirected_slot: Slot = self.slot_store.get(initial_var);
        while (true) {
            switch (redirected_slot) {
                .redirect => |redirect_idx| {
                    redirected_var = redirect_idx;
                    redirected_slot = self.slot_store.get(redirect_idx);
                },
                .root => |_| break,
            }
        }

        // then follow the chain again, but compressing each step to the concrete type
        if (initial_var != redirected_var) {
            var compressed_idx = initial_var;
            var compressed_typ: Slot = self.slot_store.get(initial_var);
            while (true) {
                switch (compressed_typ) {
                    .redirect => |redirect_idx| {
                        self.slot_store.set(compressed_idx, Slot{ .redirect = redirected_var });
                        compressed_idx = redirect_idx;
                        compressed_typ = self.slot_store.get(redirect_idx);
                    },
                    .root => |_| break,
                }
            }
        }

        // TODO: refactor to remove panic
        switch (redirected_slot) {
            .redirect => |_| @panic("redirected slot was still redirect after following chain"),
            .root => |desc_idx| {
                const desc = self.desc_store.get(desc_idx);
                return .{
                    .var_ = redirected_var,
                    .desc_idx = desc_idx,
                    .desc = desc,
                };
            },
        }
    }

    /// Given a type var, follow all redirects until finding the root descriptor, then
    /// compress the path
    ///
    /// If the type is not a redirect, the var return will match the one passed in
    ///
    /// TODO: This currently traverses the tree twice to compress, can we improve that?
    fn followRedirectsAndCompress(self: *Self, initial_var: Var) ResolvedVarDesc {
        // first, we follow the chain down to the concrete type
        var redirected_var = initial_var;
        var redirected_slot: Slot = self.slot_store.get(initial_var);
        while (true) {
            switch (redirected_slot) {
                .redirect => |redirect_idx| {
                    redirected_var = redirect_idx;
                    redirected_slot = self.slot_store.get(redirect_idx);
                },
                .root => |_| break,
            }
        }

        // then follow the chain again, but compressing each step to the concrete type
        if (initial_var != redirected_var) {
            var compressed_idx = initial_var;
            var compressed_typ: Slot = self.slot_store.get(initial_var);
            while (true) {
                switch (compressed_typ) {
                    .redirect => |redirect_idx| {
                        self.slot_store.set(compressed_idx, Slot{ .redirect = redirected_var });
                        compressed_idx = redirect_idx;
                        compressed_typ = self.slot_store.get(redirect_idx);
                    },
                    .root => |_| break,
                }
            }
        }

        // TODO: refactor to remove panic
        switch (redirected_slot) {
            .redirect => |_| @panic("redirected slot was still redirect after following chain"),
            .root => |desc_idx| {
                const desc = self.desc_store.get(desc_idx);
                return .{
                    .var_ = redirected_var,
                    .desc_idx = desc_idx,
                    .desc = desc,
                };
            },
        }
    }
};

// Reperents either type data *or* a symlink to another type variable
const Slot = union(enum) {
    root: DescStore.Idx,
    redirect: Var,

    const ArrayList = std.ArrayList(Slot);
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

    /// Insert a new slot into the store. The return type is a new `Var`.
    ///
    /// It's important to note that this `Var` is an index into the store,
    /// but it also also the type variable itself
    fn insert(self: *Self, typ: Slot) Var {
        const idx: Var = @enumFromInt(self.backing.items.len);
        self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Var, val: Slot) void {
        self.backing.items[@intFromEnum(idx)] = val;
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Var) Slot {
        return self.backing.items[@intFromEnum(idx)];
    }
};

/// Represents a store of descriptors
///
/// Indexes into the list are typesafe
const DescStore = struct {
    const Self = @This();

    backing: std.ArrayList(Desc),

    /// A type-safe index into the store
    const Idx = enum(usize) { _ };

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
};

// tests //

// helpers

/// Environment to make setting up tests easier
const TestEnv = struct {
    const Self = @This();

    gpa: std.mem.Allocator,
    ident_store: Ident.Store,
    table: UnificationTable,
    vars: VarArray,

    pub fn init(gpa: std.mem.Allocator) Self {
        return .{
            .gpa = gpa,
            .ident_store = Ident.Store.initCapacity(gpa, 16),
            .table = UnificationTable.init(gpa),
            .vars = VarArray.init(0) catch unreachable,
        };
    }

    pub fn deinit(self: *Self) void {
        self.ident_store.deinit(self.gpa);
        self.table.deinit();
    }

    const Error = error{VarIsNotRoot};

    /// Get a desc from a root var
    pub fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.table.slot_store.get(var_)) {
            .root => |desc_idx| return self.table.desc_store.get(desc_idx),
            .redirect => return error.VarIsNotRoot,
        }
    }

    fn mkTypeIdent(self: *Self, name: []const u8) TypeIdent {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        return TypeIdent{ .ident_idx = ident_idx };
    }

    // helpers - concrete - tuple

    fn mkAlias(self: *Self, name: []const u8, args: []const Var, backing_var: Var) Alias {
        std.debug.assert(args.len <= Alias.args_capacity);
        const args_arr = Alias.ArgsArray.fromSlice(args) catch unreachable;
        return .{
            .ident = self.mkTypeIdent(name),
            .args = args_arr,
            .backing_var = backing_var,
        };
    }

    fn mkAliasFromIdent(type_ident_idx: TypeIdent, args: []const Var, backing_var: Var) Alias {
        std.debug.assert(args.len <= Alias.args_capacity);
        const args_arr = Alias.ArgsArray.fromSlice(args) catch unreachable;
        return .{
            .ident = type_ident_idx,
            .args = args_arr,
            .backing_var = backing_var,
        };
    }

    // helpers - concrete - type_apply

    fn mkTypeApplyNoArg(self: *Self, name: []const u8) Content {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        return .{ .concrete = .{ .type_apply = .{
            .ident = .{ .ident_idx = ident_idx },
            .args = TypeApply.ArgsArray.init(0) catch unreachable,
        } } };
    }

    fn mkTypeApply1Arg(self: *Self, name: []const u8, arg: Var) Content {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        var args = TypeApply.ArgsArray.init(0) catch unreachable;
        args.append(arg) catch unreachable;
        return .{ .concrete = .{ .type_apply = .{ .ident = .{ .ident_idx = ident_idx }, .args = args } } };
    }

    fn mkTypeApply2Args(self: *Self, name: []const u8, arg1: Var, arg2: Var) Content {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        var args = TypeApply.ArgsArray.init(0) catch unreachable;
        args.append(arg1) catch unreachable;
        args.append(arg2) catch unreachable;
        return .{ .concrete = .{ .type_apply = .{ .ident = .{ .ident_idx = ident_idx }, .args = args } } };
    }

    // helpers - concrete - tuple

    fn mkTuple(slice: []const Var) Content {
        std.debug.assert(slice.len <= Tuple.elems_capacity);
        const args = Tuple.ElemsArray.fromSlice(slice) catch unreachable;
        return Content{ .concrete = .{ .tuple = .{ .elems = args } } };
    }

    // helpers - concrete - func

    fn mkFunc(args: []const Var, ret: Var) Content {
        std.debug.assert(args.len <= Func.arg_capacity);
        const args_arr = Func.ArgsArray.fromSlice(args) catch unreachable;
        return Content{ .concrete = .{ .func = .{ .args = args_arr, .ret = ret } } };
    }
};

// path compression

test "resolveAndCompressVar - flattens redirect chain to flex_var" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const c = env.table.freshFlexVar();
    const b = env.table.freshRedirect(c);
    const a = env.table.freshRedirect(b);

    const result = env.table.followRedirectsAndCompress(a);
    try std.testing.expectEqual(result.desc.content, Content{ .flex_var = null });
    try std.testing.expectEqual(result.var_, c);
    try std.testing.expectEqual(env.table.slot_store.get(a), Slot{ .redirect = c });
    try std.testing.expectEqual(env.table.slot_store.get(b), Slot{ .redirect = c });
}

test "resolveAndCompressVar - no-op on already root" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_desc_idx = env.table.desc_store.insert(.{ .content = str, .rank = Rank.top_level });
    const str_var = env.table.slot_store.insert(.{ .root = str_desc_idx });

    const result = env.table.followRedirectsAndCompress(str_var);

    try std.testing.expectEqual(result.desc.content, str);
    try std.testing.expectEqual(result.var_, str_var);
    try std.testing.expectEqual(env.table.slot_store.get(str_var), Slot{ .root = str_desc_idx });
}

test "resolveAndCompressVar - flattens redirect chain to concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const c = env.table.freshFromContent(str);
    const b = env.table.freshRedirect(c);
    const a = env.table.freshRedirect(b);

    const result = env.table.resolveAndCompressVar(a);
    try std.testing.expectEqual(result.desc.content, str);
    try std.testing.expectEqual(result.var_, c);
    try std.testing.expectEqual(env.table.slot_store.get(a), Slot{ .redirect = c });
    try std.testing.expectEqual(env.table.slot_store.get(b), Slot{ .redirect = c });
}

// unification - flex_vars

test "unify - identical" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.table.freshFlexVar();
    const desc = try env.getDescForRootVar(a);

    const result = unify(&env.table, &env.vars, a, a);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(desc, try env.getDescForRootVar(a));
}

test "unify - both flex vars" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.table.freshFlexVar();
    const b = env.table.freshFlexVar();

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
}

test "unify - a is flex_var and b is not" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.table.freshFlexVar();
    const b = env.table.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
}

// unification - aliases

test "unify - structural alias with same args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.table.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));
    const alias = Content{ .structural_alias = env.mkAlias("AliasName", &[_]Var{ str, bool_ }, backing) };

    const a = env.table.freshFromContent(alias);
    const b = env.table.freshFromContent(alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{str}));
    const a_alias = Content{ .structural_alias = env.mkAlias("AliasA", &[_]Var{str}, backing) };
    const b_alias = Content{ .structural_alias = env.mkAlias("AliasB", &[_]Var{str}, backing) };

    const a = env.table.freshFromContent(a_alias);
    const b = env.table.freshFromContent(b_alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.table.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.table.freshFromContent(a_alias);
    const b = env.table.freshFromContent(b_alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - structural vs opaque alias with same name and args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{str}));

    const alias_ident = env.mkTypeIdent("Thing");
    const a_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };

    const a = env.table.freshFromContent(a_alias);
    const b = env.table.freshFromContent(b_alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.table.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.table.freshFromContent(a_alias);
    const b = env.table.freshFromContent(b_alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with sam args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.table.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.table.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.table.freshFromContent(a_alias);
    const b = env.table.freshFromContent(b_alias);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/flex_vars

test "unify - a is type_apply and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.table.freshFromContent(str);
    const b = env.table.freshFlexVar();

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a is flex_var and b is type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.table.freshFlexVar();
    const b = env.table.freshFromContent(str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - type_apply

test "unify - a & b are same type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.table.freshFromContent(str);
    const b = env.table.freshFromContent(str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff type_apply (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const bool_ = env.mkTypeApplyNoArg("Bool");

    const a = env.table.freshFromContent(bool_);
    const b = env.table.freshFromContent(str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 1 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.table.freshFromContent(str);

    const maybe_str = env.mkTypeApply1Arg("Maybe", str_var);

    const a = env.table.freshFromContent(maybe_str);
    const b = env.table.freshFromContent(maybe_str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(maybe_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 2 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.table.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.table.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);

    const a = env.table.freshFromContent(result_str_bool);
    const b = env.table.freshFromContent(result_str_bool);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(result_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with flipped args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.table.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.table.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);
    const result_bool_str = env.mkTypeApply2Args("Result", bool_var, str_var);

    const a = env.table.freshFromContent(result_str_bool);
    const b = env.table.freshFromContent(result_bool_str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - tuple

test "unify - a & b are same tuple" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.table.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.table.freshFromContent(bool_);

    const tuple_str_bool = TestEnv.mkTuple(&[_]Var{ str_var, bool_var });

    const a = env.table.freshFromContent(tuple_str_bool);
    const b = env.table.freshFromContent(tuple_str_bool);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(tuple_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are tuples with args flipped (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.table.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.table.freshFromContent(bool_);

    const tuple_str_bool = TestEnv.mkTuple(&[_]Var{ str_var, bool_var });
    const tuple_bool_str = TestEnv.mkTuple(&[_]Var{ bool_var, str_var });

    const a = env.table.freshFromContent(tuple_str_bool);
    const b = env.table.freshFromContent(tuple_bool_str);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - num

test "unify - num flex_var with int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const flex = Content{ .concrete = types.num_flex_var };
    const int_i32 = Content{ .concrete = types.int_i32 };

    const a = env.table.freshFromContent(flex);
    const b = env.table.freshFromContent(int_i32);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - num int(flex_var) with int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.int_flex_var };
    const b_content = Content{ .concrete = types.int_i64 };

    const a = env.table.freshFromContent(a_content);
    const b = env.table.freshFromContent(b_content);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_flex_var };
    const b_content = Content{ .concrete = types.frac_f64 };

    const a = env.table.freshFromContent(a_content);
    const b = env.table.freshFromContent(b_content);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num int concrete == int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const i64a = Content{ .concrete = types.int_i64 };
    const i64b = Content{ .concrete = types.int_i64 };

    const a = env.table.freshFromContent(i64a);
    const b = env.table.freshFromContent(i64b);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(i64b, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac concrete != frac concrete (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_f32 };
    const b_content = Content{ .concrete = types.frac_f64 };

    const a = env.table.freshFromContent(a_content);
    const b = env.table.freshFromContent(b_content);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int concrete != int concrete (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.int_i32 };
    const b_content = Content{ .concrete = types.int_i64 };

    const a = env.table.freshFromContent(a_content);
    const b = env.table.freshFromContent(b_content);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int vs frac (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_desc = Content{ .concrete = types.int_i32 };
    const frac_desc = Content{ .concrete = types.frac_f32 };

    const a = env.table.freshFromContent(int_desc);
    const b = env.table.freshFromContent(frac_desc);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac(flex_var)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_flex_var };
    const b_content = Content{ .concrete = types.frac_flex_var };

    const a = env.table.freshFromContent(a_content);
    const b = env.table.freshFromContent(b_content);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - func

test "unify - func are same" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.table.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.table.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = TestEnv.mkFunc(&[_]Var{ str, num }, int_i32);

    const a = env.table.freshFromContent(func);
    const b = env.table.freshFromContent(func);

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.table.freshFromContent(Content{ .concrete = types.int_i32 });
    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.table.freshFromContent(TestEnv.mkFunc(&[_]Var{int_i32}, str));
    const b = env.table.freshFromContent(TestEnv.mkFunc(&[_]Var{str}, str));

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return types (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.table.freshFromContent(Content{ .concrete = types.int_i32 });
    const str = env.table.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.table.freshFromContent(TestEnv.mkFunc(&[_]Var{str}, int_i32));
    const b = env.table.freshFromContent(TestEnv.mkFunc(&[_]Var{str}, str));

    const result = unify(&env.table, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.table.slot_store.get(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

////////////////////////////////////////////////////////////////////////////////
// OLD
////////////////////////////////////////////////////////////////////////////////

// unification - structural aliases

// test "unify - structural_alias - same name and args unifies" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);

//     const a_content = Content{
//         .structural_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const b_content = a_content;

//     const a_var = solver.freshFromContent(a_content);
//     const b_var = solver.freshFromContent(b_content);

//     try std.testing.expectEqual(solver.unify_old(a_var, b_var), null);
// }

// test "unify - structural_alias - same name and args do not unify" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const bool_ = solver.freshFromContent(Content.bool_old);

//     const a_content = Content{
//         .structural_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };
//     const a = solver.freshFromContent(a_content);

//     const mb_err = solver.unify_old(a, bool_);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .apply_name_mismatch = .{
//             .left = TypeNameOld.str,
//             .right = TypeNameOld.bool_,
//         },
//     });
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a, .right = bool_ },
//     //         .{ .left = str, .right = bool_ },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }

// test "unify - structural_alias - diff types do not unify" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const bool_ = solver.freshFromContent(Content.bool_old);

//     const a = Content{
//         .structural_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const b = Content{
//         .structural_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{bool_}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const a_var = solver.freshFromContent(a);
//     const b_var = solver.freshFromContent(b);

//     const mb_err = solver.unify_old(a_var, b_var);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .apply_name_mismatch = .{
//             .left = TypeNameOld.str,
//             .right = TypeNameOld.bool_,
//         },
//     });
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a_var, .right = b_var },
//     //         .{ .left = str, .right = bool_ },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }

// // unification - nominal aliases

// test "unify - nominal_alias - identical vars unify" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);

//     const desc = Content{
//         .opaque_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const nominal_var = solver.freshFromContent(desc);

//     try std.testing.expectEqual(solver.unify_old(nominal_var, nominal_var), null);
// }

// test "unify - nominal_alias - diff names do not unify" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);

//     const a_nominal_name: TypeNameOld = @enumFromInt(100);
//     const a_nominal = Content{
//         .opaque_alias = .{
//             .ident = a_nominal_name,
//             .args = try Content.Alias.ArgsArray.init(0),
//             .backing_var = str,
//         },
//     };

//     const b_nominal_name: TypeNameOld = @enumFromInt(200);
//     const b_nominal = Content{
//         .opaque_alias = .{
//             .ident = b_nominal_name,
//             .args = try Content.Alias.ArgsArray.init(0),
//             .backing_var = str,
//         },
//     };

//     const a = solver.freshFromContent(a_nominal);
//     const b = solver.freshFromContent(b_nominal);

//     const mb_err = solver.unify_old(a, b);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .nominal_alias_name_mismatch = .{
//             .left = a_nominal_name,
//             .right = b_nominal_name,
//         },
//     });
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a, .right = b },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }

// test "unify - nominal_alias - same name names diff args" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const bool_ = solver.freshFromContent(Content.bool_old);

//     const a_nominal_name: TypeNameOld = @enumFromInt(100);
//     const a_nominal = Content{
//         .opaque_alias = .{
//             .ident = a_nominal_name,
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const b_nominal = Content{
//         .opaque_alias = .{
//             .ident = a_nominal_name,
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{bool_}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const a = solver.freshFromContent(a_nominal);
//     const b = solver.freshFromContent(b_nominal);

//     const mb_err = solver.unify_old(a, b);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .apply_name_mismatch = .{
//             .left = TypeNameOld.str,
//             .right = TypeNameOld.bool_,
//         },
//     });
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a, .right = b },
//     //         .{ .left = str, .right = bool_ },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }

// test "unify - nominal_alias vs flat_type - fails" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);

//     const nominal = Content{
//         .opaque_alias = .{
//             .ident = @enumFromInt(300),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const nominal_var = solver.freshFromContent(nominal);
//     const flat_var = solver.freshFromContent(Content.str_old);

//     const mb_err = solver.unify_old(nominal_var, flat_var);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .type_mismatch = .{
//             .left = nominal,
//             .right = Content.str_old,
//         },
//     });
// }

// test "unify - nominal_alias vs structural_alias - fails" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);

//     const nominal = Content{
//         .opaque_alias = .{
//             .ident = @enumFromInt(100),
//             .args = Content.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
//             .backing_var = str,
//         },
//     };

//     const structural = Content{
//         .structural_alias = .{
//             .ident = @enumFromInt(200),
//             .args = try Content.Alias.ArgsArray.init(0),
//             .backing_var = str,
//         },
//     };

//     const nominal_var = solver.freshFromContent(nominal);
//     const structural_var = solver.freshFromContent(structural);

//     const mb_err = solver.unify_old(nominal_var, structural_var);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .type_mismatch = .{
//             .left = nominal,
//             .right = Content.str_old,
//         },
//     });
// }

// // unification - flat type - func

// test "unify - func - same args and return type unifies" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const ret = solver.freshFromContent(Content.bool_old);

//     const a_func = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
//     } });

//     const b_func = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
//     } });

//     try std.testing.expectEqual(solver.unify_old(a_func, b_func), null);
// }

// test "unify - func - different arity fails" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const bool_var = solver.freshFromContent(Content.bool_old);

//     const one_arg = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = bool_var },
//     } });
//     const two_args = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{ str, str }), .ret = bool_var },
//     } });

//     const mb_err = solver.unify_old(one_arg, two_args);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .func_arity_mismatch = .{ .left = 1, .right = 2 },
//     });
//     // try std.testing.expectEqual(1, err.trace.len);
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{.{ .left = one_arg, .right = two_args }},
//     //     err.getTraceSlice(),
//     // );
// }

// test "unify - func - same arity, differing arg types fails" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const bool_var = solver.freshFromContent(Content.bool_old);
//     const ret = solver.freshFromContent(Content.str_old);

//     const a = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
//     } });

//     const b = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{bool_var}), .ret = ret },
//     } });

//     const mb_err = solver.unify_old(a, b);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .apply_name_mismatch = .{ .left = TypeNameOld.str, .right = TypeNameOld.bool_ },
//     });
//     // try std.testing.expectEqual(2, err.trace.len);
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a, .right = b },
//     //         .{ .left = str, .right = bool_var },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }

// test "unify - func - same args, different return types fails" {
//     const gpa = std.testing.allocator;
//     var solver = UnificationTable.init(gpa);
//     defer solver.deinit();

//     const str = solver.freshFromContent(Content.str_old);
//     const ret1 = solver.freshFromContent(Content.str_old);
//     const ret2 = solver.freshFromContent(Content.bool_old);

//     const a = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret1 },
//     } });

//     const b = solver.freshFromContent(.{ .concrete = .{
//         .func = .{ .args = try types.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret2 },
//     } });

//     const mb_err = solver.unify_old(a, b);
//     try std.testing.expect(mb_err != null);

//     const err = mb_err.?;
//     try std.testing.expectEqual(err.cause, UnificationTable.UnifyError.Cause{
//         .apply_name_mismatch = .{ .left = TypeNameOld.str, .right = TypeNameOld.bool_ },
//     });
//     // try std.testing.expectEqual(2, err.trace.len);
//     // try std.testing.expectEqualSlices(
//     //     Solver.UnifyError.TraceVars,
//     //     &[_]Solver.UnifyError.TraceVars{
//     //         .{ .left = a, .right = b },
//     //         .{ .left = ret1, .right = ret2 },
//     //     },
//     //     err.getTraceSlice(),
//     // );
// }
