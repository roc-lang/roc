const std = @import("std");

const exitOnOutOfMemory = @import("../collections.zig").utils.exitOnOom;
const Region = @import("../base/Region.zig");
const Ident = @import("../base/Ident.zig");
const store = @import("./store.zig");
const types = @import("./types.zig");

const Slot = store.Slot;
const ResolvedVarDesc = store.ResolvedVarDesc;
const ResolvedVarDescs = store.ResolvedVarDescs;

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

/// Unify two type variables
///
/// This function
/// * Resolve type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
pub fn unify(types_store: *store.Store, vars: *VarArray, a: Var, b: Var) Result {
    var unify_pool = UnifyPool.init(types_store, vars);
    unify_pool.unifyGuarded(a, b) catch |err| switch (err) {
        error.TypeMismatch => {
            types_store.union_(a, b, .{
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

/// Unifciation with a pool of variables
const UnifyPool = struct {
    const Self = @This();

    types_store: *store.Store,
    vars: *VarArray,

    /// Init a unify_pool
    /// Caller owns the memory of the provided values
    fn init(types_store: *store.Store, vars: *VarArray) Self {
        return .{ .types_store = types_store, .vars = vars };
    }

    // merge

    /// Link the variables & updated the content in the type_store
    /// In the old compiler, this function was called "merge"
    fn merge(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) void {
        self.types_store.union_(vars.a.var_, vars.b.var_, .{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
    }

    // unification

    /// Error thrown during unification when there's a type mismatch
    const Error = error{TypeMismatch};

    /// Unify checking for equivalance
    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) error{TypeMismatch}!void {
        switch (self.types_store.checkVarsEquiv(a_var, b_var)) {
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
        switch (b_content) {
            .flex_var => self.merge(vars, .effectful),
            .effectful => self.merge(vars, .effectful),
            .err => self.merge(vars, .err),
            else => return error.TypeMismatch,
        }
    }

    // Unify pure //

    /// Unify when `a` was a pure
    fn unifyPure(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => self.merge(vars, .pure),
            .pure => self.merge(vars, .pure),
            .effectful => self.merge(vars, .effectful),
            .err => self.merge(vars, .err),
            else => return error.TypeMismatch,
        }
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
        try self.unifyGuarded(a_func.eff, b_func.eff);

        self.merge(vars, vars.b.desc.content);
    }
};

// tests //

/// Environment to make setting up tests easier
const TestEnv = struct {
    const Self = @This();

    gpa: std.mem.Allocator,
    ident_store: Ident.Store,
    types_store: store.Store,
    vars: VarArray,

    pub fn init(gpa: std.mem.Allocator) Self {
        return .{
            .gpa = gpa,
            .ident_store = Ident.Store.initCapacity(gpa, 16),
            .types_store = store.Store.init(gpa),
            .vars = VarArray.init(0) catch unreachable,
        };
    }

    pub fn deinit(self: *Self) void {
        self.ident_store.deinit(self.gpa);
        self.types_store.deinit();
    }

    const Error = error{VarIsNotRoot};

    /// Get a desc from a root var
    pub fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.types_store.getSlot(var_)) {
            .root => |desc_idx| return self.types_store.getDesc(desc_idx),
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

    fn mkFunc(args: []const Var, ret: Var, eff: Var) Content {
        std.debug.assert(args.len <= Func.arg_capacity);
        const args_arr = Func.ArgsArray.fromSlice(args) catch unreachable;
        return Content{ .concrete = .{ .func = .{ .args = args_arr, .ret = ret, .eff = eff } } };
    }

    fn mkFuncFlex(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.{ .flex_var = null });
        return Self.mkFunc(args, ret, eff_var);
    }

    fn mkFuncPure(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.pure);
        return Self.mkFunc(args, ret, eff_var);
    }

    fn mkFuncEff(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.effectful);
        return Self.mkFunc(args, ret, eff_var);
    }
};

// unification - flex_vars

test "unify - identical" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const desc = try env.getDescForRootVar(a);

    const result = unify(&env.types_store, &env.vars, a, a);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(desc, try env.getDescForRootVar(a));
}

test "unify - both flex vars" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

test "unify - a is flex_var and b is not" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

// unification - aliases

test "unify - structural alias with same args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));
    const alias = Content{ .structural_alias = env.mkAlias("AliasName", &[_]Var{ str, bool_ }, backing) };

    const a = env.types_store.freshFromContent(alias);
    const b = env.types_store.freshFromContent(alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{str}));
    const a_alias = Content{ .structural_alias = env.mkAlias("AliasA", &[_]Var{str}, backing) };
    const b_alias = Content{ .structural_alias = env.mkAlias("AliasB", &[_]Var{str}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - structural vs opaque alias with same name and args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{str}));

    const alias_ident = env.mkTypeIdent("Thing");
    const a_alias = Content{ .structural_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with sam args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(TestEnv.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };
    const b_alias = Content{ .opaque_alias = TestEnv.mkAliasFromIdent(alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

// unification - pure/effectful

test "unify - pure with pure" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.pure);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.pure, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with effectful" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.effectful);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.pure, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with effectful" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.effectful);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with pure (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.pure);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with err (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.err);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with err (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.err);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with concrete type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

test "unify - effectful with concrete type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

// unification - concrete/flex_vars

test "unify - a is type_apply and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a is flex_var and b is type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - type_apply

test "unify - a & b are same type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff type_apply (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const bool_ = env.mkTypeApplyNoArg("Bool");

    const a = env.types_store.freshFromContent(bool_);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 1 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const maybe_str = env.mkTypeApply1Arg("Maybe", str_var);

    const a = env.types_store.freshFromContent(maybe_str);
    const b = env.types_store.freshFromContent(maybe_str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(maybe_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 2 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);

    const a = env.types_store.freshFromContent(result_str_bool);
    const b = env.types_store.freshFromContent(result_str_bool);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(result_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with flipped args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);
    const result_bool_str = env.mkTypeApply2Args("Result", bool_var, str_var);

    const a = env.types_store.freshFromContent(result_str_bool);
    const b = env.types_store.freshFromContent(result_bool_str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - tuple

test "unify - a & b are same tuple" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = TestEnv.mkTuple(&[_]Var{ str_var, bool_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_str_bool);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(tuple_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are tuples with args flipped (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = TestEnv.mkTuple(&[_]Var{ str_var, bool_var });
    const tuple_bool_str = TestEnv.mkTuple(&[_]Var{ bool_var, str_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_bool_str);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - num

test "unify - num flex_var with int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const flex = Content{ .concrete = types.num_flex_var };
    const int_i32 = Content{ .concrete = types.int_i32 };

    const a = env.types_store.freshFromContent(flex);
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - num int(flex_var) with int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.int_flex_var };
    const b_content = Content{ .concrete = types.int_i64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_flex_var };
    const b_content = Content{ .concrete = types.frac_f64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num int concrete == int concrete" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const i64a = Content{ .concrete = types.int_i64 };
    const i64b = Content{ .concrete = types.int_i64 };

    const a = env.types_store.freshFromContent(i64a);
    const b = env.types_store.freshFromContent(i64b);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(i64b, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac concrete != frac concrete (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_f32 };
    const b_content = Content{ .concrete = types.frac_f64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int concrete != int concrete (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.int_i32 };
    const b_content = Content{ .concrete = types.int_i64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int vs frac (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_desc = Content{ .concrete = types.int_i32 };
    const frac_desc = Content{ .concrete = types.frac_f32 };

    const a = env.types_store.freshFromContent(int_desc);
    const b = env.types_store.freshFromContent(frac_desc);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac(flex_var)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .concrete = types.frac_flex_var };
    const b_content = Content{ .concrete = types.frac_flex_var };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

// unification - concrete/concrete - func

test "unify - func are same" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncFlex(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{int_i32}, str));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return types (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, int_i32));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs pure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs effectful" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first eff, second pure (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(eff_func);
    const b = env.types_store.freshFromContent(pure_func);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first pure, second eff" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .concrete = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .concrete = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(pure_func);
    const b = env.types_store.freshFromContent(eff_func);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(eff_func, (try env.getDescForRootVar(b)).content);
}
