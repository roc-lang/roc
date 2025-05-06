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
const Record = types.Record;
const RecordField = types.RecordField;
const RecordFieldArray = types.RecordField.Array;
const RecordFieldsArray = types.RecordFields.Array;

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

    /// Create a new type variable *in this pool*
    fn fresh(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) Var {
        const var_ = self.types_store.register(.{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
        self.vars.append(var_) catch |err| switch (err) {
            error.Overflow => @panic("Pool var list overflowed"),
        };
        return var_;
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
                        try self.unifyTypeApply(vars, a_type_apply, b_type_apply);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        try self.unifyTuple(vars, a_tuple, b_tuple);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .num => |a_num| {
                switch (b_flat_type) {
                    .num => |b_num| {
                        try self.unifyNum(vars, a_num, b_num);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .func => |a_func| {
                switch (b_flat_type) {
                    .func => |b_func| {
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record => |a_record| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_record.areAllFieldsOptional()) {
                            try self.unifyGuarded(a_record.ext, vars.b.var_);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        try self.unifyTwoRecords(vars, a_record, b_record);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_record => {
                switch (b_flat_type) {
                    .empty_record => {
                        self.merge(vars, Content{ .concrete = .empty_record });
                    },
                    .record => |b_record| {
                        if (b_record.areAllFieldsOptional()) {
                            try self.unifyGuarded(vars.a.var_, b_record.ext);
                        } else {
                            return error.TypeMismatch;
                        }
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
    fn unifyTypeApply(
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
    fn unifyTuple(
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
    fn unifyNum(
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
    fn unifyFunc(
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

    /// unify two records
    ///
    /// this:
    /// * unwraps extensible variable to get all records fields
    /// * checks that parallel args unify
    fn unifyTwoRecords(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_record: Record,
        b_record: Record,
    ) error{TypeMismatch}!void {
        // First, unwrap all fields for record a, panicaing with various non-recoverable error
        // These pancis will likely be changed/removed in the future
        var a_fields = RecordFieldArray.fromSlice(a_record.fields.slice()) catch unreachable;
        var a_ext = b_record.ext;
        switch (self.gatherRecordFields(a_record, &a_fields)) {
            .ok => |next_ext| {
                a_ext = next_ext;
            },
            .invalid_ext => @panic("unifyTwoRecords: Record a ext was invalid"),
            .too_many_fields => @panic("unifyTwoRecords: Record a had more than 16 fields"),
        }

        var b_fields = RecordFieldArray.fromSlice(b_record.fields.slice()) catch unreachable;
        var b_ext = b_record.ext;
        switch (self.gatherRecordFields(b_record, &b_fields)) {
            .ok => |next_ext| {
                b_ext = next_ext;
            },
            .invalid_ext => @panic("unifyTwoRecords: Record a ext was invalid"),
            .too_many_fields => @panic("unifyTwoRecords: Record a had more than 16 fields"),
        }

        // Then partition the fields
        var partitioned = RecordField.Partitioned.init();
        RecordField.parition(&a_fields, &b_fields, &partitioned) catch |err| switch (err) {
            error.Overflow => @panic("unifyTwoRecords: Paritioning of records a and b had more than 16 fields"),
        };

        // Determine how the fields of a & b extend
        const a_has_uniq_fields = partitioned.only_in_a.len > 0;
        const b_has_uniq_fields = partitioned.only_in_b.len > 0;

        var fields_ext: FieldsExtension = .exactly_the_same;
        if (a_has_uniq_fields and b_has_uniq_fields) {
            fields_ext = .both_extend;
        } else if (a_has_uniq_fields) {
            fields_ext = .a_extends_b;
        } else if (b_has_uniq_fields) {
            fields_ext = .b_extends_a;
        }

        // Unify fields
        switch (fields_ext) {
            .exactly_the_same => {
                // Unify exts (these will both be the empty record)
                try self.unifyGuarded(a_ext, b_ext);

                // Unify shared fields
                self.unifySharedFields(vars, &partitioned.in_both, null, a_ext) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when unify share records for 'exactly_the_same'"),
                    error.TypeMismatch => return error.TypeMismatch,
                };
            },
            .a_extends_b => {
                // Create a new variable of a record with only a's uniq fields
                const only_in_a_var = self.fresh(vars, Content{ .concrete = FlatType{ .record = .{
                    .fields = partitioned.only_in_a,
                    .ext = a_ext,
                } } });

                // Unify the sub record with b's ext
                try self.unifyGuarded(only_in_a_var, b_ext);

                // Unify shared fields
                self.unifySharedFields(vars, &partitioned.in_both, null, only_in_a_var) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when unify share records for 'a_extends_b'"),
                    error.TypeMismatch => return error.TypeMismatch,
                };
            },
            .b_extends_a => {
                // Create a new variable of a record with only b's uniq fields
                const only_in_b_var = self.fresh(vars, Content{ .concrete = FlatType{ .record = .{
                    .fields = partitioned.only_in_b,
                    .ext = b_ext,
                } } });

                // Unify the sub record with a's ext
                try self.unifyGuarded(a_ext, only_in_b_var);

                // Unify shared fields
                self.unifySharedFields(vars, &partitioned.in_both, null, only_in_b_var) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when unify share records for 'b_extends_a'"),
                    error.TypeMismatch => return error.TypeMismatch,
                };
            },
            .both_extend => {
                // Create a new variable of a record with only b's uniq fields
                const only_in_a_var = self.fresh(vars, Content{ .concrete = FlatType{ .record = .{
                    .fields = partitioned.only_in_a,
                    .ext = a_ext,
                } } });
                const only_in_b_var = self.fresh(vars, Content{ .concrete = FlatType{ .record = .{
                    .fields = partitioned.only_in_b,
                    .ext = b_ext,
                } } });
                const new_ext_var = self.fresh(vars, .{ .flex_var = null });

                // Unify the sub records with exts
                try self.unifyGuarded(a_ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_ext); // FIXED HERE

                // Create a new array of all non-shared fields
                var extended_fields = RecordFieldArray.init(0) catch unreachable;
                extended_fields.appendSlice(partitioned.only_in_a.slice()) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when creating shared fields for 'b_extends_a'"),
                };
                extended_fields.appendSlice(partitioned.only_in_b.slice()) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when creating shared fields for 'b_extends_a'"),
                };

                // Unify shared fields
                self.unifySharedFields(vars, &partitioned.in_both, &extended_fields, new_ext_var) catch |err| switch (err) {
                    error.Overflow => @panic("unifyTwoRecords: Overflowed when unify share records for 'b_extends_a'"),
                    error.TypeMismatch => return error.TypeMismatch,
                };
            },
        }
    }

    /// Given a list of shared fields & a list of extended fields, unify the shared
    /// Then merge a new record with both shared+extended fields
    fn unifySharedFields(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_fields: *RecordFieldsArray,
        mb_extended_fields: ?*RecordFieldArray,
        ext: Var,
    ) error{ Overflow, TypeMismatch }!void {
        // Create a new array of to hold all fields
        var combined_fields = try RecordFieldArray.init(0);

        // Unify shared fields and append
        for (0..shared_fields.len) |i| {
            const fields = shared_fields.get(i);
            try self.unifyGuarded(fields.a.var_, fields.b.var_);
            try combined_fields.append(fields.a);
        }

        // Append combined fields
        if (mb_extended_fields) |extended_fields| {
            try combined_fields.appendSlice(extended_fields.slice());
        }

        // Merge vars
        self.merge(vars, Content{ .concrete = FlatType{ .record = .{
            .fields = combined_fields,
            .ext = ext,
        } } });
    }

    const FieldsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const GatherFieldsResult = union(enum) { ok: Var, invalid_ext, too_many_fields };

    /// Extend a record's `ext` variable, gathering all fields into an array.
    /// Gathered fields are appended to the `fields` array
    fn gatherRecordFields(self: *Self, record: Record, fields: *RecordFieldArray) GatherFieldsResult {
        var ext_var = record.ext;
        while (true) {
            switch (self.types_store.resolveVar(ext_var).desc.content) {
                // TODO: rigid var!
                .flex_var => {
                    return .{ .ok = ext_var };
                },
                .structural_alias => |alias| {
                    ext_var = alias.backing_var;
                },
                .opaque_alias => |alias| {
                    ext_var = alias.backing_var;
                },
                .concrete => |flat_type| {
                    switch (flat_type) {
                        .record => |ext_record| {
                            fields.appendSlice(ext_record.fields.slice()) catch |err| switch (err) {
                                error.Overflow => return .too_many_fields,
                            };
                            ext_var = ext_record.ext;
                        },
                        .empty_record => {
                            return .{ .ok = ext_var };
                        },
                        else => return .invalid_ext,
                    }
                },
                else => return .invalid_ext,
            }
        }
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

    const Error = error{ VarIsNotRoot, DescIsNotRecord };

    /// Get a desc from a root var
    pub fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.types_store.getSlot(var_)) {
            .root => |desc_idx| return self.types_store.getDesc(desc_idx),
            .redirect => return error.VarIsNotRoot,
        }
    }

    /// Unwrap a record or throw
    pub fn getRecordOrErr(desc: Desc) error{DescIsNotRecord}!Record {
        switch (desc.content) {
            .concrete => |flat_type| {
                switch (flat_type) {
                    .record => |record| {
                        return record;
                    },
                    else => return error.DescIsNotRecord,
                }
            },
            else => return error.DescIsNotRecord,
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

    // helpers - concrete - func

    fn mkRecord(fields: []const RecordField, ext_var: Var) Content {
        std.debug.assert(fields.len <= RecordField.array_capacity);
        const fields_arr = RecordFieldArray.fromSlice(fields) catch unreachable;
        return Content{ .concrete = .{ .record = .{ .fields = fields_arr, .ext = ext_var } } };
    }

    fn mkRecordOpen(self: *Self, fields: []const RecordField) Content {
        const ext_var = self.types_store.freshFromContent(.{ .flex_var = null });
        return Self.mkRecord(fields, ext_var);
    }

    fn mkRecordClosed(self: *Self, fields: []const RecordField) Content {
        const ext_var = self.types_store.freshFromContent(.{ .concrete = .empty_record });
        return Self.mkRecord(fields, ext_var);
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

// unification - concrete/concrete - records closed

test "unify - identical closed records" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const fields = [_]RecordField{
        .{ .name = @enumFromInt(1), .var_ = str, .typ = .required },
    };
    const record = env.mkRecordClosed(&fields);

    const a = env.types_store.freshFromContent(record);
    const b = env.types_store.freshFromContent(record);

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(record, (try env.getDescForRootVar(b)).content);
}

test "unify - closed record mismatch on diff fields (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field1 = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field2 = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const a = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{ field1, field2 }));
    const b = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field1}));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - concrete/concrete - records open

test "unify - open record a extends b" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_a_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };

    const a = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only }));
    const b = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_shared}));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len);
    try std.testing.expectEqual(field_shared, b_record.fields.get(0));

    try std.testing.expectEqual(1, env.vars.len);
    try std.testing.expectEqual(env.vars.get(0), b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len);
    try std.testing.expectEqual(field_a_only, b_ext_record.fields.get(0));

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);
}

test "unify - open record b extends a" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_b_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };

    const a = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_shared}));
    const b = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only }));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len);
    try std.testing.expectEqual(field_shared, b_record.fields.get(0));

    try std.testing.expectEqual(1, env.vars.len);
    try std.testing.expectEqual(env.vars.get(0), b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len);
    try std.testing.expectEqual(field_b_only, b_ext_record.fields.get(0));

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);
}

test "unify - both extend open record" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_a_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };
    const field_b_only = RecordField{ .name = @enumFromInt(3), .var_ = bool_, .typ = .required };

    const a = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only }));
    const b = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only }));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_record.fields.len);
    try std.testing.expectEqual(field_shared, b_record.fields.get(0));

    const b_ext = env.types_store.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    try std.testing.expectEqual(3, env.vars.len);

    const only_a_var = env.vars.get(0);
    const only_a_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_record.fields.len);
    try std.testing.expectEqual(field_a_only, only_a_record.fields.get(0));

    const only_b_var = env.vars.get(1);
    const only_b_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_record.fields.len);
    try std.testing.expectEqual(field_b_only, only_b_record.fields.get(0));

    const ext_var = env.vars.get(2);
    const ext_content = env.types_store.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - record mismatch on shared field (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_a = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_b = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const a = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_a}));
    const b = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_b}));

    const result = unify(&env.types_store, &env.vars, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - concrete/concrete - records open+closed

test "unify - open record extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field_x = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_y = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_x, field_y }));
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x}));

    const result = unify(&env.types_store, &env.vars, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(closed)).content);
}

test "unify - closed record extends open" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field_x = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_y = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x}));
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{ field_x, field_y }));

    const result = unify(&env.types_store, &env.vars, open, closed);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
}

test "unify - open vs closed with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_x_str = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_x_int = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}));
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}));

    const result = unify(&env.types_store, &env.vars, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));

    const desc = try env.getDescForRootVar(closed);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_x_str = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_x_int = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}));
    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}));

    const result = unify(&env.types_store, &env.vars, closed, open);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = open }, env.types_store.getSlot(closed));

    const desc = try env.getDescForRootVar(open);
    try std.testing.expectEqual(Content.err, desc.content);
}
