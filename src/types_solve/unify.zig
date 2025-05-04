const std = @import("std");
const exitOnOutOfMemory = @import("../collections.zig").utils.exitOnOom;

const shared = @import("./shared.zig");
const FlatType = @import("./flat_type.zig");

const TypeName = FlatType.TypeName;
const Var = shared.Var;
const ElemsInt = shared.ElemsInt;
const MAX_ELEMS = shared.MAX_ELEMS;

/// Represents what the a type *is*
///
/// Numbers are special cased here. This means that when constraints, types
/// like `Num(Int(Unsigned64))` should be reperesntsed as it's specific
/// `flat_type.num` *not* as `flat_type.apply`. See 'Num' struct for additional
/// details
const Desc = union(enum) {
    flex_var,
    structural_alias: Alias,
    nominal_alias: Alias,
    flat_type: FlatType.FlatType,

    // a nominal or structural alias
    // can hold up to 16 arguments
    const Alias = struct {
        name: TypeName,
        args: ArgsArray,
        backing_var: Var,

        /// Represents the max capacity of the args array
        pub const args_array_capacity = 16;
        /// Bounded array to hold args
        pub const ArgsArray = std.BoundedArray(Var, args_array_capacity);
    };

    const SelfD = @This();

    // Constants

    const bool_: SelfD = SelfD{
        .flat_type = .{ .type_apply = .{
            .name = TypeName.bool_,
            .args = FlatType.TypeApply.ArgsArray.init(0) catch unreachable,
        } },
    };
    const str: SelfD = SelfD{
        .flat_type = .{ .type_apply = .{
            .name = TypeName.str,
            .args = FlatType.TypeApply.ArgsArray.init(0) catch unreachable,
        } },
    };

    /// make a List with the provided type applied
    pub fn mkList(v: Var) SelfD {
        var args = FlatType.TypeApply.ArgsArray.init(0) catch unreachable;
        args.append(v) catch unreachable;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = TypeName.list, .args = args } } };
    }

    /// make a Maybe with the provided type applied
    pub fn mkMaybe(v: Var) SelfD {
        var args = FlatType.TypeApply.ArgsArray.init(0) catch unreachable;
        args.append(v) catch unreachable;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = TypeName.maybe, .args = args } } };
    }

    /// make a List with the provided type applied
    pub fn mkResult(ok: Var, err: Var) SelfD {
        var args = FlatType.TypeApply.ArgsArray.init(0) catch unreachable;
        args.append(ok) catch unreachable;
        args.append(err) catch unreachable;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = TypeName.result, .args = args } } };
    }

    /// make a List with the provided type applied
    pub fn mkTuple(slice: []const Var) SelfD {
        const args = FlatType.Tuple.ElemsArray.fromSlice(slice) catch unreachable;
        return SelfD{ .flat_type = .{ .tuple = .{ .elems = args } } };
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

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Desc {
        return self.backing.items[@intFromEnum(idx)];
    }
};

// Reperents either type data *or* a symlink to another type variable
const Binding = union(enum) {
    root: DescStore.Idx,
    redirect: Var,
};

/// Represents a store of bindings
const BindingStore = struct {
    const Self = @This();

    backing: std.ArrayList(Binding),

    fn init(gpa: std.mem.Allocator, capacity: usize) Self {
        const arr_list = std.ArrayList(Binding).initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
        return .{ .backing = arr_list };
    }

    fn deinit(self: *Self) void {
        self.backing.deinit();
    }

    /// Insert a new binding into the store. The return type is a new `Var`.
    ///
    /// It's important to note that this `Var` is an index into the store,
    /// but it also also the type variable itself
    fn insert(self: *Self, typ: Binding) Var {
        const idx: Var = @enumFromInt(self.backing.items.len);
        self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
        return idx;
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Var, val: Binding) void {
        self.backing.items[@intFromEnum(idx)] = val;
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Var) Binding {
        return self.backing.items[@intFromEnum(idx)];
    }
};

/// Type solver
const Solver = struct {
    descriptor_store: DescStore,
    binding_store: BindingStore,

    const Self = @This();

    /// Init the solver
    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: eventually use herusitics here to determin sensible defaults
        return .{
            .descriptor_store = DescStore.init(gpa, 64),
            .binding_store = BindingStore.init(gpa, 64),
        };
    }

    /// Deinit the solver
    pub fn deinit(self: *Self) void {
        self.descriptor_store.deinit();
        self.binding_store.deinit();
    }

    /// Create a new unbound, flexible type variable
    pub fn fresh(self: *Self) Var {
        const flex_var_idx = self.descriptor_store.insert(Desc.flex_var);
        const binding_var = self.binding_store.insert(.{ .root = flex_var_idx });
        return binding_var;
    }

    /// Create a bound variable with the provided descriptor
    /// Mainly used for tests
    pub fn mkRoot(self: *Self, desc: Desc) Var {
        const desc_idx = self.descriptor_store.insert(desc);
        const binding_var = self.binding_store.insert(.{ .root = desc_idx });
        return binding_var;
    }

    /// Create a bound variable redirecting to the provided var
    /// Mainly used for tests
    pub fn mkRedirect(self: *Self, redirect_var: Var) Var {
        const binding_var = self.binding_store.insert(.{ .redirect = redirect_var });
        return binding_var;
    }

    const VarAndDesc = struct {
        type_var: Var,
        type_descriptor: Desc,
    };

    /// Given a type var, follow all redirects until finding the root descriptor, then
    /// compress the path
    ///
    /// if the type is not a redirect, the var return will match the one passed in
    fn followRedirectsAndCompress(self: *Self, initial_var: Var) VarAndDesc {
        // first, we follow the chain down to the concrete type
        var redirected_var = initial_var;
        var redirected_binding: Binding = self.binding_store.get(initial_var);
        while (true) {
            switch (redirected_binding) {
                .redirect => |redirect_idx| {
                    redirected_var = redirect_idx;
                    redirected_binding = self.binding_store.get(redirect_idx);
                },
                .root => |_| break,
            }
        }

        // then follow the chain again, but compressing each step to the concrete type
        if (initial_var != redirected_var) {
            var compressed_idx = initial_var;
            var compressed_typ: Binding = self.binding_store.get(initial_var);
            while (true) {
                switch (compressed_typ) {
                    .redirect => |redirect_idx| {
                        self.binding_store.set(compressed_idx, Binding{ .redirect = redirected_var });
                        compressed_idx = redirect_idx;
                        compressed_typ = self.binding_store.get(redirect_idx);
                    },
                    .root => |_| break,
                }
            }
        }

        // TODO: refactor to remove panic
        switch (redirected_binding) {
            .redirect => |_| @panic("redirected type was still redriect after following chain"),
            .root => |content_idx| {
                return .{ .type_descriptor = self.descriptor_store.get(content_idx), .type_var = redirected_var };
            },
        }
    }

    const AreEqual = union(enum) {
        are_equiv,
        are_not_equiv: struct { a: VarAndDesc, b: VarAndDesc },
    };

    /// Check if two variables are equivalant
    /// This will follow all redirects and compress the path
    fn areVarsEquiv(self: *Self, a_var: Var, b_var: Var) AreEqual {
        const a_val = self.followRedirectsAndCompress(a_var);
        const b_val = self.followRedirectsAndCompress(b_var);
        if (a_val.type_var == b_val.type_var) {
            return .are_equiv;
        }
        return .{ .are_not_equiv = .{ .a = a_val, .b = b_val } };
    }

    /// Unify two types
    /// If this returns null, then unification was successful
    pub fn unify(self: *Self, a_type_var: Var, b_type_var: Var) ?UnifyError {
        var unify_err = UnifyError.init();
        if (self.unify_help(&unify_err, a_type_var, b_type_var)) |did_unify| {
            switch (did_unify) {
                .ok => return null,
                .err => return unify_err,
            }
        } else |err| switch (err) {
            error.Overflow => {
                unify_err.cause = UnifyError.Cause{ .overflowed_trace = .{
                    .left = a_type_var,
                    .right = b_type_var,
                } };
                return unify_err;
            },
        }
    }

    // pub fn unify_guarded(self: *Self, a_var: Var, b_var: Var) DidUnify {
    //     switch (self.areVarsEquiv(a_var, b_var)) {
    //         .are_equiv => return null,
    //         .are_not_equiv => |a_data, b_data| => {
    //         }
    //     }
    // }

    // Unify two types, updating the error context
    fn unify_help(self: *Self, mb_err: *UnifyError, a_type_var: Var, b_type_var: Var) error{Overflow}!DidUnify {
        try mb_err.appendTrace(a_type_var, b_type_var);

        const a_type = self.followRedirectsAndCompress(a_type_var);
        const b_type = self.followRedirectsAndCompress(b_type_var);

        switch (a_type.type_descriptor) {
            .flex_var => {
                // if both a & b are flex vars, symlink link a -> b
                self.binding_store.set(a_type_var, Binding{ .redirect = b_type.type_var });
                return DidUnify.ok;
            },
            .structural_alias => |a_alias| {
                switch (b_type.type_descriptor) {
                    .flex_var => {
                        // a is concrete and b is a flex_var, redirect b -> a
                        self.binding_store.set(b_type_var, Binding{ .redirect = a_type.type_var });
                        return DidUnify.ok;
                    },
                    .structural_alias => |b_alias| {
                        return self.unify_structural_alias(mb_err, a_alias, b_alias);
                    },
                    else => {
                        return try self.unify_help(mb_err, a_alias.backing_var, b_type_var);
                    },
                }
            },
            .nominal_alias => |a_alias| {
                switch (b_type.type_descriptor) {
                    .flex_var => {
                        // a is concrete and b is a flex_var, redirect b -> a
                        self.binding_store.set(b_type_var, Binding{ .redirect = a_type.type_var });
                        return DidUnify.ok;
                    },
                    .structural_alias => |b_alias| {
                        // if b is a structural alias, then recurse
                        return try self.unify_help(mb_err, a_type.type_var, b_alias.backing_var);
                    },
                    .nominal_alias => |b_alias| {
                        return self.unify_nominal_alias(mb_err, a_alias, b_alias);
                    },
                    .flat_type => |_| {
                        mb_err.cause = UnifyError.Cause{ .type_mismatch = .{
                            .left = a_type.type_descriptor,
                            .right = b_type.type_descriptor,
                        } };
                        return DidUnify.err;
                    },
                }
            },
            .flat_type => |a_flat_type| {
                switch (b_type.type_descriptor) {
                    .flex_var => {
                        // a is concrete and b is a flex_var, redirect b -> a
                        self.binding_store.set(b_type_var, Binding{ .redirect = a_type.type_var });
                        return DidUnify.ok;
                    },
                    .structural_alias => |b_alias| {
                        // if b is a structural alias, then recurse
                        return try self.unify_help(mb_err, a_type.type_var, b_alias.backing_var);
                    },
                    .nominal_alias => {
                        mb_err.cause = UnifyError.Cause{ .type_mismatch = .{
                            .left = a_type.type_descriptor,
                            .right = b_type.type_descriptor,
                        } };
                        return DidUnify.err;
                    },
                    .flat_type => |b_flat_type| {
                        return self.unify_flat_type(
                            mb_err,
                            a_flat_type,
                            b_flat_type,
                        );
                    },
                }
            },
        }
    }

    // flat types

    /// unify a flat type
    fn unify_flat_type(
        self: *Self,
        mb_err: *UnifyError,
        a_flat_type: FlatType.FlatType,
        b_flat_type: FlatType.FlatType,
    ) error{Overflow}!DidUnify {
        switch (a_flat_type) {
            .type_apply => |a_type_apply| {
                switch (b_flat_type) {
                    .type_apply => |b_type_apply| {
                        return self.unify_type_apply(mb_err, a_type_apply, b_type_apply);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                }
            },
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        return self.unify_tuple(mb_err, a_tuple, b_tuple);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                }
            },
            .num => |a_num| {
                switch (b_flat_type) {
                    .num => |b_num| {
                        return unify_num(mb_err, a_num, b_num);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                }
            },
            .func => |a_func| {
                switch (b_flat_type) {
                    .func => |b_func| {
                        return self.unify_func(mb_err, a_func, b_func);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                }
            },
            .empty_record => {
                switch (b_flat_type) {
                    .empty_record => {
                        // TODO: Rust compiler uses "merge" here. I think this is imporatnt
                        // for ranked types, but idk
                        return DidUnify.ok;
                    },
                    .record => |b_record| {
                        if (b_record.areAllFieldsOptional()) {
                            return DidUnify.ok;
                        }
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                }
            },
            .record => |a_record| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_record.areAllFieldsOptional()) {
                            return DidUnify.ok;
                        }
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
                    .record => |b_record| {
                        return self.unify_record(mb_err, a_record, b_record);
                    },
                    else => |_| {
                        return mb_err.setFlatTypeMismatchAndReturnErr(a_flat_type, b_flat_type);
                    },
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
        mb_err: *UnifyError,
        a_type_apply: FlatType.TypeApply,
        b_type_apply: FlatType.TypeApply,
    ) error{Overflow}!DidUnify {
        if (a_type_apply.name != b_type_apply.name) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .apply_name_mismatch = .{
                .left = a_type_apply.name,
                .right = b_type_apply.name,
            } });
        }
        if (a_type_apply.args.len != b_type_apply.args.len) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .apply_arity_mismatch = .{
                .left = a_type_apply.args.len,
                .right = b_type_apply.args.len,
            } });
        }

        var did_unify: DidUnify = undefined;
        for (0..a_type_apply.args.len) |i| {
            did_unify = try self.unify_help(mb_err, a_type_apply.args.buffer[i], b_type_apply.args.buffer[i]);
            if (did_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_unify;
        }

        return DidUnify.ok;
    }

    /// unify tuples
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_tuple(
        self: *Self,
        mb_err: *UnifyError,
        a_tuple: FlatType.Tuple,
        b_tuple: FlatType.Tuple,
    ) error{Overflow}!DidUnify {
        if (a_tuple.elems.len != b_tuple.elems.len) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .tuple_arity_mismatch = .{
                .left = a_tuple.elems.len,
                .right = b_tuple.elems.len,
            } });
        }

        var did_unify: DidUnify = undefined;
        for (0..a_tuple.elems.len) |i| {
            did_unify = try self.unify_help(mb_err, a_tuple.elems.buffer[i], b_tuple.elems.buffer[i]);
            if (did_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_unify;
        }

        return DidUnify.ok;
    }

    /// unify numbers
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_num(
        mb_err: *UnifyError,
        a_num: FlatType.Num,
        b_num: FlatType.Num,
    ) DidUnify {
        switch (a_num) {
            .flex_var => return DidUnify.ok,
            .int => |a_int| switch (b_num) {
                .flex_var => return DidUnify.ok,
                .frac => return mb_err.setNumMismatchAndReturnErr(a_num, b_num),
                .int => |b_int| {
                    if (a_int == .flex_var or b_int == .flex_var) {
                        return DidUnify.ok;
                    } else if (a_int == b_int) {
                        return DidUnify.ok;
                    } else {
                        return mb_err.setNumMismatchAndReturnErr(a_num, b_num);
                    }
                },
            },
            .frac => |a_frac| switch (b_num) {
                .flex_var => return DidUnify.ok,
                .int => return mb_err.setNumMismatchAndReturnErr(a_num, b_num),
                .frac => |b_frac| {
                    if (a_frac == .flex_var or b_frac == .flex_var) {
                        return DidUnify.ok;
                    } else if (a_frac == b_frac) {
                        return DidUnify.ok;
                    } else {
                        return mb_err.setNumMismatchAndReturnErr(a_num, b_num);
                    }
                },
            },
        }
    }

    /// unify functions
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    /// * that the return types unify
    fn unify_func(
        self: *Self,
        mb_err: *UnifyError,
        a_func: FlatType.Func,
        b_func: FlatType.Func,
    ) error{Overflow}!DidUnify {
        if (a_func.args.len != b_func.args.len) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .func_arity_mismatch = .{
                .left = a_func.args.len,
                .right = b_func.args.len,
            } });
        }

        var did_arg_unify: DidUnify = undefined;
        for (0..a_func.args.len) |i| {
            did_arg_unify = try self.unify_help(mb_err, a_func.args.buffer[i], b_func.args.buffer[i]);
            if (did_arg_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_arg_unify;
        }

        const did_ret_unify = try self.unify_help(mb_err, a_func.ret, b_func.ret);
        if (did_ret_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_ret_unify;

        // TODO: Once we have effects & lambda sets, there's additional
        // unification that needs to happen here

        return DidUnify.ok;
    }

    /// unify records
    /// unify records
    ///
    /// this checks:
    /// * that each record field matches
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unify_record(
        self: *Self,
        mb_err: *UnifyError,
        a_record: FlatType.Record,
        b_record: FlatType.Record,
    ) error{Overflow}!DidUnify {
        var a_fields = try FlatType.RecordFieldArray.fromSlice(a_record.fields.slice());
        const a_ext = self.gather_fields(&a_fields, a_record) catch |err| switch (err) {
            error.Overflow => return mb_err.setRecTooManyFieldsAndReturnErr(a_record, b_record),
            error.InvalidRecordExtType => return mb_err.setRecInvalidExtAndReturnErr(a_record, b_record),
        };

        var b_fields = try FlatType.RecordFieldArray.fromSlice(b_record.fields.slice());
        const b_ext = self.gather_fields(&b_fields, b_record) catch |err| switch (err) {
            error.Overflow => return mb_err.setRecTooManyFieldsAndReturnErr(a_record, b_record),
            error.InvalidRecordExtType => return mb_err.setRecInvalidExtAndReturnErr(a_record, b_record),
        };

        var partitioned = FlatType.RecordField.Partitioned.init();
        FlatType.RecordField.parition(&a_fields, &b_fields, &partitioned) catch |err| switch (err) {
            // TODO: Should we throw a different error here? This should be impossible
            // If there were too many fields, `gather_fields` would've errored before this
            error.Overflow => return mb_err.setRecTooManyFieldsAndReturnErr(a_record, b_record),
        };

        // both a and be are the empty record
        if (partitioned.only_in_a.len == 0 and partitioned.only_in_b.len == 0) {
            return self.unify_help(mb_err, a_ext.type_var, b_ext.type_var);
        }

        // a is empty record, b is not
        if (partitioned.only_in_a.len == 0 and partitioned.only_in_b.len != 0) {
            const only_b = Desc{
                .flat_type = FlatType.FlatType{
                    .record = FlatType.Record{
                        .fields = partitioned.only_in_b,
                        .ext = b_ext.type_var,
                    },
                },
            };
            const only_b_var = self.mkRoot(only_b);

            const did_only_b_unify = try self.unify_help(mb_err, a_ext.type_var, only_b_var);
            if (did_only_b_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_only_b_unify;

            // TODO
        }

        @panic("TODO");
    }

    /// Represents possible errors produces by gathering fields
    const GatherFieldsErr = error{InvalidRecordExtType};

    /// The final ext in an extensible record chain
    const GatherFieldsOk = struct {
        type_var: Var,
        ext: Ext,

        const Ext = enum { flex_var, empty_record };
    };

    /// Collect all of a record's fields, looping and unwrapping the record's 'extensible'
    /// type variable.
    ///
    /// It returns the final 'ext' of the last record in the chain
    ///
    /// Will error if:
    /// * There are more than 64 record fields
    /// * The 'ext' type var is invalid
    fn gather_fields(
        self: *Self,
        fields: *FlatType.RecordFieldArray,
        record: FlatType.Record,
    ) error{ Overflow, InvalidRecordExtType }!GatherFieldsOk {
        var ext_type = self.followRedirectsAndCompress(record.ext);
        while (true) {
            switch (ext_type.type_descriptor) {
                .flex_var => return .{ .type_var = ext_type.type_var, .ext = .flex_var },
                .structural_alias => |alias| {
                    // According to rust compiler: according to elm/compiler: "TODO may be dropping useful alias info here"
                    ext_type = self.followRedirectsAndCompress(alias.backing_var);
                },
                .nominal_alias => |alias| {
                    // According to rust compiler: according to elm/compiler: "TODO may be dropping useful alias info here"
                    ext_type = self.followRedirectsAndCompress(alias.backing_var);
                },
                .flat_type => |flat_type| switch (flat_type) {
                    .empty_record => return .{ .type_var = ext_type.type_var, .ext = .empty_record },
                    .record => |sub_record| {
                        try fields.appendSlice(sub_record.fields.slice());
                        ext_type = self.followRedirectsAndCompress(sub_record.ext);
                    },
                    // These are listed explicitly for future resiliance
                    .type_apply => return GatherFieldsErr.InvalidRecordExtType,
                    .tuple => return GatherFieldsErr.InvalidRecordExtType,
                    .num => return GatherFieldsErr.InvalidRecordExtType,
                    .func => return GatherFieldsErr.InvalidRecordExtType,
                },
            }
        }
    }

    // structural alias

    /// unify structural alias
    ///
    /// this checks:
    /// * that the alias names match
    /// * that parallel arguments unify
    fn unify_structural_alias(
        self: *Self,
        mb_err: *UnifyError,
        a_alias: Desc.Alias,
        b_alias: Desc.Alias,
    ) error{Overflow}!DidUnify {
        if (a_alias.name != b_alias.name) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .structural_alias_name_mismatch = .{
                .left = a_alias.name,
                .right = b_alias.name,
            } });
        }
        if (a_alias.args.len != b_alias.args.len) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .structural_alias_arity_mismatch = .{
                .left = a_alias.args.len,
                .right = b_alias.args.len,
            } });
        }

        var did_unify: DidUnify = undefined;
        for (0..a_alias.args.len) |i| {
            did_unify = try self.unify_help(mb_err, a_alias.args.buffer[i], b_alias.args.buffer[i]);
            if (did_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_unify;
        }

        return DidUnify.ok;
    }

    // nominal alias

    /// unify nominal alias
    ///
    /// this checks:
    /// * that the alias names match
    /// * that parallel arguments unify
    fn unify_nominal_alias(
        self: *Self,
        mb_err: *UnifyError,
        a_alias: Desc.Alias,
        b_alias: Desc.Alias,
    ) error{Overflow}!DidUnify {
        if (a_alias.name != b_alias.name) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .nominal_alias_name_mismatch = .{
                .left = a_alias.name,
                .right = b_alias.name,
            } });
        }
        if (a_alias.args.len != b_alias.args.len) {
            return mb_err.setCauseAndReturnErr(UnifyError.Cause{ .nominal_alias_arity_mismatch = .{
                .left = a_alias.args.len,
                .right = b_alias.args.len,
            } });
        }

        var did_unify: DidUnify = undefined;
        for (0..a_alias.args.len) |i| {
            did_unify = try self.unify_help(mb_err, a_alias.args.buffer[i], b_alias.args.buffer[i]);
            if (did_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_unify;
        }

        return DidUnify.ok;
    }

    /// type wrapper around bool to make success/failure more clear
    pub const DidUnify = enum(u1) { err = 0, ok = 1 };

    /// A type to represent a unification error
    ///
    /// Includes a trace of all the intermediate variables that did unify down the recursive stack
    ///
    /// TODO: Currently only supports a depth of 8. Is that reasonable? What should happen if it's exceeded?
    pub const UnifyError = struct {
        trace: TraceArray,
        cause: Cause,

        /// Total capacity of the trace array
        const trace_capacity = 8;

        /// Represents a step in the trace
        pub const TraceVars = LeftRight(Var);

        /// A bounded array of trace variables
        pub const TraceArray = std.BoundedArray(TraceVars, trace_capacity);

        pub const Cause = union(enum) {
            // base
            type_mismatch: LeftRight(Desc),
            overflowed_trace: LeftRight(Var),

            // structural aliases
            structural_alias_name_mismatch: LeftRight(TypeName),
            structural_alias_arity_mismatch: LeftRight(usize),

            // nominal aliases
            nominal_alias_name_mismatch: LeftRight(TypeName),
            nominal_alias_arity_mismatch: LeftRight(usize),
            unexpected_structural_alias: LeftRight(Desc),

            // flat type - apply
            apply_name_mismatch: LeftRight(TypeName),
            apply_arity_mismatch: LeftRight(usize),

            // flat type - tuple
            tuple_arity_mismatch: LeftRight(usize),

            // flat type - func
            func_arity_mismatch: LeftRight(usize),

            // flat type - record
            rec_invalid_ext_typ: LeftRight(Desc),
            rec_gathered_too_many_fields: LeftRight(Desc),
        };

        /// Represents two values in unfication
        pub fn LeftRight(comptime T: type) type {
            return struct { left: T, right: T };
        }

        const SelfU = @This();

        /// Initialize an undefined unify err
        pub fn init() SelfU {
            return .{ .trace = TraceArray.init(0) catch unreachable, .cause = undefined };
        }

        // trace helpers

        /// Append trace variables to the end of the trace list
        pub fn appendTrace(self: *SelfU, leftTraceVar: Var, rightTraceVar: Var) error{Overflow}!void {
            try self.trace.append(.{ .left = leftTraceVar, .right = rightTraceVar });
        }

        /// Drop the last trace
        pub fn dropLastTrace(self: *SelfU) void {
            _ = self.trace.pop();
        }

        /// Get a slice of the trace.
        /// This only lives as long 'self'
        pub fn getTraceSlice(self: *const SelfU) []const TraceVars {
            return self.trace.constSlice();
        }

        // cause helpers
        // TODO: these are inlined, but should they be?

        /// Set the cause and return a unify err
        pub inline fn setCauseAndReturnErr(self: *SelfU, cause: UnifyError.Cause) DidUnify {
            self.cause = cause;
            return DidUnify.err;
        }

        /// Set the cause and return a unify err
        pub inline fn setTypeMismatchAndReturnErr(
            self: *SelfU,
            a_desc: Desc,
            b_desc: Desc,
        ) DidUnify {
            self.cause = UnifyError.Cause{ .type_mismatch = .{
                .left = a_desc,
                .right = b_desc,
            } };
            return DidUnify.err;
        }

        /// Set the cause and return a unify err
        pub inline fn setFlatTypeMismatchAndReturnErr(
            self: *SelfU,
            a_flat_type: FlatType.FlatType,
            b_flat_type: FlatType.FlatType,
        ) DidUnify {
            self.cause = UnifyError.Cause{ .type_mismatch = .{
                .left = Desc{ .flat_type = a_flat_type },
                .right = Desc{ .flat_type = b_flat_type },
            } };
            return DidUnify.err;
        }

        /// Set the cause and return a unify err
        pub inline fn setNumMismatchAndReturnErr(
            self: *SelfU,
            a_num: FlatType.Num,
            b_num: FlatType.Num,
        ) DidUnify {
            self.cause = UnifyError.Cause{ .type_mismatch = .{
                .left = Desc{ .flat_type = FlatType.FlatType{ .num = a_num } },
                .right = Desc{ .flat_type = FlatType.FlatType{ .num = b_num } },
            } };
            return DidUnify.err;
        }

        /// Set the cause and return a unify err
        pub inline fn setRecInvalidExtAndReturnErr(
            self: *SelfU,
            a_rec: FlatType.Record,
            b_rec: FlatType.Record,
        ) DidUnify {
            self.cause = UnifyError.Cause{ .rec_invalid_ext_typ = .{
                .left = Desc{ .flat_type = FlatType.FlatType{ .record = a_rec } },
                .right = Desc{ .flat_type = FlatType.FlatType{ .record = b_rec } },
            } };
            return DidUnify.err;
        }

        /// Set the cause and return a unify err
        pub inline fn setRecTooManyFieldsAndReturnErr(
            self: *SelfU,
            a_rec: FlatType.Record,
            b_rec: FlatType.Record,
        ) DidUnify {
            self.cause = UnifyError.Cause{ .rec_gathered_too_many_fields = .{
                .left = Desc{ .flat_type = FlatType.FlatType{ .record = a_rec } },
                .right = Desc{ .flat_type = FlatType.FlatType{ .record = b_rec } },
            } };
            return DidUnify.err;
        }
    };
};

// tests

// path compression

test "followRedirectsAndCompress - flattens redirect chain to flex_var" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const c = solver.fresh();
    const b = solver.mkRedirect(c);
    const a = solver.mkRedirect(b);

    const result = solver.followRedirectsAndCompress(a);
    try std.testing.expectEqual(result.type_descriptor, Desc.flex_var);
    try std.testing.expectEqual(result.type_var, c);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = c });
    try std.testing.expectEqual(solver.binding_store.get(b), Binding{ .redirect = c });
}

test "followRedirectsAndCompress - no-op on already root" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str_desc_idx = solver.descriptor_store.insert(Desc.str);
    const str_var = solver.binding_store.insert(.{ .root = str_desc_idx });

    const result = solver.followRedirectsAndCompress(str_var);

    try std.testing.expectEqual(result.type_descriptor, Desc.str);
    try std.testing.expectEqual(result.type_var, str_var);
    try std.testing.expectEqual(solver.binding_store.get(str_var), Binding{ .root = str_desc_idx });
}

test "followRedirectsAndCompress - flattens redirect chain to concrete" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const c = solver.mkRoot(Desc.bool_);
    const b = solver.mkRedirect(c);
    const a = solver.mkRedirect(b);

    const result = solver.followRedirectsAndCompress(a);
    try std.testing.expectEqual(result.type_descriptor, Desc.bool_);
    try std.testing.expectEqual(result.type_var, c);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = c });
    try std.testing.expectEqual(solver.binding_store.get(b), Binding{ .redirect = c });
}

// unification - flex_vars

test "unify - a redirects to b - both flex_vars" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.fresh();
    const b = solver.fresh();

    try std.testing.expectEqual(solver.unify(a, b), null);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = b });
}

test "unify - a redirects to b - a is flex_var and b is concrete" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const b = solver.mkRoot(Desc.bool_);
    const a = solver.fresh();

    try std.testing.expectEqual(solver.unify(a, b), null);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = b });
}

test "unify - b redirects to a - a concrete and b unknown" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Desc.bool_);
    const b = solver.fresh();

    try std.testing.expectEqual(solver.unify(a, b), null);
    try std.testing.expectEqual(solver.binding_store.get(b), Binding{ .redirect = a });
}

// unification - flat type - type apply

test "unify - type_apply - eql - both a & b same prim" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Desc.bool_);
    const b = solver.mkRoot(Desc.bool_);

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - type_apply - not eql - both a & b diff prim" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Desc.str);
    const b = solver.mkRoot(Desc.bool_);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    });
    try std.testing.expectEqual(1, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
        },
        err.getTraceSlice(),
    );
}

test "unify - type_apply - eql - both a & b same type with single arg" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const a_list_str = solver.mkRoot(Desc.mkList(str));
    const b_list_str = solver.mkRoot(Desc.mkList(str));

    try std.testing.expectEqual(solver.unify(a_list_str, b_list_str), null);
}

test "unify - type_apply - eql - both a & b same type with diff arg" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const list_str = solver.mkRoot(Desc.mkList(str));

    const bool_ = solver.mkRoot(Desc.bool_);
    const list_bool = solver.mkRoot(Desc.mkList(bool_));

    const mb_err = solver.unify(list_str, list_bool);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    });
    try std.testing.expectEqual(2, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = list_str, .right = list_bool },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

test "unify - type_apply - eql - both a & b same type with two args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a = solver.mkRoot(Desc.mkResult(str, bool_));
    const b = solver.mkRoot(Desc.mkResult(str, bool_));

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - type_apply - eql - both a & b same type with flipped args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a = solver.mkRoot(Desc.mkResult(str, bool_));
    const b = solver.mkRoot(Desc.mkResult(bool_, str));

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    });
    try std.testing.expectEqual(2, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

test "unify - type_apply - not eql - both a & b diff type with same args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const list_str = Desc.mkList(str);
    const maybe_str = Desc.mkMaybe(str);

    const a = solver.mkRoot(list_str);
    const b = solver.mkRoot(maybe_str);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.list, .right = TypeName.maybe },
    }, err.cause);
    try std.testing.expectEqual(1, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{.{ .left = a, .right = b }},
        err.getTraceSlice(),
    );
}

test "unify - type_apply - not eql - both a & b same type with diff arg deeply nested" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const list_str = solver.mkRoot(Desc.mkList(str));
    const list_list_str = solver.mkRoot(Desc.mkList(list_str));
    const list_list_list_str = solver.mkRoot(Desc.mkList(list_list_str));

    const bool_ = solver.mkRoot(Desc.bool_);
    const list_bool = solver.mkRoot(Desc.mkList(bool_));
    const list_list_bool = solver.mkRoot(Desc.mkList(list_bool));
    const list_list_list_bool = solver.mkRoot(Desc.mkList(list_list_bool));

    const mb_err = solver.unify(list_list_list_str, list_list_list_bool);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    }, err.cause);
    try std.testing.expectEqual(4, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = list_list_list_str, .right = list_list_list_bool },
            .{ .left = list_list_str, .right = list_list_bool },
            .{ .left = list_str, .right = list_bool },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

// unification - tuple

test "unify - tuple - eql - both a & b are same" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a = solver.mkRoot(Desc.mkTuple(&[_]Var{ str, bool_ }));
    const b = solver.mkRoot(Desc.mkTuple(&[_]Var{ str, bool_ }));

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - tuple - eql - both a & b are flipped" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a = solver.mkRoot(Desc.mkTuple(&[_]Var{ str, bool_ }));
    const b = solver.mkRoot(Desc.mkTuple(&[_]Var{ bool_, str }));

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    }, err.cause);
    try std.testing.expectEqual(2, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

// unification - structural aliases

test "unify - structural_alias - same name and args unifies" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);

    const a_desc = Desc{
        .structural_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const b_desc = a_desc;

    const a_var = solver.mkRoot(a_desc);
    const b_var = solver.mkRoot(b_desc);

    try std.testing.expectEqual(solver.unify(a_var, b_var), null);
}

test "unify - structural_alias - same name and args do not unify" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a_desc = Desc{
        .structural_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };
    const a = solver.mkRoot(a_desc);

    const mb_err = solver.unify(a, bool_);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{
            .left = TypeName.str,
            .right = TypeName.bool_,
        },
    });
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = bool_ },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

test "unify - structural_alias - diff types do not unify" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a = Desc{
        .structural_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const b = Desc{
        .structural_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{bool_}) catch unreachable,
            .backing_var = str,
        },
    };

    const a_var = solver.mkRoot(a);
    const b_var = solver.mkRoot(b);

    const mb_err = solver.unify(a_var, b_var);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{
            .left = TypeName.str,
            .right = TypeName.bool_,
        },
    });
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a_var, .right = b_var },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

// unification - nominal aliases

test "unify - nominal_alias - identical vars unify" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);

    const desc = Desc{
        .nominal_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const nominal_var = solver.mkRoot(desc);

    try std.testing.expectEqual(solver.unify(nominal_var, nominal_var), null);
}

test "unify - nominal_alias - diff names do not unify" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);

    const a_nominal_name: TypeName = @enumFromInt(100);
    const a_nominal = Desc{
        .nominal_alias = .{
            .name = a_nominal_name,
            .args = try Desc.Alias.ArgsArray.init(0),
            .backing_var = str,
        },
    };

    const b_nominal_name: TypeName = @enumFromInt(200);
    const b_nominal = Desc{
        .nominal_alias = .{
            .name = b_nominal_name,
            .args = try Desc.Alias.ArgsArray.init(0),
            .backing_var = str,
        },
    };

    const a = solver.mkRoot(a_nominal);
    const b = solver.mkRoot(b_nominal);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .nominal_alias_name_mismatch = .{
            .left = a_nominal_name,
            .right = b_nominal_name,
        },
    });
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
        },
        err.getTraceSlice(),
    );
}

test "unify - nominal_alias - same name names diff args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_ = solver.mkRoot(Desc.bool_);

    const a_nominal_name: TypeName = @enumFromInt(100);
    const a_nominal = Desc{
        .nominal_alias = .{
            .name = a_nominal_name,
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const b_nominal = Desc{
        .nominal_alias = .{
            .name = a_nominal_name,
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{bool_}) catch unreachable,
            .backing_var = str,
        },
    };

    const a = solver.mkRoot(a_nominal);
    const b = solver.mkRoot(b_nominal);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{
            .left = TypeName.str,
            .right = TypeName.bool_,
        },
    });
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bool_ },
        },
        err.getTraceSlice(),
    );
}

test "unify - nominal_alias vs flat_type - fails" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);

    const nominal = Desc{
        .nominal_alias = .{
            .name = @enumFromInt(300),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const nominal_var = solver.mkRoot(nominal);
    const flat_var = solver.mkRoot(Desc.str);

    const mb_err = solver.unify(nominal_var, flat_var);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .type_mismatch = .{
            .left = nominal,
            .right = Desc.str,
        },
    });
}

test "unify - nominal_alias vs structural_alias - fails" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);

    const nominal = Desc{
        .nominal_alias = .{
            .name = @enumFromInt(100),
            .args = Desc.Alias.ArgsArray.fromSlice(&[_]Var{str}) catch unreachable,
            .backing_var = str,
        },
    };

    const structural = Desc{
        .structural_alias = .{
            .name = @enumFromInt(200),
            .args = try Desc.Alias.ArgsArray.init(0),
            .backing_var = str,
        },
    };

    const nominal_var = solver.mkRoot(nominal);
    const structural_var = solver.mkRoot(structural);

    const mb_err = solver.unify(nominal_var, structural_var);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .type_mismatch = .{
            .left = nominal,
            .right = Desc.str,
        },
    });
}

// unification - flat type - num

test "unify - num - flex_var unifies with flex_var" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(.{ .flat_type = .{ .num = .flex_var } });
    const b = solver.mkRoot(.{ .flat_type = .{ .num = .flex_var } });

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - num - flex_var unifies with int" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(.{ .flat_type = .{ .num = .flex_var } });
    const b = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .i64 } } });

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - num - flex_var unifies with frac" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(.{ .flat_type = .{ .num = .flex_var } });
    const b = solver.mkRoot(.{ .flat_type = .{ .num = .{ .frac = .dec } } });

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - num - int flex_var unifies with int concrete" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .flex_var } } });
    const b = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .i64 } } });

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - num - frac flex_var unifies with frac concrete" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(.{ .flat_type = .{ .num = .{ .frac = .flex_var } } });
    const b = solver.mkRoot(.{ .flat_type = .{ .num = .{ .frac = .f32 } } });

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - num - int does not unify with frac" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const int_var = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .i32 } } });
    const frac_var = solver.mkRoot(.{ .flat_type = .{ .num = .{ .frac = .f32 } } });

    const mb_err = solver.unify(int_var, frac_var);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .type_mismatch = .{
            .left = Desc{ .flat_type = .{ .num = .{ .int = .i32 } } },
            .right = Desc{ .flat_type = .{ .num = .{ .frac = .f32 } } },
        },
    });
    try std.testing.expectEqual(1, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = int_var, .right = frac_var },
        },
        err.getTraceSlice(),
    );
}

test "unify - num - i16 does not unify with i32" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const i16_var = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .i16 } } });
    const i32_var = solver.mkRoot(.{ .flat_type = .{ .num = .{ .int = .i32 } } });

    const mb_err = solver.unify(i16_var, i32_var);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .type_mismatch = .{
            .left = Desc{ .flat_type = .{ .num = .{ .int = .i16 } } },
            .right = Desc{ .flat_type = .{ .num = .{ .int = .i32 } } },
        },
    });
    try std.testing.expectEqual(1, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = i16_var, .right = i32_var },
        },
        err.getTraceSlice(),
    );
}

// unification - flat type - func

test "unify - func - same args and return type unifies" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const ret = solver.mkRoot(Desc.bool_);

    const a_func = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
    } });

    const b_func = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
    } });

    try std.testing.expectEqual(solver.unify(a_func, b_func), null);
}

test "unify - func - different arity fails" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_var = solver.mkRoot(Desc.bool_);

    const one_arg = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = bool_var },
    } });
    const two_args = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{ str, str }), .ret = bool_var },
    } });

    const mb_err = solver.unify(one_arg, two_args);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .func_arity_mismatch = .{ .left = 1, .right = 2 },
    });
    try std.testing.expectEqual(1, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{.{ .left = one_arg, .right = two_args }},
        err.getTraceSlice(),
    );
}

test "unify - func - same arity, differing arg types fails" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const bool_var = solver.mkRoot(Desc.bool_);
    const ret = solver.mkRoot(Desc.str);

    const a = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret },
    } });

    const b = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{bool_var}), .ret = ret },
    } });

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    });
    try std.testing.expectEqual(2, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bool_var },
        },
        err.getTraceSlice(),
    );
}

test "unify - func - same args, different return types fails" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Desc.str);
    const ret1 = solver.mkRoot(Desc.str);
    const ret2 = solver.mkRoot(Desc.bool_);

    const a = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret1 },
    } });

    const b = solver.mkRoot(.{ .flat_type = .{
        .func = .{ .args = try FlatType.Func.ArgsArray.fromSlice(&[_]Var{str}), .ret = ret2 },
    } });

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = TypeName.str, .right = TypeName.bool_ },
    });
    try std.testing.expectEqual(2, err.trace.len);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = ret1, .right = ret2 },
        },
        err.getTraceSlice(),
    );
}
