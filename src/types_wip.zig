const std = @import("std");
const exitOnOutOfMemory = @import("./collections.zig").utils.exitOnOom;

/// A type variable id
///
/// TODO: does this need to be u32? Can we get away with u16?
const Var = enum(u32) { _ };

/// Represents a type name, like Str, Bool, List
///
/// This includes both builtin types as well as user-defined types
///
/// TODO: does this need to be u32? Can we get away with u16?
const Name = enum(u32) {
    BOOL = 0,
    STR = 1,
    LIST = 2,
    MAYBE = 3,
    RESULT = 4,
    _,
};

/// Represents what the a type *is*
const Descriptor = union(enum) {
    flex_var,
    flat_type: FlatType,

    // A "flat" data type
    // todo: rename?
    const FlatType = union(enum) {
        type_apply: TypeApply,
        tuple: Tuple,

        // TODO: is 16 seems like a max??
        pub const MAX_ELEMS = std.math.maxInt(u4);

        /// Represents a type application, like `List String` or `Result Error Value`.
        ///
        /// Applications may have up to 12 type arguments.
        /// Only the first `arg_count` elements of `args` are considered valid.
        const TypeApply = struct { name: Name, arg_count: u4, args: [MAX_ELEMS]Var };

        /// Represents a tuple
        ///
        /// Tuples may have up to 12 type elements.
        /// Only the first `arg_count` elements of `elements` are considered valid.
        const Tuple = struct { elem_count: u4, elems: [MAX_ELEMS]Var };
    };

    const SelfD = @This();

    // Constants

    const BOOL: SelfD = SelfD{
        .flat_type = .{ .type_apply = .{ .name = Name.BOOL, .arg_count = 0, .args = undefined } },
    };
    const STR: SelfD = SelfD{
        .flat_type = .{ .type_apply = .{ .name = Name.STR, .arg_count = 0, .args = undefined } },
    };

    /// make a List with the provided type applied
    pub fn mkList(v: Var) SelfD {
        var args: [FlatType.MAX_ELEMS]Var = undefined;
        args[0] = v;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = Name.LIST, .arg_count = 1, .args = args } } };
    }

    /// make a Maybe with the provided type applied
    pub fn mkMaybe(v: Var) SelfD {
        var args: [FlatType.MAX_ELEMS]Var = undefined;
        args[0] = v;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = Name.MAYBE, .arg_count = 1, .args = args } } };
    }

    /// make a List with the provided type applied
    pub fn mkResult(ok: Var, err: Var) SelfD {
        var args: [FlatType.MAX_ELEMS]Var = undefined;
        args[0] = ok;
        args[1] = err;
        return SelfD{ .flat_type = .{ .type_apply = .{ .name = Name.RESULT, .arg_count = 2, .args = args } } };
    }

    /// make a List with the provided type applied
    pub fn mkTuple(slice: []const Var) SelfD {
        std.debug.assert(slice.len < FlatType.MAX_ELEMS + 1);
        var elems: [FlatType.MAX_ELEMS]Var = undefined;
        for (0..slice.len) |i| {
            elems[i] = slice[i];
        }
        return SelfD{ .flat_type = .{ .tuple = .{ .elem_count = 1, .elems = elems } } };
    }

    /// Represents a store of descriptors
    const Store = struct {
        backing: std.ArrayList(SelfD),

        const Idx = enum(usize) { _ };

        const SelfS = @This();

        fn init(gpa: std.mem.Allocator, capacity: usize) SelfS {
            const arr_list = std.ArrayList(SelfD).initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
            return .{ .backing = arr_list };
        }

        fn insert(self: *SelfS, typ: SelfD) Idx {
            const idx: Idx = @enumFromInt(self.backing.items.len);
            self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
            return idx;
        }

        fn deinit(self: *SelfS) void {
            self.backing.deinit();
        }

        fn get(self: *const SelfS, idx: Idx) SelfD {
            return self.backing.items[@intFromEnum(idx)];
        }
    };
};

// Reperents either type data *or* a symlink to another type variable
const Binding = union(enum) {
    root: Descriptor.Store.Idx,
    redirect: Var,

    const SelfB = @This();

    /// Represents a store of bindings
    const Store = struct {
        backing: std.ArrayList(SelfB),

        const SelfS = @This();

        fn init(gpa: std.mem.Allocator, capacity: usize) SelfS {
            const arr_list = std.ArrayList(SelfB).initCapacity(gpa, capacity) catch |err| exitOnOutOfMemory(err);
            return .{ .backing = arr_list };
        }

        fn deinit(self: *SelfS) void {
            self.backing.deinit();
        }

        /// Insert a new binding into the store. The return type is a new `Var`.
        ///
        /// It's important to note that this `Var` is an index into the store,
        /// but it also also the type variable itself
        fn insert(self: *SelfS, typ: SelfB) Var {
            const idx: Var = @enumFromInt(self.backing.items.len);
            self.backing.append(typ) catch |err| exitOnOutOfMemory(err);
            return idx;
        }

        fn set(self: *SelfS, idx: Var, val: SelfB) void {
            self.backing.items[@intFromEnum(idx)] = val;
        }

        fn get(self: *const SelfS, idx: Var) SelfB {
            return self.backing.items[@intFromEnum(idx)];
        }
    };
};

/// Type solver
const Solver = struct {
    descriptor_store: Descriptor.Store,
    binding_store: Binding.Store,

    const Self = @This();

    /// Init the solver
    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: eventually use herusitics here to determin sensible defaults
        return .{
            .descriptor_store = Descriptor.Store.init(gpa, 64),
            .binding_store = Binding.Store.init(gpa, 64),
        };
    }

    /// Deinit the solver
    pub fn deinit(self: *Self) void {
        self.descriptor_store.deinit();
        self.binding_store.deinit();
    }

    /// Create a new unbound, flexible type variable
    pub fn fresh(self: *Self) Var {
        const flex_var_idx = self.descriptor_store.insert(Descriptor.flex_var);
        const binding_var = self.binding_store.insert(.{ .root = flex_var_idx });
        return binding_var;
    }

    /// Create a bound variable with the provided descriptor
    /// Mainly used for tests
    pub fn mkRoot(self: *Self, desc: Descriptor) Var {
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

    /// Given a type var, follow all redirects until finding the root descriptor, then
    /// compress the path
    ///
    /// if the type is not a redirect, the var return will match the one passed in
    fn followRedirectsAndCompress(self: *Self, initial_var: Var) struct {
        type_var: Var,
        type_descriptor: Descriptor,
    } {
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

    /// A type to represent a unification error
    ///
    /// Includes a trace of all the intermediate variables that did unify down the recursive stack
    ///
    /// TODO: Currently only supports a depth of 8. Is that reasonable? What should happen if it's exceeded?
    pub const UnifyError = struct {
        trace: [MaxUnifyTrace]TraceVars,
        depth: u3,
        cause: Cause,

        /// Total dept of unification error trace aka 8
        const MaxUnifyTrace = std.math.maxInt(u3);

        /// Represents a step in the trace
        pub const TraceVars = struct { left: Var, right: Var };

        /// Represents the cause of a failure in unification
        pub const Cause = union(enum) {
            type_mismatch: struct { left: Descriptor, right: Descriptor },
            apply_name_mismatch: struct { left: Name, right: Name },
            apply_arg_count_mismatch: struct { left: u8, right: u8 },
            tuple_arity_mismatch: struct { left: u8, right: u8 },
        };

        const SelfU = @This();

        /// Initialize an undefined unify err
        pub fn init() SelfU {
            return .{ .trace = undefined, .depth = 0, .cause = undefined };
        }

        /// Append trace variables to the end of the trace list
        ///
        /// TODO: What do we do if we exceed depth here? Should we truncate from the beginning?
        pub fn appendTrace(self: *SelfU, leftTraceVar: Var, rightTraceVar: Var) void {
            if (self.depth < MaxUnifyTrace) {
                self.trace[self.depth] = .{ .left = leftTraceVar, .right = rightTraceVar };
                self.depth = self.depth + 1;
            }
        }

        /// Drop the last trtace
        pub fn dropLastTrace(self: *SelfU) void {
            self.depth = self.depth - 1;
        }

        /// Get a slice of the trace.
        /// This only lives as long 'self'
        pub fn getTraceSlice(self: *const SelfU) []const TraceVars {
            return self.trace[0..self.depth];
        }
    };

    /// Unify two types
    pub fn unify(self: *Self, a_type_var: Var, b_type_var: Var) ?UnifyError {
        var err = UnifyError.init();
        switch (self.unify_help(&err, a_type_var, b_type_var)) {
            .ok => return null,
            .err => return err,
        }
    }

    /// type wrapper around bool to make success/failure more clear
    pub const DidUnify = enum(u1) { ok, err };

    // Unify two types, updating the error context
    fn unify_help(self: *Self, mb_err: *UnifyError, a_type_var: Var, b_type_var: Var) DidUnify {
        mb_err.appendTrace(a_type_var, b_type_var);

        const a_type = followRedirectsAndCompress(self, a_type_var);
        const b_type = followRedirectsAndCompress(self, b_type_var);

        switch (a_type.type_descriptor) {
            .flex_var => {
                // if both a & b are flex vars, symlink link a -> b
                self.binding_store.set(a_type_var, Binding{ .redirect = b_type.type_var });
                return DidUnify.ok;
            },
            .flat_type => |a_flat_type| {
                switch (b_type.type_descriptor) {
                    .flex_var => {
                        // if b is a flat_type and a is a flex_var, symlink a -> b
                        self.binding_store.set(b_type_var, Binding{ .redirect = a_type.type_var });
                        return DidUnify.ok;
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

    /// unify a flat type
    fn unify_flat_type(
        self: *Self,
        mb_err: *UnifyError,
        a_flat_type: Descriptor.FlatType,
        b_flat_type: Descriptor.FlatType,
    ) DidUnify {
        switch (a_flat_type) {
            .type_apply => |a_type_apply| {
                switch (b_flat_type) {
                    .type_apply => |b_type_apply| {
                        return self.unify_type_apply(mb_err, a_type_apply, b_type_apply);
                    },
                    else => |_| {
                        mb_err.cause = UnifyError.Cause{ .type_mismatch = .{
                            .left = Descriptor{ .flat_type = a_flat_type },
                            .right = Descriptor{ .flat_type = b_flat_type },
                        } };
                        return DidUnify.err;
                    },
                }
            },
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        return self.unify_tuple(mb_err, a_tuple, b_tuple);
                    },
                    else => |_| {
                        mb_err.cause = UnifyError.Cause{ .type_mismatch = .{
                            .left = Descriptor{ .flat_type = a_flat_type },
                            .right = Descriptor{ .flat_type = b_flat_type },
                        } };
                        return DidUnify.err;
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
        a_type_apply: Descriptor.FlatType.TypeApply,
        b_type_apply: Descriptor.FlatType.TypeApply,
    ) DidUnify {
        if (a_type_apply.name != b_type_apply.name) {
            mb_err.cause =
                UnifyError.Cause{ .apply_name_mismatch = .{
                    .left = a_type_apply.name,
                    .right = b_type_apply.name,
                } };
            return DidUnify.err;
        }
        if (a_type_apply.arg_count != b_type_apply.arg_count) {
            mb_err.cause =
                UnifyError.Cause{ .apply_arg_count_mismatch = .{
                    .left = a_type_apply.arg_count,
                    .right = b_type_apply.arg_count,
                } };
            return DidUnify.err;
        }

        var did_unify: DidUnify = undefined;
        for (0..a_type_apply.arg_count) |i| {
            did_unify = self.unify_help(mb_err, a_type_apply.args[i], b_type_apply.args[i]);
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
        a_tuple: Descriptor.FlatType.Tuple,
        b_tuple: Descriptor.FlatType.Tuple,
    ) DidUnify {
        if (a_tuple.elem_count != b_tuple.elem_count) {
            mb_err.cause =
                UnifyError.Cause{ .tuple_arity_mismatch = .{
                    .left = a_tuple.elem_count,
                    .right = b_tuple.elem_count,
                } };
            return DidUnify.err;
        }

        var did_unify: DidUnify = undefined;
        for (0..a_tuple.elem_count) |i| {
            did_unify = self.unify_help(mb_err, a_tuple.elems[i], b_tuple.elems[i]);
            if (did_unify == DidUnify.ok) mb_err.dropLastTrace() else return did_unify;
        }

        return DidUnify.ok;
    }
};

// path compression

test "followRedirectsAndCompress - flattens redirect chain to flex_var" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const c = solver.fresh();
    const b = solver.mkRedirect(c);
    const a = solver.mkRedirect(b);

    const result = solver.followRedirectsAndCompress(a);
    try std.testing.expectEqual(result.type_descriptor, Descriptor.flex_var);
    try std.testing.expectEqual(result.type_var, c);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = c });
    try std.testing.expectEqual(solver.binding_store.get(b), Binding{ .redirect = c });
}

test "followRedirectsAndCompress - no-op on already root" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str_desc_idx = solver.descriptor_store.insert(Descriptor.STR);
    const str_var = solver.binding_store.insert(.{ .root = str_desc_idx });

    const result = solver.followRedirectsAndCompress(str_var);

    try std.testing.expectEqual(result.type_descriptor, Descriptor.STR);
    try std.testing.expectEqual(result.type_var, str_var);
    try std.testing.expectEqual(solver.binding_store.get(str_var), Binding{ .root = str_desc_idx });
}

test "followRedirectsAndCompress - flattens redirect chain to concrete" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const c = solver.mkRoot(Descriptor.BOOL);
    const b = solver.mkRedirect(c);
    const a = solver.mkRedirect(b);

    const result = solver.followRedirectsAndCompress(a);
    try std.testing.expectEqual(result.type_descriptor, Descriptor.BOOL);
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

    const b = solver.mkRoot(Descriptor.BOOL);
    const a = solver.fresh();

    try std.testing.expectEqual(solver.unify(a, b), null);
    try std.testing.expectEqual(solver.binding_store.get(a), Binding{ .redirect = b });
}

test "unify - b redirects to a - a concrete and b unknown" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Descriptor.BOOL);
    const b = solver.fresh();

    try std.testing.expectEqual(solver.unify(a, b), null);
    try std.testing.expectEqual(solver.binding_store.get(b), Binding{ .redirect = a });
}

// unification - flat type - type apply

test "unify - type_apply - eql - both a & b same prim" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Descriptor.BOOL);
    const b = solver.mkRoot(Descriptor.BOOL);

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - type_apply - not eql - both a & b diff prim" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const a = solver.mkRoot(Descriptor.STR);
    const b = solver.mkRoot(Descriptor.BOOL);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.STR, .right = Name.BOOL },
    });
    try std.testing.expectEqual(1, err.depth);
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

    const str = solver.mkRoot(Descriptor.STR);
    const a_list_str = solver.mkRoot(Descriptor.mkList(str));
    const b_list_str = solver.mkRoot(Descriptor.mkList(str));

    try std.testing.expectEqual(solver.unify(a_list_str, b_list_str), null);
}

test "unify - type_apply - eql - both a & b same type with diff arg" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const list_str = solver.mkRoot(Descriptor.mkList(str));

    const bbool = solver.mkRoot(Descriptor.BOOL);
    const list_bool = solver.mkRoot(Descriptor.mkList(bbool));

    const mb_err = solver.unify(list_str, list_bool);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.STR, .right = Name.BOOL },
    });
    try std.testing.expectEqual(2, err.depth);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = list_str, .right = list_bool },
            .{ .left = str, .right = bbool },
        },
        err.getTraceSlice(),
    );
}

test "unify - type_apply - eql - both a & b same type with two args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const bbool = solver.mkRoot(Descriptor.BOOL);

    const a = solver.mkRoot(Descriptor.mkResult(str, bbool));
    const b = solver.mkRoot(Descriptor.mkResult(str, bbool));

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - type_apply - eql - both a & b same type with flipped args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const bbool = solver.mkRoot(Descriptor.BOOL);

    const a = solver.mkRoot(Descriptor.mkResult(str, bbool));
    const b = solver.mkRoot(Descriptor.mkResult(bbool, str));

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(err.cause, Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.STR, .right = Name.BOOL },
    });
    try std.testing.expectEqual(2, err.depth);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bbool },
        },
        err.getTraceSlice(),
    );
}

test "unify - type_apply - not eql - both a & b diff type with same args" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const list_str = Descriptor.mkList(str);
    const maybe_str = Descriptor.mkMaybe(str);

    const a = solver.mkRoot(list_str);
    const b = solver.mkRoot(maybe_str);

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.LIST, .right = Name.MAYBE },
    }, err.cause);
    try std.testing.expectEqual(1, err.depth);
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

    const str = solver.mkRoot(Descriptor.STR);
    const list_str = solver.mkRoot(Descriptor.mkList(str));
    const list_list_str = solver.mkRoot(Descriptor.mkList(list_str));
    const list_list_list_str = solver.mkRoot(Descriptor.mkList(list_list_str));

    const bbool = solver.mkRoot(Descriptor.BOOL);
    const list_bool = solver.mkRoot(Descriptor.mkList(bbool));
    const list_list_bool = solver.mkRoot(Descriptor.mkList(list_bool));
    const list_list_list_bool = solver.mkRoot(Descriptor.mkList(list_list_bool));

    const mb_err = solver.unify(list_list_list_str, list_list_list_bool);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.STR, .right = Name.BOOL },
    }, err.cause);
    try std.testing.expectEqual(4, err.depth);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = list_list_list_str, .right = list_list_list_bool },
            .{ .left = list_list_str, .right = list_list_bool },
            .{ .left = list_str, .right = list_bool },
            .{ .left = str, .right = bbool },
        },
        err.getTraceSlice(),
    );
}

// unification - tuple

test "unify - tuple - eql - both a & b are same" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const bbool = solver.mkRoot(Descriptor.BOOL);

    const a = solver.mkRoot(Descriptor.mkTuple(&[_]Var{ str, bbool }));
    const b = solver.mkRoot(Descriptor.mkTuple(&[_]Var{ str, bbool }));

    try std.testing.expectEqual(solver.unify(a, b), null);
}

test "unify - tuple - eql - both a & b are flipped" {
    const gpa = std.testing.allocator;
    var solver = Solver.init(gpa);
    defer solver.deinit();

    const str = solver.mkRoot(Descriptor.STR);
    const bbool = solver.mkRoot(Descriptor.BOOL);

    const a = solver.mkRoot(Descriptor.mkTuple(&[_]Var{ str, bbool }));
    const b = solver.mkRoot(Descriptor.mkTuple(&[_]Var{ bbool, str }));

    const mb_err = solver.unify(a, b);
    try std.testing.expect(mb_err != null);

    const err = mb_err.?;
    try std.testing.expectEqual(Solver.UnifyError.Cause{
        .apply_name_mismatch = .{ .left = Name.STR, .right = Name.BOOL },
    }, err.cause);
    try std.testing.expectEqual(2, err.depth);
    try std.testing.expectEqualSlices(
        Solver.UnifyError.TraceVars,
        &[_]Solver.UnifyError.TraceVars{
            .{ .left = a, .right = b },
            .{ .left = str, .right = bbool },
        },
        err.getTraceSlice(),
    );
}
