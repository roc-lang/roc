//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
const types_store = @import("store.zig");
const types_mod = @import("types.zig");

const TypesStore = types_store.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const NominalType = types_mod.NominalType;
const Tuple = types_mod.Tuple;
const Rank = types_mod.Rank;
const Ident = base.Ident;

/// Hard ceiling on instantiation recursion depth. Finite types never approach
/// this; a self-referential static-dispatch `where` constraint (e.g.
/// `Vec(a) ... where [a.join : Vec(a), a -> a]` used nested) would otherwise
/// recurse forever, minting fresh constrained vars at each level that the
/// `var_map` memo cannot dedup. On hitting this ceiling the instantiator stops
/// and flags `recursion_overflow`; the caller turns that into an infinite-type
/// error rather than hanging.
pub const max_instantiation_depth: u32 = 8192;

/// Type to manage instantiation.
///
/// Entry point is `instantiateVar`
///
/// This type does not own any of it's fields – it's a convenience wrapper to
/// making threading it's field through all the recursive functions easier
pub const Instantiator = struct {
    // not owned
    store: *TypesStore,
    idents: *const base.Ident.Store,
    var_map: *std.AutoHashMap(Var, Var),
    constraint_fn_var_map: ?*std.AutoHashMap(Var, Var) = null,

    current_rank: Rank,
    rigid_behavior: RigidBehavior,
    rank_behavior: RankBehavior = .respect_rank,

    /// Live recursion depth, guarded against non-terminating instantiation.
    depth: u32 = 0,
    /// Set once `depth` exceeds `max_instantiation_depth`. The entry point
    /// (`Check.instantiateVarHelp`) turns this into an infinite-type error
    /// instead of using the (partial, err-filled) result.
    recursion_overflow: bool = false,

    /// Controls whether to respect rank when deciding what to instantiate
    pub const RankBehavior = enum {
        /// Only instantiate generalized types (type checker semantics)
        respect_rank,
        /// Instantiate all types regardless of rank (runtime semantics)
        ignore_rank,
    };

    /// The mode to use when instantiating rigids
    pub const RigidBehavior = union(enum) {
        /// In this mode, all rigids are instantiated as new flex vars
        /// Note that the the rigid var structure will be preserved.
        /// E.g. `a -> a`, `a` will reference the same new rigid var
        fresh_flex,

        /// In this mode, all rigids are instantiated as new rigid variables
        /// Note that the the rigid var structure will be preserved.
        /// E.g. `a -> a`, `a` will reference the same new flex var
        fresh_rigid,

        /// In this mode, all rigids  we be substituted with values in the provided map.
        /// If a rigid var is not in the map, then that variable will be set to
        /// `.err` & in debug mode it will error
        substitute_rigids: *std.AutoHashMapUnmanaged(Ident.Idx, Var),

        /// In this mode, rigids present in the provided map are substituted,
        /// and any other rigids are instantiated as fresh rigid variables.
        substitute_rigids_fresh: *std.AutoHashMapUnmanaged(Ident.Idx, Var),

        /// In this mode, rigids present in the provided map are substituted,
        /// and any other rigids are instantiated as fresh flex variables.
        substitute_rigids_fresh_flex: *std.AutoHashMapUnmanaged(Ident.Idx, Var),
    };

    const Self = @This();

    fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.idents.getText(idx);
    }

    // instantiation //

    /// Instantiate a variable
    pub fn instantiateVar(
        self: *Self,
        initial_var: Var,
    ) std.mem.Allocator.Error!Var {
        const resolved = self.store.resolveVar(initial_var);
        const resolved_var = resolved.var_;

        // Guard against non-terminating instantiation (e.g. a self-referential
        // static-dispatch `where` constraint). Stop recursing and flag overflow;
        // the caller reports an infinite-type error.
        self.depth += 1;
        defer self.depth -= 1;
        if (self.depth > max_instantiation_depth) {
            self.recursion_overflow = true;
            return try self.store.freshFromContentWithRank(.err, self.current_rank);
        }

        // Non-generalized variables should _not_ be instantiated (unless configured to ignore rank)
        if (self.rank_behavior == .respect_rank and resolved.desc.rank != .generalized) {
            return resolved_var;
        }

        // Check if we've already instantiated this variable
        if (self.var_map.count() > 0) {
            if (self.var_map.get(resolved_var)) |fresh_var| {
                return fresh_var;
            }
        }

        switch (resolved.desc.content) {
            .rigid => |rigid| {
                // If this var is rigid, then create a new var depending on the
                // provided behavior
                const fresh_type: enum { flex, rigid } = blk: {
                    switch (self.rigid_behavior) {
                        .fresh_rigid => {
                            break :blk .rigid;
                        },
                        .fresh_flex => {
                            break :blk .flex;
                        },
                        .substitute_rigids => |rigid_subs| {
                            // If this is a var that we're substituting, then we
                            // we just return it.

                            const existing_var = inner_blk: {
                                if (rigid_subs.get(rigid.name)) |existing_flex| {
                                    break :inner_blk existing_flex;
                                } else {
                                    std.debug.assert(false);
                                    break :inner_blk try self.store.freshFromContentWithRank(
                                        .err,
                                        self.current_rank,
                                    );
                                }
                            };

                            // Remember this substitution for recursive references
                            try self.var_map.put(resolved_var, existing_var);

                            return existing_var;
                        },
                        .substitute_rigids_fresh => |rigid_subs| {
                            if (rigid_subs.get(rigid.name)) |existing_var| {
                                try self.var_map.put(resolved_var, existing_var);
                                return existing_var;
                            }
                            break :blk .rigid;
                        },
                        .substitute_rigids_fresh_flex => |rigid_subs| {
                            if (rigid_subs.get(rigid.name)) |existing_var| {
                                try self.var_map.put(resolved_var, existing_var);
                                return existing_var;
                            }
                            break :blk .flex;
                        },
                    }
                };

                // Remember this substitution for recursive references
                // IMPORTANT: This has to be inserted _before_ we recurse into `instantiateContent`
                const fresh_var = try self.store.freshFromContentWithRank(.{ .flex = Flex.init() }, self.current_rank);
                try self.var_map.put(resolved_var, fresh_var);

                // Copy the rigid var's constraints
                const fresh_constraints = try self.instantiateStaticDispatchConstraints(rigid.constraints);

                // Copy the rigid var's constraints
                const fresh_content = switch (fresh_type) {
                    .flex => Content{ .flex = Flex{ .name = rigid.name, .constraints = fresh_constraints } },
                    .rigid => Content{ .rigid = Rigid{ .name = rigid.name, .constraints = fresh_constraints } },
                };

                // Update the placeholder fresh var with the real content
                try self.store.dangerousSetVarDesc(
                    fresh_var,
                    .{
                        .content = fresh_content,
                        .rank = self.current_rank,
                    },
                );

                return fresh_var;
            },
            else => {
                // Generate the content

                // Remember this substitution for recursive references
                // IMPORTANT: This has to be inserted _before_ we recurse into `instantiateContent`
                const fresh_var = try self.store.fresh();
                try self.var_map.put(resolved_var, fresh_var);

                const fresh_content = try self.instantiateContent(resolved.desc.content);

                // Update the placeholder fresh var with the real content
                try self.store.dangerousSetVarDesc(
                    fresh_var,
                    .{
                        .content = fresh_content,
                        .rank = self.current_rank,
                    },
                );

                return fresh_var;
            },
        }
    }

    fn instantiateContent(self: *Self, content: Content) std.mem.Allocator.Error!Content {
        return switch (content) {
            .flex => |flex| Content{ .flex = try self.instantiateFlex(flex) },
            .rigid => {
                // Rigids should be handled by `instantiateVar`
                // If we have run into one here, it is  abug
                unreachable;
            },
            .alias => |alias| {
                // Instantiate the structure recursively
                return try self.instantiateAlias(alias);
            },
            .structure => |flat_type| blk: {
                // Instantiate the structure recursively
                const fresh_flat_type = try self.instantiateFlatType(flat_type);
                break :blk Content{ .structure = fresh_flat_type };
            },
            .err => Content.err,
        };
    }

    fn instantiateFlex(self: *Self, flex: Flex) std.mem.Allocator.Error!Flex {
        const fresh_constraints = try self.instantiateStaticDispatchConstraints(flex.constraints);

        return Flex{ .name = flex.name, .constraints = fresh_constraints };
    }

    fn instantiateAlias(self: *Self, alias: Alias) std.mem.Allocator.Error!Content {
        var arg_span = alias.vars.nonempty;
        arg_span.dropFirstElem();

        var fresh_vars_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.store.gpa);
        const fresh_vars_alloc = fresh_vars_sfa.get();
        var fresh_vars = try std.ArrayList(Var).initCapacity(fresh_vars_alloc, arg_span.count);
        defer fresh_vars.deinit(fresh_vars_alloc);

        const args_start: usize = @intFromEnum(arg_span.start);
        for (0..arg_span.count) |i| {
            const arg_var = self.store.vars.items.items[args_start + i];
            fresh_vars.appendAssumeCapacity(try self.instantiateVar(arg_var));
        }

        const backing_var = self.store.getAliasBackingVar(alias);
        const fresh_backing_var = try self.instantiateVar(backing_var);

        return self.store.mkAliasWithSourceDeclAndBuiltinOrigin(
            alias.ident,
            fresh_backing_var,
            fresh_vars.items,
            alias.origin_module,
            alias.source_decl.toOptional(),
            alias.source_decl.originIsBuiltin(),
        );
    }

    fn instantiateFlatType(self: *Self, flat_type: FlatType) std.mem.Allocator.Error!FlatType {
        return switch (flat_type) {
            .tuple => |tuple| FlatType{ .tuple = try self.instantiateTuple(tuple) },
            .nominal_type => |nominal| FlatType{ .nominal_type = try self.instantiateNominalType(nominal) },
            .fn_pure => |func| FlatType{ .fn_pure = try self.instantiateFunc(func) },
            .fn_effectful => |func| FlatType{ .fn_effectful = try self.instantiateFunc(func) },
            .fn_unbound => |func| FlatType{ .fn_unbound = try self.instantiateFunc(func) },
            .record => |record| FlatType{ .record = try self.instantiateRecord(record) },
            .record_unbound => |fields| FlatType{ .record_unbound = try self.instantiateRecordFields(fields) },
            .empty_record => FlatType.empty_record,
            .tag_union => |tag_union| FlatType{ .tag_union = try self.instantiateTagUnion(tag_union) },
            .empty_tag_union => FlatType.empty_tag_union,
        };
    }

    fn instantiateNominalType(self: *Self, nominal: NominalType) std.mem.Allocator.Error!NominalType {
        const backing_var = self.store.getNominalBackingVar(nominal);
        const fresh_backing_var = try self.instantiateVar(backing_var);

        const arg_span = TypesStore.getNominalArgsRange(nominal);
        var fresh_vars_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.store.gpa);
        const fresh_vars_alloc = fresh_vars_sfa.get();
        var fresh_vars = try std.ArrayList(Var).initCapacity(fresh_vars_alloc, arg_span.count);
        defer fresh_vars.deinit(fresh_vars_alloc);

        const args_start: usize = @intFromEnum(arg_span.start);
        for (0..arg_span.count) |i| {
            const arg_var = self.store.vars.items.items[args_start + i];
            fresh_vars.appendAssumeCapacity(try self.instantiateVar(arg_var));
        }

        return (try self.store.mkNominalWithSourceDeclAndBuiltinOrigin(
            nominal.ident,
            fresh_backing_var,
            fresh_vars.items,
            nominal.origin_module,
            nominal.sourceDeclOptional(),
            nominal.isOpaque(),
            nominal.originIsBuiltin(),
        )).structure.nominal_type;
    }

    fn instantiateTuple(self: *Self, tuple: Tuple) std.mem.Allocator.Error!Tuple {
        // Use index-based iteration to avoid iterator invalidation
        // (see comment in instantiateFunc for details)
        var fresh_elems_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.store.gpa);
        const fresh_elems_alloc = fresh_elems_sfa.get();
        var fresh_elems = try std.ArrayList(Var).initCapacity(fresh_elems_alloc, tuple.elems.count);
        defer fresh_elems.deinit(fresh_elems_alloc);

        const elems_start: usize = @intFromEnum(tuple.elems.start);
        for (0..tuple.elems.count) |i| {
            const elem_var = self.store.vars.items.items[elems_start + i];
            fresh_elems.appendAssumeCapacity(try self.instantiateVar(elem_var));
        }

        const fresh_elems_range = try self.store.appendVars(fresh_elems.items);
        return Tuple{ .elems = fresh_elems_range };
    }
    fn instantiateFunc(self: *Self, func: Func) std.mem.Allocator.Error!Func {
        // IMPORTANT: We must use index-based iteration here, not slice-based.
        // The slice would point into the backing ArrayList, but instantiateVar
        // can recursively call appendVars which may reallocate the array,
        // invalidating the slice pointer.
        var fresh_args_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.store.gpa);
        const fresh_args_alloc = fresh_args_sfa.get();
        var fresh_args = try std.ArrayList(Var).initCapacity(fresh_args_alloc, func.args.count);
        defer fresh_args.deinit(fresh_args_alloc);

        const args_start: usize = @intFromEnum(func.args.start);
        for (0..func.args.count) |i| {
            // Re-fetch the var on each iteration since the backing array may have moved
            const arg_var = self.store.vars.items.items[args_start + i];
            fresh_args.appendAssumeCapacity(try self.instantiateVar(arg_var));
        }

        const fresh_ret = try self.instantiateVar(func.ret);
        const fresh_args_range = try self.store.appendVars(fresh_args.items);
        return Func{
            .args = fresh_args_range,
            .ret = fresh_ret,
            .needs_instantiation = true,
        };
    }

    fn instantiateRecordFields(self: *Self, fields: RecordField.SafeMultiList.Range) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
        // IMPORTANT: We must use index-based iteration here, not slice-based.
        // The slice would point into the backing MultiArrayList, but instantiateVar
        // can recursively call appendRecordFields which may reallocate the array,
        // invalidating the slice pointers.
        if (fields.count == 0) {
            return try self.store.appendRecordFields(&.{});
        }

        var fresh_fields_sfa = std.heap.stackFallback(16 * @sizeOf(RecordField), self.store.gpa);
        const fresh_fields_alloc = fresh_fields_sfa.get();
        var fresh_fields = try std.ArrayList(RecordField).initCapacity(fresh_fields_alloc, fields.count);
        defer fresh_fields.deinit(fresh_fields_alloc);

        const fields_start: usize = @intFromEnum(fields.start);
        for (0..fields.count) |i| {
            // Re-fetch the field data on each iteration since the backing array may have moved
            const field = self.store.record_fields.get(@enumFromInt(fields_start + i));
            const fresh_type = try self.instantiateVar(field.var_);
            fresh_fields.appendAssumeCapacity(RecordField{
                .name = field.name,
                .var_ = fresh_type,
            });
        }

        return try self.store.appendRecordFields(fresh_fields.items);
    }

    fn instantiateRecord(self: *Self, record: Record) std.mem.Allocator.Error!Record {
        // IMPORTANT: We must use index-based iteration here, not slice-based.
        // The slice would point into the backing MultiArrayList, but instantiateVar
        // can recursively call appendRecordFields which may reallocate the array,
        // invalidating the slice pointers.
        if (record.fields.count == 0) {
            return Record{
                .fields = try self.store.appendRecordFields(&.{}),
                .ext = try self.instantiateVar(record.ext),
            };
        }

        var fresh_fields_sfa = std.heap.stackFallback(16 * @sizeOf(RecordField), self.store.gpa);
        const fresh_fields_alloc = fresh_fields_sfa.get();
        var fresh_fields = try std.ArrayList(RecordField).initCapacity(fresh_fields_alloc, record.fields.count);
        defer fresh_fields.deinit(fresh_fields_alloc);

        const fields_start: usize = @intFromEnum(record.fields.start);
        for (0..record.fields.count) |i| {
            // Re-fetch the field data on each iteration since the backing array may have moved
            const field = self.store.record_fields.get(@enumFromInt(fields_start + i));
            const fresh_type = try self.instantiateVar(field.var_);
            fresh_fields.appendAssumeCapacity(RecordField{
                .name = field.name,
                .var_ = fresh_type,
            });
        }

        const fields_range = try self.store.appendRecordFields(fresh_fields.items);
        return Record{
            .fields = fields_range,
            .ext = try self.instantiateVar(record.ext),
        };
    }

    fn instantiateTagUnion(self: *Self, tag_union: TagUnion) std.mem.Allocator.Error!TagUnion {
        // IMPORTANT: We must use index-based iteration here, not slice-based.
        // The slice would point into the backing MultiArrayList, but instantiateVar
        // can recursively call appendTags which may reallocate the array,
        // invalidating the slice pointers.
        if (tag_union.tags.count == 0) {
            return TagUnion{
                .tags = try self.store.appendTags(&.{}),
                .ext = try self.instantiateVar(tag_union.ext),
            };
        }

        var fresh_tags_sfa = std.heap.stackFallback(16 * @sizeOf(Tag), self.store.gpa);
        const fresh_tags_alloc = fresh_tags_sfa.get();
        var fresh_tags = try std.ArrayList(Tag).initCapacity(fresh_tags_alloc, tag_union.tags.count);
        defer fresh_tags.deinit(fresh_tags_alloc);

        const tags_start: usize = @intFromEnum(tag_union.tags.start);
        for (0..tag_union.tags.count) |tag_i| {
            // Re-fetch the tag data on each iteration since the backing array may have moved
            const tag = self.store.tags.get(@enumFromInt(tags_start + tag_i));
            const tag_name = tag.name;
            const tag_args = tag.args;

            var fresh_args_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.store.gpa);
            const fresh_args_alloc = fresh_args_sfa.get();
            var fresh_args = try std.ArrayList(Var).initCapacity(fresh_args_alloc, tag_args.count);
            defer fresh_args.deinit(fresh_args_alloc);

            // Skip the loop entirely for tags with no arguments.
            // This avoids accessing tag_args.start which may be undefined when count is 0.
            if (tag_args.count > 0) {
                // Use index-based iteration to avoid iterator invalidation
                // (see comment in instantiateFunc for details)
                const args_start: usize = @intFromEnum(tag_args.start);
                for (0..tag_args.count) |i| {
                    const arg_var = self.store.vars.items.items[args_start + i];
                    fresh_args.appendAssumeCapacity(try self.instantiateVar(arg_var));
                }
            }

            const fresh_args_range = try self.store.appendVars(fresh_args.items);

            fresh_tags.appendAssumeCapacity(Tag{
                .name = tag_name,
                .args = fresh_args_range,
            });
        }

        // Sort the fresh tags alphabetically by name before appending.
        // This ensures tag discriminants are consistent after instantiation.
        std.mem.sort(Tag, fresh_tags.items, self, struct {
            fn less(instantiator: *const Self, a: Tag, b: Tag) bool {
                return std.mem.order(u8, instantiator.getIdentText(a.name), instantiator.getIdentText(b.name)) == .lt;
            }
        }.less);

        const tags_range = try self.store.appendTags(fresh_tags.items);
        return TagUnion{
            .tags = tags_range,
            .ext = try self.instantiateVar(tag_union.ext),
        };
    }

    pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.getIdentText(idx);
    }

    fn instantiateStaticDispatchConstraints(self: *Self, constraints: StaticDispatchConstraint.SafeList.Range) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        const constraints_len = constraints.len();
        if (constraints_len == 0) {
            return StaticDispatchConstraint.SafeList.Range.empty();
        } else {
            var fresh_constraints_sfa = std.heap.stackFallback(8 * @sizeOf(StaticDispatchConstraint), self.store.gpa);
            const fresh_constraints_alloc = fresh_constraints_sfa.get();
            var fresh_constraints = try std.ArrayList(StaticDispatchConstraint).initCapacity(fresh_constraints_alloc, constraints.len());
            defer fresh_constraints.deinit(fresh_constraints_alloc);

            // IMPORTANT: We must re-fetch on each iteration, not cache the slice.
            // The slice would point into the backing ArrayList, but instantiateVar
            // can recursively instantiate flex vars with constraints, which calls
            // appendStaticDispatchConstraints, potentially reallocating the array
            // and invalidating any cached slice.
            const constraints_start: usize = @intFromEnum(constraints.start);
            for (0..constraints_len) |i| {
                // Re-fetch the constraint on each iteration since the backing array may have moved
                const constraint = self.store.static_dispatch_constraints.items.items[constraints_start + i];
                fresh_constraints.appendAssumeCapacity(try self.instantiateStaticDispatchConstraint(constraint));
            }

            const fresh_constraints_range = try self.store.appendStaticDispatchConstraints(fresh_constraints.items);
            return fresh_constraints_range;
        }
    }

    fn instantiateStaticDispatchConstraint(self: *Self, constraint: StaticDispatchConstraint) std.mem.Allocator.Error!StaticDispatchConstraint {
        var result = constraint;
        result.fn_var = try self.instantiateVar(constraint.fn_var);
        if (self.constraint_fn_var_map) |map| {
            try map.put(constraint.fn_var, result.fn_var);
        }
        return result;
    }
};
