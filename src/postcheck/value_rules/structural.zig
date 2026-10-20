//! Structural equality and hashing, defined once for every consumer.
//!
//! Derived `is_eq` compares a value's components and derived `to_hash` feeds
//! them into a hasher, in an order and with tag handling that must not depend
//! on which code computes them: a value hashed by one lowering and probed by
//! another must land in the same bucket, and two consumers must never disagree
//! on whether two values are equal. Each consumer decides, per type, how a
//! comparison or hash proceeds (a `Step`); the walkers here own what every
//! step means:
//!
//! - an aggregate's components are compared first to last, stopping at the
//!   first unequal one, and hashed first to last, each feeding the hasher the
//!   previous one produced;
//! - two tag values are equal when their discriminants are equal and the
//!   variant's payloads are, compared like an aggregate's components;
//! - a tag value hashes its variant index as a `u64`, then its payloads;
//! - a list hashes its length as a `u64`, then its items, and two lists are
//!   equal when their lengths and items are;
//! - a value whose type has no components (the empty record, a
//!   zero-sized union) equals every value of its type and hashes nothing.
//!
//! `EqLower` and `HashLower` realize those rules as LIR, built from each
//! comparison's continuation backward, for the direct solved-to-LIR lowering
//! and Boxy lowering. `EqEval` and `HashEval` realize them over runtime values
//! for the Boxy runtime's descriptor-driven equality and hashing.

const std = @import("std");

/// One component of an aggregate as a lowering reads it: its type, and the
/// index LIR reads it at.
pub fn Component(comptime Ty: type) type {
    return struct {
        ty: Ty,
        field_index: u32,
    };
}

/// Structural equality lowered to LIR.
///
/// `Ctx` supplies:
///
/// ```
/// Ty, Local, Stmt, Wrap, TagUnion, SwitchBranch, PayloadOperands, WalkError
/// Outcome                                     // has `equal` and `unequal` Stmts
/// allocator() Allocator
/// eqStep(Request) WalkError!Step                  // how to compare at the request's type
/// wrapRequest(Wrap, Request) Request          // the comparison a wrap delegates to
/// unwrap(Wrap, Request, compared: Stmt) WalkError!Stmt
/// releaseWrap(Wrap) void                      // an unfinished wrap's cleanup
/// outcome(Request) WalkError!Outcome              // where an aggregate continues when equal/unequal
/// finishOutcome(Request, Outcome, body: Stmt) WalkError!Stmt
/// assignBool(target, value: bool, next) WalkError!Stmt
/// boolLocal() WalkError!Local
/// componentLocal(Ty) WalkError!Local
/// isZstLocal(Local) bool
/// readField(dest, source, field_index, next) WalkError!Stmt
/// boolSwitch(cond, then, else) WalkError!Stmt
/// discriminantLocal() WalkError!Local
/// readDiscriminant(dest, source, next) WalkError!Stmt
/// discriminantsEqual(target, lhs, rhs, next) WalkError!Stmt
/// discriminantSwitch(cond, branches: []const Branch, default) WalkError!Stmt
/// variantCount(TagUnion) u32
/// payloadCount(TagUnion, variant) u32
/// payloadOperands(TagUnion, Request, variant, index) WalkError!PayloadOperands
/// readPayloads(TagUnion, Request, variant, index, PayloadOperands, next) WalkError!Stmt
/// ```
pub fn EqLower(comptime Ctx: type) type {
    return struct {
        const Ty = Ctx.Ty;
        const Local = Ctx.Local;
        const Stmt = Ctx.Stmt;
        pub const Error = Ctx.WalkError;

        pub const Request = struct {
            target: Local,
            lhs: Local,
            rhs: Local,
            ty: Ty,
            negated: bool,
            next: Stmt,
        };

        /// How a comparison proceeds at one type.
        pub const Step = union(enum) {
            /// The consumer lowered the whole comparison.
            done: Stmt,
            /// Compare at this type, reading the operands as they are.
            retarget: Ty,
            /// Compare what the wrap reads from the operands.
            wrap: Ctx.Wrap,
            /// Every value of the type equals every other.
            always_equal,
            /// Compare these components; owned, freed with `Ctx.allocator()`.
            components: []const Component(Ty),
            /// Compare discriminants, then the shared variant's payloads.
            tag_union: Ctx.TagUnion,
        };

        /// A switch arm: the consumer's branch type, with `value` and `body`.
        pub const Branch = Ctx.SwitchBranch;

        const Frame = struct {
            request: Request,
            awaiting: bool = false,
            state: union(enum) {
                wrap: Ctx.Wrap,
                components: struct {
                    components: []const Component(Ty),
                    outcome: Ctx.Outcome,
                    current: Stmt,
                    remaining: usize,
                    pending: struct { lhs: Local, rhs: Local, field_index: u32 } = undefined,
                },
                tag_union: struct {
                    tag_union: Ctx.TagUnion,
                    outcome: Ctx.Outcome,
                    lhs_disc: Local,
                    rhs_disc: Local,
                    same_disc: Local,
                    branches: []Branch,
                    variant: u32 = 0,
                    started: bool = false,
                    current: Stmt = undefined,
                    remaining: u32 = 0,
                    pending: struct { operands: Ctx.PayloadOperands, index: u32 } = undefined,
                },
            },
        };

        /// Lower the comparison `root` describes.
        pub fn lower(ctx: Ctx, root: Request) Error!Stmt {
            const allocator = ctx.allocator();
            var frames: std.ArrayList(Frame) = .empty;
            defer {
                var index = frames.items.len;
                while (index > 0) {
                    index -= 1;
                    release(ctx, &frames.items[index]);
                }
                frames.deinit(allocator);
            }
            var delivered = try beginEq(ctx, &frames, root);
            while (frames.items.len != 0) {
                switch (try stepFrame(ctx, &frames, delivered)) {
                    .request => |request| delivered = try beginEq(ctx, &frames, request),
                    .done => |stmt| delivered = stmt,
                }
            }
            return delivered.?;
        }

        fn release(ctx: Ctx, frame: *Frame) void {
            const allocator = ctx.allocator();
            switch (frame.state) {
                .wrap => |wrap| ctx.releaseWrap(wrap),
                .components => |components| allocator.free(components.components),
                .tag_union => |tag_union| allocator.free(tag_union.branches),
            }
        }

        fn push(ctx: Ctx, frames: *std.ArrayList(Frame), frame: Frame) Error!?Stmt {
            var pushed = frame;
            frames.append(ctx.allocator(), pushed) catch |err| {
                release(ctx, &pushed);
                return err;
            };
            return null;
        }

        /// The comparison of `request` when it needs no component
        /// comparison; otherwise null, with the frame that builds it pushed.
        fn beginEq(ctx: Ctx, frames: *std.ArrayList(Frame), root: Request) Error!?Stmt {
            var request = root;
            while (true) {
                switch (try ctx.eqStep(request)) {
                    .done => |stmt| return stmt,
                    .retarget => |ty| {
                        request.ty = ty;
                        continue;
                    },
                    .always_equal => return try ctx.assignBool(request.target, !request.negated, request.next),
                    .wrap => |wrap| return try push(ctx, frames, .{ .request = request, .state = .{ .wrap = wrap } }),
                    .components => |components| {
                        errdefer ctx.allocator().free(components);
                        const outcome = try ctx.outcome(request);
                        return try push(ctx, frames, .{ .request = request, .state = .{ .components = .{
                            .components = components,
                            .outcome = outcome,
                            .current = outcome.equal,
                            .remaining = components.len,
                        } } });
                    },
                    .tag_union => |tag_union| {
                        if (ctx.isZstLocal(request.lhs) and ctx.isZstLocal(request.rhs)) {
                            return try ctx.assignBool(request.target, !request.negated, request.next);
                        }
                        const outcome = try ctx.outcome(request);
                        const lhs_disc = try ctx.discriminantLocal();
                        const rhs_disc = try ctx.discriminantLocal();
                        const same_disc = try ctx.boolLocal();
                        const branches = try ctx.allocator().alloc(Branch, ctx.variantCount(tag_union));
                        return try push(ctx, frames, .{ .request = request, .state = .{ .tag_union = .{
                            .tag_union = tag_union,
                            .outcome = outcome,
                            .lhs_disc = lhs_disc,
                            .rhs_disc = rhs_disc,
                            .same_disc = same_disc,
                            .branches = branches,
                        } } });
                    },
                }
            }
        }

        const FrameStep = union(enum) {
            request: Request,
            done: Stmt,
        };

        fn finish(ctx: Ctx, frames: *std.ArrayList(Frame), stmt: Stmt) FrameStep {
            var frame = frames.pop().?;
            switch (frame.state) {
                // A finished wrap has already done its cleanup.
                .wrap => {},
                .components, .tag_union => release(ctx, &frame),
            }
            return .{ .done = stmt };
        }

        fn stepFrame(ctx: Ctx, frames: *std.ArrayList(Frame), delivered: ?Stmt) Error!FrameStep {
            const frame = &frames.items[frames.items.len - 1];
            const child: ?Stmt = if (frame.awaiting) delivered.? else null;
            frame.awaiting = false;
            const request = frame.request;
            switch (frame.state) {
                .wrap => |wrap| {
                    const compared = child orelse {
                        frame.awaiting = true;
                        return .{ .request = ctx.wrapRequest(wrap, request) };
                    };
                    return finish(ctx, frames, try ctx.unwrap(wrap, request, compared));
                },
                .components => |*state| {
                    // Each component's comparison reads its operands first.
                    if (child) |compared| {
                        var current = compared;
                        const pending = state.pending;
                        if (!ctx.isZstLocal(pending.rhs)) current = try ctx.readField(pending.rhs, request.rhs, pending.field_index, current);
                        if (!ctx.isZstLocal(pending.lhs)) current = try ctx.readField(pending.lhs, request.lhs, pending.field_index, current);
                        state.current = current;
                    }
                    // Components are compared last first, so the comparison
                    // of each one runs before the later ones it guards.
                    if (state.remaining == 0) return finish(ctx, frames, try ctx.finishOutcome(request, state.outcome, state.current));
                    state.remaining -= 1;
                    const component = state.components[state.remaining];
                    const lhs = try ctx.componentLocal(component.ty);
                    const rhs = try ctx.componentLocal(component.ty);
                    const equal = try ctx.boolLocal();
                    const on_compared = try ctx.boolSwitch(equal, state.current, state.outcome.unequal);
                    state.pending = .{ .lhs = lhs, .rhs = rhs, .field_index = component.field_index };
                    frame.awaiting = true;
                    return .{ .request = .{ .target = equal, .lhs = lhs, .rhs = rhs, .ty = component.ty, .negated = false, .next = on_compared } };
                },
                .tag_union => |*state| {
                    if (child) |compared| {
                        state.current = try ctx.readPayloads(state.tag_union, request, state.variant, state.pending.index, state.pending.operands, compared);
                    }
                    const variant_count = ctx.variantCount(state.tag_union);
                    while (state.variant < variant_count) {
                        if (!state.started) {
                            state.started = true;
                            state.current = state.outcome.equal;
                            state.remaining = ctx.payloadCount(state.tag_union, state.variant);
                        }
                        // Payloads, like components, are compared last first.
                        if (state.remaining > 0) {
                            state.remaining -= 1;
                            const index = state.remaining;
                            const operands = try ctx.payloadOperands(state.tag_union, request, state.variant, index);
                            const equal = try ctx.boolLocal();
                            const on_compared = try ctx.boolSwitch(equal, state.current, state.outcome.unequal);
                            state.pending = .{ .operands = operands, .index = index };
                            frame.awaiting = true;
                            return .{ .request = .{
                                .target = equal,
                                .lhs = operands.lhs,
                                .rhs = operands.rhs,
                                .ty = operands.ty,
                                .negated = false,
                                .next = on_compared,
                            } };
                        }
                        state.branches[state.variant] = .{ .value = state.variant, .body = state.current };
                        state.variant += 1;
                        state.started = false;
                    }
                    // Different discriminants are unequal; equal ones compare
                    // the variant's payloads.
                    const payload_switch = try ctx.discriminantSwitch(state.lhs_disc, state.branches, state.outcome.unequal);
                    const disc_switch = try ctx.boolSwitch(state.same_disc, payload_switch, state.outcome.unequal);
                    const compare_disc = try ctx.discriminantsEqual(state.same_disc, state.lhs_disc, state.rhs_disc, disc_switch);
                    const read_rhs = try ctx.readDiscriminant(state.rhs_disc, request.rhs, compare_disc);
                    const read_lhs = try ctx.readDiscriminant(state.lhs_disc, request.lhs, read_rhs);
                    return finish(ctx, frames, try ctx.finishOutcome(request, state.outcome, read_lhs));
                },
            }
        }
    };
}

/// Structural hashing lowered to LIR.
///
/// `Ctx` supplies:
///
/// ```
/// Ty, Local, Stmt, Wrap, TagUnion, SwitchBranch, JoinPoint, PayloadOperand, WalkError
/// allocator() Allocator
/// hashStep(Request) WalkError!Step
/// wrapRequest(Wrap, Request) Request
/// unwrap(Wrap, Request, hashed: Stmt) WalkError!Stmt
/// releaseWrap(Wrap) void
/// assignLocal(target, source, next) WalkError!Stmt
/// hasherLocal(like: Local) WalkError!Local        // a hasher stored like `like`
/// componentLocal(Ty) WalkError!Local
/// isZstLocal(Local) bool
/// readField(dest, source, field_index, next) WalkError!Stmt
/// joinPoint() JoinPoint                        // a fresh join point id
/// jump(JoinPoint) WalkError!Stmt
/// resultJoin(JoinPoint, target, next, body) WalkError!Stmt
/// discriminantLocal() WalkError!Local
/// readDiscriminant(dest, source, next) WalkError!Stmt
/// discriminantSwitch(cond, branches: []const Branch, default) WalkError!Stmt
/// unreachableDiscriminant() WalkError!Stmt
/// variantCount(TagUnion) u32
/// payloadCount(TagUnion, variant) u32
/// writeVariantIndex(target, hasher, variant, next) WalkError!Stmt
/// payloadOperand(TagUnion, Request, variant, index) WalkError!PayloadOperand
/// readPayload(TagUnion, Request, variant, index, PayloadOperand, next) WalkError!Stmt
/// ```
pub fn HashLower(comptime Ctx: type) type {
    return struct {
        const Ty = Ctx.Ty;
        const Local = Ctx.Local;
        const Stmt = Ctx.Stmt;
        pub const Error = Ctx.WalkError;

        pub const Request = struct {
            target: Local,
            value: Local,
            hasher: Local,
            ty: Ty,
            next: Stmt,
        };

        /// How a hash proceeds at one type.
        pub const Step = union(enum) {
            /// The consumer lowered the whole hash.
            done: Stmt,
            /// Hash at this type, reading the value as it is.
            retarget: Ty,
            /// Hash what the wrap reads from the value.
            wrap: Ctx.Wrap,
            /// The type has no components: the hasher passes through.
            nothing,
            /// Hash these components; owned, freed with `Ctx.allocator()`.
            components: []const Component(Ty),
            /// Hash the variant index, then the variant's payloads.
            tag_union: Ctx.TagUnion,
        };

        /// A switch arm: the consumer's branch type, with `value` and `body`.
        pub const Branch = Ctx.SwitchBranch;

        /// One variant's hash: its index written into the hasher, then its
        /// payloads threaded through hashers between that and the result.
        const VariantState = struct {
            tag_union: Ctx.TagUnion,
            request: Request,
            variant: u32,
            payload_count: u32,
            after_index: Local,
            intermediate: []Local,
            remaining: u32,
            current: Stmt,
            pending: Ctx.PayloadOperand = undefined,
        };

        const Frame = struct {
            request: Request,
            awaiting: bool = false,
            state: union(enum) {
                wrap: Ctx.Wrap,
                components: struct {
                    components: []const Component(Ty),
                    intermediate: []Local,
                    remaining: usize,
                    current: Stmt,
                    pending: Local = undefined,
                },
                /// A union whose single variant needs no discriminant.
                variant: VariantState,
                tag_union: struct {
                    tag_union: Ctx.TagUnion,
                    discriminant: Local,
                    done: Ctx.JoinPoint,
                    branches: []Branch,
                    index: u32 = 0,
                    variant: ?VariantState = null,
                },
            },
        };

        /// Lower the hash `root` describes.
        pub fn lower(ctx: Ctx, root: Request) Error!Stmt {
            const allocator = ctx.allocator();
            var frames: std.ArrayList(Frame) = .empty;
            defer {
                var index = frames.items.len;
                while (index > 0) {
                    index -= 1;
                    release(ctx, &frames.items[index]);
                }
                frames.deinit(allocator);
            }
            var delivered = try beginHash(ctx, &frames, root);
            while (frames.items.len != 0) {
                switch (try stepFrame(ctx, &frames, delivered)) {
                    .request => |request| delivered = try beginHash(ctx, &frames, request),
                    .done => |stmt| delivered = stmt,
                }
            }
            return delivered.?;
        }

        fn release(ctx: Ctx, frame: *Frame) void {
            const allocator = ctx.allocator();
            switch (frame.state) {
                .wrap => |wrap| ctx.releaseWrap(wrap),
                .components => |components| {
                    allocator.free(components.components);
                    allocator.free(components.intermediate);
                },
                .variant => |variant| allocator.free(variant.intermediate),
                .tag_union => |tag_union| {
                    allocator.free(tag_union.branches);
                    if (tag_union.variant) |variant| allocator.free(variant.intermediate);
                },
            }
        }

        fn push(ctx: Ctx, frames: *std.ArrayList(Frame), frame: Frame) Error!?Stmt {
            var pushed = frame;
            frames.append(ctx.allocator(), pushed) catch |err| {
                release(ctx, &pushed);
                return err;
            };
            return null;
        }

        /// The hashers a chain of `count` hashes threads between its input
        /// and output hashers.
        fn intermediateHashers(ctx: Ctx, hasher: Local, count: usize) Error![]Local {
            const intermediate = try ctx.allocator().alloc(Local, count -| 1);
            errdefer ctx.allocator().free(intermediate);
            for (intermediate) |*local| local.* = try ctx.hasherLocal(hasher);
            return intermediate;
        }

        /// The hash of `request` when it needs no component hash; otherwise
        /// null, with the frame that builds it pushed.
        fn beginHash(ctx: Ctx, frames: *std.ArrayList(Frame), root: Request) Error!?Stmt {
            var request = root;
            while (true) {
                switch (try ctx.hashStep(request)) {
                    .done => |stmt| return stmt,
                    .retarget => |ty| {
                        request.ty = ty;
                        continue;
                    },
                    .nothing => return try ctx.assignLocal(request.target, request.hasher, request.next),
                    .wrap => |wrap| return try push(ctx, frames, .{ .request = request, .state = .{ .wrap = wrap } }),
                    .components => |components| {
                        errdefer ctx.allocator().free(components);
                        if (components.len == 0) {
                            ctx.allocator().free(components);
                            return try ctx.assignLocal(request.target, request.hasher, request.next);
                        }
                        const intermediate = try intermediateHashers(ctx, request.hasher, components.len);
                        return try push(ctx, frames, .{ .request = request, .state = .{ .components = .{
                            .components = components,
                            .intermediate = intermediate,
                            .remaining = components.len,
                            .current = request.next,
                        } } });
                    },
                    .tag_union => |tag_union| {
                        const variant_count = ctx.variantCount(tag_union);
                        if (variant_count == 1 and ctx.isZstLocal(request.value)) {
                            return try push(ctx, frames, .{ .request = request, .state = .{ .variant = try beginVariant(ctx, tag_union, request, 0) } });
                        }
                        const discriminant = try ctx.discriminantLocal();
                        const done = ctx.joinPoint();
                        const branches = try ctx.allocator().alloc(Branch, variant_count);
                        return try push(ctx, frames, .{ .request = request, .state = .{ .tag_union = .{
                            .tag_union = tag_union,
                            .discriminant = discriminant,
                            .done = done,
                            .branches = branches,
                        } } });
                    },
                }
            }
        }

        fn beginVariant(ctx: Ctx, tag_union: Ctx.TagUnion, request: Request, variant: u32) Error!VariantState {
            const payload_count = ctx.payloadCount(tag_union, variant);
            const after_index = if (payload_count == 0) request.target else try ctx.hasherLocal(request.hasher);
            return .{
                .tag_union = tag_union,
                .request = request,
                .variant = variant,
                .payload_count = payload_count,
                .after_index = after_index,
                .intermediate = if (payload_count == 0) &.{} else try intermediateHashers(ctx, after_index, payload_count),
                .remaining = payload_count,
                .current = request.next,
            };
        }

        /// The next payload hash of a variant, or null once every payload is
        /// hashed. Payloads are built last first, threading the hashers.
        fn nextPayload(ctx: Ctx, state: *VariantState) Error!?Request {
            if (state.remaining == 0) return null;
            state.remaining -= 1;
            const index = state.remaining;
            const input = if (index == 0) state.after_index else state.intermediate[index - 1];
            const output = if (index == state.payload_count - 1) state.request.target else state.intermediate[index];
            state.pending = try ctx.payloadOperand(state.tag_union, state.request, state.variant, index);
            return .{ .target = output, .value = state.pending.local, .hasher = input, .ty = state.pending.ty, .next = state.current };
        }

        /// A variant's hash once its payloads are hashed: its index written
        /// first.
        fn finishVariant(ctx: Ctx, state: *const VariantState) Error!Stmt {
            return try ctx.writeVariantIndex(state.after_index, state.request.hasher, state.variant, state.current);
        }

        fn finish(ctx: Ctx, frames: *std.ArrayList(Frame), stmt: Stmt) FrameStep {
            var frame = frames.pop().?;
            switch (frame.state) {
                .wrap => {},
                .components, .variant, .tag_union => release(ctx, &frame),
            }
            return .{ .done = stmt };
        }

        const FrameStep = union(enum) {
            request: Request,
            done: Stmt,
        };

        fn stepFrame(ctx: Ctx, frames: *std.ArrayList(Frame), delivered: ?Stmt) Error!FrameStep {
            const frame = &frames.items[frames.items.len - 1];
            const child: ?Stmt = if (frame.awaiting) delivered.? else null;
            frame.awaiting = false;
            const request = frame.request;
            switch (frame.state) {
                .wrap => |wrap| {
                    const hashed = child orelse {
                        frame.awaiting = true;
                        return .{ .request = ctx.wrapRequest(wrap, request) };
                    };
                    return finish(ctx, frames, try ctx.unwrap(wrap, request, hashed));
                },
                .components => |*state| {
                    if (child) |hashed| {
                        const component = state.components[state.remaining];
                        state.current = if (ctx.isZstLocal(state.pending))
                            hashed
                        else
                            try ctx.readField(state.pending, request.value, component.field_index, hashed);
                    }
                    if (state.remaining == 0) return finish(ctx, frames, state.current);
                    // Components hash first to last; built from the result
                    // backward, the last one is lowered first.
                    state.remaining -= 1;
                    const index = state.remaining;
                    const component = state.components[index];
                    const input = if (index == 0) request.hasher else state.intermediate[index - 1];
                    const output = if (index == state.components.len - 1) request.target else state.intermediate[index];
                    state.pending = try ctx.componentLocal(component.ty);
                    frame.awaiting = true;
                    return .{ .request = .{ .target = output, .value = state.pending, .hasher = input, .ty = component.ty, .next = state.current } };
                },
                .variant => |*state| {
                    if (child) |hashed| state.current = try readPendingPayload(ctx, state, hashed);
                    if (try nextPayload(ctx, state)) |payload| {
                        frame.awaiting = true;
                        return .{ .request = payload };
                    }
                    return finish(ctx, frames, try finishVariant(ctx, state));
                },
                .tag_union => |*state| {
                    if (child) |hashed| {
                        const variant = &state.variant.?;
                        variant.current = try readPendingPayload(ctx, variant, hashed);
                    }
                    const variant_count = ctx.variantCount(state.tag_union);
                    while (true) {
                        if (state.variant) |*variant| {
                            if (try nextPayload(ctx, variant)) |payload| {
                                frame.awaiting = true;
                                return .{ .request = payload };
                            }
                            state.branches[state.index] = .{ .value = state.index, .body = try finishVariant(ctx, variant) };
                            ctx.allocator().free(variant.intermediate);
                            state.variant = null;
                            state.index += 1;
                        }
                        if (state.index == variant_count) break;
                        state.variant = try beginVariant(ctx, state.tag_union, .{
                            .target = request.target,
                            .value = request.value,
                            .hasher = request.hasher,
                            .ty = request.ty,
                            .next = try ctx.jump(state.done),
                        }, state.index);
                    }
                    const switch_stmt = try ctx.discriminantSwitch(state.discriminant, state.branches, try ctx.unreachableDiscriminant());
                    const read_discriminant = try ctx.readDiscriminant(state.discriminant, request.value, switch_stmt);
                    return finish(ctx, frames, try ctx.resultJoin(state.done, request.target, request.next, read_discriminant));
                },
            }
        }

        /// Read the payload whose hash `hashed` is, in front of it.
        fn readPendingPayload(ctx: Ctx, state: *const VariantState, hashed: Stmt) Error!Stmt {
            return try ctx.readPayload(state.tag_union, state.request, state.variant, state.remaining, state.pending, hashed);
        }
    };
}

/// Two operands a comparison reads.
pub fn OperandPair(comptime Operand: type) type {
    return struct { lhs: Operand, rhs: Operand };
}

/// Which variant two tag values hold.
pub fn VariantPair(comptime Operand: type) type {
    return union(enum) {
        /// Different variants: the values are unequal.
        different,
        /// Both hold the variant with this discriminant.
        same: u32,
        /// Both hold their union's extension, whose values compare as these.
        extension: OperandPair(Operand),
    };
}

/// Structural equality evaluated over runtime values.
///
/// `Ctx` supplies:
///
/// ```
/// Operand, WalkError, Aggregate, TagUnion, List
/// eqStep(lhs, rhs) WalkError!Step
/// componentCount(Aggregate) u32
/// componentOperands(Aggregate, index) WalkError!?OperandPair   // null: padding, which holds no value
/// variants(TagUnion) WalkError!VariantPair
/// payloadCount(TagUnion, discriminant) u32
/// payloadOperands(TagUnion, discriminant, index) WalkError!OperandPair
/// listLens(List) [2]u64
/// itemOperands(List, index) WalkError!OperandPair
/// ```
pub fn EqEval(comptime Ctx: type) type {
    return struct {
        const Operand = Ctx.Operand;
        pub const Error = Ctx.WalkError;
        pub const Pair = OperandPair(Operand);

        /// How a comparison proceeds at one value.
        pub const Step = union(enum) {
            /// The consumer compared the values.
            done: bool,
            /// Compare these operands instead.
            retarget: Pair,
            always_equal,
            components: Ctx.Aggregate,
            tag_union: Ctx.TagUnion,
            list: Ctx.List,
        };

        pub fn eq(ctx: Ctx, lhs: Operand, rhs: Operand) Error!bool {
            switch (try ctx.eqStep(lhs, rhs)) {
                .done => |equal| return equal,
                .retarget => |pair| return try eq(ctx, pair.lhs, pair.rhs),
                .always_equal => return true,
                // Components compare first to last, stopping at the first
                // unequal one.
                .components => |aggregate| {
                    var index: u32 = 0;
                    while (index < ctx.componentCount(aggregate)) : (index += 1) {
                        const pair = (try ctx.componentOperands(aggregate, index)) orelse continue;
                        if (!try eq(ctx, pair.lhs, pair.rhs)) return false;
                    }
                    return true;
                },
                // Equal discriminants, then the variant's payloads.
                .tag_union => |tag_union| switch (try ctx.variants(tag_union)) {
                    .different => return false,
                    .extension => |pair| return try eq(ctx, pair.lhs, pair.rhs),
                    .same => |discriminant| {
                        var index: u32 = 0;
                        while (index < ctx.payloadCount(tag_union, discriminant)) : (index += 1) {
                            const pair = try ctx.payloadOperands(tag_union, discriminant, index);
                            if (!try eq(ctx, pair.lhs, pair.rhs)) return false;
                        }
                        return true;
                    },
                },
                // Equal lengths, then the items in order.
                .list => |list| {
                    const lens = ctx.listLens(list);
                    if (lens[0] != lens[1]) return false;
                    var index: u64 = 0;
                    while (index < lens[0]) : (index += 1) {
                        const pair = try ctx.itemOperands(list, index);
                        if (!try eq(ctx, pair.lhs, pair.rhs)) return false;
                    }
                    return true;
                },
            }
        }
    };
}

/// Which variant a tag value holds, for hashing.
pub fn HashVariant(comptime Operand: type) type {
    return union(enum) {
        /// The variant with this discriminant, at this index of its row.
        variant: struct { discriminant: u32, row_index: u64 },
        /// The union's extension, which hashes as this value.
        extension: Operand,
    };
}

/// Structural hashing evaluated over runtime values.
///
/// `Ctx` supplies:
///
/// ```
/// Operand, Hasher, WalkError, Aggregate, TagUnion, List
/// hashStep(value, hasher) WalkError!Step
/// componentCount(Aggregate) u32
/// componentOperand(Aggregate, index) WalkError!?Operand   // null: padding
/// variant(TagUnion) WalkError!HashVariant
/// payloadCount(TagUnion, discriminant) u32
/// payloadOperand(TagUnion, discriminant, index) WalkError!Operand
/// writeU64(hasher, value) Hasher
/// listLen(List) u64
/// itemOperand(List, index) WalkError!Operand
/// ```
pub fn HashEval(comptime Ctx: type) type {
    return struct {
        const Operand = Ctx.Operand;
        const Hasher = Ctx.Hasher;
        pub const Error = Ctx.WalkError;

        /// How a hash proceeds at one value.
        pub const Step = union(enum) {
            /// The consumer hashed the value into this hasher.
            done: Hasher,
            /// Hash this operand instead.
            retarget: Operand,
            /// The value has no components: the hasher passes through.
            nothing,
            components: Ctx.Aggregate,
            tag_union: Ctx.TagUnion,
            list: Ctx.List,
        };

        pub fn hash(ctx: Ctx, value: Operand, hasher: Hasher) Error!Hasher {
            switch (try ctx.hashStep(value, hasher)) {
                .done => |result| return result,
                .retarget => |operand| return try hash(ctx, operand, hasher),
                .nothing => return hasher,
                // Components hash first to last, each feeding the hasher the
                // previous one produced.
                .components => |aggregate| {
                    var state = hasher;
                    var index: u32 = 0;
                    while (index < ctx.componentCount(aggregate)) : (index += 1) {
                        const component = (try ctx.componentOperand(aggregate, index)) orelse continue;
                        state = try hash(ctx, component, state);
                    }
                    return state;
                },
                // The variant's index in its row, then its payloads.
                .tag_union => |tag_union| switch (try ctx.variant(tag_union)) {
                    .extension => |operand| return try hash(ctx, operand, hasher),
                    .variant => |variant| {
                        var state = ctx.writeU64(hasher, variant.row_index);
                        var index: u32 = 0;
                        while (index < ctx.payloadCount(tag_union, variant.discriminant)) : (index += 1) {
                            state = try hash(ctx, try ctx.payloadOperand(tag_union, variant.discriminant, index), state);
                        }
                        return state;
                    },
                },
                // The length, then the items in order.
                .list => |list| {
                    const len = ctx.listLen(list);
                    var state = ctx.writeU64(hasher, len);
                    var index: u64 = 0;
                    while (index < len) : (index += 1) {
                        state = try hash(ctx, try ctx.itemOperand(list, index), state);
                    }
                    return state;
                },
            }
        }
    };
}
