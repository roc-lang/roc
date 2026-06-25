//! Compiler-internal plans for optimized builtin `Iter` consumers.
//!
//! These data structures describe private iterator cursor state in post-check
//! IR. They do not change the public Roc `Iter` value representation.

/// Identifier for an iterator plan owned by a post-check program.
pub const IterPlanId = enum(u32) { _ };

/// Whether an iterator plan can reach a `Done` step.
pub const DoneReachability = enum {
    reachable,
    never,
};

/// Whether an iterator plan can produce a public step variant if it is
/// materialized. Optimized consumers use this as explicit reachability data.
pub const StepReachability = packed struct {
    append: bool = false,
    one: bool = false,
    skip: bool = false,
    done: bool = false,
};

/// Length information carried by a plan. `known` points at a Monotype
/// expression whose value is the known `U64` length in the current
/// specialization.
pub fn Length(comptime ExprId: type) type {
    return union(enum) {
        known: ExprId,
        unknown,
    };
}

pub const RangeInclusivity = enum {
    exclusive,
    inclusive,
};

pub fn IterPlan(
    comptime ExprId: type,
    comptime FnId: type,
    comptime TypeId: type,
) type {
    return struct {
        item_ty: TypeId,
        length: Length(ExprId),
        steps: StepReachability,
        done: DoneReachability,
        /// Ordinary public `Iter` value for this plan. The iterator-plan
        /// elimination rewrite uses this when a plan crosses a public
        /// observation boundary or is not yet consumed by an optimized
        /// consumer.
        materialized: ?ExprId = null,
        data: Data,

        pub const Data = union(enum) {
            list: ListIter,
            range: RangeIter,
            unbounded_range: UnboundedRangeIter,
            single: SingleIter,
            append: AppendIter,
            concat: ConcatIter,
            map: MapIter,
            filter: FilterIter,
            custom: CustomIter,
            public: PublicIter,
        };

        pub const ListIter = struct {
            list: ExprId,
            index: ExprId,
            len: ExprId,
        };

        pub const RangeIter = struct {
            current: ExprId,
            end: ExprId,
            step: ExprId,
            inclusivity: RangeInclusivity,
        };

        pub const UnboundedRangeIter = struct {
            current: ExprId,
            step: ExprId,
        };

        pub const SingleIter = struct {
            item: ExprId,
            emitted: ExprId,
        };

        pub const AppendIter = struct {
            before: IterPlanId,
            after: ExprId,
            phase: ExprId,
        };

        pub const ConcatIter = struct {
            first: IterPlanId,
            second: IterPlanId,
            phase: ExprId,
        };

        pub const MapIter = struct {
            source: IterPlanId,
            mapping_fn: ExprId,
        };

        pub const FilterKind = enum {
            keep_if,
            drop_if,
        };

        pub const FilterIter = struct {
            source: IterPlanId,
            predicate_fn: ExprId,
            kind: FilterKind,
        };

        pub const CustomIter = struct {
            state: ExprId,
            step_fn: ExprId,
        };

        pub const PublicIter = struct {
            iter_value: ExprId,
            materializer: ?FnId = null,
        };
    };
}
