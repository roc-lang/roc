//! Regression tests for issue #806.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

const issue806GuardPageSize = 4096;
const issue806UnsafePageSpan = 3;
const issue806UnsafeAggregateBytes = issue806GuardPageSize * issue806UnsafePageSpan;

const issue806RightmostField = "value.b.b.b.b.b.b.b.b.b.b.b.b";

const issue806LargeAggregateDefinitions =
    \\R1 : { a : U64, b : U64 }
    \\R2 : { a : R1, b : R1 }
    \\R3 : { a : R2, b : R2 }
    \\R4 : { a : R3, b : R3 }
    \\R5 : { a : R4, b : R4 }
    \\R6 : { a : R5, b : R5 }
    \\R7 : { a : R6, b : R6 }
    \\R8 : { a : R7, b : R7 }
    \\R9 : { a : R8, b : R8 }
    \\R10 : { a : R9, b : R9 }
    \\R11 : { a : R10, b : R10 }
    \\R12 : { a : R11, b : R11 }
    \\
    \\make1 : U64 -> R1
    \\make1 = |n| { a: n, b: n + 1 }
    \\
    \\make2 : U64 -> R2
    \\make2 = |n| { a: make1(n), b: make1(n + 2) }
    \\
    \\make3 : U64 -> R3
    \\make3 = |n| { a: make2(n), b: make2(n + 4) }
    \\
    \\make4 : U64 -> R4
    \\make4 = |n| { a: make3(n), b: make3(n + 8) }
    \\
    \\make5 : U64 -> R5
    \\make5 = |n| { a: make4(n), b: make4(n + 16) }
    \\
    \\make6 : U64 -> R6
    \\make6 = |n| { a: make5(n), b: make5(n + 32) }
    \\
    \\make7 : U64 -> R7
    \\make7 = |n| { a: make6(n), b: make6(n + 64) }
    \\
    \\make8 : U64 -> R8
    \\make8 = |n| { a: make7(n), b: make7(n + 128) }
    \\
    \\make9 : U64 -> R9
    \\make9 = |n| { a: make8(n), b: make8(n + 256) }
    \\
    \\make10 : U64 -> R10
    \\make10 = |n| { a: make9(n), b: make9(n + 512) }
    \\
    \\make11 : U64 -> R11
    \\make11 = |n| { a: make10(n), b: make10(n + 1024) }
    \\
    \\make12 : U64 -> R12
    \\make12 = |n| { a: make11(n), b: make11(n + 2048) }
    \\
++ "rightmost : R12 -> U64\n" ++
    "rightmost = |value| " ++ issue806RightmostField ++ "\n\n" ++
    \\
;

const issue806MultiTagDefinitions =
    \\MultiWrapped := [Empty, Payload(R12)]
    \\
    \\multiWrap : U64 -> MultiWrapped
    \\multiWrap = |n| Payload(make12(n))
    \\
;

const issue806MixedDefinitions =
    \\Mixed := { tiny : U8, payload : R12, flag : U16, tail : U8 }
    \\
    \\makeMixed : U64 -> Mixed
    \\makeMixed = |n| { tiny: 1, payload: make12(n), flag: 2, tail: 3 }
    \\
;

const issue806IdentityDefinitions =
    \\identityLarge : R12 -> R12
    \\identityLarge = |value| value
    \\
;

const issue806ReturnDefinitions =
    \\returnLarge : U64 -> R12
    \\returnLarge = |n| make12(n)
    \\
;

const issue806CallArgumentDefinitions =
    \\useLargeArg : R12 -> U64
    \\useLargeArg = |value| rightmost(value)
    \\
;

const issue806MainPrefix =
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |_args| {
;

const issue806MainSuffix =
    \\
    \\    Ok({})
    \\}
;

const issue806StructAppBody = issue806LargeAggregateDefinitions ++ issue806MainPrefix ++
    \\    value = make12(1)
    \\    _ = rightmost(value)
    \\
++ issue806MainSuffix;

const issue806MultiTagAppBody = issue806LargeAggregateDefinitions ++ issue806MultiTagDefinitions ++ issue806MainPrefix ++
    \\    _ = match multiWrap(1) {
    \\        Payload(value) => rightmost(value)
    \\        Empty => 0
    \\    }
    \\
++ issue806MainSuffix;

const issue806InlineClosureCaptureAppBody = issue806LargeAggregateDefinitions ++ issue806MainPrefix ++
    \\    captured = make12(1)
    \\    boxed = Box.box(|_| rightmost(captured))
    \\    closure = Box.unbox(boxed)
    \\    _ = closure({})
    \\
++ issue806MainSuffix;

const issue806MatchPayloadAppBody = issue806LargeAggregateDefinitions ++ issue806MultiTagDefinitions ++ issue806MainPrefix ++
    \\    value = match multiWrap(1) {
    \\        Payload(payload) => payload
    \\        Empty => make12(0)
    \\    }
    \\    _ = rightmost(value)
    \\
++ issue806MainSuffix;

const issue806CopyCallReturnAppBody = issue806LargeAggregateDefinitions ++ issue806ReturnDefinitions ++ issue806IdentityDefinitions ++ issue806CallArgumentDefinitions ++ issue806MainPrefix ++
    \\    literal = make12(1)
    \\    copied = literal
    \\    fromCall = returnLarge(2)
    \\    _ = useLargeArg(fromCall)
    \\    throughArg = identityLarge(copied)
    \\    _ = rightmost(throughArg)
    \\
++ issue806MainSuffix;

const issue806JoinMixedAppBody = issue806LargeAggregateDefinitions ++ issue806MixedDefinitions ++ issue806MainPrefix ++
    \\    branched = if 1 == 1 {
    \\        make12(1)
    \\    } else {
    \\        make12(2)
    \\    }
    \\    mixed = makeMixed(3)
    \\    _ = rightmost(branched)
    \\    _ = rightmost(mixed.payload)
    \\
++ issue806MainSuffix;

test "issue 806: large stack record writes are explicit before backend consumers" {
    try harness.expectLirInspection(issue806StructAppBody, expectLargeStackStructAssignsAreFrameProbed);
}

test "issue 806: multi-variant large stack tag writes are explicit before backend consumers" {
    try harness.expectLirInspection(issue806MultiTagAppBody, expectLargeDiscriminatedStackTagAssignsAreFrameProbed);
}

test "issue 806: large aggregate copy call and return paths are explicit before backend consumers" {
    try harness.expectLirInspection(issue806CopyCallReturnAppBody, expectCopyCallReturnLargeStackAggregateEdgesAreFrameProbed);
}

test "issue 806: large aggregate join and mixed-field paths are explicit before backend consumers" {
    try harness.expectLirInspection(issue806JoinMixedAppBody, expectJoinMixedLargeStackAggregateEdgesAreFrameProbed);
}

test "issue 806: large match payload extraction is explicit before backend consumers" {
    try harness.expectLirInspection(issue806MatchPayloadAppBody, expectLargeMatchPayloadExtractionIsExplicitAndFrameProbed);
}

test "issue 806: large closure captures are explicit before backend consumers" {
    try harness.expectLirInspection(issue806InlineClosureCaptureAppBody, expectLargeClosureCapturesAreExplicitAndFrameProbed);
}

fn expectLargeStackStructAssignsAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeAggregateLocal(store, layouts, .struct_));
    try std.testing.expect(hasLargeStackStructAssign(store, layouts));
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn expectLargeMixedStackStructAssignsAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeMixedStructLocal(store, layouts));
    try expectLargeStackStructAssignsAreFrameProbed(store, layouts);
}

fn expectLargeStackTagAssignsAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeAggregateLocal(store, layouts, .tag_union));
    try std.testing.expect(hasLargeStackTagAssign(store, layouts));
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn expectLargeDiscriminatedStackTagAssignsAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeDiscriminatedTagLocal(store, layouts));
    try expectLargeStackTagAssignsAreFrameProbed(store, layouts);
}

fn expectCopyCallReturnLargeStackAggregateEdgesAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeAggregateLocal(store, layouts, .struct_));

    try std.testing.expect(hasLargeStackCallReturn(store, layouts));
    try std.testing.expect(hasLargeStackCallArgument(store, layouts));
    try std.testing.expect(hasLargeStackReturn(store, layouts));
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn expectJoinMixedLargeStackAggregateEdgesAreFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeMixedStructLocal(store, layouts));

    try expectLargeMixedStackStructAssignsAreFrameProbed(store, layouts);
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn hasLargeStackStructAssign(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_struct => |assign| if (isUnsafeLargeAggregateLocal(store, layouts, assign.target)) return true,
            else => {},
        }
    }
    return false;
}

fn hasLargeStackTagAssign(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_tag => |assign| if (isUnsafeLargeAggregateLocal(store, layouts, assign.target)) return true,
            else => {},
        }
    }
    return false;
}

fn hasLargeStackCallReturn(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call => |assign| if (isUnsafeLargeAggregateLocal(store, layouts, assign.target)) return true,
            .assign_call_erased => |assign| if (isUnsafeLargeAggregateLocal(store, layouts, assign.target)) return true,
            else => {},
        }
    }
    return false;
}

fn hasLargeStackCallArgument(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.proc_specs.items) |proc| {
        if (spanHasUnsafeLargeAggregateLocal(store, layouts, proc.args)) return true;
    }

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call => |assign| if (spanHasUnsafeLargeAggregateLocal(store, layouts, assign.args)) return true,
            .assign_call_erased => |assign| if (spanHasUnsafeLargeAggregateLocal(store, layouts, assign.args)) return true,
            else => {},
        }
    }
    return false;
}

fn hasLargeStackReturn(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.proc_specs.items) |proc| {
        if (isUnsafeLargeAggregateLayoutIdx(layouts, proc.ret_layout)) return true;
    }

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .ret => |ret| if (isUnsafeLargeAggregateLocal(store, layouts, ret.value)) return true,
            else => {},
        }
    }
    return false;
}

fn expectLargeClosureCapturesAreExplicitAndFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try rejectConsumerOwnedLargeStackClosureCaptures(store, layouts);
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn expectLargeMatchPayloadExtractionIsExplicitAndFrameProbed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try rejectConsumerOwnedLargeStackPatternPayloads(store, layouts);
    try expectLargeAggregateProcsRequireStackProbe(store, layouts);
}

fn rejectConsumerOwnedLargeStackClosureCaptures(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasPackedErasedFn(store));

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    if (isUnsafeLargeAggregateLocal(store, layouts, capture)) {
                        return error.Issue806UnsafeLargeStackClosureCapture;
                    }
                }
                if (assign.capture_layout) |capture_layout| {
                    if (isUnsafeLargeAggregateLayoutIdx(layouts, capture_layout)) {
                        return error.Issue806UnsafeLargeStackClosureCapture;
                    }
                }
            },
            else => {},
        }
    }
}

fn hasPackedErasedFn(store: *const lir.LirStore) bool {
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_packed_erased_fn => return true,
            else => {},
        }
    }
    return false;
}

fn rejectConsumerOwnedLargeStackPatternPayloads(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasLargeDiscriminatedTagLocal(store, layouts));

    for (store.patterns.items, 0..) |_, index| {
        if (patternHasUnsafeLargeAggregate(store, layouts, @enumFromInt(@as(u32, @intCast(index))))) {
            return error.Issue806UnsafeLargeStackPatternPayload;
        }
    }

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .switch_initialized_payload => |switch_payload| {
                if (isUnsafeLargeAggregateLocal(store, layouts, switch_payload.payload)) {
                    return error.Issue806UnsafeLargeStackPatternPayload;
                }
            },
            else => {},
        }
    }
}

fn expectLargeAggregateProcsRequireStackProbe(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    for (store.proc_specs.items) |proc| {
        if (!procHasUnsafeLargeAggregate(store, layouts, proc)) continue;

        if (proc.stack_probe != .required) {
            return error.Issue806MissingStackProbe;
        }
    }
}

fn procHasUnsafeLargeAggregate(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    proc: lir.LirProcSpec,
) bool {
    if (spanHasUnsafeLargeAggregateLocal(store, layouts, proc.args)) return true;
    if (spanHasUnsafeLargeAggregateLocal(store, layouts, proc.frame_locals)) return true;
    if (isUnsafeLargeAggregateLayoutIdx(layouts, proc.ret_layout)) return true;
    return false;
}

fn hasLargeAggregateLocal(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    aggregate_tag: layout.LayoutTag,
) bool {
    for (store.locals.items) |local| {
        const local_layout = layouts.getLayout(local.layout_idx);
        if (local_layout.tag == aggregate_tag and layoutIsLargerThanGuardPages(layouts, local_layout)) {
            return true;
        }
    }
    return false;
}

fn hasLargeDiscriminatedTagLocal(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.locals.items) |local| {
        const local_layout = layouts.getLayout(local.layout_idx);
        if (local_layout.tag != .tag_union or !layoutIsLargerThanGuardPages(layouts, local_layout)) continue;

        const tag_data = layouts.getTagUnionData(local_layout.getTagUnion().idx);
        if (tag_data.discriminant_size > 0) {
            return true;
        }
    }
    return false;
}

fn hasLargeMixedStructLocal(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) bool {
    for (store.locals.items) |local| {
        const local_layout = layouts.getLayout(local.layout_idx);
        if (local_layout.tag != .struct_ or !layoutIsLargerThanGuardPages(layouts, local_layout)) continue;

        const struct_data = layouts.getStructData(local_layout.getStruct().idx);
        var has_large_field = false;
        var has_small_field = false;
        for (0..struct_data.fields.count) |field_index| {
            if (layouts.getStructFieldIsPadding(local_layout.getStruct().idx, @intCast(field_index))) continue;

            const field_layout = layouts.getLayout(layouts.getStructFieldLayout(local_layout.getStruct().idx, @intCast(field_index)));
            const field_size = layouts.layoutSizeAlign(field_layout).size;
            has_large_field = has_large_field or field_size > issue806UnsafeAggregateBytes;
            has_small_field = has_small_field or field_size > 0 and field_size <= @sizeOf(u64);
        }
        if (has_large_field and has_small_field) return true;
    }
    return false;
}

fn isUnsafeLargeAggregateLocal(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    local: lir.LocalId,
) bool {
    return isUnsafeLargeAggregateLayoutIdx(layouts, store.getLocal(local).layout_idx);
}

fn spanHasUnsafeLargeAggregateLocal(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    span: lir.LocalSpan,
) bool {
    for (store.getLocalSpan(span)) |local| {
        if (isUnsafeLargeAggregateLocal(store, layouts, local)) return true;
    }
    return false;
}

fn isUnsafeLargeAggregateLayoutIdx(layouts: *const layout.Store, layout_idx: layout.Idx) bool {
    return isUnsafeLargeAggregateLayout(layouts, layouts.getLayout(layout_idx));
}

fn isUnsafeLargeAggregateLayout(layouts: *const layout.Store, local_layout: layout.Layout) bool {
    return switch (local_layout.tag) {
        .struct_, .tag_union => layoutIsLargerThanGuardPages(layouts, local_layout),
        else => false,
    };
}

fn layoutIsLargerThanGuardPages(layouts: *const layout.Store, local_layout: layout.Layout) bool {
    return layouts.layoutSizeAlign(local_layout).size > issue806UnsafeAggregateBytes;
}

fn patternHasUnsafeLargeAggregate(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    pattern_id: lir.LirPatternId,
) bool {
    if (pattern_id.isNone()) return false;

    return switch (store.getPattern(pattern_id)) {
        .bind => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.layout_idx),
        .wildcard => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.layout_idx),
        .int_literal, .float_literal, .str_literal => false,
        .tag => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.union_layout) or
            patternSpanHasUnsafeLargeAggregate(store, layouts, pattern.args),
        .struct_ => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.struct_layout) or
            patternSpanHasUnsafeLargeAggregate(store, layouts, pattern.fields),
        .list => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.elem_layout) or
            patternSpanHasUnsafeLargeAggregate(store, layouts, pattern.prefix) or
            patternHasUnsafeLargeAggregate(store, layouts, pattern.rest) or
            patternSpanHasUnsafeLargeAggregate(store, layouts, pattern.suffix),
        .as_pattern => |pattern| isUnsafeLargeAggregateLayoutIdx(layouts, pattern.layout_idx) or
            patternHasUnsafeLargeAggregate(store, layouts, pattern.inner),
    };
}

fn patternSpanHasUnsafeLargeAggregate(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    span: lir.LirPatternSpan,
) bool {
    for (store.getPatternSpan(span)) |pattern_id| {
        if (patternHasUnsafeLargeAggregate(store, layouts, pattern_id)) return true;
    }
    return false;
}
