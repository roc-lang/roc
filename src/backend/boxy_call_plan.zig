//! Operand plans for the Boxy runtime value operations.
//!
//! Every `assign_boxy_*` value statement (and `boxy_tag_match`) lowers to one
//! call of a `roc_boxy_*` C-ABI wrapper in `src/eval/boxy_abi.zig`. The plan
//! for a statement lists that wrapper's parameters once, in C parameter
//! order, as backend-neutral operands. Each backend marshals a plan with one
//! generic routine: it materializes each operand in its own representation
//! (a frame slot, a wasm value, an LLVM value) and passes it in the listed
//! position.

const base = @import("base");
const layout = @import("layout");
const lir = @import("lir");

const LIR = lir.LIR;
const LocalId = lir.LocalId;

/// Boxy runtime wrapper a plan calls.
pub const BoxyBuiltinFn = @import("dev/LirCodeGen.zig").BoxyBuiltinFn;

/// The most parameters any planned wrapper takes.
pub const max_operands = 9;

/// One C parameter of a Boxy runtime wrapper.
pub const Operand = union(enum) {
    /// Pointer to storage the wrapper writes the statement's result value into.
    out,
    /// Pointer to a pointer-sized slot the wrapper writes the result's
    /// descriptor into.
    out_desc,
    /// Pointer to a local's value bytes, or null when its layout is zero-sized.
    value: LocalId,
    /// `value` of the local when present, a null pointer otherwise.
    opt_value: ?LocalId,
    /// Resolved `*const BoxyTypeDesc`.
    desc: LIR.BoxyDescRef,
    /// `desc` when present, a null pointer otherwise.
    opt_desc: ?LIR.BoxyDescRef,
    /// The layout index of a local, as a `u32`.
    local_layout: LocalId,
    /// A `u32` immediate.
    imm32: u32,
    /// A `u8` immediate (a `BoxyTransferMode`).
    imm8: u8,
};

/// Where a plan's `out_desc` result is bound after the call.
pub const OutDescBinding = union(enum) {
    /// The plan has no `out_desc` operand.
    none,
    /// The target local's own `boxy_desc` local, when it has one.
    target_boxy_desc,
    /// This explicit descriptor local, when the statement names one.
    local: ?LocalId,
};

/// A wrapper call with its operands in C parameter order.
pub const Plan = struct {
    func: BoxyBuiltinFn,
    /// The local the statement assigns; unused by `boxy_tag_match`.
    target: LocalId = undefined,
    /// The result layout the statement states explicitly, apart from the
    /// target local's layout (`assign_boxy_unbox`).
    result_layout: ?layout.Idx = null,
    out_desc: OutDescBinding = .none,
    buf: [max_operands]Operand = undefined,
    len: u8 = 0,

    /// The operands in C parameter order.
    pub fn operands(self: *const Plan) []const Operand {
        return self.buf[0..self.len];
    }

    fn init(func: BoxyBuiltinFn, target: LocalId, out_desc: OutDescBinding, ops: []const Operand) Plan {
        var plan: Plan = .{ .func = func, .target = target, .out_desc = out_desc };
        @memcpy(plan.buf[0..ops.len], ops);
        plan.len = @intCast(ops.len);
        return plan;
    }
};

fn mode(m: LIR.BoxyTransferMode) Operand {
    return .{ .imm8 = @backingInt(m) };
}

fn layoutImm(idx: layout.Idx) Operand {
    return .{ .imm32 = @backingInt(idx) };
}

/// `roc_boxy_box(out, out_desc, payload, payload_layout, source_desc, payload_desc, payload_mode, target_layout)`.
fn box(target: LocalId, s: anytype) Plan {
    const payload_desc = s.payload_desc orelse base.invariant(
        "LIR/codegen invariant violated: assign_boxy_box reached a backend without a payload descriptor",
        .{},
    );
    return .init(.box, target, .target_boxy_desc, &.{
        .out,                        .out_desc,                      .{ .value = s.payload },
        layoutImm(s.payload_layout), .{ .opt_desc = s.source_desc }, .{ .desc = payload_desc },
        mode(s.payload_mode),        .{ .local_layout = target },
    });
}

/// `roc_boxy_record_update(out, out_desc, base, base_layout, base_desc, fields, fields_layout, fields_desc, target_layout)`.
fn recordUpdate(target: LocalId, s: anytype) Plan {
    return .init(.record_update, target, .target_boxy_desc, &.{
        .out,                        .out_desc,                  .{ .value = s.base },
        .{ .local_layout = s.base }, .{ .desc = s.base_desc },   .{ .value = s.fields },
        layoutImm(s.fields_layout),  .{ .desc = s.fields_desc }, .{ .local_layout = target },
    });
}

/// `roc_boxy_unbox(out, out_desc, source, source_layout, source_desc, target_desc, target_layout, source_mode)`.
fn unbox(target: LocalId, s: anytype) Plan {
    var plan: Plan = .init(.unbox, target, .target_boxy_desc, &.{
        .out,                          .out_desc,                  .{ .value = s.source },
        .{ .local_layout = s.source }, .{ .desc = s.source_desc }, .{ .opt_desc = s.target_desc },
        layoutImm(s.target_layout),    mode(s.source_mode),
    });
    plan.result_layout = s.target_layout;
    return plan;
}

/// `roc_boxy_adapt(out, out_desc, source, source_desc, target_desc, adapter, source_mode)`.
fn adapt(target: LocalId, s: anytype) Plan {
    return .init(.adapt, target, .target_boxy_desc, &.{
        .out,                           .out_desc,                      .{ .value = s.source },
        .{ .opt_desc = s.source_desc }, .{ .opt_desc = s.target_desc }, .{ .imm32 = @backingInt(s.adapter) },
        mode(s.source_mode),
    });
}

/// `roc_boxy_inspect(out, source, source_layout, source_desc)`.
fn inspect(target: LocalId, s: anytype) Plan {
    return .init(.inspect, target, .none, &.{
        .out, .{ .value = s.source }, .{ .local_layout = s.source }, .{ .desc = s.source_desc },
    });
}

/// `roc_boxy_eq(out, lhs, rhs, value_layout, desc)`. Both operands share
/// one layout, so the layout of `lhs` describes both.
fn eq(target: LocalId, s: anytype) Plan {
    return .init(.eq, target, .none, &.{
        .out, .{ .value = s.lhs }, .{ .value = s.rhs }, .{ .local_layout = s.lhs }, .{ .desc = s.desc },
    });
}

/// `roc_boxy_hash(out, value, value_layout, desc, hasher)`.
fn hash(target: LocalId, s: anytype) Plan {
    return .init(.hash, target, .none, &.{
        .out, .{ .value = s.value }, .{ .local_layout = s.value }, .{ .desc = s.desc }, .{ .value = s.hasher },
    });
}

/// `roc_boxy_tag(out, target_desc, tag_name, payload, payload_layout, payload_desc, payload_mode, target_layout)`.
fn tag(target: LocalId, s: anytype) Plan {
    return .init(.tag, target, .none, &.{
        .out,                        .{ .desc = s.target_desc },  .{ .imm32 = @backingInt(s.tag_name) },
        .{ .opt_value = s.payload }, layoutImm(s.payload_layout), .{ .opt_desc = s.payload_desc },
        mode(s.payload_mode),        .{ .local_layout = target },
    });
}

/// `roc_boxy_tag_payload(out, out_desc, source, source_layout, source_desc, tag_name, payload_index, target_layout, source_mode)`.
fn tagPayload(target: LocalId, s: anytype) Plan {
    return .init(.tag_payload, target, .{ .local = s.target_desc }, &.{
        .out,                          .out_desc,                   .{ .value = s.source },
        .{ .local_layout = s.source }, .{ .desc = s.source_desc },  .{ .imm32 = @backingInt(s.tag_name) },
        .{ .imm32 = s.payload_index }, .{ .local_layout = target }, mode(s.source_mode),
    });
}

/// `roc_boxy_tag_match(source, source_layout, source_desc, tag_name) -> bool`.
pub fn tagMatch(s: anytype) Plan {
    return .init(.tag_match, undefined, .none, &.{
        .{ .value = s.source }, .{ .local_layout = s.source }, .{ .desc = s.source_desc }, .{ .imm32 = @backingInt(s.tag_name) },
    });
}

const plans_by_statement = .{
    .assign_boxy_box = box,
    .assign_boxy_record_update = recordUpdate,
    .assign_boxy_unbox = unbox,
    .assign_boxy_adapt = adapt,
    .assign_boxy_inspect = inspect,
    .assign_boxy_eq = eq,
    .assign_boxy_hash = hash,
    .assign_boxy_tag = tag,
    .assign_boxy_tag_payload = tagPayload,
};

/// The plan for the Boxy value statement whose `LIR.CFStmt` tag is
/// `stmt_tag`, assigning `target`, with the statement's out-of-line fields `s`.
pub fn forStatement(comptime stmt_tag: @typeInfo(LIR.CFStmt).@"union".tag_type.?, target: LocalId, s: anytype) Plan {
    return @field(plans_by_statement, @tagName(stmt_tag))(target, s);
}
