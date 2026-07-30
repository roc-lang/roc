//! Target-agnostic lowering of a hosted-call signature into register/memory placements,
//! computed from the per-target C-ABI classifiers (`aarch64.zig`, `x86_64.zig`, `wasm.zig`).
//!
//! A `LoweredCall` says, for the return value and each argument, whether it travels in
//! registers (which bytes go in which register file, and which pieces form one LLVM
//! carrier) or "indirect" (memory). Each consumer interprets "indirect" for its own world:
//!   - the LLVM backend emits a pointer parameter, adding `byval` only for targets whose C
//!     ABI passes that memory-class argument as a stack copy rather than a pointer;
//!   - the dev backend and the interpreter's call trampoline place the bytes in the stack
//!     argument area or pass a pointer to a copy, per the target.
//!
//! This is the single place the classification is turned into placements, shared by every
//! consumer so they agree by construction.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");
const aarch64 = @import("aarch64.zig");
const x86_64 = @import("x86_64.zig");
const wasm = @import("wasm.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;

/// Which register file a piece of a value travels in.
pub const RegClass = enum { integer, float, vector };

/// One register's worth of a value: which register file, the byte offset within the value's
/// in-memory representation that this register carries, and how many bytes (1..16; >8 only for
/// a 128-bit value in a single SSE register).
pub const RegPiece = struct {
    class: RegClass,
    offset: u16,
    size: u8,
    /// Exact lane shape for a vector-register piece. Keeping this in the ABI
    /// lowering prevents consumers from reconstructing vector type information
    /// from byte size, which would lose the natural C signature.
    vector_kind: ?layout.Vector = null,
};

/// How LLVM must preserve the logical register pieces as one C-ABI carrier.
/// Physical-call consumers use `pieces`; LLVM consumers must additionally use
/// this carrier instead of reconstructing argument grouping from the pieces.
pub const RegisterCarrier = union(enum) {
    /// Arguments are separate LLVM parameters. A multi-piece scalar return is
    /// one LLVM struct because LLVM functions have only one return value.
    piecewise,
    /// All pieces are one LLVM struct, including a one-piece aggregate return.
    structure,
    /// All pieces are one naturally aligned LLVM integer (for example AAPCS64
    /// `i128`, and a SysV `i128` argument that does not fit in the remaining
    /// registers). This preserves atomic allocation and the base AAPCS64
    /// even-register rule.
    integer,
    /// All pieces are one homogeneous LLVM array. The optional byte alignment
    /// is the ELF AArch64 `alignstack` parameter attribute required by HFAs
    /// and transparent vector aggregates.
    array: ?u8,
};

/// Logical register pieces for one value plus their indivisible LLVM form.
pub const RegisterPlacement = struct {
    pieces: []const RegPiece,
    carrier: RegisterCarrier = .piecewise,
};

/// How a single value (an argument or the return) is passed.
pub const Placement = union(enum) {
    /// Zero-sized: not passed at all.
    none,
    /// Passed/returned in these registers, in order.
    registers: RegisterPlacement,
    /// Passed in memory: by `byval`/`sret` pointer or stack copy, per the consumer/target.
    indirect,
};

/// The full lowering of a hosted call's signature.
pub const LoweredCall = struct {
    /// Whether `*RocOps` is prepended as the leading integer-register argument.
    leading_ops: bool,
    ret: Placement,
    /// One placement per source argument, in order (not counting the leading `*RocOps`).
    args: []const Placement,
};

/// One register piece after the target ABI has assigned a concrete argument
/// register number. Register numbers are indices in the target's integer or
/// SIMD argument-register file; on Win64 both files use the shared argument
/// position as the index.
pub const AssignedRegPiece = struct {
    piece: RegPiece,
    register_index: u8,
};

/// A by-value argument in the outgoing stack argument area. `offset` is
/// relative to the start of that area (and excludes Win64 shadow space).
pub const StackValue = struct {
    offset: u16,
    size: u32,
    alignment: u8,
};

/// Where an ABI-mandated pointer to an indirectly passed argument travels.
pub const PointerLocation = union(enum) {
    register: u8,
    stack: u16,
};

/// Physical argument placement after register exhaustion, all-or-nothing
/// aggregate allocation, stack alignment, and Win64 shared argument positions
/// have been applied.
pub const PhysicalArg = union(enum) {
    none,
    registers: []const AssignedRegPiece,
    stack_value: StackValue,
    indirect: PointerLocation,
};

/// Fully assigned physical argument locations and outgoing stack size for a call.
pub const PhysicalCall = struct {
    args: []const PhysicalArg,
    /// Bytes occupied by the outgoing stack argument area, excluding Win64's
    /// mandatory 32-byte shadow space.
    stack_size: u16,
};

/// The host targets whose C ABI we lower for. AArch64 has distinct LLVM carrier
/// attributes on ELF, Mach-O, and Windows, and Apple removes the base AAPCS64
/// even-register rule. X86-64 splits System V vs Windows; wasm's pointer width
/// is reflected in the store's `targetUsize`.
pub const Target = enum {
    /// AAPCS64 on ELF targets.
    aarch64,
    aarch64_macho,
    aarch64_windows,
    x86_64_sysv,
    x86_64_windows,
    wasm32,
    wasm64,
};

/// Select the explicit AArch64 C-ABI variant for an operating system.
pub fn aarch64Target(os: std.Target.Os.Tag) Target {
    return switch (os) {
        .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos => .aarch64_macho,
        .windows => .aarch64_windows,
        else => .aarch64,
    };
}

fn isAarch64(target: Target) bool {
    return switch (target) {
        .aarch64, .aarch64_macho, .aarch64_windows => true,
        .x86_64_sysv, .x86_64_windows, .wasm32, .wasm64 => false,
    };
}

/// Lower a hosted call's signature for `target`. `arg_idxs`/`ret_idx` are layout indices;
/// `needs_ops` (from `needsRocOps`) decides the leading `*RocOps`. Allocations for the
/// returned slices come from `arena`.
pub fn lower(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    arg_idxs: []const Idx,
    ret_idx: Idx,
    needs_ops: bool,
) std.mem.Allocator.Error!LoweredCall {
    const args = try arena.alloc(Placement, arg_idxs.len);
    for (arg_idxs, args) |arg_idx, *placement| {
        placement.* = try placementFor(arena, store, target, arg_idx, .arg);
    }
    const ret = try placementFor(arena, store, target, ret_idx, .ret);

    // Clang's SysV IR ABI changes a register-class aggregate parameter to a
    // byval pointer when the complete aggregate no longer fits in the
    // remaining INTEGER/SSE register pools. Scalar IR parameters stay scalar
    // and LLVM performs their eventual stack placement itself. Apply the
    // aggregate rule here so generated LLVM signatures exactly match C.
    if (target == .x86_64_sysv) {
        var gp_used: u8 = if (ret == .indirect) 1 else 0;
        if (needs_ops) gp_used += 1;
        var sse_used: u8 = 0;
        for (args, arg_idxs) |*placement, arg_idx| {
            switch (placement.*) {
                .none, .indirect => {},
                .registers => |registers| {
                    const counts = pieceCounts(registers.pieces);
                    const fits = gp_used + counts.gp <= sysv_gp_registers and
                        sse_used + counts.sse <= simd_argument_registers;
                    if (fits) {
                        gp_used += counts.gp;
                        sse_used += counts.sse;
                    } else if (isCAbiAggregate(store, arg_idx)) {
                        placement.* = .indirect;
                    } else if (isCAbiI128Scalar(store, arg_idx)) {
                        // Clang uses two i64 parameters while both INTEGER
                        // eightbytes fit, but one atomic i128 parameter when
                        // the scalar must go to the stack.
                        placement.registers.carrier = .integer;
                    }
                },
            }
        }
    }

    return .{
        .leading_ops = needs_ops,
        .ret = ret,
        .args = args,
    };
}

const sysv_gp_registers: u8 = 6;
const aarch64_gp_registers: u8 = 8;
const simd_argument_registers: u8 = 8;
const win64_argument_registers: u8 = 4;

const PieceCounts = struct { gp: u8, sse: u8 };

fn pieceCounts(pieces: []const RegPiece) PieceCounts {
    var counts: PieceCounts = .{ .gp = 0, .sse = 0 };
    for (pieces) |piece| switch (piece.class) {
        .integer => counts.gp += 1,
        .float, .vector => counts.sse += 1,
    };
    return counts;
}

/// Whether the generated C type is an aggregate rather than a transparent
/// scalar/vector leaf. This distinction matters only to the SysV LLVM IR
/// signature when register exhaustion forces a byval parameter.
fn isCAbiAggregate(store: *const Store, idx: Idx) bool {
    const lay = store.getLayout(idx);
    return switch (lay.tag) {
        .struct_, .closure => true,
        // Glue exposes this layout as a pointer typedef.
        .erased_callable => false,
        // Glue exposes Dec as `struct RocDec { __int128 num; }`, not as a C
        // scalar. SysV therefore changes it to aligned `byval` when its two
        // INTEGER eightbytes do not both fit.
        .scalar => lay.getScalar().tag == .frac and lay.getScalar().getFrac() == .dec,
        .tag_union => blk: {
            const info = store.getTagUnionInfo(lay);
            if (info.variants.len == 1 and info.data.discriminant_size == 0) {
                break :blk isCAbiAggregate(store, info.variants.get(0).payload_layout);
            }
            break :blk true;
        },
        else => false,
    };
}

fn isCAbiI128Scalar(store: *const Store, idx: Idx) bool {
    const lay = store.getLayout(idx);
    return switch (lay.tag) {
        .scalar => lay.getScalar().tag == .int and store.layoutSize(lay) == 16,
        .tag_union => blk: {
            const info = store.getTagUnionInfo(lay);
            if (info.variants.len == 1 and info.data.discriminant_size == 0) {
                break :blk isCAbiI128Scalar(store, info.variants.get(0).payload_layout);
            }
            break :blk false;
        },
        else => false,
    };
}

fn cAbiI128VectorKind(store: *const Store, idx: Idx) ?layout.Vector {
    const lay = store.getLayout(idx);
    return switch (lay.tag) {
        .scalar => if (lay.getScalar().tag == .int and store.layoutSize(lay) == 16)
            if (idx.isSigned()) .i64x2 else .u64x2
        else
            null,
        .tag_union => blk: {
            const info = store.getTagUnionInfo(lay);
            if (info.variants.len == 1 and info.data.discriminant_size == 0) {
                break :blk cAbiI128VectorKind(store, info.variants.get(0).payload_layout);
            }
            break :blk null;
        },
        else => null,
    };
}

/// Assign the logical C-ABI placements from `lowered` to concrete native
/// argument registers and stack slots. This is used by consumers that marshal
/// physical calls themselves (the dev backend and interpreter trampoline).
/// LLVM and wasm consume `LoweredCall` directly because their own backends
/// perform the final physical assignment.
pub fn assignPhysicalArgs(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    lowered: LoweredCall,
    arg_idxs: []const Idx,
) std.mem.Allocator.Error!PhysicalCall {
    std.debug.assert(lowered.args.len == arg_idxs.len);
    std.debug.assert(target != .wasm32 and target != .wasm64);

    const args = try arena.alloc(PhysicalArg, lowered.args.len);
    var gp_used: u8 = 0;
    var sse_used: u8 = 0;
    var win_position: u8 = 0;
    var stack_size: u16 = 0;

    if (lowered.ret == .indirect) switch (target) {
        .x86_64_sysv, .x86_64_windows => {
            gp_used = 1;
            win_position = 1;
        },
        .aarch64, .aarch64_macho, .aarch64_windows => {}, // AAPCS64 uses the dedicated x8 indirect-result register.
        .wasm32, .wasm64 => unreachable,
    };
    if (lowered.leading_ops) switch (target) {
        .x86_64_windows => {
            gp_used += 1;
            win_position += 1;
        },
        .x86_64_sysv, .aarch64, .aarch64_macho, .aarch64_windows => gp_used += 1,
        .wasm32, .wasm64 => unreachable,
    };

    for (lowered.args, arg_idxs, args) |placement, arg_idx, *assigned| {
        const size_align = store.layoutSizeAlign(store.getLayout(arg_idx));
        const natural_alignment: u8 = @intCast(@min(@as(u32, 16), @max(@as(u32, 1), size_align.alignment.toByteUnits())));
        const value_alignment: u8 = if (target == .aarch64_macho) natural_alignment else @max(8, natural_alignment);
        const stack_slot_alignment: u8 = if (target == .aarch64_macho) 1 else 8;

        switch (placement) {
            .none => assigned.* = .none,
            .indirect => switch (target) {
                .x86_64_sysv => assigned.* = .{ .stack_value = assignStackValue(
                    &stack_size,
                    size_align.size,
                    value_alignment,
                    stack_slot_alignment,
                ) },
                .aarch64, .aarch64_macho, .aarch64_windows => {
                    if (gp_used < aarch64_gp_registers) {
                        assigned.* = .{ .indirect = .{ .register = gp_used } };
                        gp_used += 1;
                    } else {
                        assigned.* = .{ .indirect = .{ .stack = assignStackPointer(&stack_size) } };
                    }
                },
                .x86_64_windows => {
                    if (win_position < win64_argument_registers) {
                        assigned.* = .{ .indirect = .{ .register = win_position } };
                    } else {
                        assigned.* = .{ .indirect = .{ .stack = assignWinStackSlot(&stack_size, win_position) } };
                    }
                    win_position += 1;
                    gp_used = win_position;
                },
                .wasm32, .wasm64 => unreachable,
            },
            .registers => |registers| switch (target) {
                .x86_64_windows => {
                    const pieces = registers.pieces;
                    std.debug.assert(pieces.len == 1);
                    if (win_position < win64_argument_registers) {
                        const regs = try arena.alloc(AssignedRegPiece, 1);
                        regs[0] = .{ .piece = pieces[0], .register_index = win_position };
                        assigned.* = .{ .registers = regs };
                    } else {
                        assigned.* = .{ .stack_value = .{
                            .offset = assignWinStackSlot(&stack_size, win_position),
                            .size = size_align.size,
                            .alignment = 8,
                        } };
                    }
                    win_position += 1;
                    gp_used = win_position;
                },
                .x86_64_sysv, .aarch64, .aarch64_macho, .aarch64_windows => {
                    const pieces = registers.pieces;
                    const counts = pieceCounts(pieces);
                    const aarch64_target = isAarch64(target);
                    const gp_limit: u8 = if (aarch64_target) aarch64_gp_registers else sysv_gp_registers;
                    // AAPCS64 rule C.8 rounds NGRN to an even register before
                    // allocating a 16-byte-aligned, multi-register argument.
                    // Apple's arm64 ABI explicitly removes that rule.
                    const first_gp = if (aarch64_target and target != .aarch64_macho and counts.gp > 1 and value_alignment == 16)
                        std.mem.alignForward(u8, gp_used, 2)
                    else
                        gp_used;
                    if (first_gp + counts.gp <= gp_limit and sse_used + counts.sse <= simd_argument_registers) {
                        const regs = try arena.alloc(AssignedRegPiece, pieces.len);
                        var next_gp = first_gp;
                        var next_sse = sse_used;
                        for (pieces, regs) |piece, *reg| switch (piece.class) {
                            .integer => {
                                reg.* = .{ .piece = piece, .register_index = next_gp };
                                next_gp += 1;
                            },
                            .float, .vector => {
                                reg.* = .{ .piece = piece, .register_index = next_sse };
                                next_sse += 1;
                            },
                        };
                        gp_used = next_gp;
                        sse_used = next_sse;
                        assigned.* = .{ .registers = regs };
                    } else {
                        // SysV and AAPCS64 allocate every register piece of one
                        // argument atomically. If either register pool cannot
                        // hold the complete argument, no registers are consumed.
                        assigned.* = .{ .stack_value = assignStackValue(
                            &stack_size,
                            size_align.size,
                            value_alignment,
                            stack_slot_alignment,
                        ) };
                        if (aarch64_target and counts.sse != 0) {
                            // AAPCS64 rule C.5 closes the SIMD register pool once
                            // an HFA cannot be allocated in full.
                            sse_used = simd_argument_registers;
                        }
                        if (aarch64_target and counts.gp != 0) {
                            // AAPCS64 rule C.13 similarly closes the general
                            // register pool when a multiword value cannot fit.
                            gp_used = aarch64_gp_registers;
                        }
                    }
                },
                .wasm32, .wasm64 => unreachable,
            },
        }
    }

    return .{
        .args = args,
        .stack_size = @intCast(std.mem.alignForward(u16, stack_size, 8)),
    };
}

fn assignStackValue(stack_size: *u16, size: u32, alignment: u8, slot_alignment: u8) StackValue {
    stack_size.* = std.mem.alignForward(u16, stack_size.*, alignment);
    const offset = stack_size.*;
    stack_size.* += @intCast(std.mem.alignForward(u32, size, slot_alignment));
    return .{ .offset = offset, .size = size, .alignment = alignment };
}

fn assignStackPointer(stack_size: *u16) u16 {
    const offset = std.mem.alignForward(u16, stack_size.*, 8);
    stack_size.* = offset + 8;
    return offset;
}

fn assignWinStackSlot(stack_size: *u16, position: u8) u16 {
    std.debug.assert(position >= win64_argument_registers);
    const offset: u16 = @as(u16, position - win64_argument_registers) * 8;
    stack_size.* = @max(stack_size.*, offset + 8);
    return offset;
}

const Context = enum { arg, ret };

fn placementFor(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    idx: Idx,
    ctx: Context,
) std.mem.Allocator.Error!Placement {
    const lay = store.getLayout(idx);
    if (store.layoutSize(lay) == 0) return .none;

    return switch (target) {
        .aarch64, .aarch64_macho, .aarch64_windows => placementAarch64(arena, store, target, idx, ctx),
        .x86_64_sysv => placementSysV(arena, store, idx, ctx),
        .x86_64_windows => placementWin64(arena, store, idx, ctx),
        .wasm32, .wasm64 => placementWasm(arena, store, idx),
    };
}

fn onePiece(arena: std.mem.Allocator, class: RegClass, offset: u16, size: u8) std.mem.Allocator.Error!Placement {
    const pieces = try arena.alloc(RegPiece, 1);
    pieces[0] = .{ .class = class, .offset = offset, .size = size };
    return .{ .registers = .{ .pieces = pieces } };
}

/// Split a `size`-byte integer value into 8-byte general-purpose register pieces.
fn integerPieces(
    arena: std.mem.Allocator,
    size: u32,
    carrier: RegisterCarrier,
) std.mem.Allocator.Error!Placement {
    const count = (size + 7) / 8;
    const pieces = try arena.alloc(RegPiece, count);
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        pieces[i] = .{
            .class = .integer,
            .offset = @intCast(i * 8),
            .size = @intCast(@min(8, size - i * 8)),
        };
    }
    return .{ .registers = .{ .pieces = pieces, .carrier = carrier } };
}

fn placementAarch64(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    idx: Idx,
    ctx: Context,
) std.mem.Allocator.Error!Placement {
    const lay = store.getLayout(idx);
    const size = store.layoutSize(lay);
    switch (aarch64.classifyType(store, idx)) {
        .memory => return .indirect,
        .integer => return onePiece(arena, .integer, 0, @intCast(size)),
        .double_integer => return integerPieces(arena, size, .{ .array = null }),
        .float_array => |fa| {
            const elem_bytes: u8 = @intCast(fa.elem_bits / 8);
            const pieces = try arena.alloc(RegPiece, fa.count);
            var i: u8 = 0;
            while (i < fa.count) : (i += 1) {
                pieces[i] = .{ .class = .float, .offset = @as(u16, i) * elem_bytes, .size = elem_bytes };
            }
            return .{ .registers = .{
                .pieces = pieces,
                .carrier = if (ctx == .arg)
                    .{ .array = if (target == .aarch64) @max(elem_bytes, 8) else null }
                else
                    .structure,
            } };
        },
        .vector => |kind| {
            // A transparent single-vector aggregate travels exactly like the
            // vector itself, but keeps clang's aggregate carrier: a one-element
            // array argument (with the ELF alignstack attribute) and a
            // struct-valued return.
            const pieces = try arena.alloc(RegPiece, 1);
            pieces[0] = .{ .class = .vector, .offset = 0, .size = 16, .vector_kind = kind };
            return .{ .registers = .{
                .pieces = pieces,
                .carrier = if (ctx == .arg)
                    .{ .array = if (target == .aarch64) 16 else null }
                else
                    .structure,
            } };
        },
        .byval => switch (lay.tag) {
            .scalar => {
                const scalar = lay.getScalar();
                if (scalar.tag == .frac) {
                    return switch (scalar.getFrac()) {
                        .f32 => onePiece(arena, .float, 0, 4),
                        .f64 => onePiece(arena, .float, 0, 8),
                        .dec => integerPieces(arena, size, .integer),
                    };
                }
                if (scalar.tag == .vector) {
                    const pieces = try arena.alloc(RegPiece, 1);
                    pieces[0] = .{ .class = .vector, .offset = 0, .size = 16, .vector_kind = scalar.getVector() };
                    return .{ .registers = .{ .pieces = pieces } };
                }
                return integerPieces(arena, size, if (size == 16) .integer else .piecewise);
            },
            .box, .box_of_zst, .ptr => return integerPieces(arena, size, .piecewise),
            .tag_union => {
                const info = store.getTagUnionInfo(lay);
                std.debug.assert(info.variants.len == 1 and info.data.discriminant_size == 0);
                return placementAarch64(arena, store, target, info.variants.get(0).payload_layout, ctx);
            },
            else => unreachable,
        },
    }
}

fn placementSysV(arena: std.mem.Allocator, store: *const Store, idx: Idx, ctx: Context) std.mem.Allocator.Error!Placement {
    const size = store.layoutSize(store.getLayout(idx));
    const classes = x86_64.classifySystemV(store, idx, if (ctx == .ret) .ret else .arg);
    if (classes[0] == .memory) return .indirect;

    var pieces = std.ArrayList(RegPiece).empty;
    var i: usize = 0;
    while (i < classes.len) : (i += 1) {
        if (classes[i] == .none) break;
        const offset: u16 = @intCast(i * 8);
        const piece_size: u8 = @intCast(@min(@as(u32, 8), size - offset));
        switch (classes[i]) {
            .integer => try pieces.append(arena, .{ .class = .integer, .offset = offset, .size = piece_size }),
            .sse, .float, .float_combine => {
                if (i + 1 < classes.len and classes[i + 1] == .sseup) {
                    try pieces.append(arena, .{
                        .class = .vector,
                        .offset = offset,
                        .size = @intCast(@min(@as(u32, 16), size - offset)),
                        .vector_kind = findVectorKind(store, idx) orelse unreachable,
                    });
                    i += 1;
                } else {
                    try pieces.append(arena, .{ .class = .float, .offset = offset, .size = piece_size });
                }
            },
            // x87/win_i128 do not occur for Roc types under System V; a bare
            // SSEUP is invalid after the classifier's cleanup.
            else => return .indirect,
        }
    }
    return .{ .registers = .{
        .pieces = try pieces.toOwnedSlice(arena),
    } };
}

fn placementWin64(arena: std.mem.Allocator, store: *const Store, idx: Idx, ctx: Context) std.mem.Allocator.Error!Placement {
    const size = store.layoutSize(store.getLayout(idx));
    switch (x86_64.classifyWindows(store, idx)) {
        .memory => return .indirect,
        .integer => return onePiece(arena, .integer, 0, @intCast(size)),
        .sse => return onePiece(arena, .float, 0, @intCast(@min(@as(u32, 16), size))),
        // Win64 passes a scalar 128-bit integer in memory but returns it in
        // XMM0 as <2 x i64>. Generated RocDec is a C aggregate instead, so it
        // is indirect in both directions.
        .win_i128 => if (ctx == .ret and !isCAbiAggregate(store, idx)) {
            if (findVectorKind(store, idx)) |kind| {
                const pieces = try arena.alloc(RegPiece, 1);
                pieces[0] = .{ .class = .vector, .offset = 0, .size = 16, .vector_kind = kind };
                return .{ .registers = .{ .pieces = pieces } };
            }
            const pieces = try arena.alloc(RegPiece, 1);
            pieces[0] = .{
                .class = .vector,
                .offset = 0,
                .size = 16,
                .vector_kind = cAbiI128VectorKind(store, idx) orelse unreachable,
            };
            return .{ .registers = .{ .pieces = pieces } };
        } else return .indirect,
        else => return .indirect,
    }
}

fn placementWasm(arena: std.mem.Allocator, store: *const Store, idx: Idx) std.mem.Allocator.Error!Placement {
    switch (wasm.classifyType(store, idx)) {
        .indirect => return .indirect,
        .direct => |direct_idx| {
            const size = store.layoutSize(store.getLayout(direct_idx));
            const dlay = store.getLayout(direct_idx);
            const is_float = dlay.tag == .scalar and dlay.getScalar().tag == .frac and dlay.getScalar().getFrac() != .dec;
            const is_vector = dlay.tag == .scalar and dlay.getScalar().tag == .vector;
            const class: RegClass = if (is_vector) .vector else if (is_float) .float else .integer;
            if (wasm.lowerAsDoubleI64(store, direct_idx)) {
                // A value wider than 64 bits is passed as two i64s.
                return integerPieces(arena, size, .piecewise);
            }
            if (is_vector) {
                const pieces = try arena.alloc(RegPiece, 1);
                pieces[0] = .{ .class = .vector, .offset = 0, .size = @intCast(size), .vector_kind = dlay.getScalar().getVector() };
                return .{ .registers = .{ .pieces = pieces } };
            }
            return onePiece(arena, class, 0, @intCast(size));
        },
    }
}

fn findVectorKind(store: *const Store, idx: Idx) ?layout.Vector {
    const lay = store.getLayout(idx);
    return switch (lay.tag) {
        .scalar => if (lay.getScalar().tag == .vector) lay.getScalar().getVector() else null,
        .struct_ => blk: {
            const struct_idx = lay.getStruct().idx;
            const count = store.getStructData(struct_idx).fields.count;
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                if (store.getStructFieldIsPadding(struct_idx, i)) continue;
                if (findVectorKind(store, store.getStructFieldLayout(struct_idx, i))) |kind| break :blk kind;
            }
            break :blk null;
        },
        .closure => findVectorKind(store, lay.getClosure().captures_layout_idx),
        .tag_union => blk: {
            const info = store.getTagUnionInfo(lay);
            if (info.variants.len == 1 and info.data.discriminant_size == 0) {
                break :blk findVectorKind(store, info.variants.get(0).payload_layout);
            }
            break :blk null;
        },
        else => null,
    };
}

const testing = std.testing;

fn testStruct(store: *Store, field_idxs: []const Idx) std.mem.Allocator.Error!Idx {
    var fields: [16]layout.StructField = undefined;
    for (field_idxs, 0..) |field_idx, i| {
        fields[i] = .{ .index = @intCast(i), .layout = field_idx };
    }
    return store.putStructFields(fields[0..field_idxs.len]);
}

fn expectRegisters(expected: []const RegPiece, actual: Placement) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    try testing.expect(actual == .registers);
    try testing.expectEqualSlices(RegPiece, expected, actual.registers.pieces);
}

test "AArch64 ABI target preserves operating-system variants" {
    try testing.expectEqual(Target.aarch64, aarch64Target(.linux));
    try testing.expectEqual(Target.aarch64_macho, aarch64Target(.macos));
    try testing.expectEqual(Target.aarch64_macho, aarch64Target(.ios));
    try testing.expectEqual(Target.aarch64_windows, aarch64Target(.windows));
}

test "lower aarch64: random_plant(i32) -> Plant is all registers, no ops" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const call = try lower(arena, &store, .aarch64, &.{.i32}, plant, false);

    try testing.expect(!call.leading_ops);
    // i32 arg -> one integer register, 4 bytes.
    try testing.expectEqual(@as(usize, 1), call.args.len);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[0]);
    // Plant return -> one integer register, 8 bytes.
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
}

test "lower aarch64: HFA and large aggregates" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // { f64, f64 } -> two float registers.
    const two_f64 = try testStruct(&store, &.{ .f64, .f64 });
    const c1 = try lower(arena, &store, .aarch64, &.{}, two_f64, false);
    try expectRegisters(&.{
        .{ .class = .float, .offset = 0, .size = 8 },
        .{ .class = .float, .offset = 8, .size = 8 },
    }, c1.ret);

    // RocStr arg -> indirect (24 bytes); needs ops since Str is heap.
    const c2 = try lower(arena, &store, .aarch64, &.{.str}, .i32, true);
    try testing.expect(c2.leading_ops);
    try testing.expectEqual(Placement.indirect, c2.args[0]);
}

test "lower aarch64: pointer-shaped byval layouts use integer registers" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // Idx 32 has the bit pattern that makes a box/ptr layout's raw data
    // decode as ScalarTag.frac with invalid precision if treated as scalar.
    const target_elem_idx_int: u32 = 32;
    var elem_idx_opt: ?Idx = null;
    var i: u32 = 0;
    while (store.layouts.len() <= target_elem_idx_int) : (i += 1) {
        const idx = try store.insertLayout(layout.Layout.list(@enumFromInt(i)));
        if (@intFromEnum(idx) == target_elem_idx_int) elem_idx_opt = idx;
    }
    const elem_idx = elem_idx_opt.?;

    const box_idx = try store.insertLayout(layout.Layout.box(elem_idx));
    const ptr_idx = try store.insertLayout(layout.Layout.ptr(elem_idx));
    const call = try lower(arena, &store, .aarch64, &.{ box_idx, ptr_idx }, box_idx, false);

    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.args[0]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.args[1]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
}

test "lower x86_64 sysv: Plant in one int eightbyte, mixed struct splits" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const c1 = try lower(arena, &store, .x86_64_sysv, &.{.i32}, plant, false);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, c1.ret);

    const mixed = try testStruct(&store, &.{ .i64, .f64 });
    const c2 = try lower(arena, &store, .x86_64_sysv, &.{}, mixed, false);
    try expectRegisters(&.{
        .{ .class = .integer, .offset = 0, .size = 8 },
        .{ .class = .float, .offset = 8, .size = 8 },
    }, c2.ret);
}

test "lower native vectors preserve a single vector register piece" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const sysv = try lower(arena, &store, .x86_64_sysv, &.{.u8x16}, .i32x4, false);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .u8x16 }}, sysv.args[0]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i32x4 }}, sysv.ret);

    const aarch = try lower(arena, &store, .aarch64, &.{.u16x8}, .u64x2, false);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .u16x8 }}, aarch.args[0]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .u64x2 }}, aarch.ret);

    const wasm_call = try lower(arena, &store, .wasm32, &.{.i8x16}, .i64x2, false);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i8x16 }}, wasm_call.args[0]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i64x2 }}, wasm_call.ret);

    const win = try lower(arena, &store, .x86_64_windows, &.{.u32x4}, .u32x4, false);
    try testing.expectEqual(Placement.indirect, win.args[0]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .u32x4 }}, win.ret);
}

test "lower aarch64 vector aggregates" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // A transparent single-vector aggregate is one Q-register value.
    const wrapper = try testStruct(&store, &.{.u8x16});
    const single = try lower(arena, &store, .aarch64, &.{wrapper}, wrapper, false);
    const expected = [_]RegPiece{
        .{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .u8x16 },
    };
    try expectRegisters(&expected, single.args[0]);
    try expectRegisters(&expected, single.ret);

    // Aggregates with two or more vector members are memory-class in both
    // directions at the host boundary.
    const multi = try testStruct(&store, &.{ .u8x16, .u8x16, .u8x16 });
    const call = try lower(arena, &store, .aarch64, &.{multi}, multi, false);
    try testing.expectEqual(Placement.indirect, call.args[0]);
    try testing.expectEqual(Placement.indirect, call.ret);
}

test "lower transparent tag payloads with their C ABI leaf class" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const vector_tag = try store.putTagUnion(&.{.i16x8});
    const float_tag = try store.putTagUnion(&.{.f64});

    const aarch = try lower(arena, &store, .aarch64, &.{ vector_tag, float_tag }, vector_tag, false);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i16x8 }}, aarch.args[0]);
    try expectRegisters(&.{.{ .class = .float, .offset = 0, .size = 8 }}, aarch.args[1]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i16x8 }}, aarch.ret);

    const win = try lower(arena, &store, .x86_64_windows, &.{ vector_tag, float_tag }, vector_tag, false);
    try testing.expectEqual(Placement.indirect, win.args[0]);
    try expectRegisters(&.{.{ .class = .float, .offset = 0, .size = 8 }}, win.args[1]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i16x8 }}, win.ret);

    const sysv = try lower(arena, &store, .x86_64_sysv, &.{vector_tag}, vector_tag, false);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i16x8 }}, sysv.args[0]);
    try expectRegisters(&.{.{ .class = .vector, .offset = 0, .size = 16, .vector_kind = .i16x8 }}, sysv.ret);
}

test "lower win64: 16-byte struct is indirect, Plant is one register" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const c1 = try lower(arena, &store, .x86_64_windows, &.{}, plant, false);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, c1.ret);

    const two_words = try testStruct(&store, &.{ .i64, .i64 });
    const c2 = try lower(arena, &store, .x86_64_windows, &.{two_words}, .i32, false);
    try testing.expectEqual(Placement.indirect, c2.args[0]);

    const scalar_i128 = try lower(arena, &store, .x86_64_windows, &.{.i128}, .i128, false);
    try testing.expectEqual(Placement.indirect, scalar_i128.args[0]);
    try expectRegisters(&.{.{
        .class = .vector,
        .offset = 0,
        .size = 16,
        .vector_kind = .i64x2,
    }}, scalar_i128.ret);

    const dec = try lower(arena, &store, .x86_64_windows, &.{.dec}, .dec, false);
    try testing.expectEqual(Placement.indirect, dec.args[0]);
    try testing.expectEqual(Placement.indirect, dec.ret);
}

test "lower wasm32: single-variant tag union returns payload register" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const box_idx = try store.insertLayout(layout.Layout.box(.u64));
    const wrapped_u64 = try store.putTagUnion(&.{.u64});
    const call = try lower(arena, &store, .wasm32, &.{ box_idx, box_idx }, wrapped_u64, false);

    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[0]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[1]);
}

test "physical sysv allocation spills whole aggregates and aligns vectors" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const pair = try testStruct(&store, &.{ .i64, .i64 });
    const arg_idxs = [_]Idx{
        .i64, .i64, .i64, .i64,   .i64, pair,
        .f64, .f64, .f64, .f64,   .f64, .f64,
        .f64, .f64, .i32, .u8x16,
    };
    const lowered = try lower(arena, &store, .x86_64_sysv, &arg_idxs, .i32, false);

    // Only one GP register remains at `pair`, so the complete two-eightbyte
    // aggregate becomes byval in the LLVM signature.
    try testing.expectEqual(Placement.indirect, lowered.args[5]);

    const physical = try assignPhysicalArgs(arena, &store, .x86_64_sysv, lowered, &arg_idxs);
    try testing.expect(physical.args[5] == .stack_value);
    try testing.expectEqual(@as(u16, 0), physical.args[5].stack_value.offset);
    // i32 uses the one GP register left after the aggregate rollback. All
    // eight SSE registers are occupied, so the vector is a 16-aligned stack
    // value following the 16-byte pair.
    try testing.expect(physical.args[14] == .registers);
    try testing.expectEqual(@as(u8, 5), physical.args[14].registers[0].register_index);
    try testing.expect(physical.args[15] == .stack_value);
    try testing.expectEqual(@as(u16, 16), physical.args[15].stack_value.offset);
    try testing.expectEqual(@as(u8, 16), physical.args[15].stack_value.alignment);
}

test "physical win64 allocation uses shared argument positions" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const args = [_]Idx{ .i64, .f64, .u8x16, .i32 };
    // RocStr return is indirect and therefore occupies Win64 position zero.
    const lowered = try lower(arena, &store, .x86_64_windows, &args, .str, false);
    const physical = try assignPhysicalArgs(arena, &store, .x86_64_windows, lowered, &args);

    try testing.expectEqual(@as(u8, 1), physical.args[0].registers[0].register_index);
    try testing.expectEqual(@as(u8, 2), physical.args[1].registers[0].register_index);
    try testing.expectEqual(PointerLocation{ .register = 3 }, physical.args[2].indirect);
    try testing.expect(physical.args[3] == .stack_value);
    try testing.expectEqual(@as(u16, 0), physical.args[3].stack_value.offset);
    try testing.expectEqual(@as(u16, 8), physical.stack_size);
}

test "physical aarch64 allocation spills complete HFA and integer aggregates" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const hfa2 = try testStruct(&store, &.{ .f64, .f64 });
    const pair = try testStruct(&store, &.{ .i64, .u64 });
    const args = [_]Idx{
        .f64, .f64,   .f64, .f64, .f64, .f64, .f64,
        hfa2, .i32x4, .i64, .i64, .i64, .i64, .i64,
        .i64, .i64,   pair, .u64,
    };
    const lowered = try lower(arena, &store, .aarch64, &args, .i32, false);
    const physical = try assignPhysicalArgs(arena, &store, .aarch64, lowered, &args);

    try testing.expect(physical.args[7] == .stack_value);
    try testing.expectEqual(@as(u16, 0), physical.args[7].stack_value.offset);
    try testing.expectEqual(@as(u8, 8), physical.args[7].stack_value.alignment);
    // Rule C.5 closes the SIMD pool, so the following direct vector also stacks.
    try testing.expect(physical.args[8] == .stack_value);
    try testing.expectEqual(@as(u16, 16), physical.args[8].stack_value.offset);
    try testing.expectEqual(@as(u8, 16), physical.args[8].stack_value.alignment);

    try testing.expect(physical.args[16] == .stack_value);
    try testing.expectEqual(@as(u16, 32), physical.args[16].stack_value.offset);
    // Rule C.13 closes the GP pool after the pair fails to fit in x7 alone.
    try testing.expect(physical.args[17] == .stack_value);
    try testing.expectEqual(@as(u16, 48), physical.args[17].stack_value.offset);
}

test "physical aarch64 multi-vector aggregate passes a pointer in a GP register" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const hva2 = try testStruct(&store, &.{ .u8x16, .i16x8 });
    const args = [_]Idx{ hva2, .u8x16 };
    const lowered = try lower(arena, &store, .aarch64, &args, .i32, false);
    try testing.expectEqual(Placement.indirect, lowered.args[0]);

    const physical = try assignPhysicalArgs(arena, &store, .aarch64, lowered, &args);
    try testing.expectEqual(PointerLocation{ .register = 0 }, physical.args[0].indirect);
    // The pointer occupies a GP register, so the direct vector still takes q0.
    try testing.expectEqual(@as(u8, 0), physical.args[1].registers[0].register_index);
}

test "LLVM carriers preserve native multi-register argument identity" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const pair = try testStruct(&store, &.{ .i64, .i64 });
    const wrapper = try testStruct(&store, &.{.u8x16});
    const hva = try testStruct(&store, &.{ .u8x16, .i16x8 });
    const f32_hfa = try testStruct(&store, &.{ .f32, .f32 });

    const aarch = try lower(arena, &store, .aarch64, &.{ .i128, pair, wrapper, f32_hfa, hva }, wrapper, false);
    try testing.expectEqual(RegisterCarrier.integer, aarch.args[0].registers.carrier);
    try testing.expectEqual(RegisterCarrier{ .array = null }, aarch.args[1].registers.carrier);
    try testing.expectEqual(RegisterCarrier{ .array = 16 }, aarch.args[2].registers.carrier);
    try testing.expectEqual(layout.Vector.u8x16, aarch.args[2].registers.pieces[0].vector_kind.?);
    try testing.expectEqual(RegisterCarrier{ .array = 8 }, aarch.args[3].registers.carrier);
    // A multi-vector aggregate is memory-class in both directions.
    try testing.expectEqual(Placement.indirect, aarch.args[4]);
    // AAPCS64 returns a transparent vector aggregate as one struct-valued LLVM result.
    try testing.expectEqual(RegisterCarrier.structure, aarch.ret.registers.carrier);
    try testing.expectEqual(layout.Vector.u8x16, aarch.ret.registers.pieces[0].vector_kind.?);

    const macho = try lower(arena, &store, .aarch64_macho, &.{ f32_hfa, wrapper }, wrapper, false);
    try testing.expectEqual(RegisterCarrier{ .array = null }, macho.args[0].registers.carrier);
    try testing.expectEqual(RegisterCarrier{ .array = null }, macho.args[1].registers.carrier);

    const windows = try lower(arena, &store, .aarch64_windows, &.{ f32_hfa, wrapper }, wrapper, false);
    try testing.expectEqual(RegisterCarrier{ .array = null }, windows.args[0].registers.carrier);
    try testing.expectEqual(RegisterCarrier{ .array = null }, windows.args[1].registers.carrier);

    const sysv_i128_args = [_]Idx{ .i64, .i64, .i64, .i64, .i64, .i128 };
    const sysv_i128 = try lower(arena, &store, .x86_64_sysv, &sysv_i128_args, .i32, false);
    try testing.expectEqual(RegisterCarrier.integer, sysv_i128.args[5].registers.carrier);

    const sysv_register_i128 = try lower(arena, &store, .x86_64_sysv, &.{.i128}, .i32, false);
    try testing.expectEqual(RegisterCarrier.piecewise, sysv_register_i128.args[0].registers.carrier);

    const wrapped_i128 = try store.putTagUnion(&.{.i128});
    const sysv_wrapped_args = [_]Idx{ .i64, .i64, .i64, .i64, .i64, wrapped_i128 };
    const sysv_wrapped = try lower(arena, &store, .x86_64_sysv, &sysv_wrapped_args, .i32, false);
    try testing.expectEqual(RegisterCarrier.integer, sysv_wrapped.args[5].registers.carrier);

    const sysv_dec_args = [_]Idx{ .i64, .i64, .i64, .i64, .i64, .dec };
    const sysv_dec = try lower(arena, &store, .x86_64_sysv, &sysv_dec_args, .i32, false);
    try testing.expectEqual(Placement.indirect, sysv_dec.args[5]);
}

test "physical aarch64 aligns 16-byte multiword arguments to even GP registers" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const args = [_]Idx{ .i64, .i128 };
    const lowered = try lower(arena, &store, .aarch64, &args, .i32, false);
    const physical = try assignPhysicalArgs(arena, &store, .aarch64, lowered, &args);

    try testing.expectEqual(@as(u8, 0), physical.args[0].registers[0].register_index);
    try testing.expectEqual(@as(u8, 2), physical.args[1].registers[0].register_index);
    try testing.expectEqual(@as(u8, 3), physical.args[1].registers[1].register_index);

    const macho_lowered = try lower(arena, &store, .aarch64_macho, &args, .i32, false);
    const macho_physical = try assignPhysicalArgs(arena, &store, .aarch64_macho, macho_lowered, &args);
    try testing.expectEqual(@as(u8, 1), macho_physical.args[1].registers[0].register_index);
    try testing.expectEqual(@as(u8, 2), macho_physical.args[1].registers[1].register_index);

    const windows_lowered = try lower(arena, &store, .aarch64_windows, &args, .i32, false);
    const windows_physical = try assignPhysicalArgs(arena, &store, .aarch64_windows, windows_lowered, &args);
    try testing.expectEqual(@as(u8, 2), windows_physical.args[1].registers[0].register_index);
    try testing.expectEqual(@as(u8, 3), windows_physical.args[1].registers[1].register_index);
}

test "physical Apple arm64 stack arguments use compact natural alignment" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const args = [_]Idx{ .i64, .i64, .i64, .i64, .i64, .i64, .i64, .i64, .u8, .u16, .u32 };
    const macho_lowered = try lower(arena, &store, .aarch64_macho, &args, .i32, false);
    const macho = try assignPhysicalArgs(arena, &store, .aarch64_macho, macho_lowered, &args);
    try testing.expectEqual(@as(u16, 0), macho.args[8].stack_value.offset);
    try testing.expectEqual(@as(u16, 2), macho.args[9].stack_value.offset);
    try testing.expectEqual(@as(u16, 4), macho.args[10].stack_value.offset);
    try testing.expectEqual(@as(u16, 8), macho.stack_size);

    const elf_lowered = try lower(arena, &store, .aarch64, &args, .i32, false);
    const elf = try assignPhysicalArgs(arena, &store, .aarch64, elf_lowered, &args);
    try testing.expectEqual(@as(u16, 0), elf.args[8].stack_value.offset);
    try testing.expectEqual(@as(u16, 8), elf.args[9].stack_value.offset);
    try testing.expectEqual(@as(u16, 16), elf.args[10].stack_value.offset);
    try testing.expectEqual(@as(u16, 24), elf.stack_size);
}
