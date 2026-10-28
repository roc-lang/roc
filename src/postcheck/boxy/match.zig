//! `.boxy` match lowering: the shared decision-tree compiler
//! (`match_tree.zig`) over checked patterns, committed layouts, and the
//! descriptors in scope (design.md "Pattern Lowering").
//!
//! The compiler emits its decision tree back to front, each statement given
//! its successor; branch bodies and guards are lowered by the procedure
//! builder when the emitter asks for them, front to back, into holes whose
//! statements the emitter then links.

const std = @import("std");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const body_mod = @import("body.zig");
const layouts_mod = @import("layouts.zig");
const match_tree = @import("../match_tree.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const LIR = lir_core.LIR;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const TypeId = types.TypeId;
const Builder = body_mod.Builder;
const invariant = types.invariant;
const invariantFmt = types.invariantFmt;

const CheckedExprId = checked.CheckedExprId;
const CheckedPatternId = checked.CheckedPatternId;

/// A match being lowered: its emitter, and what its branch requests need.
pub const State = struct {
    arena: std.heap.ArenaAllocator,
    emitter: *Compiler.Emitter = undefined,
    /// The join the branch bodies jump to with their values; a match in
    /// tail position returns from each branch instead.
    done: LIR.JoinPointId,
    tail: bool,
    /// The merge of the binders the match reassigns, restored before each
    /// branch body and guard.
    merge: ?body_mod.MergeId,
    /// The hole the decision tree goes into.
    remainder: CFStmtId,
    /// The type of each occurrence local.
    occ_types: std.AutoHashMapUnmanaged(LocalId, TypeId) = .empty,
    /// The local each pattern binder is bound to.
    binder_locals: std.AutoHashMapUnmanaged(checked.PatternBinderId, LocalId) = .empty,
    /// How many of the match's branch patterns each body belongs to.
    alternatives: std.AutoHashMapUnmanaged(CheckedExprId, u32) = .empty,
    /// The join holding the one lowering of a body several patterns share.
    shared_bodies: std.AutoHashMapUnmanaged(CheckedExprId, LIR.JoinPointId) = .empty,
    /// The shared bodies' joins, in the order they were lowered, with their
    /// bodies' holes.
    shared_joins: std.ArrayList(struct { id: LIR.JoinPointId, body: CFStmtId }) = .empty,
    /// The names of open-row record fields a record pattern reads by name,
    /// indexed past every type's static fields (`named_field_base`).
    named_fields: std.ArrayList(types.NameId) = .empty,
    /// String pattern shapes, by their encoding.
    shapes: std.StringHashMapUnmanaged(u32) = .empty,
};

/// Field indices at and past this one name an open row's field by name.
const named_field_base: u32 = 1 << 31;

pub const Compiler = match_tree.Compiler(Ctx);

/// The accessor context the decision-tree compiler reads patterns and emits
/// LIR through.
pub const Ctx = struct {
    b: *Builder,
    state: *State,

    pub const PatId = CheckedPatternId;
    pub const TypeId = types.TypeId;
    pub const ExprId = CheckedExprId;
    pub const LocalId = checked.PatternBinderId;
    pub const BindingSpan = NoBindings;
    pub const LirLocal = LIR.LocalId;
    pub const CFStmtId = LIR.CFStmtId;
    pub const JoinPointId = LIR.JoinPointId;
    pub const StrArm = LIR.StrMatchArm;
    pub const LowerError = Allocator.Error;

    /// Checked branches bind nothing beyond their patterns.
    pub const NoBindings = struct {
        pub fn empty() NoBindings {
            return .{};
        }
    };

    pub const SubPat = struct { index: u32, ty: types.TypeId, pat: CheckedPatternId };

    pub const ListPatView = struct {
        fixed_count: u32,
        rest: ?struct { index: u32, pattern: ?CheckedPatternId },
    };

    fn arena(self: Ctx) Allocator {
        return self.state.arena.allocator();
    }

    fn pattern(self: Ctx, pat: CheckedPatternId) checked.CheckedPattern {
        return self.b.bodies().pattern(pat);
    }

    fn store(self: Ctx) *types.Store {
        return &self.b.program.store;
    }

    fn occType(self: Ctx, local: LIR.LocalId) types.TypeId {
        return self.state.occ_types.get(local) orelse invariant("a match occurrence local has no recorded type");
    }

    fn noteOcc(self: Ctx, local: LIR.LocalId, ty: types.TypeId) Allocator.Error!void {
        try self.state.occ_types.put(self.arena(), local, ty);
    }

    /// Record the scrutinee's type for the reads of its components.
    pub fn noteScrutinee(self: Ctx, scrutinee: LIR.LocalId, ty: types.TypeId) Allocator.Error!void {
        try self.noteOcc(scrutinee, ty);
    }

    // -- Construction --

    pub fn patKind(self: Ctx, pat: CheckedPatternId) match_tree.PatKind {
        return switch (self.pattern(pat).data) {
            .assign => .bind,
            .underscore => .wildcard,
            .as => .as_pattern,
            .record_destructure => .record,
            .tuple => .tuple,
            .list => .list,
            .nominal => .nominal,
            .applied_tag => .tag,
            // A builtin numeral's kind of number is its occurrence's: an
            // integer one switches, any other compares (`intSwitchValue`).
            .numeral_literal => |numeral| if (numeral.guard != null) .guarded_lit else .int_lit,
            .str_literal => |literal| if (literal.guard != null) .guarded_lit else .str_lit,
            .str_interpolation => .str_pattern,
            .runtime_error, .pending => invariant("an erroneous pattern reached Boxy match lowering"),
        };
    }

    /// The primitive a builtin numeral pattern matches at the occurrence
    /// type `ty`.
    fn literalPrimitive(self: Ctx, ty: types.TypeId) types.Primitive {
        return switch (self.store().get(self.b.structuralType(ty) catch invariant("out of memory converting a numeral pattern's type"))) {
            .primitive => |primitive| primitive,
            else => invariant("a builtin numeral pattern's occurrence is not a primitive"),
        };
    }

    /// The local `binder` is bound to, made on first use.
    fn binderLocal(self: Ctx, binder: checked.PatternBinderId, pat: CheckedPatternId) Allocator.Error!void {
        if (self.state.binder_locals.contains(binder)) return;
        const ty = try self.b.patternType(pat);
        const local = try self.b.localOf(ty, .frame);
        try self.state.binder_locals.put(self.arena(), binder, local);
        try self.b.binders.put(self.b.allocator, binder, local);
    }

    pub fn bindLocal(self: Ctx, pat: CheckedPatternId) checked.PatternBinderId {
        const binder = self.pattern(pat).data.assign;
        self.binderLocal(binder, pat) catch invariant("out of memory binding a pattern");
        return binder;
    }

    pub fn asInfo(self: Ctx, pat: CheckedPatternId) struct { pattern: CheckedPatternId, local: checked.PatternBinderId } {
        const as = self.pattern(pat).data.as;
        self.binderLocal(as.binder, pat) catch invariant("out of memory binding a pattern");
        return .{ .pattern = as.pattern, .local = as.binder };
    }

    /// The destructs of a record pattern that read a field.
    fn fieldDestructs(self: Ctx, pat: CheckedPatternId) Allocator.Error![]const checked.CheckedRecordDestruct {
        var out: std.ArrayList(checked.CheckedRecordDestruct) = .empty;
        for (self.pattern(pat).data.record_destructure) |destruct| switch (destruct.kind) {
            .rest => {},
            .required, .sub_pattern => try out.append(self.arena(), destruct),
        };
        return out.items;
    }

    pub fn recordDestructCount(self: Ctx, pat: CheckedPatternId) u32 {
        return @intCast((self.fieldDestructs(pat) catch invariant("out of memory reading a record pattern")).len);
    }

    pub fn recordDestruct(self: Ctx, pat: CheckedPatternId, ty: types.TypeId, i: u32) Allocator.Error!SubPat {
        const destruct = (try self.fieldDestructs(pat))[i];
        const sub = switch (destruct.kind) {
            .required, .sub_pattern => |p| p,
            .rest => unreachable, // fieldDestructs keeps only field reads
        };
        const name = try self.store().internName(self.b.view.nameStore().recordFieldLabelText(destruct.label));
        const record_ty = try self.b.structuralType(ty);
        const record = switch (self.store().get(record_ty)) {
            .record => |record| record,
            else => invariant("a record pattern's type is not a record"),
        };
        const fields = self.store().fieldSpan(record.fields);
        for (fields, 0..) |field, index| {
            if (field.name != name) continue;
            // An optional field is read as its presence slot.
            return .{ .index = @intCast(index), .ty = try self.b.program.layouts.fieldStorage(field), .pat = sub };
        }
        // A field of an open row's remainder: only the record's descriptor
        // knows where it is.
        if (record.ext == null) invariant("a record pattern's field is absent from its type");
        const position: u32 = @intCast(self.state.named_fields.items.len);
        try self.state.named_fields.append(self.arena(), name);
        return .{ .index = named_field_base + position, .ty = try self.b.patternType(sub), .pat = sub };
    }

    pub fn tupleItemCount(self: Ctx, pat: CheckedPatternId) u32 {
        return @intCast(self.pattern(pat).data.tuple.len);
    }

    pub fn tupleItem(self: Ctx, pat: CheckedPatternId, ty: types.TypeId, i: u32) Allocator.Error!SubPat {
        const tuple_ty = try self.b.structuralType(ty);
        const elems = switch (self.store().get(tuple_ty)) {
            .tuple => |span| self.store().span(span),
            else => invariant("a tuple pattern's type is not a tuple"),
        };
        return .{ .index = i, .ty = elems[i], .pat = self.pattern(pat).data.tuple[i] };
    }

    pub fn nominalInner(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) Allocator.Error!SubPat {
        return .{ .index = 0, .ty = try self.b.structuralType(ty), .pat = self.pattern(pat).data.nominal.backing_pattern };
    }

    /// The tags a tag pattern on `ty` selects among: the joined row's for a
    /// closed union (in discriminant order), the type's own otherwise.
    fn unionTags(self: Ctx, ty: types.TypeId) Allocator.Error![]const types.Tag {
        const structural = try self.b.structuralType(ty);
        const shown = if (try self.b.closed(structural)) self.b.program.joins.representative(structural) else structural;
        return switch (self.store().get(shown)) {
            .tag_union => |tag_union| self.store().tagSpan(tag_union.tags),
            else => invariant("a tag pattern's type is not a tag union"),
        };
    }

    fn isBool(self: Ctx, ty: types.TypeId) Allocator.Error!bool {
        return switch (self.store().get(try self.b.structuralType(ty))) {
            .primitive => |primitive| primitive == .bool,
            else => false,
        };
    }

    pub fn tagVariant(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) Allocator.Error!layout.VariantIndex {
        const tag = self.pattern(pat).data.applied_tag;
        const name = try self.store().internName(self.b.view.nameStore().tagLabelText(tag.name));
        if (try self.isBool(ty)) return @intFromBool(std.mem.eql(u8, self.store().nameText(name), "True"));
        const structural = try self.b.structuralType(ty);
        if (try self.b.closed(structural)) {
            return @intCast((try self.b.selectVariant(structural, self.b.ownTag(structural, name, tag.args.len))).index);
        }
        for (try self.unionTags(ty), 0..) |candidate, index| {
            if (candidate.name == name and candidate.payloads.len == tag.args.len) return @intCast(index);
        }
        invariant("a tag pattern's tag is absent from its type");
    }

    pub fn tagPayloadCount(self: Ctx, pat: CheckedPatternId) u32 {
        return @intCast(self.pattern(pat).data.applied_tag.args.len);
    }

    pub fn tagPayload(self: Ctx, pat: CheckedPatternId, ty: types.TypeId, i: u32) Allocator.Error!SubPat {
        const variant = try self.tagVariant(pat, ty);
        const payloads = self.store().span((try self.unionTags(ty))[variant].payloads);
        return .{ .index = i, .ty = payloads[i], .pat = self.pattern(pat).data.applied_tag.args[i] };
    }

    pub fn tagVariantCount(self: Ctx, ty: types.TypeId) ?u32 {
        if (self.isBool(ty) catch invariant("out of memory reading a tag pattern's type")) return 2;
        const structural = self.b.structuralType(ty) catch invariant("out of memory reading a tag pattern's type");
        if (!(self.b.closed(structural) catch invariant("out of memory reading a tag pattern's type"))) return null;
        return @intCast((self.unionTags(ty) catch invariant("out of memory reading a tag pattern's type")).len);
    }

    pub fn tagTestKind(self: Ctx, ty: types.TypeId) match_tree.TestKind {
        const structural = self.b.structuralType(ty) catch invariant("out of memory reading a tag pattern's type");
        // A union a body knows only by its descriptor is tested by name.
        return if (self.b.closed(structural) catch invariant("out of memory reading a tag pattern's type")) .tag else .tag_name;
    }

    pub fn recordRest(self: Ctx, pat: CheckedPatternId, _: types.TypeId) Allocator.Error!?SubPat {
        for (self.pattern(pat).data.record_destructure) |destruct| switch (destruct.kind) {
            .rest => |rest| {
                // `..` with nothing to bind reads nothing.
                if (self.pattern(rest).data == .underscore) return null;
                invariant("record rest patterns that bind the rest are not yet lowered by Boxy");
            },
            .required, .sub_pattern => {},
        };
        return null;
    }

    /// A pattern on an optional field matches the field's presence as
    /// `Try(value, [MissingField])`, which its slot stores another way.
    pub fn retype(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) ?struct { key: u32, ty: types.TypeId } {
        if (self.b.program.layouts.presenceSlotPayload(ty) == null) return null;
        const pattern_ty = self.b.patternType(pat) catch invariant("out of memory reading a pattern's type");
        if (pattern_ty == ty) return null;
        return .{ .key = 0, .ty = pattern_ty };
    }

    pub fn callableVariant(_: Ctx, _: CheckedPatternId, _: types.TypeId) u32 {
        invariant("a callable pattern reached Boxy match lowering");
    }

    pub fn callablePayload(_: Ctx, _: CheckedPatternId, _: types.TypeId) Allocator.Error!?SubPat {
        invariant("a callable pattern reached Boxy match lowering");
    }

    pub fn callableVariantCount(_: Ctx, _: types.TypeId) ?u32 {
        invariant("a callable pattern reached Boxy match lowering");
    }

    pub fn listView(self: Ctx, pat: CheckedPatternId) ListPatView {
        const list = self.pattern(pat).data.list;
        return .{
            .fixed_count = @intCast(list.patterns.len),
            .rest = if (list.rest) |rest| .{ .index = rest.index, .pattern = rest.pattern } else null,
        };
    }

    pub fn listElemPat(self: Ctx, pat: CheckedPatternId, i: u32) CheckedPatternId {
        return self.pattern(pat).data.list.patterns[i];
    }

    pub fn listElemTy(self: Ctx, ty: types.TypeId) types.TypeId {
        return switch (self.store().get(self.b.structuralType(ty) catch invariant("out of memory reading a list pattern's type"))) {
            .list => |elem| elem,
            else => invariant("a list pattern's type is not a List"),
        };
    }

    /// The exact numeral a builtin numeral pattern names.
    fn exactNumeral(self: Ctx, pat: CheckedPatternId) checked.exact_numeral.Exact {
        return self.b.view.view.module_env.exactNumeral(self.pattern(pat).data.numeral_literal.literal);
    }

    /// An integer pattern's value at its primitive, as a two's complement
    /// 128-bit integer; null when it does not fit the primitive.
    fn intValue(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) ?i128 {
        const exact_numeral = checked.exact_numeral;
        const target: exact_numeral.Target = switch (self.literalPrimitive(ty)) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| @field(exact_numeral.Target, @tagName(p)),
            else => return null,
        };
        const bits = exact_numeral.intBits(self.exactNumeral(pat), target) orelse return null;
        return switch (bits) {
            .i128 => |value| value,
            .u128 => |value| @bitCast(value),
        };
    }

    pub fn ctorKey(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) Allocator.Error!u128 {
        return switch (self.pattern(pat).data) {
            .applied_tag => try self.tagVariant(pat, ty),
            // A numeral matched where a body knows its number type only by
            // descriptor is identified by its exact value.
            .numeral_literal => if (!try self.b.closed(try self.b.structuralType(ty))) try self.exactKey(pat) else switch (self.literalPrimitive(ty)) {
                .dec => @bitCast((try checked.exact_numeral.decBits(self.arena(), self.exactNumeral(pat))) orelse
                    invariant("a numeral pattern does not fit its Dec type")),
                // IEEE `==`: +0.0 and -0.0 are one constructor.
                .f32 => blk: {
                    const value = try checked.exact_numeral.floatBits(f32, self.arena(), self.exactNumeral(pat));
                    break :blk @as(u32, @bitCast(if (value == 0.0) @as(f32, 0.0) else value));
                },
                .f64 => blk: {
                    const value = try checked.exact_numeral.floatBits(f64, self.arena(), self.exactNumeral(pat));
                    break :blk @as(u64, @bitCast(if (value == 0.0) @as(f64, 0.0) else value));
                },
                else => @bitCast(self.intValue(pat, ty) orelse invariant("a numeral pattern does not fit its integer type")),
            },
            .str_literal => |literal| try self.strShapeKey(self.b.bodies().stringLiteral(literal.literal), &.{}, .exact),
            .str_interpolation => |str| try self.strShapeKey(self.b.bodies().stringLiteral(str.prefix), str.steps, str.end),
            .list => |list| list.patterns.len,
            .assign, .underscore, .as, .record_destructure, .tuple, .nominal, .runtime_error, .pending => invariant("a constructor key was asked of a pattern with no constructor"),
        };
    }

    /// The key of a numeral's exact value, interned among the match's
    /// shapes.
    fn exactKey(self: Ctx, pat: CheckedPatternId) Allocator.Error!u128 {
        const exact = self.exactNumeral(pat);
        var encoded: std.ArrayList(u8) = .empty;
        try encoded.append(self.arena(), 2);
        try appendLenPrefixed(&encoded, self.arena(), exact.before);
        try appendLenPrefixed(&encoded, self.arena(), exact.after);
        var scale: [4]u8 = undefined;
        std.mem.writeInt(u32, &scale, exact.scale, .little);
        try encoded.appendSlice(self.arena(), &scale);
        try encoded.appendSlice(self.arena(), &.{ @intFromBool(exact.is_negative), @intFromBool(exact.is_fractional) });
        const entry = try self.state.shapes.getOrPut(self.arena(), encoded.items);
        if (!entry.found_existing) entry.value_ptr.* = @intCast(self.state.shapes.count() - 1);
        return entry.value_ptr.*;
    }

    fn strShapeKey(self: Ctx, prefix: []const u8, steps: []const checked.CheckedStrPatternStep, end: checked.CheckedStrPatternEnd) Allocator.Error!u128 {
        var encoded: std.ArrayList(u8) = .empty;
        try appendLenPrefixed(&encoded, self.arena(), prefix);
        for (steps) |step| try appendLenPrefixed(&encoded, self.arena(), self.b.bodies().stringLiteral(step.delimiter));
        try encoded.append(self.arena(), switch (end) {
            .exact => 0,
            .tail => 1,
        });
        const entry = try self.state.shapes.getOrPut(self.arena(), encoded.items);
        if (!entry.found_existing) entry.value_ptr.* = @intCast(self.state.shapes.count() - 1);
        return entry.value_ptr.*;
    }

    fn appendLenPrefixed(list: *std.ArrayList(u8), allocator: Allocator, bytes: []const u8) Allocator.Error!void {
        var len_bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &len_bytes, @intCast(bytes.len), .little);
        try list.appendSlice(allocator, &len_bytes);
        try list.appendSlice(allocator, bytes);
    }

    pub fn intSwitchValue(self: Ctx, pat: CheckedPatternId, ty: types.TypeId) ?u64 {
        if (self.pattern(pat).data != .numeral_literal or self.pattern(pat).data.numeral_literal.guard != null) return null;
        const value = self.intValue(pat, ty) orelse return null;
        // A narrow signed condition switches only on sign-bit-clear values,
        // where every executor's widening agrees.
        const width: u16 = switch (self.literalPrimitive(ty)) {
            .u8 => 8,
            .u16 => 16,
            .u32 => 32,
            .u64, .i64 => 64,
            .i8 => if (value < 0 or value > std.math.maxInt(i8)) return null else 8,
            .i16 => if (value < 0 or value > std.math.maxInt(i16)) return null else 16,
            .i32 => if (value < 0 or value > std.math.maxInt(i32)) return null else 32,
            else => return null,
        };
        const bits: u128 = @bitCast(value);
        const mask: u128 = if (width == 64) std.math.maxInt(u64) else (@as(u128, 1) << @intCast(width)) - 1;
        return @intCast(bits & mask);
    }

    pub fn strLitIsSetArm(_: Ctx) bool {
        // An arm with no steps and an exact end is byte equality, which is
        // exactly `Str` equality.
        return true;
    }

    pub fn strCaptureCount(self: Ctx, pat: CheckedPatternId) u32 {
        return switch (self.pattern(pat).data) {
            .str_interpolation => |str| @intCast(str.steps.len),
            else => 0,
        };
    }

    pub fn strCapturePat(self: Ctx, pat: CheckedPatternId, i: u32) ?CheckedPatternId {
        return self.pattern(pat).data.str_interpolation.steps[i].capture;
    }

    // -- Emission --

    pub fn stmtCount(self: Ctx) usize {
        return self.b.store.cfStmtCount();
    }

    pub fn freshJoinPointId(self: Ctx) LIR.JoinPointId {
        return self.b.freshJoin();
    }

    pub fn joinJump(self: Ctx, id: LIR.JoinPointId) Allocator.Error!LIR.CFStmtId {
        return try self.b.store.addCFStmt(.{ .jump = .{ .target = id } }, self.b.origin(.lowering_glue));
    }

    pub fn addExitJoin(self: Ctx, id: LIR.JoinPointId, body: LIR.CFStmtId, remainder: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.b.addJoinStmt(id, &.{}, body, remainder);
    }

    pub fn failTerminal(self: Ctx) Allocator.Error!LIR.CFStmtId {
        return try self.b.store.addCFStmt(.runtime_error, self.b.origin(.lowering_glue));
    }

    pub fn lirLocalForOcc(self: Ctx, step: match_tree.Step, ty: types.TypeId, parent: ?LIR.LocalId) Allocator.Error!LIR.LocalId {
        const local = if (try self.b.closed(ty))
            try self.b.localOf(ty, .frame)
        else switch (step) {
            // A dynamic nominal is its backing, read through its own
            // descriptor.
            .nominal_backing => parent.?,
            // Components of a dynamic value are views of its storage.
            .field, .tag_payload => try self.b.localOf(ty, .view),
            else => try self.b.localOf(ty, .frame),
        };
        try self.noteOcc(local, ty);
        return local;
    }

    pub fn lirLocalDiscriminant(self: Ctx) Allocator.Error!LIR.LocalId {
        return try self.b.closedLocal(.u32);
    }

    pub fn lirLocalU64(self: Ctx) Allocator.Error!LIR.LocalId {
        return try self.b.closedLocal(.u64);
    }

    pub fn isZstLirLocal(self: Ctx, local: LIR.LocalId) bool {
        return self.b.store.getLocal(local).layout_idx == .zst;
    }

    pub fn readDiscriminant(self: Ctx, disc: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.b.store.addCFStmt(.{ .assign_ref = .{ .target = disc, .op = LIR.RefOp.discriminant(source), .next = next } }, self.b.origin(.source));
    }

    pub fn readField(self: Ctx, dest: LIR.LocalId, ty: types.TypeId, source: LIR.LocalId, index: u32, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const record_ty = try self.b.structuralType(self.occType(source));
        const start = try self.b.beginBackward();
        if (index >= named_field_base) {
            const name = self.state.named_fields.items[index - named_field_base];
            try self.b.emitDyn(dest, .{ .field_addr_named = .{ .base = source, .desc = try self.b.descOf(record_ty), .name = try self.b.nameDesc(name) } });
        } else {
            try self.b.readFieldInto(dest, record_ty, source, index, ty);
        }
        return try self.b.endBackward(start, next);
    }

    pub fn readTagPayload(self: Ctx, dest: LIR.LocalId, ty: types.TypeId, source: LIR.LocalId, variant: layout.VariantIndex, index: u32, _: bool, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const union_ty = try self.b.structuralType(self.occType(source));
        const start = try self.b.beginBackward();
        if (try self.b.closed(union_ty)) {
            const committed = self.b.program.result.layouts.getLayout(try self.b.program.layouts.layoutOf(union_ty));
            if (committed.tag != .tag_union) {
                // A union without storage has only zero-sized payloads.
                if (self.b.store.getLocal(dest).layout_idx != .zst) invariant("a storage-free union has a payload with storage");
                try self.b.emitLinear(.{ .assign_struct = .{ .target = dest, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            } else {
                const payloads = self.store().span((try self.unionTags(union_ty))[variant].payloads);
                try self.b.readTagPayloadInto(dest, source, union_ty, variant, payloads, index);
            }
        } else {
            const name = (try self.unionTags(union_ty))[variant].name;
            try self.b.emitDyn(dest, .{ .tag_payload_addr = .{ .base = source, .desc = try self.b.descOf(union_ty), .name = try self.b.nameDesc(name), .payload_index = index } });
        }
        _ = ty;
        return try self.b.endBackward(start, next);
    }

    pub fn readCallablePayload(_: Ctx, _: LIR.LocalId, _: types.TypeId, _: LIR.LocalId, _: u32, _: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        invariant("a callable pattern reached Boxy match lowering");
    }

    /// The list type and element type of a list occurrence.
    fn listTypes(self: Ctx, source: LIR.LocalId) Allocator.Error!struct { list: types.TypeId, elem: types.TypeId } {
        const list_ty = try self.b.structuralType(self.occType(source));
        return switch (self.store().get(list_ty)) {
            .list => |elem| .{ .list = list_ty, .elem = elem },
            else => invariant("a list pattern's value is not a List"),
        };
    }

    pub fn readListLen(self: Ctx, dest: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const list = try self.listTypes(source);
        const start = try self.b.beginBackward();
        try self.b.emitListLowLevel(dest, .list_len, &.{source}, list.elem, list.list);
        return try self.b.endBackward(start, next);
    }

    pub fn readListElemFront(self: Ctx, dest: LIR.LocalId, source: LIR.LocalId, index: u64, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const list = try self.listTypes(source);
        const start = try self.b.beginBackward();
        const at = try self.b.intValue(index, try self.store().primitive(.u64));
        try self.b.emitListLowLevel(dest, .list_get_unsafe, &.{ source, at }, list.elem, list.list);
        return try self.b.endBackward(start, next);
    }

    pub fn readListElemBack(self: Ctx, dest: LIR.LocalId, source: LIR.LocalId, len_local: LIR.LocalId, back: u64, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const list = try self.listTypes(source);
        const start = try self.b.beginBackward();
        const from_end = try self.b.intValue(back, try self.store().primitive(.u64));
        const at = try self.b.closedLocal(.u64);
        try self.b.emitLowLevel(at, .num_int_sub_wrap, &.{ len_local, from_end });
        try self.b.emitListLowLevel(dest, .list_get_unsafe, &.{ source, at }, list.elem, list.list);
        return try self.b.endBackward(start, next);
    }

    pub fn readListRest(self: Ctx, dest: LIR.LocalId, source: LIR.LocalId, len_local: LIR.LocalId, front: u64, back: u64, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const list = try self.listTypes(source);
        const u64_ty = try self.store().primitive(.u64);
        const start = try self.b.beginBackward();
        const fixed = try self.b.intValue(front + back, u64_ty);
        const rest_len = try self.b.closedLocal(.u64);
        try self.b.emitLowLevel(rest_len, .num_int_sub_wrap, &.{ len_local, fixed });
        // `{ len, start }` in field-name order.
        const range_ty = try self.store().intern(.{ .record = .{ .fields = &.{
            .{ .name = try self.store().internName("len"), .ty = u64_ty, .kind = .inline_slot },
            .{ .name = try self.store().internName("start"), .ty = u64_ty, .kind = .inline_slot },
        }, .ext = null } });
        const range = try self.b.buildStruct(range_ty, &.{ rest_len, try self.b.intValue(front, u64_ty) });
        try self.b.emitListLowLevel(dest, .list_sublist, &.{ source, range }, list.elem, list.list);
        return try self.b.endBackward(start, next);
    }

    pub fn readNominalBacking(self: Ctx, dest: LIR.LocalId, _: types.TypeId, source: LIR.LocalId, _: types.TypeId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        if (dest == source) return next;
        return try self.b.store.addCFStmt(.{ .assign_ref = .{ .target = dest, .op = LIR.RefOp.local(source), .next = next } }, self.b.origin(.source));
    }

    pub fn readRecordRest(_: Ctx, _: LIR.LocalId, _: types.TypeId, _: LIR.LocalId, _: types.TypeId, _: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        invariant("a record rest occurrence reached Boxy match lowering");
    }

    pub fn readRetype(self: Ctx, dest: LIR.LocalId, ty: types.TypeId, source: LIR.LocalId, source_ty: types.TypeId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const start = try self.b.beginBackward();
        const presence = try self.b.presenceTry(source, source_ty, ty);
        try self.b.emitLinear(.{ .assign_ref = .{ .target = dest, .op = LIR.RefOp.local(presence), .next = undefined } });
        return try self.b.endBackward(start, next);
    }

    pub fn tagNameTest(self: Ctx, source: LIR.LocalId, ty: types.TypeId, pat: CheckedPatternId, on_match: LIR.CFStmtId, on_miss: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const union_ty = try self.b.structuralType(ty);
        const tag = self.pattern(pat).data.applied_tag;
        const name = try self.store().internName(self.b.view.nameStore().tagLabelText(tag.name));
        const start = try self.b.beginBackward();
        const desc = try self.b.descOf(union_ty);
        const actual = try self.b.closedLocal(.u32);
        try self.b.emitDyn(actual, .{ .tag_discriminant = .{ .base = source, .desc = desc } });
        const expected = try self.b.closedLocal(.u32);
        try self.b.emitDyn(expected, .{ .tag_discriminant_of = .{ .desc = desc, .name = try self.b.nameDesc(name) } });
        const matches = try self.b.closedLocal(.bool);
        try self.b.emitLowLevel(matches, .num_is_eq, &.{ actual, expected });
        try self.b.emitTerminal(try self.boolSwitchStmt(matches, on_match, on_miss));
        return self.b.endBackwardTerminal(start);
    }

    pub fn literalGuardTemp(self: Ctx, _: CheckedPatternId) Allocator.Error!LIR.LocalId {
        return try self.b.closedLocal(.bool);
    }

    pub fn switchStmt(self: Ctx, cond: LIR.LocalId, values: []const u64, bodies: []const LIR.CFStmtId, default: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const branches = try self.arena().alloc(LIR.CFSwitchBranch, values.len);
        for (branches, values, bodies) |*branch, value, body| branch.* = .{ .value = value, .body = body };
        return try self.b.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.b.store.addCFSwitchBranches(branches),
            .default_branch = default,
        } }, self.b.origin(.source));
    }

    pub fn lenGteTest(self: Ctx, len_local: LIR.LocalId, min: u64, then: LIR.CFStmtId, els: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const start = try self.b.beginBackward();
        const required = try self.b.intValue(min, try self.store().primitive(.u64));
        const fits = try self.b.closedLocal(.bool);
        try self.b.emitLowLevel(fits, .num_is_gte, &.{ len_local, required });
        try self.b.emitTerminal(try self.boolSwitchStmt(fits, then, els));
        return self.b.endBackwardTerminal(start);
    }

    pub fn literalEqTest(self: Ctx, source: LIR.LocalId, ty: types.TypeId, pat: CheckedPatternId, on_match: LIR.CFStmtId, on_miss: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const start = try self.b.beginBackward();
        const matches = try self.b.closedLocal(.bool);
        switch (self.pattern(pat).data) {
            .numeral_literal => |numeral| {
                const structural = try self.b.structuralType(ty);
                if (try self.b.closed(structural)) {
                    const expected = try self.b.builtinNumeral(numeral.literal, structural);
                    try self.b.emitLowLevel(matches, .num_is_eq, &.{ source, expected });
                } else {
                    // The literal at the number type the descriptor names.
                    const expected = try self.b.descriptorNumeral(numeral.literal, ty);
                    try self.b.emitDyn(matches, .{ .eq = .{ .lhs = source, .rhs = expected, .desc = try self.b.descOf(ty) } });
                }
            },
            .str_literal => |literal| {
                const expected = try self.b.strLiteral(self.b.bodies().stringLiteral(literal.literal));
                try self.b.emitLowLevel(matches, .str_is_eq, &.{ source, expected });
            },
            else => invariant("an equality arm was asked of a non-literal pattern"),
        }
        try self.b.emitTerminal(try self.boolSwitchStmt(matches, on_match, on_miss));
        return self.b.endBackwardTerminal(start);
    }

    pub fn strArmCaptureLocals(self: Ctx, pat: CheckedPatternId) Allocator.Error![]const ?LIR.LocalId {
        return switch (self.pattern(pat).data) {
            .str_literal => &.{},
            .str_interpolation => |str| blk: {
                const locals = try self.arena().alloc(?LIR.LocalId, str.steps.len);
                for (locals, str.steps) |*out, step| {
                    out.* = if (step.capture != null) try self.b.closedLocal(.str) else null;
                }
                break :blk locals;
            },
            else => invariant("a string arm was asked of a non-string pattern"),
        };
    }

    fn lirStr(self: Ctx, literal: checked.CheckedStringLiteralId) Allocator.Error!LIR.StrLiteral {
        const text = self.b.bodies().stringLiteral(literal);
        return .{ .backing = try self.b.store.insertString(text), .offset = 0, .len = @intCast(text.len) };
    }

    pub fn buildStrArm(self: Ctx, pat: CheckedPatternId, capture_locals: []const ?LIR.LocalId, on_match: LIR.CFStmtId) Allocator.Error!LIR.StrMatchArm {
        return switch (self.pattern(pat).data) {
            .str_literal => |literal| .{
                .prefix = try self.lirStr(literal.literal),
                .steps = try self.b.store.addStrMatchSteps(&.{}),
                .end = .exact,
                .on_match = on_match,
            },
            .str_interpolation => |str| blk: {
                const steps = try self.arena().alloc(LIR.StrMatchStep, str.steps.len);
                for (steps, str.steps, capture_locals) |*out, step, capture| {
                    out.* = .{
                        .capture = if (capture) |local| .{ .view = local } else .discard,
                        .delimiter = try self.lirStr(step.delimiter),
                    };
                }
                break :blk .{
                    .prefix = try self.lirStr(str.prefix),
                    .steps = try self.b.store.addStrMatchSteps(steps),
                    .end = switch (str.end) {
                        .exact => .exact,
                        .tail => .tail,
                    },
                    .on_match = on_match,
                };
            },
            else => invariant("a string arm was asked of a non-string pattern"),
        };
    }

    pub fn strMatchSet(self: Ctx, source: LIR.LocalId, arms: []const LIR.StrMatchArm, on_miss: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.b.store.addCFStmt(.{ .str_match_set = .{
            .source = source,
            .arms = try self.b.store.addStrMatchArms(arms),
            .on_miss = on_miss,
        } }, self.b.origin(.source));
    }

    pub fn bindPatternLocal(self: Ctx, binder: checked.PatternBinderId, ty: types.TypeId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        const local = self.state.binder_locals.get(binder) orelse invariant("a pattern binder has no local");
        if (self.b.store.localDyn(local)) |dyn| {
            return try self.b.store.addCFStmt(.{ .dyn = .{
                .target = local,
                .data = try self.b.store.addStmtRow(LIR.DynOp, .{ .copy = .{ .src = source, .desc = dyn.desc } }),
                .next = next,
            } }, self.b.origin(.source));
        }
        _ = ty;
        return try self.b.store.addCFStmt(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.local(source), .next = next } }, self.b.origin(.source));
    }

    pub fn guardTemp(self: Ctx, _: CheckedExprId) Allocator.Error!LIR.LocalId {
        return try self.b.closedLocal(.bool);
    }

    pub fn boolSwitch(self: Ctx, cond: LIR.LocalId, then: LIR.CFStmtId, els: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.b.store.addCFStmt(try self.boolSwitchStmt(cond, then, els), self.b.origin(.source));
    }

    fn boolSwitchStmt(self: Ctx, cond: LIR.LocalId, then: LIR.CFStmtId, els: LIR.CFStmtId) Allocator.Error!LIR.CFStmt {
        return .{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.b.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }}),
            .default_branch = els,
        } };
    }
};
