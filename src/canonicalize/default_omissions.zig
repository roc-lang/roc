//! Shared enumeration of the declared defaulted fields a LOCAL nominal
//! construction omits (design.md "Defaulted Fields").
//!
//! A construction over a record-literal backing materializes the declaration's
//! default for every defaulted field the literal does not mention. A record
//! UPDATE (`{ ..base, ... }`) omits nothing (unmentioned fields come from the
//! base value), and an `unset` field (`x: _`) is mentioned (rejecting unset of
//! a defaulted field is the checker's own axis). Non-record backings (tags,
//! tuples, values, error forms) construct no fields, so they omit nothing.
//!
//! Consumed by `DefaultCycles.zig` (the omission edges of the default-cycle
//! graph) and `DependencyGraph.zig` (the omitted defaults' expressions
//! contribute demand and name-reference edges at construction sites), so both
//! graphs agree on what "omitted" means. Foreign constructions
//! (`e_nominal_external`) never enumerate here: their declarations' defaults
//! live in the foreign module and are that module's own roots (the checker's
//! residue walk owns those edges).

const std = @import("std");
const base = @import("base");
const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");

/// One declared defaulted field that a construction omits.
pub const OmittedDefault = struct {
    field_idx: CIR.TypeAnno.RecordField.Idx,
    field_name: base.Ident.Idx,
    default_expr: CIR.Expr.Idx,
};

/// Resolve a nominal declaration's backing record annotation fields,
/// unwrapping (possibly nested) parentheses—`Foo := ({ ... })` declares
/// defaults through the wrapping (`canonicalizeNominalBackingAnno` accepts
/// them there). Null when the statement is not a nominal declaration, is a
/// never-filled forward placeholder, or its backing annotation is not a
/// record.
pub fn backingRecordFields(
    env: *const ModuleEnv,
    decl_stmt: CIR.Statement.Idx,
) ?CIR.TypeAnno.RecordField.Span {
    const stmt = env.store.getStatement(decl_stmt);
    if (stmt != .s_nominal_decl) return null;
    // A forward reference prepares a nominal declaration as a placeholder
    // statement; when the real declaration is then never registered (its
    // owner's associated block is skipped after a redeclaration/rejection
    // that already reported), the placeholder survives to end of module in
    // `env.forward_type_decls` — the same explicit state the checker guards
    // for on alias declarations. A never-filled declaration declares no
    // fields, so it omits nothing; `.placeholder` must not be read as a
    // `TypeAnno.Idx` (it is the reserved index 0).
    if (stmt.s_nominal_decl.anno == .placeholder) return null;
    var anno = env.store.getTypeAnno(stmt.s_nominal_decl.anno);
    while (anno == .parens) anno = env.store.getTypeAnno(anno.parens.anno);
    if (anno != .record) return null;
    return anno.record.fields;
}

/// Iterator over the declared defaulted fields that one construction's
/// backing expression omits. Obtain via `omittedDefaults`.
pub const Iterator = struct {
    env: *const ModuleEnv,
    declared_fields: []const CIR.TypeAnno.RecordField.Idx,
    /// The construction's supplied labels; null when the backing is
    /// `e_empty_record`, which supplies nothing.
    supplied: ?Supplied,
    position: usize = 0,

    const Supplied = struct {
        fields: CIR.RecordField.Span,
        unsets: CIR.UnsetField.Span,
    };

    pub fn next(self: *Iterator) ?OmittedDefault {
        while (self.position < self.declared_fields.len) {
            const field_idx = self.declared_fields[self.position];
            self.position += 1;
            const field = self.env.store.getAnnoRecordField(field_idx);
            const default_expr = field.default_value orelse continue;
            if (self.supplied) |supplied| {
                if (suppliesLabel(self.env, supplied, field.name)) continue;
            }
            return .{
                .field_idx = field_idx,
                .field_name = field.name,
                .default_expr = default_expr,
            };
        }
        return null;
    }

    fn suppliesLabel(env: *const ModuleEnv, supplied: Supplied, name: base.Ident.Idx) bool {
        for (env.store.sliceRecordFields(supplied.fields)) |field_idx| {
            if (env.store.getRecordField(field_idx).name.eql(name)) return true;
        }
        for (env.store.sliceUnsetFields(supplied.unsets)) |unset_idx| {
            if (env.store.getUnsetField(unset_idx).name.eql(name)) return true;
        }
        return false;
    }
};

/// Enumerate the defaulted fields `backing_expr` omits when constructing the
/// nominal declared by `decl_stmt` (an `e_nominal`'s `nominal_type_decl`).
/// Yields nothing when the declaration or backing shape has no omission to
/// judge.
pub fn omittedDefaults(
    env: *const ModuleEnv,
    decl_stmt: CIR.Statement.Idx,
    backing_expr: CIR.Expr.Idx,
) Iterator {
    const none = Iterator{ .env = env, .declared_fields = &.{}, .supplied = null };
    const declared = backingRecordFields(env, decl_stmt) orelse return none;
    switch (env.store.getExpr(backing_expr)) {
        .e_record => |record| {
            // A record UPDATE omits nothing: unmentioned fields come from
            // the base value, not from defaults.
            if (record.ext != null) return none;
            return .{
                .env = env,
                .declared_fields = env.store.sliceAnnoRecordFields(declared),
                .supplied = .{ .fields = record.fields, .unsets = record.unsets },
            };
        },
        .e_empty_record => return .{
            .env = env,
            .declared_fields = env.store.sliceAnnoRecordFields(declared),
            .supplied = null,
        },
        // Only record-literal backings construct fields; every other backing
        // shape (tags, tuples, error forms) has no omission to judge. Listed
        // exhaustively so a new expression form forces a decision here.
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_num_from_numeral,
        .e_typed_int,
        .e_typed_frac,
        .e_typed_num_from_numeral,
        .e_str_segment,
        .e_str,
        .e_bytes_literal,
        .e_lookup_local,
        .e_lookup_external,
        .e_lookup_associated_local,
        .e_lookup_associated,
        .e_lookup_associated_resolved,
        .e_lookup_required,
        .e_list,
        .e_empty_list,
        .e_tuple,
        .e_match,
        .e_if,
        .e_call,
        .e_block,
        .e_tag,
        .e_nominal,
        .e_nominal_external,
        .e_zero_argument_tag,
        .e_closure,
        .e_lambda,
        .e_binop,
        .e_unary_minus,
        .e_unary_not,
        .e_field_access,
        .e_method_call,
        .e_dispatch_call,
        .e_interpolation,
        .e_structural_eq,
        .e_structural_hash,
        .e_method_eq,
        .e_type_method_call,
        .e_type_dispatch_call,
        .e_tuple_access,
        .e_runtime_error,
        .e_crash,
        .e_dbg,
        .e_expect_err,
        .e_expect,
        .e_ellipsis,
        .e_anno_only,
        .e_derived_method,
        .e_return,
        .e_break,
        .e_for,
        .e_hosted_lambda,
        .e_run_low_level,
        => return none,
    }
}
