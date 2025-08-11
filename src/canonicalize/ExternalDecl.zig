//! Represents an external declaration from another module

const base = @import("base");
const collections = @import("collections");

const DataSpan = base.DataSpan;

const ExternalDecl = @This();

/// Fully qualified name (e.g., "json.Json.utf8")
qualified_name: base.Ident.Idx,
/// Module this decl comes from (e.g., "json.Json")
module_name: base.Ident.Idx,
/// Local name within that module (e.g., "utf8")
local_name: base.Ident.Idx,
/// Type information (populated later after dependency resolution)
type_idx: ?u32,

/// Index type for referencing external declarations in storage.
pub const Idx = enum(u32) { _ };
/// A span of external declarations stored contiguously in memory.
pub const Span = struct { span: DataSpan };

/// Converts this external declaration to an S-expression tree representation for debugging
pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) !void {
    _ = self;
    _ = cir;
    try tree.pushStaticAtom("external-decl-stub");
}
