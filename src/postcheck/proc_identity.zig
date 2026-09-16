//! Content identities for lowered procedures.
//!
//! A procedure's symbol has to name the same code in every program, so it
//! cannot be built from any per-program numbering. The identity rendered here
//! depends only on what determines the procedure's compiled bytes: the lifted
//! function's checked source identity (`Lifted.Program.fnSourceDigest`), the
//! Lambda Mono ABI choices, and the solved function type with its lambda sets.
//!
//! A lambda-set member is rendered as the member function's source identity
//! followed by its captures and its own solved function type, because two
//! specializations of one lambda that differ only in the lambda sets inside
//! their captures or arguments are different procedures. That descent can lead
//! back to a type already being rendered, for example a stream whose `next`
//! closure returns a stream holding that same closure. The walk keeps the
//! types currently being rendered on a stack and writes a back-reference to
//! the stack position, counted from the referencing type, when it meets one
//! of them again, so the rendering is finite and identical for any two
//! programs whose type graphs have the same shape, whatever their type
//! variable ids are.
//!
//! Every type renders as the digest of its own rendering rather than inline,
//! and a type whose rendering refers to nothing outside it is remembered by
//! the `Memo` shared across procedures. A solved type graph shares subtypes
//! heavily (one record type reached from hundreds of lambda-set members), and
//! rendering each occurrence anew would grow with the number of paths through
//! the graph instead of its size. The digest of a type is the same whether it
//! came from the memo or from a fresh rendering, so memo state never changes
//! an identity.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const collections = @import("collections");

const Common = @import("common.zig");
const MonoType = @import("monotype/type.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");

const names = check.CheckedNames;
const TypeDigestHasher = base.TypeDigestHasher;
const Allocator = std.mem.Allocator;

/// SHA-256 content identity of one procedure.
pub const Identity = [TypeDigestHasher.digest_length]u8;

const domain = "roc.proc.identity.v1";

/// Digests of types whose renderings refer to nothing outside themselves,
/// keyed by the type's root variable. Shared by every rendering over one
/// solved program.
pub const Memo = collections.DenseMap(SolvedType.TypeVarId, Identity);

/// Rendering state of one identity: the types currently being rendered, each
/// at its stack depth, and the memo shared across identities.
const Walk = struct {
    active: collections.DenseMap(SolvedType.TypeVarId, u32),
    depth: u32,
    memo: *Memo,
};

/// The shallowest stack depth a rendering referred back to, or
/// `no_reference` when it referred to nothing outside itself.
const no_reference: u32 = std.math.maxInt(u32);

/// Renders specialization identities over one solved program.
pub const Renderer = struct {
    allocator: Allocator,
    types: SolvedType.Store.View,
    names: *const names.NameStore,
    /// Solved function type of every lifted function, indexed by `Lifted.FnId`.
    fn_tys: []const SolvedType.TypeVarId,
    /// `Lifted.Program.fnSourceDigest` of every lifted function that has a
    /// checked source, indexed by `Lifted.FnId`.
    source_digests: []const ?Identity,
    fn_by_symbol: *const std.AutoHashMap(Common.Symbol, Lifted.FnId),
    memo: *Memo,

    /// Identity of the specialization of `source` at `solved_fn_ty` with the
    /// given captures and Lambda Mono ABI choices. Two specializations with
    /// the same identity are one procedure: everything that determines the
    /// procedure's code is rendered here, so Direct LIR lowers one proc per
    /// identity.
    pub fn specIdentity(
        self: *const Renderer,
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        captures: []const SolvedType.Capture,
        capture_abi: []const u8,
        return_reuse: []const u8,
    ) Allocator.Error!Identity {
        var hasher = TypeDigestHasher.init();
        writeBytes(&hasher, domain);
        hasher.update(&self.sourceDigest(source));
        writeBytes(&hasher, capture_abi);
        writeBytes(&hasher, return_reuse);
        var walk = Walk{
            .active = collections.DenseMap(SolvedType.TypeVarId, u32).init(self.allocator),
            .depth = 0,
            .memo = self.memo,
        };
        defer walk.active.deinit();
        _ = try self.writeType(&hasher, solved_fn_ty, &walk);
        writeBytes(&hasher, "captures");
        writeU32(&hasher, @intCast(captures.len));
        for (captures) |capture| _ = try self.writeType(&hasher, capture.ty, &walk);
        return hasher.finalResult();
    }

    fn sourceDigest(self: *const Renderer, fn_id: Lifted.FnId) Identity {
        return self.source_digests[@intFromEnum(fn_id)] orelse
            Common.invariant("lifted function without a checked source template reached procedure identity rendering");
    }

    /// Writes the digest of `ty`'s rendering and returns the shallowest
    /// stack depth that rendering referred back to.
    fn writeType(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        ty: SolvedType.TypeVarId,
        walk: *Walk,
    ) Allocator.Error!u32 {
        var root = self.types.root(ty);
        var content = self.types.get(root);
        // Transparent aliases have no runtime identity; their backing does.
        while (transparentAliasBacking(content)) |backing| {
            root = self.types.root(backing);
            content = self.types.get(root);
        }
        // A type still being rendered is a back-reference whatever the memo
        // holds, so memo state cannot change the bytes written for it.
        if (walk.active.get(root)) |depth| {
            writeBytes(hasher, "cycle");
            writeU32(hasher, walk.depth - depth);
            return depth;
        }
        if (walk.memo.get(root)) |digest| {
            writeBytes(hasher, "type");
            hasher.update(&digest);
            return no_reference;
        }
        const depth = walk.depth;
        try walk.active.putNoClobber(root, depth);
        walk.depth += 1;
        defer {
            _ = walk.active.remove(root);
            walk.depth -= 1;
        }

        var sub = TypeDigestHasher.init();
        var low: u32 = no_reference;
        switch (content) {
            .mono, .link, .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached procedure identity rendering"),
            .primitive => |primitive| {
                writeBytes(&sub, "primitive");
                writeBytes(&sub, @tagName(primitive));
            },
            .zst => writeBytes(&sub, "zst"),
            .erased => |erased| {
                writeBytes(&sub, "erased");
                sub.update(&erased.source_fn_ty.bytes);
                low = @min(low, try self.writeMembers(&sub, erased.members, walk));
            },
            .func => |func| {
                writeBytes(&sub, "func");
                low = @min(low, try self.writeSpan(&sub, func.args, walk));
                low = @min(low, try self.writeType(&sub, func.callable, walk));
                low = @min(low, try self.writeType(&sub, func.ret, walk));
            },
            .list => |elem| {
                writeBytes(&sub, "list");
                low = @min(low, try self.writeType(&sub, elem, walk));
            },
            .box => |elem| {
                writeBytes(&sub, "box");
                low = @min(low, try self.writeType(&sub, elem, walk));
            },
            .tuple => |items| {
                writeBytes(&sub, "tuple");
                low = @min(low, try self.writeSpan(&sub, items, walk));
            },
            .record => |fields| {
                writeBytes(&sub, "record");
                const field_slice = self.types.fieldSpan(fields);
                writeU32(&sub, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeBytes(&sub, self.names.recordFieldLabelText(field.name));
                    MonoType.writeFieldDefaultDigest(self.names, &sub, field.default);
                    if (field.value_ty) |value_ty| {
                        writeBytes(&sub, "field-optional-value");
                        low = @min(low, try self.writeType(&sub, value_ty, walk));
                    } else {
                        writeBytes(&sub, "field-inline-value");
                    }
                    low = @min(low, try self.writeType(&sub, field.ty, walk));
                }
            },
            .tag_union => |tags| {
                writeBytes(&sub, "tag_union");
                const tag_slice = self.types.tagSpan(tags);
                writeU32(&sub, @intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    writeBytes(&sub, self.names.tagLabelText(tag.name));
                    low = @min(low, try self.writeSpan(&sub, tag.payloads, walk));
                }
            },
            .named => |named| {
                writeBytes(&sub, "named");
                sub.update(&named.named_type.module.bytes);
                writeBytes(&sub, self.names.moduleIdentityBytes(named.def.module));
                writeOptionalU32(&sub, named.def.source_decl);
                writeBytes(&sub, self.names.typeNameText(named.def.type_name));
                writeBytes(&sub, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(&sub, "builtin");
                    writeBytes(&sub, @tagName(owner));
                } else {
                    writeBytes(&sub, "not-builtin");
                }
                low = @min(low, try self.writeSpan(&sub, named.args, walk));
                // A nominal type's representation is its backing, and the
                // backing can hold lambda sets the arguments never mention,
                // so the same name at the same arguments can still be two
                // different procedures' worth of code.
                if (named.backing) |backing| {
                    writeBytes(&sub, "backing");
                    low = @min(low, try self.writeType(&sub, backing.ty, walk));
                } else {
                    writeBytes(&sub, "no-backing");
                }
            },
            .lambda_set => |members| low = @min(low, try self.writeMembers(&sub, members, walk)),
        }
        const digest = sub.finalResult();
        writeBytes(hasher, "type");
        hasher.update(&digest);
        // Back-references into this type's own stack frame are relative to
        // the referencing type, so a rendering that reaches nothing above
        // this frame is the same from every entry and can be remembered.
        if (low >= depth) {
            try walk.memo.put(root, digest);
            return no_reference;
        }
        return low;
    }

    fn writeMembers(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        members: SolvedType.Span,
        walk: *Walk,
    ) Allocator.Error!u32 {
        writeBytes(hasher, "lambda_set");
        const member_slice = self.types.memberSpan(members);
        writeU32(hasher, @intCast(member_slice.len));
        var low: u32 = no_reference;
        for (member_slice) |member| {
            const fn_id = self.fn_by_symbol.get(member.lambda) orelse
                Common.invariant("lambda-set member referenced a lifted function with no identity");
            hasher.update(&self.sourceDigest(fn_id));
            const captures = self.types.captureSpan(member.captures);
            writeU32(hasher, @intCast(captures.len));
            for (captures) |capture| low = @min(low, try self.writeType(hasher, capture.ty, walk));
            low = @min(low, try self.writeType(hasher, self.fn_tys[@intFromEnum(fn_id)], walk));
        }
        return low;
    }

    fn writeSpan(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        span: SolvedType.Span,
        walk: *Walk,
    ) Allocator.Error!u32 {
        const items = self.types.span(span);
        writeU32(hasher, @intCast(items.len));
        var low: u32 = no_reference;
        for (items) |item| low = @min(low, try self.writeType(hasher, item, walk));
        return low;
    }
};

fn transparentAliasBacking(content: SolvedType.Content) ?SolvedType.TypeVarId {
    if (std.meta.activeTag(content) != .named or content.named.kind != .alias) return null;
    return (content.named.backing orelse Common.invariant("transparent alias reached procedure identity rendering without a backing type")).ty;
}

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
    hasher.update(&buffer);
}

fn writeOptionalU32(hasher: *TypeDigestHasher, value: ?u32) void {
    if (value) |v| {
        hasher.update(&[_]u8{1});
        writeU32(hasher, v);
    } else {
        hasher.update(&[_]u8{0});
    }
}
