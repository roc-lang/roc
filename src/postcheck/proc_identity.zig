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
//! the stack position when it meets one of them again, so the rendering is
//! finite and identical for any two programs whose type graphs have the same
//! shape, whatever their type variable ids are.

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
        var active = collections.DenseMap(SolvedType.TypeVarId, u32).init(self.allocator);
        defer active.deinit();
        try self.writeType(&hasher, solved_fn_ty, &active);
        writeBytes(&hasher, "captures");
        writeU32(&hasher, @intCast(captures.len));
        for (captures) |capture| try self.writeType(&hasher, capture.ty, &active);
        return hasher.finalResult();
    }

    fn sourceDigest(self: *const Renderer, fn_id: Lifted.FnId) Identity {
        return self.source_digests[@intFromEnum(fn_id)] orelse
            Common.invariant("lifted function without a checked source template reached procedure identity rendering");
    }

    fn writeType(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        ty: SolvedType.TypeVarId,
        active: *collections.DenseMap(SolvedType.TypeVarId, u32),
    ) Allocator.Error!void {
        var root = self.types.root(ty);
        var content = self.types.get(root);
        // Transparent aliases have no runtime identity; their backing does.
        while (transparentAliasBacking(content)) |backing| {
            root = self.types.root(backing);
            content = self.types.get(root);
        }
        if (active.get(root)) |position| {
            writeBytes(hasher, "cycle");
            writeU32(hasher, position);
            return;
        }
        try active.putNoClobber(root, @intCast(active.count()));
        defer _ = active.remove(root);

        switch (content) {
            .mono, .link, .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached procedure identity rendering"),
            .primitive => |primitive| {
                writeBytes(hasher, "primitive");
                writeBytes(hasher, @tagName(primitive));
            },
            .zst => writeBytes(hasher, "zst"),
            .erased => |erased| {
                writeBytes(hasher, "erased");
                hasher.update(&erased.source_fn_ty.bytes);
                try self.writeMembers(hasher, erased.members, active);
            },
            .func => |func| {
                writeBytes(hasher, "func");
                try self.writeSpan(hasher, func.args, active);
                try self.writeType(hasher, func.callable, active);
                try self.writeType(hasher, func.ret, active);
            },
            .list => |elem| {
                writeBytes(hasher, "list");
                try self.writeType(hasher, elem, active);
            },
            .box => |elem| {
                writeBytes(hasher, "box");
                try self.writeType(hasher, elem, active);
            },
            .tuple => |items| {
                writeBytes(hasher, "tuple");
                try self.writeSpan(hasher, items, active);
            },
            .record => |fields| {
                writeBytes(hasher, "record");
                const field_slice = self.types.fieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeBytes(hasher, self.names.recordFieldLabelText(field.name));
                    MonoType.writeFieldDefaultDigest(self.names, hasher, field.default);
                    if (field.value_ty) |value_ty| {
                        writeBytes(hasher, "field-optional-value");
                        try self.writeType(hasher, value_ty, active);
                    } else {
                        writeBytes(hasher, "field-inline-value");
                    }
                    try self.writeType(hasher, field.ty, active);
                }
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                const tag_slice = self.types.tagSpan(tags);
                writeU32(hasher, @intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    writeBytes(hasher, self.names.tagLabelText(tag.name));
                    try self.writeSpan(hasher, tag.payloads, active);
                }
            },
            .named => |named| {
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, self.names.moduleIdentityBytes(named.def.module));
                writeOptionalU32(hasher, named.def.source_decl);
                writeBytes(hasher, self.names.typeNameText(named.def.type_name));
                writeBytes(hasher, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(hasher, "builtin");
                    writeBytes(hasher, @tagName(owner));
                } else {
                    writeBytes(hasher, "not-builtin");
                }
                try self.writeSpan(hasher, named.args, active);
                // A nominal type's representation is its backing, and the
                // backing can hold lambda sets the arguments never mention,
                // so the same name at the same arguments can still be two
                // different procedures' worth of code.
                if (named.backing) |backing| {
                    writeBytes(hasher, "backing");
                    try self.writeType(hasher, backing.ty, active);
                } else {
                    writeBytes(hasher, "no-backing");
                }
            },
            .lambda_set => |members| try self.writeMembers(hasher, members, active),
        }
    }

    fn writeMembers(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        members: SolvedType.Span,
        active: *collections.DenseMap(SolvedType.TypeVarId, u32),
    ) Allocator.Error!void {
        writeBytes(hasher, "lambda_set");
        const member_slice = self.types.memberSpan(members);
        writeU32(hasher, @intCast(member_slice.len));
        for (member_slice) |member| {
            const fn_id = self.fn_by_symbol.get(member.lambda) orelse
                Common.invariant("lambda-set member referenced a lifted function with no identity");
            hasher.update(&self.sourceDigest(fn_id));
            const captures = self.types.captureSpan(member.captures);
            writeU32(hasher, @intCast(captures.len));
            for (captures) |capture| try self.writeType(hasher, capture.ty, active);
            try self.writeType(hasher, self.fn_tys[@intFromEnum(fn_id)], active);
        }
    }

    fn writeSpan(
        self: *const Renderer,
        hasher: *TypeDigestHasher,
        span: SolvedType.Span,
        active: *collections.DenseMap(SolvedType.TypeVarId, u32),
    ) Allocator.Error!void {
        const items = self.types.span(span);
        writeU32(hasher, @intCast(items.len));
        for (items) |item| try self.writeType(hasher, item, active);
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
