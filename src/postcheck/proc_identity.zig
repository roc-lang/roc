//! Content identities for lowered procedures.
//!
//! A procedure's symbol has to name the same code in every program, so it
//! cannot be built from any per-program numbering. The identity rendered here
//! depends only on what determines the procedure's compiled bytes: the lifted
//! function's checked source identity (`Lifted.Program.fnSourceDigest`), the
//! Lambda Mono ABI choices, and the solved argument, result, and capture types
//! with their lambda sets. The plain procedure of a layout-keyed Builtin
//! template is the exception: Direct LIR names it by its argument and result
//! layouts instead (design.md "Layout-Keyed Builtin Procedures").
//! The outer callable set describes where a function value flows, not the
//! code of the selected procedure; its source and captures already name that
//! selection. Nested callable sets still determine dispatch and representation.
//!
//! A lambda-set member is rendered as the member function's source identity
//! followed by its captures and its own solved function type, because two
//! specializations of one lambda that differ only in the lambda sets inside
//! their captures or arguments are different procedures. That descent can lead
//! back to a type already being rendered, for example a stream whose `next`
//! closure returns a stream holding that same closure, and recursive nominal
//! types are cycles in the graph by nature.
//!
//! Two programs draw the same recursive type as different graphs, depending
//! on which parts of the type each program's lambda solving touched: one may
//! tie a single knot where another rolls it out a level, or ties one copy
//! whose references lead into a second, separately tied copy. The identity
//! must not see that difference, so a type digests as its infinite unfolding
//! through the reduction every type identity shares
//! (`base.recursive_digest`).
//!
//! Every digest is a function of the infinite type alone, so the `Memo` shared
//! across procedures remembers every type it has rendered: a solved type graph
//! shares subtypes heavily (one record type reached from hundreds of
//! lambda-set members), and rendering each occurrence anew would grow with
//! the number of paths through the graph instead of its size.

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
const recursive_digest = base.recursive_digest;
const Allocator = std.mem.Allocator;

/// SHA-256 content identity of one procedure.
pub const Identity = [TypeDigestHasher.digest_length]u8;

const domain = "roc.proc.identity.v4";

/// Everything one solved program has rendered so far, shared by every
/// identity rendered over it.
pub const Memo = struct {
    /// Digest of every type rendered so far, keyed by its root variable.
    digests: collections.DenseMap(SolvedType.TypeVarId, Identity),
    /// Every recursive group rendered so far.
    settled: recursive_digest.Settled,
    /// Reduction scratch kept across reductions.
    graph: recursive_digest.Graph,
    /// Reduction node maps kept across reductions. A fresh map spanning old
    /// and new variables would allocate and clear a chunk table for that
    /// whole span on every reduction.
    node_maps: collections.DenseMapPool(SolvedType.TypeVarId, u32),

    pub fn init(allocator: Allocator) Memo {
        return .{
            .digests = collections.DenseMap(SolvedType.TypeVarId, Identity).init(allocator),
            .settled = recursive_digest.Settled.init(allocator, domain),
            .graph = recursive_digest.Graph.init(allocator),
            .node_maps = collections.DenseMapPool(SolvedType.TypeVarId, u32).init(allocator),
        };
    }

    pub fn deinit(self: *Memo) void {
        self.digests.deinit();
        self.settled.deinit();
        self.graph.deinit();
        self.node_maps.deinit();
    }
};

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
        const signature = switch (self.types.get(self.renderedRoot(solved_fn_ty))) {
            .func => |func| func,
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .lambda_set, .erased, .zst, .hole, .mono => Common.invariant("procedure identity requires a solved function signature"),
        };
        writeBytes(&hasher, "args");
        const args = self.types.span(signature.args);
        writeU32(&hasher, @intCast(args.len));
        for (args) |arg| hasher.update(&try self.typeDigest(arg));
        writeBytes(&hasher, "ret");
        hasher.update(&try self.typeDigest(signature.ret));
        writeBytes(&hasher, "captures");
        writeU32(&hasher, @intCast(captures.len));
        for (captures) |capture| hasher.update(&try self.typeDigest(capture.ty));
        return hasher.finalResult();
    }

    fn sourceDigest(self: *const Renderer, fn_id: Lifted.FnId) Identity {
        return self.source_digests[@backingInt(fn_id)] orelse
            Common.invariant("lifted function without a checked source template reached procedure identity rendering");
    }

    /// The variable a type renders as: its root, through any transparent
    /// aliases, which have no runtime identity of their own.
    fn renderedRoot(self: *const Renderer, ty: SolvedType.TypeVarId) SolvedType.TypeVarId {
        var root = self.types.root(ty);
        var content = self.types.get(root);
        while (transparentAliasBacking(content)) |backing| {
            root = self.types.root(backing);
            content = self.types.get(root);
        }
        return root;
    }

    /// Digest of the infinite type `ty` denotes.
    fn typeDigest(self: *const Renderer, ty: SolvedType.TypeVarId) Allocator.Error!Identity {
        const root = self.renderedRoot(ty);
        if (self.memo.digests.get(root)) |digest| return digest;
        var engine = Engine.init(self);
        defer engine.deinit();
        return try engine.run(root);
    }
};

/// One reduction: the subgraph reachable from a requested type through
/// types the memo does not hold yet.
const Engine = struct {
    renderer: *const Renderer,
    gpa: Allocator,
    graph: *recursive_digest.Graph,
    node_of_ty: collections.DenseMap(SolvedType.TypeVarId, u32),
    /// The type of every node of the reduction.
    tys: std.ArrayList(SolvedType.TypeVarId) = .empty,
    /// Nodes discovered but not yet encoded.
    pending: std.ArrayList(u32) = .empty,
    /// Content of the node being encoded.
    hasher: TypeDigestHasher = undefined,

    fn init(renderer: *const Renderer) Engine {
        renderer.memo.graph.reset();
        return .{
            .renderer = renderer,
            .gpa = renderer.allocator,
            .graph = &renderer.memo.graph,
            .node_of_ty = renderer.memo.node_maps.acquire(),
        };
    }

    fn deinit(self: *Engine) void {
        self.graph.reset();
        self.renderer.memo.node_maps.release(&self.node_of_ty);
        self.tys.deinit(self.gpa);
        self.pending.deinit(self.gpa);
    }

    fn run(self: *Engine, root: SolvedType.TypeVarId) Allocator.Error!Identity {
        const root_node = try self.discover(root);
        while (self.pending.pop()) |node| try self.encode(node);
        const memo = self.renderer.memo;
        try self.graph.resolve(&memo.settled, null);
        for (self.tys.items, 0..) |ty, node| try memo.digests.put(ty, self.graph.digest(@intCast(node)));
        return self.graph.digest(root_node);
    }

    /// The node for `ty`, created and queued for encoding if new.
    fn discover(self: *Engine, ty: SolvedType.TypeVarId) Allocator.Error!u32 {
        const gop = try self.node_of_ty.getOrPut(ty);
        if (gop.found_existing) return gop.value_ptr.*;
        const node = try self.graph.addNode();
        gop.value_ptr.* = node;
        try self.tys.append(self.gpa, ty);
        try self.pending.append(self.gpa, node);
        return node;
    }

    fn putBytes(self: *Engine, bytes: []const u8) Allocator.Error!void {
        writeBytes(&self.hasher, bytes);
    }

    fn putU32(self: *Engine, value: u32) Allocator.Error!void {
        writeU32(&self.hasher, value);
    }

    /// A child slot: a type the memo holds is a settled digest, any other is
    /// a node of this reduction.
    fn putChild(self: *Engine, ty: SolvedType.TypeVarId) Allocator.Error!void {
        writeBytes(&self.hasher, "child");
        const root = self.renderer.renderedRoot(ty);
        if (self.renderer.memo.digests.get(root)) |digest| {
            try self.graph.putChild(.{ .settled = digest });
        } else {
            try self.graph.putChild(.{ .node = try self.discover(root) });
        }
    }

    fn putSpan(self: *Engine, span: SolvedType.Span) Allocator.Error!void {
        const tys = self.renderer.types.span(span);
        try self.putU32(@intCast(tys.len));
        for (tys) |ty| try self.putChild(ty);
    }

    fn putMembers(self: *Engine, members: SolvedType.Span) Allocator.Error!void {
        const renderer = self.renderer;
        try self.putBytes("lambda_set");
        const member_slice = renderer.types.memberSpan(members);
        try self.putU32(@intCast(member_slice.len));
        for (member_slice) |member| {
            const fn_id = renderer.fn_by_symbol.get(member.lambda) orelse
                Common.invariant("lambda-set member referenced a lifted function with no identity");
            try self.putBytes(&renderer.sourceDigest(fn_id));
            const captures = renderer.types.captureSpan(member.captures);
            try self.putU32(@intCast(captures.len));
            for (captures) |capture| try self.putChild(capture.ty);
            try self.putChild(renderer.fn_tys[@backingInt(fn_id)]);
        }
    }

    /// Record the encoding of one node: its content's non-reference values
    /// and its ordered children.
    fn encode(self: *Engine, node: u32) Allocator.Error!void {
        const renderer = self.renderer;
        self.hasher = TypeDigestHasher.init();
        self.graph.beginNode(node);
        const content = renderer.types.get(self.tys.items[node]);
        switch (content) {
            .mono, .link, .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached procedure identity rendering"),
            .primitive => |primitive| {
                try self.putBytes("primitive");
                try self.putBytes(@tagName(primitive));
            },
            .zst => try self.putBytes("zst"),
            .hole => try self.putBytes("hole"),
            // An erased callable is one pointer-sized value called through
            // the convention of its source function type, whichever closures
            // the program puts in it, so its members are not part of the
            // code of a procedure that holds one. A procedure that packs a
            // function into it names that function in its own body.
            .erased => |erased| {
                try self.putBytes("erased");
                try self.putBytes(&erased.source_fn_ty.bytes);
            },
            .func => |func| {
                try self.putBytes("func");
                try self.putSpan(func.args);
                try self.putChild(func.callable);
                try self.putChild(func.ret);
            },
            .list => |elem| {
                try self.putBytes("list");
                try self.putChild(elem);
            },
            .box => |elem| {
                try self.putBytes("box");
                try self.putChild(elem);
            },
            .tuple => |elems| {
                try self.putBytes("tuple");
                try self.putSpan(elems);
            },
            .record => |fields| {
                try self.putBytes("record");
                const field_slice = renderer.types.fieldSpan(fields);
                try self.putU32(@intCast(field_slice.len));
                for (field_slice) |field| {
                    try self.putBytes(renderer.names.recordFieldLabelText(field.name));
                    var default_hasher = TypeDigestHasher.init();
                    MonoType.writeFieldDefaultDigest(renderer.names, &default_hasher, field.default);
                    try self.putBytes(&default_hasher.finalResult());
                    if (field.value_ty) |value_ty| {
                        try self.putBytes("field-optional-value");
                        try self.putChild(value_ty);
                    } else {
                        try self.putBytes("field-inline-value");
                    }
                    try self.putChild(field.ty);
                }
            },
            .tag_union => |tags| {
                try self.putBytes("tag_union");
                const tag_slice = renderer.types.tagSpan(tags);
                try self.putU32(@intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    try self.putBytes(renderer.names.tagLabelText(tag.name));
                    try self.putSpan(tag.payloads);
                }
            },
            .named => |named| {
                try self.putBytes("named");
                // A root app module type is named by its declaration; its
                // backing, rendered below, names its structure.
                if (named.def.site) {
                    try self.putBytes("site");
                } else {
                    try self.putBytes(&named.named_type.module.bytes);
                    try self.putBytes(renderer.names.moduleIdentityBytes(named.def.module));
                }
                if (named.def.source_decl) |decl| {
                    try self.putBytes("source-decl");
                    try self.putU32(decl);
                } else {
                    try self.putBytes("no-source-decl");
                }
                try self.putBytes(renderer.names.typeNameText(named.def.type_name));
                try self.putBytes(@tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    try self.putBytes("builtin");
                    try self.putBytes(@tagName(owner));
                } else {
                    try self.putBytes("not-builtin");
                }
                try self.putSpan(named.args);
                // A nominal type's representation is its backing, and the
                // backing can hold lambda sets the arguments never mention,
                // so the same name at the same arguments can still be two
                // different procedures' worth of code.
                if (named.backing) |backing| {
                    try self.putBytes("backing");
                    try self.putChild(backing.ty);
                } else {
                    try self.putBytes("no-backing");
                }
            },
            .lambda_set => |members| try self.putMembers(members),
        }
        self.graph.endNode(node, self.hasher.finalResult());
    }
};

fn transparentAliasBacking(content: SolvedType.Content) ?SolvedType.TypeVarId {
    if (std.meta.activeTag(content) != .named or content.named.kind != .alias) return null;
    return (content.named.backing orelse Common.invariant("transparent alias reached procedure identity rendering without a backing type")).ty;
}

const writeBytes = recursive_digest.writeBytes;
const writeU32 = recursive_digest.writeU32;

test "procedure identity folds partial recursive copies across component boundaries" {
    const a = std.testing.allocator;
    var types = SolvedType.Store.init(a);
    defer types.deinit();
    var name_store = names.NameStore.init(a);
    defer name_store.deinit();
    var fn_by_symbol = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(a);
    defer fn_by_symbol.deinit();
    // A = (A, B), B = List(A). A partial copy C = (C, B) denotes
    // exactly A, but C is a separate strongly connected component.
    const original = try types.add(.unbound);
    const list = try types.add(.{ .list = original });
    try types.set(original, .{ .tuple = try types.addSpan(&.{ original, list }) });
    const copy = try types.add(.unbound);
    try types.set(copy, .{ .tuple = try types.addSpan(&.{ copy, list }) });
    const scalar = try types.add(.{ .primitive = .u8 });
    const different_list = try types.add(.{ .list = scalar });
    const different = try types.add(.unbound);
    try types.set(different, .{ .tuple = try types.addSpan(&.{ different, different_list }) });
    for ([_]bool{ false, true }) |copy_first| {
        var memo = Memo.init(a);
        defer memo.deinit();
        const renderer = Renderer{
            .allocator = a,
            .types = types.view(),
            .names = &name_store,
            .fn_tys = &.{},
            .source_digests = &.{},
            .fn_by_symbol = &fn_by_symbol,
            .memo = &memo,
        };
        const first = try renderer.typeDigest(if (copy_first) copy else original);
        const second = try renderer.typeDigest(if (copy_first) original else copy);
        try std.testing.expectEqual(first, second);
        try std.testing.expect(!std.mem.eql(u8, &first, &try renderer.typeDigest(different)));
    }
}

test "procedure identity excludes outer callable sets but retains nested callable sets" {
    const allocator = std.testing.allocator;
    var types = SolvedType.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    var memo = Memo.init(allocator);
    defer memo.deinit();
    var fn_by_symbol = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator);
    defer fn_by_symbol.deinit();
    var symbols = Common.SymbolGen{};
    const first = symbols.fresh();
    const second = symbols.fresh();
    // Renderer function arrays use the dense IDs assigned by this symbol table.
    const first_fn: Lifted.FnId = @fromBackingInt(@intCast(fn_by_symbol.count()));
    try fn_by_symbol.put(first, first_fn);
    const second_fn: Lifted.FnId = @fromBackingInt(@intCast(fn_by_symbol.count()));
    try fn_by_symbol.put(second, second_fn);
    const scalar = try types.add(.{ .primitive = .i64 });
    const args = try types.addSpan(&.{ scalar, scalar });
    const singleton = try types.add(.{ .lambda_set = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
    }) });
    const joined = try types.add(.{ .lambda_set = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
        .{ .lambda = second, .captures = .empty() },
    }) });
    const alone = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = singleton } });
    const beside_lambda = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = joined } });
    const takes_alone = try types.add(.{ .func = .{ .args = try types.addSpan(&.{alone}), .ret = scalar, .callable = singleton } });
    const takes_joined = try types.add(.{ .func = .{ .args = try types.addSpan(&.{beside_lambda}), .ret = scalar, .callable = singleton } });
    const returns_alone = try types.add(.{ .func = .{ .args = args, .ret = alone, .callable = singleton } });
    const returns_joined = try types.add(.{ .func = .{ .args = args, .ret = beside_lambda, .callable = singleton } });
    const renderer = Renderer{
        .allocator = allocator,
        .types = types.view(),
        .names = &name_store,
        .fn_tys = &.{ alone, beside_lambda },
        .source_digests = &.{ @splat(1), @splat(2) },
        .fn_by_symbol = &fn_by_symbol,
        .memo = &memo,
    };
    const identity = try renderer.specIdentity(first_fn, alone, &.{}, "finite", "none");
    try std.testing.expectEqual(identity, try renderer.specIdentity(first_fn, beside_lambda, &.{}, "finite", "none"));
    // Function values still distinguish the members their dispatch can select.
    try std.testing.expect(!std.mem.eql(u8, &try renderer.typeDigest(alone), &try renderer.typeDigest(beside_lambda)));
    for ([_][2]SolvedType.TypeVarId{ .{ takes_alone, takes_joined }, .{ returns_alone, returns_joined } }) |pair| {
        const left = try renderer.specIdentity(first_fn, pair[0], &.{}, "finite", "none");
        const right = try renderer.specIdentity(first_fn, pair[1], &.{}, "finite", "none");
        try std.testing.expect(!std.mem.eql(u8, &left, &right));
    }
    var mono = @import("monotype/ast.zig").ProgramBuilder.init(allocator);
    defer mono.deinit();
    const capture_local = try mono.addLocal(first, try mono.types.add(.{ .primitive = .i64 }));
    var capture = SolvedType.Capture{ .local = capture_local, .symbol = first, .binder = null, .ty = alone };
    const captures_alone = try renderer.specIdentity(first_fn, alone, &.{capture}, "finite", "none");
    capture.ty = beside_lambda;
    const captures_joined = try renderer.specIdentity(first_fn, alone, &.{capture}, "finite", "none");
    try std.testing.expect(!std.mem.eql(u8, &captures_alone, &captures_joined));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(second_fn, alone, &.{}, "finite", "none")));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(first_fn, alone, &.{}, "erased", "none")));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(first_fn, alone, &.{}, "finite", "reuse")));

    // An erased callable's members are whatever closures the program puts
    // in it; a procedure that takes one is the same code whichever they are.
    const erased_digest: names.TypeDigest = .{ .bytes = @splat(9) };
    const erased_first = try types.add(.{ .erased = .{ .source_fn_ty = erased_digest, .members = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
    }), .abi_fn = alone } });
    const erased_both = try types.add(.{ .erased = .{ .source_fn_ty = erased_digest, .members = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
        .{ .lambda = second, .captures = .empty() },
    }), .abi_fn = alone } });
    const boxed_first = try types.add(.{ .box = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = erased_first } }) });
    const boxed_both = try types.add(.{ .box = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = erased_both } }) });
    const takes_boxed_first = try types.add(.{ .func = .{ .args = try types.addSpan(&.{boxed_first}), .ret = scalar, .callable = singleton } });
    const takes_boxed_both = try types.add(.{ .func = .{ .args = try types.addSpan(&.{boxed_both}), .ret = scalar, .callable = singleton } });
    const other_erased = try types.add(.{ .erased = .{ .source_fn_ty = .{ .bytes = @splat(8) }, .abi_fn = alone } });
    const boxed_other = try types.add(.{ .box = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = other_erased } }) });
    const takes_boxed_other = try types.add(.{ .func = .{ .args = try types.addSpan(&.{boxed_other}), .ret = scalar, .callable = singleton } });
    const erased_renderer = Renderer{
        .allocator = allocator,
        .types = types.view(),
        .names = &name_store,
        .fn_tys = &.{ alone, beside_lambda },
        .source_digests = &.{ @splat(1), @splat(2) },
        .fn_by_symbol = &fn_by_symbol,
        .memo = &memo,
    };
    try std.testing.expectEqual(
        try erased_renderer.specIdentity(first_fn, takes_boxed_first, &.{}, "finite", "none"),
        try erased_renderer.specIdentity(first_fn, takes_boxed_both, &.{}, "finite", "none"),
    );
    // The erased function type is still part of the identity.
    try std.testing.expect(!std.mem.eql(
        u8,
        &try erased_renderer.specIdentity(first_fn, takes_boxed_first, &.{}, "finite", "none"),
        &try erased_renderer.specIdentity(first_fn, takes_boxed_other, &.{}, "finite", "none"),
    ));
}

test "procedure identity digests a knot entangled with a separately tied copy as that copy" {
    // `copy` and `copy_list` tie a knot of List(Node) whose second reference
    // leads into `node`, a separately tied knot of the same infinite type, as
    // one program's lambda solving can draw it. Another program draws only
    // `node`. Both must render one identity, whichever is rendered first.
    const allocator = std.testing.allocator;
    var types = SolvedType.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    var fn_by_symbol = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator);
    defer fn_by_symbol.deinit();

    const node = try types.add(.zst);
    const node_list = try types.add(.{ .list = node });
    try types.set(node, .{ .tuple = try types.addSpan(&.{ node_list, node_list }) });
    const copy = try types.add(.zst);
    const copy_list = try types.add(.{ .list = copy });
    try types.set(copy, .{ .tuple = try types.addSpan(&.{ copy_list, node_list }) });

    var digests: [2][2]Identity = undefined;
    for (&digests, [_][2]SolvedType.TypeVarId{ .{ node, copy }, .{ copy, node } }) |*out, order| {
        var memo = Memo.init(allocator);
        defer memo.deinit();
        const renderer = Renderer{
            .allocator = allocator,
            .types = types.view(),
            .names = &name_store,
            .fn_tys = &.{},
            .source_digests = &.{},
            .fn_by_symbol = &fn_by_symbol,
            .memo = &memo,
        };
        out[0] = try renderer.typeDigest(order[0]);
        out[1] = try renderer.typeDigest(order[1]);
    }
    try std.testing.expectEqual(digests[0][0], digests[0][1]);
    try std.testing.expectEqual(digests[1][0], digests[1][1]);
    try std.testing.expectEqual(digests[0][0], digests[1][0]);

    var alone_types = SolvedType.Store.init(allocator);
    defer alone_types.deinit();
    const alone = try alone_types.add(.zst);
    const alone_list = try alone_types.add(.{ .list = alone });
    try alone_types.set(alone, .{ .tuple = try alone_types.addSpan(&.{ alone_list, alone_list }) });
    var memo = Memo.init(allocator);
    defer memo.deinit();
    const renderer = Renderer{
        .allocator = allocator,
        .types = alone_types.view(),
        .names = &name_store,
        .fn_tys = &.{},
        .source_digests = &.{},
        .fn_by_symbol = &fn_by_symbol,
        .memo = &memo,
    };
    try std.testing.expectEqual(digests[0][0], try renderer.typeDigest(alone));
}
