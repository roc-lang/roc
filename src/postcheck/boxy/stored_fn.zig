//! Stored function values of `.boxy` compile-time evaluation (design.md
//! "Stored Function Values").
//!
//! A function value under `.boxy` is an erased callable: a worker's code and
//! what the worker captured, which is values, the descriptors of its scope's
//! type variables, and dictionaries, themselves erased callables. A stored
//! function records exactly that: a worker recipe, value captures as constant
//! store nodes, and an immutable graph of descriptors with their override
//! scopes, callable captures, and table inputs. Restoring it reserves the graph
//! before connecting its edges. No runtime constructor is emitted.
//!
//! The recipe is bytes the constant store keeps opaque but for its first
//! table: the modules it names, by checked key, each once, which the store
//! reads to name a stored function in an identity by its modules' link names
//! and to move it to the modules' current versions
//! (`ConstStore.recipeModuleKeys`). Then come a table of the types the recipe
//! names, generic where the worker's own types are, then the worker, then
//! the capture graph and its table-input and capture-descriptor roots. Every
//! module reference is an index into the first table.

const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");
const boxy_runtime = @import("boxy_runtime");

const types = @import("types.zig");
const Tables = @import("tables.zig");
const lower = @import("lower.zig");
const const_schema = @import("const_schema.zig");
const Codec = @import("codec.zig");
const body = @import("body.zig");
const StaticDescs = @import("static_descs.zig").StaticDescs;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const ExprId = check.CheckedBodies.ExprId;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const TypeDesc = boxy_runtime.desc.TypeDesc;
const TypeId = types.TypeId;
const Program = lower.Program;
const WorkerId = lower.WorkerId;
const invariant = types.invariant;

/// What a worker's erased callable captures: `values`, then its type
/// table, whose inputs are `descs` descriptors, then `dicts` erased
/// callables.
pub const CaptureShape = struct {
    values: []const TypeId,
    descs: u32,
    dicts: u32,
    /// The callable is an immortal dictionary (`Builder.composeDictionary`).
    composed: bool,
};

/// The capture shape of `id`, which is a worker some body packs as a
/// function value.
pub fn captureShape(program: *Program, id: WorkerId) CaptureShape {
    const worker = program.worker(id).*;
    return switch (worker.key) {
        .lambda => .{
            .values = worker.capture_types,
            .descs = @intCast(worker.scheme_vars.len - worker.own_descs),
            .dicts = worker.capturedDictionaryCount() + worker.capturedLiteralCount(),
            .composed = false,
        },
        .adapter => .{ .values = worker.capture_types, .descs = @intCast(worker.scheme_vars.len), .dicts = 0, .composed = false },
        .method_entry => |target| blk: {
            const target_worker = program.worker(target).*;
            break :blk .{
                .values = &.{},
                .descs = @intCast(target_worker.scheme_vars.len),
                .dicts = @intCast(target_worker.dictionary_params.len + target_worker.literal_params),
                .composed = true,
            };
        },
        .structural, .structural_map => .{ .values = &.{}, .descs = worker.structural_descs, .dicts = 0, .composed = true },
        .crash_dictionary, .literal_thunk, .root_slot, .callable_eval_entry => .{ .values = &.{}, .descs = 0, .dicts = 0, .composed = true },
        .codec => |index| .{
            .values = worker.capture_types,
            .descs = @intCast(worker.scheme_vars.len),
            .dicts = 0,
            .composed = program.codec.pieces.items[index].kind == .constructor,
        },
        .template, .default, .root, .static_data, .convert => invariant("a worker that is no function value reached a stored function"),
    };
}

/// One captured value of a stored function: read it at `ptr`, by `plan`,
/// at `layout_idx`.
pub const Capture = struct {
    plan: LirProgram.ConstPlanId,
    layout_idx: layout.Idx,
    ptr: [*]u8,
};

/// A function value read from a compile-time program.
pub const Described = struct {
    /// Owned by the `allocator` `describe` was given.
    recipe: []const u8,
    /// Its value captures. Immutable table captures are explicit recipe edges.
    /// Owned by the same allocator.
    captures: []const Capture,
};

/// A completed table's capture graph. These are producer records, not requests
/// to resolve methods from types. References may point forward or form cycles.
const CaptureNode = union(enum(u8)) {
    desc: struct { ty: TypeId, scoped: []const CaptureOverride },
    callable: struct { worker: CaptureWorker, captures: []const u32 },
    table: struct { worker: CaptureWorker, inputs: []const u32 },
    zero,
};
const CaptureOverride = struct { nominal: TypeId, methods: [3]?u32 };
const CaptureWorker = union(enum) {
    local: WorkerId,
    encoded: struct { bytes: []const u8, modules: []const [32]u8, types: []const TypeId, link_names: bool },

    fn reader(encoded: @FieldType(CaptureWorker, "encoded"), program: *Program) Decoder {
        return .{ .program = program, .bytes = encoded.bytes, .modules = encoded.modules, .table = @constCast(encoded.types), .link_names = encoded.link_names };
    }

    fn resolve(self: CaptureWorker, program: *Program) Allocator.Error!WorkerId {
        return switch (self) {
            .local => |id| id,
            .encoded => |encoded| blk: {
                var input = reader(encoded, program);
                break :blk try input.worker();
            },
        };
    }
};

const CaptureGraphBuilder = struct {
    retained: *Retained,
    /// Serialization-local storage; none of these edges escape the encoder.
    scratch: Allocator,
    nodes: std.ArrayList(CaptureNode) = .empty,
    words: std.HashMapUnmanaged(StaticDescs.CaptureWord, u32, StaticDescs.WordContext, std.hash_map.default_max_load_percentage) = .empty,

    fn deinit(self: *CaptureGraphBuilder) void {
        self.words.deinit(self.retained.allocator);
        self.nodes.deinit(self.retained.allocator);
    }

    fn word(self: *CaptureGraphBuilder, value: StaticDescs.CaptureWord) Allocator.Error!u32 {
        const allocator = self.retained.allocator;
        const scratch = self.scratch;
        const entry = try self.words.getOrPut(allocator, value);
        if (entry.found_existing) return entry.value_ptr.*;
        const index: u32 = @intCast(self.nodes.items.len);
        entry.value_ptr.* = index;
        try self.nodes.append(allocator, undefined);
        const producer = &self.retained.program.static_descs;
        const node: CaptureNode = switch (value) {
            .desc => |ty| .{ .desc = .{ .ty = ty, .scoped = &.{} } },
            .scoped_desc => |scoped| blk: {
                const entries = producer.scopeEntries(scoped.scope);
                const overrides = try scratch.alloc(CaptureOverride, entries.len);
                for (entries, overrides) |source, *override| {
                    override.* = .{ .nominal = source.nominal, .methods = undefined };
                    for (source.methods, &override.methods) |method, *out| out.* = if (method) |capture| try self.word(capture) else null;
                }
                break :blk .{ .desc = .{ .ty = scoped.ty, .scoped = overrides } };
            },
            .callable => |id| blk: {
                const callable = producer.captured_callables.get(id) orelse invariant("an immutable capture has no callable producer record");
                const worker = self.retained.workers.get(callable.identity) orelse invariant("an immutable capture has no worker producer record");
                break :blk .{ .callable = .{ .worker = .{ .local = worker }, .captures = try self.span(callable.words) } };
            },
            .table => |id| blk: {
                const ordinal = producer.captured_tables.get(id) orelse invariant("an immutable capture has no table producer record");
                const table = producer.unfilled_tables.items[ordinal];
                break :blk .{ .table = .{ .worker = .{ .local = table.worker }, .inputs = try self.span(table.inputs) } };
            },
            .zero => .zero,
            .node, .int => invariant("a function table input captures neither a descriptor nor a callable"),
        };
        self.nodes.items[index] = node;
        return index;
    }

    fn span(self: *CaptureGraphBuilder, values: []const StaticDescs.CaptureWord) Allocator.Error![]const u32 {
        const refs = try self.scratch.alloc(u32, values.len);
        for (refs, values) |*ref, value| ref.* = try self.word(value);
        return refs;
    }
};

fn encodeCaptureRefs(encoder: *Encoder, refs: []const u32) Allocator.Error!void {
    try encoder.word(@intCast(refs.len));
    for (refs) |ref| try encoder.word(ref);
}

fn encodeCaptureWorker(encoder: *Encoder, worker: CaptureWorker) Allocator.Error!void {
    const size_at = encoder.bytes.items.len;
    try encoder.word(0);
    const start = encoder.bytes.items.len;
    switch (worker) {
        .local => |id| try encoder.worker(id),
        .encoded => |encoded| {
            var input = CaptureWorker.reader(encoded, encoder.program);
            try encoder.importWorker(&input);
        },
    }
    std.mem.writeInt(u32, encoder.bytes.items[size_at..][0..4], @intCast(encoder.bytes.items.len - start), .little);
}

fn encodeCaptureGraph(encoder: *Encoder, graph: []const CaptureNode) Allocator.Error!void {
    try encoder.word(@intCast(graph.len));
    for (graph) |node| {
        try encoder.bytes.append(encoder.allocator, @backingInt(std.meta.activeTag(node)));
        switch (node) {
            .desc => |desc| {
                try encoder.typeOf(desc.ty);
                try encoder.word(@intCast(desc.scoped.len));
                for (desc.scoped) |override| {
                    try encoder.typeOf(override.nominal);
                    for (override.methods) |method| {
                        try encoder.bytes.append(encoder.allocator, @intFromBool(method != null));
                        if (method) |ref| try encoder.word(ref);
                    }
                }
            },
            .callable => |callable| {
                try encodeCaptureWorker(encoder, callable.worker);
                try encodeCaptureRefs(encoder, callable.captures);
            },
            .table => |table| {
                try encodeCaptureWorker(encoder, table.worker);
                try encodeCaptureRefs(encoder, table.inputs);
            },
            .zero => {},
        }
    }
}

fn decodeCaptureRefs(input: *Decoder) Allocator.Error![]const u32 {
    const refs = try input.program.arena.allocator().alloc(u32, input.word());
    for (refs) |*ref| ref.* = input.word();
    return refs;
}

fn decodeCaptureWorker(input: *Decoder, type_table: []const TypeId) CaptureWorker {
    const len = input.word();
    return .{ .encoded = .{ .bytes = input.take(len), .modules = input.modules, .types = type_table, .link_names = input.link_names } };
}

fn decodeCaptureGraph(input: *Decoder) Allocator.Error![]const CaptureNode {
    const arena = input.program.arena.allocator();
    const type_table = try arena.dupe(TypeId, input.table);
    const nodes = try arena.alloc(CaptureNode, input.word());
    for (nodes) |*node| {
        const tag: std.meta.Tag(CaptureNode) = @fromBackingInt(input.byte());
        node.* = switch (tag) {
            .desc => blk: {
                const ty = input.typeOf();
                const scoped = try arena.alloc(CaptureOverride, input.word());
                for (scoped) |*override| {
                    override.* = .{ .nominal = input.typeOf(), .methods = undefined };
                    for (&override.methods) |*method| method.* = if (input.byte() != 0) input.word() else null;
                }
                break :blk .{ .desc = .{ .ty = ty, .scoped = scoped } };
            },
            .callable => blk: {
                const worker = decodeCaptureWorker(input, type_table);
                break :blk .{ .callable = .{ .worker = worker, .captures = try decodeCaptureRefs(input) } };
            },
            .table => blk: {
                const worker = decodeCaptureWorker(input, type_table);
                break :blk .{ .table = .{ .worker = worker, .inputs = try decodeCaptureRefs(input) } };
            },
            .zero => .zero,
        };
    }
    return nodes;
}

fn captureType(graph: []const CaptureNode, index: u32) TypeId {
    return switch (graph[index]) {
        .desc => |desc| desc.ty,
        .callable, .table, .zero => invariant("a table descriptor input has no type"),
    };
}

/// Keep only the completed value's reachable capture graph, in root order.
/// Unrelated tables of a cached producer are not part of the stored value.
const CaptureSelection = struct {
    allocator: Allocator,
    scratch: Allocator,
    source: []const CaptureNode,
    domain: [32]u8 = @splat(0),
    nodes: std.ArrayList(CaptureNode) = .empty,
    indices: std.AutoHashMapUnmanaged(struct { domain: [32]u8, index: u32 }, u32) = .empty,

    fn deinit(self: *CaptureSelection) void {
        self.nodes.deinit(self.allocator);
        self.indices.deinit(self.allocator);
    }

    fn ref(self: *CaptureSelection, old: u32) Allocator.Error!u32 {
        const entry = try self.indices.getOrPut(self.allocator, .{ .domain = self.domain, .index = old });
        if (entry.found_existing) return entry.value_ptr.*;
        const index: u32 = @intCast(self.nodes.items.len);
        entry.value_ptr.* = index;
        try self.nodes.append(self.allocator, undefined);
        const node: CaptureNode = switch (self.source[old]) {
            .desc => |desc| blk: {
                const scoped = try self.scratch.dupe(CaptureOverride, desc.scoped);
                for (scoped) |*override| for (&override.methods) |*method| {
                    if (method.*) |id| method.* = try self.ref(id);
                };
                break :blk .{ .desc = .{ .ty = desc.ty, .scoped = scoped } };
            },
            .callable => |callable| .{ .callable = .{ .worker = callable.worker, .captures = try self.span(callable.captures) } },
            .table => |table| .{ .table = .{ .worker = table.worker, .inputs = try self.span(table.inputs) } },
            .zero => .zero,
        };
        self.nodes.items[index] = node;
        return index;
    }

    fn span(self: *CaptureSelection, refs: []const u32) Allocator.Error![]const u32 {
        const out = try self.scratch.alloc(u32, refs.len);
        for (refs, out) |old, *new| new.* = try self.ref(old);
        return out;
    }
};

/// A lowered compile-time program's types and workers, kept for reading the
/// function values it evaluates to. `lir_result` of each call is where the
/// program's result lives after lowering moved it out of the `Program`.
pub const Retained = struct {
    allocator: Allocator,
    program: *Program,
    workers: std.AutoHashMapUnmanaged(LIR.ProcIdentity, WorkerId) = .empty,
    metadata: ?MetadataSource = null,
    imported: std.AutoHashMapUnmanaged([32]u8, *ImportedMetadata) = .empty,

    /// Free the retained program and everything read from it.
    pub fn deinit(self: *Retained) void {
        self.workers.deinit(self.allocator);
        self.imported.deinit(self.allocator);
        self.program.release();
        self.allocator.destroy(self.program);
        self.allocator.destroy(self);
    }

    /// Read the function value of the proc `proc` whose capture record is at
    /// `capture`.
    pub fn describe(
        self: *Retained,
        allocator: Allocator,
        result: *LirProgram.Result,
        proc: LIR.LirProcSpecId,
        capture: [*]u8,
    ) Allocator.Error!Described {
        const program = self.program;
        program.result = result.*;
        defer result.* = program.result;
        const worker_id = self.workers.get(result.store.getProcSpec(proc).identity) orelse
            invariant("a stored function value's code is no worker of its program");
        return try self.describeCapture(allocator, captureShape(program, worker_id), .{ .local = worker_id }, capture);
    }

    pub fn describeImported(self: *Retained, allocator: Allocator, result: *LirProgram.Result, owner: [32]u8, identity: LIR.ProcIdentity, capture: [*]u8) Allocator.Error!Described {
        self.program.result = result.*;
        defer result.* = self.program.result;
        const imported = try self.importMetadata(owner);
        const function = imported.functions.get(identity) orelse invariant("a cached function's metadata has no such worker");
        return try self.describeCapture(allocator, function.shape, .{ .imported = .{ .metadata = imported, .function = function } }, capture);
    }

    fn importMetadata(self: *Retained, owner: [32]u8) Allocator.Error!*ImportedMetadata {
        if (self.imported.get(owner)) |loaded| return loaded;
        const source = self.metadata orelse invariant("a cached function has no metadata source");
        const loaded = try ImportedMetadata.load(self.program, source.get(source.context, owner));
        try self.imported.put(self.allocator, owner, loaded);
        return loaded;
    }

    const Recipe = union(enum) {
        local: WorkerId,
        imported: struct { metadata: *ImportedMetadata, function: ImportedFunction },
    };

    fn describeCapture(self: *Retained, allocator: Allocator, shape: CaptureShape, source: Recipe, capture: [*]u8) Allocator.Error!Described {
        const program = self.program;
        const open = !try allClosed(program, shape.values);
        const has_table = shape.descs + shape.dicts != 0;
        const word_count: u32 = @intFromBool(has_table);

        // Where each capture is: a closed record's order is values, then
        // the type table; a dynamic record's is its own descriptor and
        // capacity, the type table, then values.
        const offsets = try allocator.alloc(u32, shape.values.len + word_count);
        defer allocator.free(offsets);
        if (open) {
            const leading = body.Builder.capture_record_leading_words;
            const record_desc: *const TypeDesc = @ptrCast(@alignCast(readWord(capture)));
            for (0..word_count) |index| offsets[shape.values.len + index] = record_desc.fieldOffset(@intCast(leading + index));
            for (0..shape.values.len) |index| offsets[index] = record_desc.fieldOffset(@intCast(leading + word_count + index));
        } else if (offsets.len != 0) {
            const record = try closedRecordLayout(program, shape.values, word_count);
            if (record != .zst) {
                const layouts = &program.result.layouts;
                const struct_idx = layouts.getLayout(record).getStruct().idx;
                for (offsets, 0..) |*offset, index| offset.* = layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(index));
            } else {
                @memset(offsets, 0);
            }
        }

        // The table's producer supplies every immutable input and capture
        // edge. Its scope is data, including override callables, not a type
        // from which the receiving body reads the recorded overrides.
        const table: ?[*]u8 = if (has_table) @ptrCast(@constCast(readWord(capture + offsets[shape.values.len]))) else null;
        const bound = try allocator.alloc(TypeId, shape.descs);
        defer allocator.free(bound);
        // Producer and selected edge arrays are consumed by recipe.finish;
        // only the independently allocated recipe and value captures escape.
        var scratch = std.heap.ArenaAllocator.init(allocator);
        defer scratch.deinit();
        var local_graph = CaptureGraphBuilder{ .retained = self, .scratch = scratch.allocator() };
        defer local_graph.deinit();
        var graph: []const CaptureNode = &.{};
        var roots: []const u32 = &.{};
        var domain: [32]u8 = @splat(0);
        if (table) |address| {
            const table_words = switch (source) {
                .local => |id| (program.worker(id).needs orelse invariant("a captured table has no declared needs")).wordCount(),
                .imported => |foreign| foreign.function.table_words,
            };
            const offset = LIR.tableEntryOffset(table_words, program.static_descs.width.bytes());
            const origin = address[offset..][0..32];
            domain = origin.*;
            const index = std.mem.readInt(u32, address[offset + 32 ..][0..4], @import("builtin").cpu.arch.endian());
            if (std.mem.allEqual(u8, origin, 0)) {
                const tables = program.static_descs.unfilled_tables.items;
                if (index >= tables.len) invariant("a captured table has no producer record");
                const inputs = tables[index].inputs;
                if (inputs.len != shape.descs + shape.dicts) invariant("a captured table has the wrong input arity");
                roots = try local_graph.span(inputs);
                graph = local_graph.nodes.items;
            } else {
                const metadata = try self.importMetadata(origin.*);
                if (index >= metadata.tables.len or metadata.tables[index].len != shape.descs + shape.dicts) invariant("a cached table has no matching producer inputs");
                roots = metadata.tables[index];
                graph = metadata.graph;
            }
            for (bound, roots[0..shape.descs]) |*ty, ref| ty.* = captureType(graph, ref);
        }

        var recipe = Encoder{ .allocator = allocator, .program = program };
        defer recipe.deinit();
        switch (source) {
            .local => |worker_id| try recipe.worker(worker_id),
            .imported => |foreign| {
                var reader = foreign.metadata.reader(program, foreign.function.worker);
                try recipe.importWorker(&reader);
            },
        }
        var selected = CaptureSelection{ .allocator = allocator, .scratch = scratch.allocator(), .source = graph, .domain = domain };
        defer selected.deinit();
        const inputs = try selected.span(roots);
        var capture_descs: []const u32 = &.{};
        if (open) {
            const address: [*]const u8 = @ptrCast(readWord(capture));
            const offset = TypeDesc.layout(program.static_descs.width).size;
            const origin = address[offset..][0..32];
            const index = std.mem.readInt(u32, address[offset + 32 ..][0..4], @import("builtin").cpu.arch.endian());
            selected.domain = origin.*;
            const record: ImportedRecord = if (std.mem.allEqual(u8, origin, 0)) blk: {
                const records = program.static_descs.captured_records.items;
                if (index >= records.len) invariant("a capture record descriptor has no producer record");
                const refs = try local_graph.span(records[index].elems);
                selected.source = local_graph.nodes.items;
                break :blk .{ .leading = records[index].leading, .elems = refs };
            } else blk: {
                const metadata = try self.importMetadata(origin.*);
                if (index >= metadata.records.len) invariant("a cached capture record descriptor has no producer record");
                selected.source = metadata.graph;
                break :blk metadata.records[index];
            };
            if (record.leading != body.Builder.capture_record_leading_words + word_count or record.elems.len != shape.values.len) invariant("a captured record has the wrong producer shape");
            capture_descs = try selected.span(record.elems);
        }
        try encodeCaptureGraph(&recipe, selected.nodes.items);
        try encodeCaptureRefs(&recipe, inputs);
        try encodeCaptureRefs(&recipe, capture_descs);

        const captures = try allocator.alloc(Capture, shape.values.len);
        errdefer allocator.free(captures);
        for (shape.values, captures[0..shape.values.len], offsets[0..shape.values.len]) |generic, *slot, offset| {
            const concrete = try program.substitute(allocator, generic, bound);
            slot.* = .{
                .plan = try const_schema.constPlanOf(program, concrete),
                .layout_idx = try program.layouts.layoutOf(concrete),
                .ptr = capture + offset,
            };
        }
        return .{ .recipe = try recipe.finish(), .captures = captures };
    }
};

/// A callable and its exact symbol index in the producer's native image.
pub const CompiledFunction = struct { identity: LIR.ProcIdentity, symbol: u32 };

/// Validated view of one compiler-only unit's portable producer records.
/// Validation performs no allocation and follows only the encoded counts.
pub const MetadataView = struct {
    bytes: []const u8,
    functions_at: usize,
    function_count: u32,

    pub fn init(bytes: []const u8) error{InvalidMetadata}!MetadataView {
        var input = MetadataReader{ .bytes = bytes };
        input.modules = try input.word();
        _ = try input.takeProduct(input.modules, 32);
        const count = try input.word();
        if (count > input.remaining()) return error.InvalidMetadata;
        for (0..count) |index| {
            input.types = @intCast(index);
            try input.typeEntry();
        }
        input.types = count;
        const graph_count = try input.captureGraph();
        const tables = try input.word();
        if (tables > input.remaining() / 4) return error.InvalidMetadata;
        for (0..tables) |_| try input.captureRefs(graph_count);
        const records = try input.word();
        if (records > input.remaining() / 8) return error.InvalidMetadata;
        for (0..records) |_| {
            _ = try input.word(); // leading opaque words
            try input.captureRefs(graph_count);
        }
        const function_count = try input.word();
        if (function_count > input.remaining() / 57) return error.InvalidMetadata;
        const at = input.at;
        for (0..function_count) |_| {
            _ = try input.take(32);
            _ = try input.word(); // native image symbol
            try input.typeSpan();
            _ = try input.word(); // descriptor count
            _ = try input.word(); // dictionary count
            _ = try input.word(); // table words before its producer identity
            try input.boolean();
            const len = try input.word();
            var worker = MetadataReader{ .bytes = try input.take(len), .modules = input.modules, .types = input.types };
            try worker.worker();
            if (worker.remaining() != 0) return error.InvalidMetadata;
        }
        if (input.remaining() != 0) return error.InvalidMetadata;
        return .{ .bytes = bytes, .functions_at = at, .function_count = function_count };
    }

    pub fn functions(self: MetadataView) FunctionIterator {
        return .{ .input = .{ .bytes = self.bytes, .at = self.functions_at }, .left = self.function_count };
    }

    pub const FunctionIterator = struct {
        input: MetadataReader,
        left: u32,
        pub fn next(self: *FunctionIterator) ?CompiledFunction {
            if (self.left == 0) return null;
            self.left -= 1;
            const identity_bytes = self.input.take(32) catch unreachable; // MetadataView.init validated every function identity
            const identity: LIR.ProcIdentity = .{ .bytes = identity_bytes[0..32].* };
            const symbol = self.input.word() catch unreachable; // MetadataView.init validated every function symbol word
            const values = self.input.word() catch unreachable; // MetadataView.init validated every capture-type count
            _ = self.input.takeProduct(values, 4) catch unreachable; // MetadataView.init validated the complete capture-type span
            _ = self.input.take(13) catch unreachable; // MetadataView.init validated the three counts and composed flag
            const len = self.input.word() catch unreachable; // MetadataView.init validated every worker recipe length
            _ = self.input.take(len) catch unreachable; // MetadataView.init validated the complete worker recipe
            return .{ .identity = identity, .symbol = symbol };
        }
    };
};

const MetadataReader = struct {
    bytes: []const u8,
    at: usize = 0,
    modules: u32 = 0,
    types: u32 = 0,

    fn remaining(self: MetadataReader) usize {
        return self.bytes.len - self.at;
    }
    fn take(self: *MetadataReader, len: usize) error{InvalidMetadata}![]const u8 {
        if (len > self.remaining()) return error.InvalidMetadata;
        const bytes = self.bytes[self.at..][0..len];
        self.at += len;
        return bytes;
    }
    fn takeProduct(self: *MetadataReader, count: u32, size: usize) error{InvalidMetadata}![]const u8 {
        if (count > self.remaining() / size) return error.InvalidMetadata;
        return try self.take(@as(usize, count) * size);
    }
    fn word(self: *MetadataReader) error{InvalidMetadata}!u32 {
        return std.mem.readInt(u32, (try self.take(4))[0..4], .little);
    }
    fn byte(self: *MetadataReader) error{InvalidMetadata}!u8 {
        return (try self.take(1))[0];
    }
    fn boolean(self: *MetadataReader) error{InvalidMetadata}!void {
        if (try self.byte() > 1) return error.InvalidMetadata;
    }
    fn tag(self: *MetadataReader, comptime T: type) error{InvalidMetadata}!T {
        const value = try self.byte();
        inline for (@typeInfo(T).@"enum".field_values) |field| if (value == field) return @fromBackingInt(field);
        return error.InvalidMetadata;
    }
    fn name(self: *MetadataReader) error{InvalidMetadata}!void {
        const len = try self.word();
        _ = try self.take(len);
    }
    fn module(self: *MetadataReader) error{InvalidMetadata}!void {
        if (try self.word() >= self.modules) return error.InvalidMetadata;
    }
    fn typeRef(self: *MetadataReader) error{InvalidMetadata}!void {
        if (try self.word() >= self.types) return error.InvalidMetadata;
    }
    fn typeSpan(self: *MetadataReader) error{InvalidMetadata}!void {
        const count = try self.word();
        if (count > self.remaining() / 4) return error.InvalidMetadata;
        for (0..count) |_| try self.typeRef();
    }
    fn captureRef(self: *MetadataReader, count: u32) error{InvalidMetadata}!void {
        if (try self.word() >= count) return error.InvalidMetadata;
    }
    fn captureRefs(self: *MetadataReader, nodes: u32) error{InvalidMetadata}!void {
        const count = try self.word();
        if (count > self.remaining() / 4) return error.InvalidMetadata;
        for (0..count) |_| try self.captureRef(nodes);
    }
    fn captureGraph(self: *MetadataReader) error{InvalidMetadata}!u32 {
        const count = try self.word();
        if (count > self.remaining()) return error.InvalidMetadata;
        for (0..count) |_| switch (try self.tag(std.meta.Tag(CaptureNode))) {
            .desc => {
                try self.typeRef();
                const scoped = try self.word();
                if (scoped > self.remaining() / 7) return error.InvalidMetadata;
                for (0..scoped) |_| {
                    try self.typeRef();
                    for (0..3) |_| {
                        const present = try self.byte();
                        if (present > 1) return error.InvalidMetadata;
                        if (present != 0) try self.captureRef(count);
                    }
                }
            },
            .callable, .table => {
                const len = try self.word();
                var worker_input = MetadataReader{ .bytes = try self.take(len), .modules = self.modules, .types = self.types };
                try worker_input.worker();
                if (worker_input.remaining() != 0) return error.InvalidMetadata;
                try self.captureRefs(count);
            },
            .zero => {},
        };
        return count;
    }
    fn ext(self: *MetadataReader) error{InvalidMetadata}!void {
        const id = try self.word();
        if (id != no_type and id >= self.types) return error.InvalidMetadata;
    }
    fn typeEntry(self: *MetadataReader) error{InvalidMetadata}!void {
        switch (try self.tag(std.meta.Tag(types.Node))) {
            .primitive => {
                _ = try self.tag(types.Primitive);
            },
            .empty_record => {},
            .record => {
                const fields = try self.word();
                if (fields > self.remaining() / 14) return error.InvalidMetadata;
                for (0..fields) |_| {
                    try self.name();
                    try self.typeRef();
                    _ = try self.tag(types.FieldKind);
                    _ = try self.word();
                    const has_default = try self.byte();
                    if (has_default > 1) return error.InvalidMetadata;
                    if (has_default == 1) {
                        try self.module();
                        _ = try self.word();
                    }
                }
                try self.ext();
                const declared = try self.byte();
                if (declared > 1) return error.InvalidMetadata;
                if (declared == 1) {
                    try self.typeSpan();
                    const entries = try self.word();
                    if (entries > self.remaining() / 5) return error.InvalidMetadata;
                    for (0..entries) |_| switch (try self.tag(std.meta.Tag(types.DeclaredEntry))) {
                        .named => try self.name(),
                        .padding => _ = try self.word(),
                    };
                }
            },
            .tuple => try self.typeSpan(),
            .tag_union => {
                const tags = try self.word();
                if (tags > self.remaining() / 8) return error.InvalidMetadata;
                for (0..tags) |_| {
                    try self.name();
                    try self.typeSpan();
                }
                try self.ext();
            },
            .list, .box => try self.typeRef(),
            .nominal => {
                try self.module();
                _ = try self.word();
                try self.typeSpan();
                try self.name();
                try self.boolean();
                const builtin = try self.byte();
                if (builtin != 0) {
                    var valid = false;
                    inline for (@typeInfo(checked.CheckedBuiltinNominal).@"enum".field_values) |field| if (builtin - 1 == field) {
                        valid = true;
                    };
                    if (!valid) return error.InvalidMetadata;
                }
                try self.boolean();
            },
            .function => {
                try self.typeSpan();
                try self.typeRef();
                try self.boolean();
            },
            .variable => {
                _ = try self.word();
            },
        }
    }
    fn worker(self: *MetadataReader) error{InvalidMetadata}!void {
        while (true) switch (try self.tag(Tag)) {
            .template, .default => {
                try self.module();
                _ = try self.word();
                return;
            },
            .template_list_int => {
                try self.module();
                _ = try self.word();
                _ = try self.tag(lower.ListIntItem);
                return;
            },
            .lambda => {
                _ = try self.word();
                if (try self.word() > 1) return error.InvalidMetadata;
            },
            .adapter => {
                try self.typeRef();
                try self.typeRef();
            },
            .method_entry => {},
            .literal_thunk => {
                try self.typeRef();
                _ = try self.word();
            },
            .root_slot => {
                try self.module();
                _ = try self.word();
                try self.typeRef();
                return;
            },
            .callable_eval_entry => {
                try self.module();
                try self.module();
                _ = try self.word();
                return;
            },
            .structural => {
                try self.module();
                _ = try self.tag(@FieldType(lower.WorkerKey, "structural"));
                return;
            },
            .crash_dictionary => {
                try self.module();
                _ = try self.tag(@FieldType(lower.WorkerKey, "crash_dictionary"));
                return;
            },
            .structural_map => {
                try self.module();
                try self.name();
                _ = try self.word();
                _ = try self.word();
                if (try self.word() > 1) return error.InvalidMetadata;
                return;
            },
            .codec => {
                try self.module();
                const kind = try self.tag(Codec.PieceKind);
                _ = try self.word();
                switch (Codec.pieceOperands(kind)) {
                    .none => {},
                    .types => {
                        try self.typeRef();
                        try self.typeRef();
                    },
                    .type_and_name => {
                        try self.typeRef();
                        try self.name();
                    },
                    .type_and_word => {
                        try self.typeRef();
                        _ = try self.word();
                    },
                }
                try self.typeSpan();
                try self.typeRef();
                try self.typeSpan();
                return;
            },
        };
    }
};

/// The unit owning `owner` supplies validated, immutable metadata bytes.
pub const MetadataSource = struct {
    context: *anyopaque,
    get: *const fn (*anyopaque, [32]u8) []const u8,
};

const ImportedFunction = struct {
    shape: CaptureShape,
    worker: []const u8,
    table_words: u32,
};

const ImportedRecord = struct { leading: u32, elems: []const u32 };

const ImportedMetadata = struct {
    modules: []const [32]u8,
    types: []const TypeId,
    graph: []const CaptureNode = &.{},
    tables: []const []const u32 = &.{},
    records: []const ImportedRecord = &.{},
    functions: std.AutoHashMapUnmanaged(LIR.ProcIdentity, ImportedFunction) = .empty,

    fn reader(self: *const ImportedMetadata, program: *Program, bytes: []const u8) Decoder {
        return .{ .program = program, .link_names = true, .bytes = bytes, .modules = self.modules, .table = @constCast(self.types) };
    }

    fn load(program: *Program, bytes: []const u8) Allocator.Error!*ImportedMetadata {
        const arena = program.arena.allocator();
        var input = Decoder{ .program = program, .link_names = true, .bytes = bytes };
        input.readModules();
        try input.readTable();
        defer program.allocator.free(input.table);
        const result = try arena.create(ImportedMetadata);
        result.* = .{ .modules = input.modules, .types = try arena.dupe(TypeId, input.table) };
        result.graph = try decodeCaptureGraph(&input);
        const table_count = input.word();
        const tables = try arena.alloc([]const u32, table_count);
        for (tables) |*table| table.* = try decodeCaptureRefs(&input);
        result.tables = tables;
        const records = try arena.alloc(ImportedRecord, input.word());
        for (records) |*record| {
            const leading = input.word();
            record.* = .{ .leading = leading, .elems = try decodeCaptureRefs(&input) };
        }
        result.records = records;
        const count = input.word();
        for (0..count) |_| {
            const identity: LIR.ProcIdentity = .{ .bytes = input.take(32)[0..32].* };
            _ = input.word(); // native symbol belongs to the loader
            const values = try input.typeSpan();
            const descs = input.word();
            const dicts = input.word();
            const table_words = input.word();
            const composed = input.byte() != 0;
            const worker_len = input.word();
            const worker = input.take(worker_len);
            try result.functions.put(arena, identity, .{ .shape = .{ .values = values, .descs = descs, .dicts = dicts, .composed = composed }, .worker = worker, .table_words = table_words });
        }
        std.debug.assert(input.at == bytes.len);
        return result;
    }
};

/// Record only callable workers the backend emitted and immutable inputs of
/// the tables explicitly filled by their producer. Ordinary objects never request this table. `compiled`
/// is the backend producer's procedure list, not a guessed reachability scan.
pub fn encodeMetadata(retained: *Retained, result: *LirProgram.Result, compiled: []const CompiledFunction) Allocator.Error![]const u8 {
    const program = retained.program;
    program.result = result.*;
    defer result.* = program.result;
    const allocator = program.allocator;
    var encoder = Encoder{ .allocator = allocator, .program = program, .link_names = true };
    defer encoder.deinit();
    // Encoding copies these graph roots and edges into its owned result.
    var scratch = std.heap.ArenaAllocator.init(allocator);
    defer scratch.deinit();
    var graph = CaptureGraphBuilder{ .retained = retained, .scratch = scratch.allocator() };
    defer graph.deinit();
    const tables = try scratch.allocator().alloc([]const u32, program.static_descs.unfilled_tables.items.len);
    for (program.static_descs.unfilled_tables.items, tables) |table, *inputs| inputs.* = try graph.span(table.inputs);
    const records = try scratch.allocator().alloc(ImportedRecord, program.static_descs.captured_records.items.len);
    for (program.static_descs.captured_records.items, records) |record, *out| out.* = .{ .leading = record.leading, .elems = try graph.span(record.elems) };
    try encodeCaptureGraph(&encoder, graph.nodes.items);
    try encoder.word(@intCast(tables.len));
    for (tables) |inputs| try encodeCaptureRefs(&encoder, inputs);
    try encoder.word(@intCast(records.len));
    for (records) |record| {
        try encoder.word(record.leading);
        try encodeCaptureRefs(&encoder, record.elems);
    }
    const count_at = encoder.bytes.items.len;
    try encoder.word(0);
    var count: u32 = 0;
    // Lowering declares which workers can be function values. ARC may emit
    // additional direct-call ownership variants; those have no capture recipe
    // and do not belong in this table. Join the backend's emitted order with
    // the stable identity relation retained before LIR procedure compaction;
    // Worker.proc no longer indexes the same procedure in the finished store.
    for (compiled) |function| {
        const id = retained.workers.get(function.identity) orelse continue;
        const record = program.worker(id);
        switch (record.key) {
            .template, .default, .root, .static_data, .convert => continue,
            .lambda, .adapter, .method_entry, .structural, .structural_map, .crash_dictionary, .literal_thunk, .root_slot, .callable_eval_entry, .codec => {},
        }
        const shape = captureShape(program, id);
        try encoder.bytes.appendSlice(allocator, &function.identity.bytes);
        try encoder.word(function.symbol);
        try encoder.typeSpan(shape.values);
        try encoder.word(shape.descs);
        try encoder.word(shape.dicts);
        try encoder.word(if (shape.descs + shape.dicts == 0) 0 else (record.needs orelse invariant("a captured function table has no declared needs")).wordCount());
        try encoder.bytes.append(allocator, @intFromBool(shape.composed));
        const size_at = encoder.bytes.items.len;
        try encoder.word(0);
        const start = encoder.bytes.items.len;
        try encoder.worker(id);
        std.mem.writeInt(u32, encoder.bytes.items[size_at..][0..4], @intCast(encoder.bytes.items.len - start), .little);
        count += 1;
    }
    std.mem.writeInt(u32, encoder.bytes.items[count_at..][0..4], count, .little);
    return try encoder.finish();
}

/// Keep `program` for reading the function values its program evaluates to.
pub fn retain(allocator: Allocator, program: *Program) Allocator.Error!*Retained {
    const retained = try allocator.create(Retained);
    errdefer allocator.destroy(retained);
    retained.* = .{ .allocator = allocator, .program = program };
    errdefer retained.workers.deinit(allocator);
    for (program.workers.items, 0..) |worker, index| {
        try retained.workers.put(allocator, program.result.store.getProcSpec(worker.proc).identity, @fromBackingInt(@intCast(index)));
    }
    return retained;
}

fn readWord(ptr: [*]const u8) *const anyopaque {
    return @ptrFromInt(std.mem.readInt(usize, ptr[0..@sizeOf(usize)], @import("builtin").cpu.arch.endian()));
}

fn allClosed(program: *Program, tys: []const TypeId) Allocator.Error!bool {
    var converter = program.converter();
    for (tys) |ty| {
        if (!try converter.representationClosed(ty)) return false;
    }
    return true;
}

/// A closed capture record's layout (`Builder.captureLayout`): `values`,
/// then `word_count` pointer-sized words.
fn closedRecordLayout(program: *Program, values: []const TypeId, word_count: u32) Allocator.Error!layout.Idx {
    const allocator = program.allocator;
    const fields = try allocator.alloc(layout.StructField, values.len + word_count);
    defer allocator.free(fields);
    for (fields, 0..) |*field, index| {
        field.* = .{
            .index = @intCast(index),
            .layout = if (index < values.len) try program.layouts.layoutOf(values[index]) else .opaque_ptr,
        };
    }
    return try program.result.layouts.putStructFields(fields);
}

const Tag = enum(u8) {
    template,
    lambda,
    adapter,
    default,
    method_entry,
    literal_thunk,
    root_slot,
    structural,
    structural_map,
    crash_dictionary,
    codec,
    callable_eval_entry,
    /// A template's integer list worker (`lower.TemplateBinding.list_int`):
    /// the module and template, then one byte naming the item type
    /// (`lower.ListIntItem`).
    template_list_int,
};

fn appendWord(allocator: Allocator, list: *std.ArrayList(u8), value: u32) Allocator.Error!void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    try list.appendSlice(allocator, &buffer);
}

const no_type = std.math.maxInt(u32);

const Encoder = struct {
    allocator: Allocator,
    link_names: bool = false,
    program: *Program,
    /// The table of modules, by checked key, each once.
    modules: std.ArrayList([32]u8) = .empty,
    module_index: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    /// The table of types: each is a `types.Node` whose components are
    /// earlier entries, so a type shared across the recipe is written once.
    table: std.ArrayList(u8) = .empty,
    table_len: u32 = 0,
    table_index: collections.DenseMapUnmanaged(TypeId, u32) = .empty,
    bytes: std.ArrayList(u8) = .empty,

    fn deinit(self: *Encoder) void {
        self.modules.deinit(self.allocator);
        self.module_index.deinit(self.allocator);
        self.table.deinit(self.allocator);
        self.table_index.deinit(self.allocator);
        self.bytes.deinit(self.allocator);
    }

    fn finish(self: *Encoder) Allocator.Error![]const u8 {
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(self.allocator);
        try appendWord(self.allocator, &out, @intCast(self.modules.items.len));
        for (self.modules.items) |key| try out.appendSlice(self.allocator, &key);
        try appendWord(self.allocator, &out, self.table_len);
        try out.appendSlice(self.allocator, self.table.items);
        try out.appendSlice(self.allocator, self.bytes.items);
        return try out.toOwnedSlice(self.allocator);
    }

    /// `index`'s entry of the module table.
    fn moduleEntry(self: *Encoder, index: types.ModuleIndex) Allocator.Error!u32 {
        const key = if (self.link_names) self.program.modules.linkName(index) else self.program.module(index).key.bytes;
        const entry = try self.module_index.getOrPut(self.allocator, key);
        if (!entry.found_existing) {
            entry.value_ptr.* = @intCast(self.modules.items.len);
            try self.modules.append(self.allocator, key);
        }
        return entry.value_ptr.*;
    }

    fn word(self: *Encoder, value: u32) Allocator.Error!void {
        try appendWord(self.allocator, &self.bytes, value);
    }

    fn tag(self: *Encoder, value: Tag) Allocator.Error!void {
        try self.bytes.append(self.allocator, @backingInt(value));
    }

    fn module(self: *Encoder, index: types.ModuleIndex) Allocator.Error!void {
        try self.word(try self.moduleEntry(index));
    }

    fn typeOf(self: *Encoder, ty: TypeId) Allocator.Error!void {
        try self.word(try self.tableIndex(ty));
    }

    fn typeSpan(self: *Encoder, tys: []const TypeId) Allocator.Error!void {
        try self.word(@intCast(tys.len));
        for (tys) |ty| try self.typeOf(ty);
    }

    fn name(self: *Encoder, id: types.NameId) Allocator.Error!void {
        const text = self.program.store.nameText(id);
        try self.word(@intCast(text.len));
        try self.bytes.appendSlice(self.allocator, text);
    }

    /// `root`'s entry of the table, written along with its components'.
    fn tableIndex(self: *Encoder, root: TypeId) Allocator.Error!u32 {
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .ty = root, .expanded = false });
        const store = &self.program.store;
        while (stack.pop()) |frame| {
            if (self.table_index.contains(frame.ty)) continue;
            if (frame.expanded) {
                try self.tableEntry(frame.ty);
                try self.table_index.put(self.allocator, frame.ty, self.table_len);
                self.table_len += 1;
                continue;
            }
            try stack.append(self.allocator, .{ .ty = frame.ty, .expanded = true });
            const start = stack.items.len;
            switch (store.get(frame.ty)) {
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try stack.append(self.allocator, .{ .ty = field.ty, .expanded = false });
                    if (record.ext) |ext| try stack.append(self.allocator, .{ .ty = ext, .expanded = false });
                    for (store.paddingTypes(record)) |padding| try stack.append(self.allocator, .{ .ty = padding, .expanded = false });
                },
                .tuple => |items| for (store.span(items)) |item| try stack.append(self.allocator, .{ .ty = item, .expanded = false }),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |entry| for (store.span(entry.payloads)) |payload| try stack.append(self.allocator, .{ .ty = payload, .expanded = false });
                    if (tag_union.ext) |ext| try stack.append(self.allocator, .{ .ty = ext, .expanded = false });
                },
                .list, .box => |child| try stack.append(self.allocator, .{ .ty = child, .expanded = false }),
                .nominal => |nominal| for (store.nominalArgs(nominal)) |arg| try stack.append(self.allocator, .{ .ty = arg, .expanded = false }),
                .function => |function| {
                    for (store.span(function.args)) |arg| try stack.append(self.allocator, .{ .ty = arg, .expanded = false });
                    try stack.append(self.allocator, .{ .ty = function.ret, .expanded = false });
                },
                .primitive, .empty_record, .variable => {},
            }
            std.mem.reverse(Frame, stack.items[start..]);
        }
        return self.table_index.get(root).?;
    }

    fn tableEntry(self: *Encoder, ty: TypeId) Allocator.Error!void {
        const store = &self.program.store;
        const node = store.get(ty);
        try self.table.append(self.allocator, @backingInt(std.meta.activeTag(node)));
        switch (node) {
            .primitive => |primitive| try self.table.append(self.allocator, @backingInt(primitive)),
            .empty_record => {},
            .record => |record| {
                const fields = store.fieldSpan(record.fields);
                try self.tableWord(@intCast(fields.len));
                for (fields) |field| {
                    try self.tableName(field.name);
                    try self.tableType(field.ty);
                    try self.table.append(self.allocator, @backingInt(field.kind));
                    try self.tableWord(field.slot_variable);
                    try self.table.append(self.allocator, @intFromBool(field.default != null));
                    if (field.default) |default| {
                        try self.tableWord(try self.moduleEntry(default.module));
                        try self.tableWord(default.expr_node);
                    }
                }
                try self.tableExt(record.ext);
                try self.table.append(self.allocator, @intFromBool(record.declared != null));
                if (record.declared) |declared| {
                    try self.tableTypes(store.span(declared.padding));
                    const entries = store.declaredEntries(declared);
                    try self.tableWord(@intCast(entries.len));
                    for (entries) |entry| {
                        try self.table.append(self.allocator, @backingInt(std.meta.activeTag(entry)));
                        switch (entry) {
                            .named => |label| try self.tableName(label),
                            .padding => |ordinal| try self.tableWord(ordinal),
                        }
                    }
                }
            },
            .tuple => |items| try self.tableTypes(store.span(items)),
            .tag_union => |tag_union| {
                const tags = store.tagSpan(tag_union.tags);
                try self.tableWord(@intCast(tags.len));
                for (tags) |entry| {
                    try self.tableName(entry.name);
                    try self.tableTypes(store.span(entry.payloads));
                }
                try self.tableExt(tag_union.ext);
            },
            .list, .box => |child| try self.tableType(child),
            .nominal => |nominal| {
                try self.tableWord(try self.moduleEntry(nominal.decl.module));
                try self.tableWord(@backingInt(nominal.decl.decl));
                try self.tableTypes(store.nominalArgs(nominal));
                try self.tableName(nominal.name);
                try self.table.append(self.allocator, @intFromBool(nominal.is_opaque));
                try self.table.append(self.allocator, if (nominal.builtin) |builtin| @backingInt(builtin) + 1 else 0);
                try self.table.append(self.allocator, @intFromBool(nominal.has_backing));
            },
            .function => |function| {
                try self.tableTypes(store.span(function.args));
                try self.tableType(function.ret);
                try self.table.append(self.allocator, @intFromBool(function.effectful));
            },
            .variable => |index| try self.tableWord(index),
        }
    }

    fn tableWord(self: *Encoder, value: u32) Allocator.Error!void {
        try appendWord(self.allocator, &self.table, value);
    }

    fn tableType(self: *Encoder, ty: TypeId) Allocator.Error!void {
        try self.tableWord(self.table_index.get(ty).?);
    }

    fn tableTypes(self: *Encoder, tys: []const TypeId) Allocator.Error!void {
        try self.tableWord(@intCast(tys.len));
        for (tys) |ty| try self.tableType(ty);
    }

    fn tableExt(self: *Encoder, ext: ?TypeId) Allocator.Error!void {
        try self.tableWord(if (ext) |ty| self.table_index.get(ty).? else no_type);
    }

    fn tableName(self: *Encoder, id: types.NameId) Allocator.Error!void {
        const text = self.program.store.nameText(id);
        try self.tableWord(@intCast(text.len));
        try self.table.appendSlice(self.allocator, text);
    }

    /// Copy the explicit recipe, remapping its module/type-table references.
    /// This never creates a worker or reads a source body.
    fn importWorker(self: *Encoder, input: *Decoder) Allocator.Error!void {
        const kind: Tag = @fromBackingInt(@intCast(input.byte()));
        try self.tag(kind);
        switch (kind) {
            .template, .default => {
                try self.module(input.module());
                try self.word(input.word());
            },
            .template_list_int => {
                try self.module(input.module());
                try self.word(input.word());
                try self.bytes.append(self.allocator, input.byte());
            },
            .lambda => {
                try self.word(input.word());
                try self.word(input.word());
                try self.importWorker(input);
            },
            .adapter => {
                try self.typeOf(input.typeOf());
                try self.typeOf(input.typeOf());
                try self.importWorker(input);
            },
            .method_entry => try self.importWorker(input),
            .literal_thunk => {
                try self.typeOf(input.typeOf());
                try self.word(input.word());
                try self.importWorker(input);
            },
            .root_slot => {
                try self.module(input.module());
                try self.word(input.word());
                try self.typeOf(input.typeOf());
            },
            .callable_eval_entry => {
                try self.module(input.module());
                try self.module(input.module());
                try self.word(input.word());
            },
            .structural, .crash_dictionary => {
                try self.module(input.module());
                try self.bytes.append(self.allocator, input.byte());
            },
            .structural_map => {
                try self.module(input.module());
                try self.name(try input.name());
                try self.word(input.word());
                try self.word(input.word());
                try self.word(input.word());
            },
            .codec => {
                try self.module(input.module());
                const piece: Codec.PieceKind = @fromBackingInt(@intCast(input.byte()));
                try self.bytes.append(self.allocator, @backingInt(piece));
                try self.word(input.word());
                switch (Codec.pieceOperands(piece)) {
                    .none => {},
                    .types => {
                        try self.typeOf(input.typeOf());
                        try self.typeOf(input.typeOf());
                    },
                    .type_and_name => {
                        try self.typeOf(input.typeOf());
                        try self.name(try input.name());
                    },
                    .type_and_word => {
                        try self.typeOf(input.typeOf());
                        try self.word(input.word());
                    },
                }
                try self.typeSpan(try input.typeSpan());
                try self.typeOf(input.typeOf());
                try self.typeSpan(try input.typeSpan());
            },
        }
    }

    fn worker(self: *Encoder, id: WorkerId) Allocator.Error!void {
        const program = self.program;
        const record = program.worker(id).*;
        switch (record.key) {
            .template => |key| {
                try self.tag(switch (key.binding) {
                    .generic => .template,
                    .list_int => .template_list_int,
                });
                try self.module(key.module);
                try self.word(@backingInt(key.template));
                switch (key.binding) {
                    .generic => {},
                    .list_int => |item| try self.bytes.append(self.allocator, @backingInt(item)),
                }
            },
            .lambda => |key| {
                try self.tag(.lambda);
                try self.word(@backingInt(key.expr));
                try self.word(@intFromBool(key.captured_scheme));
                try self.worker(key.outer);
            },
            .adapter => |key| {
                try self.tag(.adapter);
                try self.typeOf(key.from);
                try self.typeOf(key.to);
                try self.worker(key.outer);
            },
            .default => |key| {
                try self.tag(.default);
                try self.module(key.module);
                try self.word(@backingInt(key.expr));
            },
            .method_entry => |target| {
                try self.tag(.method_entry);
                try self.worker(target);
            },
            .literal_thunk => |key| {
                try self.tag(.literal_thunk);
                try self.typeOf(key.bound);
                try self.word(key.literal_requirement);
                try self.worker(key.target);
            },
            .root_slot => |key| {
                try self.tag(.root_slot);
                try self.module(key.module);
                try self.word(@backingInt(key.root));
                try self.typeOf(record.ret);
            },
            .structural => |method| {
                try self.tag(.structural);
                try self.module(record.module);
                try self.bytes.append(self.allocator, @backingInt(method));
            },
            .structural_map => |map| {
                try self.tag(.structural_map);
                try self.module(record.module);
                try self.name(map.name);
                try self.word(map.payload_index);
                try self.word(map.arity);
                try self.word(@intFromBool(map.effectful));
            },
            .crash_dictionary => |kind| {
                try self.tag(.crash_dictionary);
                try self.module(record.module);
                try self.bytes.append(self.allocator, @backingInt(kind));
            },
            .callable_eval_entry => |binding| {
                try self.tag(.callable_eval_entry);
                try self.module(record.module);
                try self.module(binding.module);
                try self.word(@backingInt(binding.template));
            },
            .codec => |index| {
                try self.tag(.codec);
                try self.module(record.module);
                const piece = program.codec.pieces.items[index];
                try self.bytes.append(self.allocator, @backingInt(piece.kind));
                try self.word(Codec.pieceFamilyRoot(program, piece));
                switch (Codec.pieceOperands(piece.kind)) {
                    .none => {},
                    .types => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.typeOf(@fromBackingInt(piece.b));
                    },
                    .type_and_name => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.name(@fromBackingInt(piece.b));
                    },
                    .type_and_word => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.word(piece.b);
                    },
                }
                try self.typeSpan(record.params);
                try self.typeOf(record.ret);
                try self.typeSpan(record.capture_types);
            },
            .root, .static_data, .convert => invariant("a worker that is no function value reached a stored function"),
        }
    }
};

const Decoder = struct {
    program: *Program,
    link_names: bool = false,
    bytes: []const u8,
    at: usize = 0,
    modules: []const [32]u8 = &.{},
    table: []TypeId = &.{},

    fn take(self: *Decoder, len: usize) []const u8 {
        defer self.at += len;
        return self.bytes[self.at..][0..len];
    }

    fn word(self: *Decoder) u32 {
        return std.mem.readInt(u32, self.take(4)[0..4], .little);
    }

    fn byte(self: *Decoder) u8 {
        return self.take(1)[0];
    }

    fn module(self: *Decoder) types.ModuleIndex {
        const key = self.modules[self.word()];
        if (!self.link_names) return self.program.moduleByKey(.{ .bytes = key }).index;
        for (self.program.modules.views.items) |view| if (std.mem.eql(u8, &view.link_name, &key)) return view.index;
        invariant("stored-function metadata names a module outside its checked imports");
    }

    /// The table of modules, which `module` indexes.
    fn readModules(self: *Decoder) void {
        const count = self.word();
        const bytes = self.take(count * 32);
        self.modules = @as([*]const [32]u8, @ptrCast(bytes.ptr))[0..count];
    }

    fn typeOf(self: *Decoder) TypeId {
        return self.table[self.word()];
    }

    fn typeSpan(self: *Decoder) Allocator.Error![]const TypeId {
        const out = try self.program.arena.allocator().alloc(TypeId, self.word());
        for (out) |*ty| ty.* = self.typeOf();
        return out;
    }

    fn ext(self: *Decoder) ?TypeId {
        const index = self.word();
        return if (index == no_type) null else self.table[index];
    }

    fn readTable(self: *Decoder) Allocator.Error!void {
        self.table = try self.program.allocator.alloc(TypeId, self.word());
        errdefer self.program.allocator.free(self.table);
        for (self.table) |*ty| ty.* = try self.tableEntry();
    }

    /// One entry of the table, whose components are entries before it.
    fn tableEntry(self: *Decoder) Allocator.Error!TypeId {
        const program = self.program;
        const allocator = program.allocator;
        const store = &program.store;
        const node_tag: std.meta.Tag(types.Node) = @fromBackingInt(@intCast(self.byte()));
        return switch (node_tag) {
            .primitive => try store.intern(.{ .primitive = @fromBackingInt(@intCast(self.byte())) }),
            .empty_record => try store.intern(.empty_record),
            .record => blk: {
                const fields = try allocator.alloc(types.Field, self.word());
                defer allocator.free(fields);
                for (fields) |*field| {
                    const field_name = try self.name();
                    const ty = self.typeOf();
                    const kind: types.FieldKind = @fromBackingInt(@intCast(self.byte()));
                    field.* = .{ .name = field_name, .ty = ty, .kind = kind, .slot_variable = self.word() };
                    if (self.byte() != 0) {
                        const module_index = self.module();
                        field.default = .{ .module = module_index, .expr_node = self.word() };
                    }
                }
                const remainder = self.ext();
                var declared: ?types.DeclaredOrderInput = null;
                if (self.byte() != 0) {
                    const padding = try self.typeSpan();
                    const entries = try program.arena.allocator().alloc(types.DeclaredEntry, self.word());
                    for (entries) |*entry| {
                        const kind: std.meta.Tag(types.DeclaredEntry) = @fromBackingInt(@intCast(self.byte()));
                        entry.* = switch (kind) {
                            .named => .{ .named = try self.name() },
                            .padding => .{ .padding = self.word() },
                        };
                    }
                    declared = .{ .padding = padding, .entries = entries };
                }
                break :blk try store.intern(.{ .record = .{ .fields = fields, .ext = remainder, .declared = declared } });
            },
            .tuple => blk: {
                const items = try self.typeSpan();
                break :blk try store.intern(.{ .tuple = items });
            },
            .tag_union => blk: {
                const tags = try allocator.alloc(types.TagInput, self.word());
                defer allocator.free(tags);
                for (tags) |*entry| {
                    const tag_name = try self.name();
                    entry.* = .{ .name = tag_name, .payloads = try self.typeSpan() };
                }
                break :blk try store.intern(.{ .tag_union = .{ .tags = tags, .ext = self.ext() } });
            },
            .list => try store.intern(.{ .list = self.typeOf() }),
            .box => try store.intern(.{ .box = self.typeOf() }),
            .nominal => blk: {
                const module_index = self.module();
                const decl: checked.CheckedNominalDeclarationId = @fromBackingInt(self.word());
                const args = try self.typeSpan();
                const nominal_name = try self.name();
                const is_opaque = self.byte() != 0;
                const builtin = self.byte();
                break :blk try store.intern(.{ .nominal = .{
                    .decl = .{ .module = module_index, .decl = decl },
                    .args = args,
                    .name = nominal_name,
                    .is_opaque = is_opaque,
                    .builtin = if (builtin == 0) null else @fromBackingInt(@intCast(builtin - 1)),
                    .has_backing = self.byte() != 0,
                } });
            },
            .function => blk: {
                const args = try self.typeSpan();
                const ret = self.typeOf();
                break :blk try store.intern(.{ .function = .{ .args = args, .ret = ret, .effectful = self.byte() != 0 } });
            },
            .variable => try store.variable(self.word()),
        };
    }

    fn name(self: *Decoder) Allocator.Error!types.NameId {
        const len = self.word();
        return try self.program.store.internName(self.take(len));
    }

    fn worker(self: *Decoder) Allocator.Error!WorkerId {
        const program = self.program;
        const tag: Tag = @fromBackingInt(@intCast(self.byte()));
        switch (tag) {
            .template => {
                const module_index = self.module();
                return try program.templateWorker(module_index, @fromBackingInt(self.word()));
            },
            .template_list_int => {
                const module_index = self.module();
                const template = self.word();
                const item: lower.ListIntItem = @fromBackingInt(self.byte());
                return try program.listIntTemplateWorker(module_index, @fromBackingInt(template), item);
            },
            .lambda => {
                const expr: ExprId = @fromBackingInt(self.word());
                const captured_scheme = self.word() != 0;
                const outer = try self.worker();
                var builder = try body.Builder.init(program, outer);
                defer builder.deinit();
                return try builder.storedLambdaWorker(outer, expr, captured_scheme);
            },
            .adapter => {
                const from = self.typeOf();
                const to = self.typeOf();
                return try program.adapterWorker(try self.worker(), from, to);
            },
            .default => {
                const module_index = self.module();
                return try program.defaultWorker(module_index, @fromBackingInt(self.word()));
            },
            .method_entry => return try program.methodEntryWorker(try self.worker()),
            .literal_thunk => {
                const bound_ty = self.typeOf();
                const requirement = self.word();
                const bound = switch (program.store.get(bound_ty)) {
                    .tuple => |items| program.store.span(items),
                    .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a stored literal thunk's types are not a tuple"),
                };
                return try program.literalThunkWorker(try self.worker(), try program.arena.allocator().dupe(TypeId, bound), requirement);
            },
            // The compile-time program read the root's slot; its value is
            // stored now, and the root's wrapper restores it.
            .root_slot => {
                const module_index = self.module();
                const root: checked.ComptimeRootId = @fromBackingInt(self.word());
                _ = self.typeOf();
                const wrapper = program.wrapper_by_root.get(.{ .module = module_index, .root = root }) orelse
                    invariant("a literal argument root has no entry wrapper");
                return try program.methodEntryWorker(try program.templateRefWorker(wrapper));
            },
            .structural => {
                const module_index = self.module();
                return try program.structuralWorker(module_index, @fromBackingInt(@intCast(self.byte())));
            },
            .structural_map => {
                const module_index = self.module();
                const name_id = try self.name();
                const payload_index = self.word();
                const arity = self.word();
                return try program.structuralMapWorker(module_index, .{ .name = name_id, .payload_index = payload_index, .arity = arity, .effectful = self.word() != 0 });
            },
            .crash_dictionary => {
                const module_index = self.module();
                return try program.crashDictionaryWorker(module_index, @fromBackingInt(@intCast(self.byte())));
            },
            .callable_eval_entry => {
                const module_index = self.module();
                const binding_module = self.module();
                const template: checked.CallableEvalTemplateId = @fromBackingInt(self.word());
                return try program.callableEvalEntryWorker(module_index, .{ .module = binding_module, .template = template });
            },
            .codec => {
                const module_index = self.module();
                const kind: Codec.PieceKind = @fromBackingInt(@intCast(self.byte()));
                const root = self.word();
                var a: u32 = 0;
                var b_operand: u32 = 0;
                switch (Codec.pieceOperands(kind)) {
                    .none => {},
                    .types => {
                        a = @backingInt(self.typeOf());
                        b_operand = @backingInt(self.typeOf());
                    },
                    .type_and_name => {
                        a = @backingInt(self.typeOf());
                        b_operand = @backingInt(try self.name());
                    },
                    .type_and_word => {
                        a = @backingInt(self.typeOf());
                        b_operand = self.word();
                    },
                }
                const params = try self.typeSpan();
                const ret = self.typeOf();
                const captures = try self.typeSpan();
                return try Codec.restorePiece(program, module_index, root, kind, a, b_operand, params, ret, captures);
            },
        }
    }
};

/// A stored function value's worker and the types its descriptors describe.
pub const Decoded = struct {
    worker: WorkerId,
    bound: []const TypeId,
    inputs: []const Tables.Operand,
    capture_descs: []const Tables.Operand,
};

fn captureOperands(arena: Allocator, words: []const Tables.Operand, refs: []const u32) Allocator.Error![]const Tables.Operand {
    const out = try arena.alloc(Tables.Operand, refs.len);
    for (out, refs) |*operand, ref| operand.* = words[ref];
    return out;
}

/// Read a stored function's recipe.
pub fn decode(b: *body.Builder, recipe: []const u8) Allocator.Error!Decoded {
    var decoder = Decoder{ .program = b.program, .bytes = recipe };
    decoder.readModules();
    try decoder.readTable();
    defer b.allocator.free(decoder.table);
    const worker = try decoder.worker();
    const graph = try decodeCaptureGraph(&decoder);
    const inputs = try decodeCaptureRefs(&decoder);
    const capture_descs = try decodeCaptureRefs(&decoder);
    const shape = captureShape(b.program, worker);
    if (inputs.len != shape.descs + shape.dicts) invariant("a stored function's immutable inputs differ from its worker's");
    const arena = b.program.arena.allocator();
    const bound = try arena.alloc(TypeId, shape.descs);
    for (bound, inputs[0..shape.descs]) |*ty, ref| ty.* = captureType(graph, ref);
    const words = try arena.alloc(Tables.Operand, graph.len);
    // Reserve before connecting any edge. Scoped descriptors can capture
    // overrides that capture those descriptors, and dictionaries can cycle.
    for (graph, words) |node, *word| word.* = if (node == .zero) .{ .static = .zero } else .{ .recipe = try b.recipes.reserve() };
    for (graph, words) |node, word| {
        const recipe_node: Tables.Recipe = switch (node) {
            .desc => |desc| blk: {
                const scoped = try arena.alloc(Tables.ScopedOverride, desc.scoped.len);
                for (desc.scoped, scoped) |original, *override| {
                    override.* = .{ .nominal = original.nominal, .methods = undefined };
                    for (original.methods, &override.methods) |method, *out| out.* = if (method) |ref| words[ref] else null;
                }
                break :blk .{ .desc = .{ .ty = desc.ty, .scoped = scoped } };
            },
            .callable => |callable| .{ .callable = .{ .entry = try callable.worker.resolve(b.program), .captures = try captureOperands(arena, words, callable.captures) } },
            .table => |table| .{ .table = .{ .worker = try table.worker.resolve(b.program), .inputs = try captureOperands(arena, words, table.inputs) } },
            .zero => continue,
        };
        try b.recipes.set(word.recipe, recipe_node);
    }
    return .{ .worker = worker, .bound = bound, .inputs = try captureOperands(arena, words, inputs), .capture_descs = try captureOperands(arena, words, capture_descs) };
}

test "stored type metadata preserves field defaults across module reindexing" {
    const allocator = std.testing.allocator;
    var source: Program = undefined;
    source.allocator = allocator;
    source.modules = .{ .allocator = allocator };
    defer source.modules.deinit();
    source.store = types.Store.init(allocator);
    defer source.store.deinit();
    var destination: Program = undefined;
    destination.allocator = allocator;
    destination.modules = .{ .allocator = allocator };
    defer destination.modules.deinit();
    destination.store = types.Store.init(allocator);
    defer destination.store.deinit();
    destination.arena = std.heap.ArenaAllocator.init(allocator);
    defer destination.arena.deinit();
    var source_modules: [2]types.ModuleIndex = undefined;
    var destination_modules: [2]types.ModuleIndex = undefined;
    for (0..2) |index| {
        var view: checked.ImportedModuleView = undefined;
        view.key = .{ .bytes = @splat(@intCast(index + 1)) };
        view.module_identity.stable_hash = view.key.bytes;
        source_modules[index] = try source.modules.add(view);
        view.key.bytes = @splat(@intCast(2 - index));
        view.module_identity.stable_hash = view.key.bytes;
        destination_modules[index] = try destination.modules.add(view);
    }
    const name = try source.store.internName("count");
    const number = try source.store.primitive(.u8);
    var fields = [_]types.Field{.{ .name = name, .ty = number, .kind = .inline_slot }};
    const required = try source.store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    fields[0].default = .{ .module = source_modules[0], .expr_node = 10 };
    const defaulted = try source.store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    const root = try source.store.intern(.{ .tuple = &.{ required, defaulted } });
    var encoder: Encoder = .{ .allocator = allocator, .program = &source };
    defer encoder.deinit();
    const root_index = try encoder.tableIndex(root);
    const bytes = try encoder.finish();
    defer allocator.free(bytes);

    // Validate exactly the entries the encoder produced, including truncation.
    var reader: MetadataReader = .{ .bytes = encoder.table.items, .modules = @intCast(encoder.modules.items.len) };
    for (0..encoder.table_len) |_| {
        try reader.typeEntry();
        reader.types += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), reader.remaining());
    for (0..encoder.table.items.len) |len| {
        var truncated: MetadataReader = .{ .bytes = encoder.table.items[0..len], .modules = 1 };
        const result: error{InvalidMetadata}!void = blk: {
            for (0..encoder.table_len) |_| {
                truncated.typeEntry() catch |err| break :blk err;
                truncated.types += 1;
            }
        };
        try std.testing.expectError(error.InvalidMetadata, result);
    }

    var decoder: Decoder = .{ .program = &destination, .bytes = bytes };
    decoder.readModules();
    try decoder.readTable();
    defer allocator.free(decoder.table);
    const restored = destination.store.span(destination.store.get(decoder.table[root_index]).tuple);
    try std.testing.expect(restored[0] != restored[1]);
    try std.testing.expectEqual(@as(?types.FieldDefault, null), destination.store.fieldSpan(destination.store.get(restored[0]).record.fields)[0].default);
    const restored_default = destination.store.fieldSpan(destination.store.get(restored[1]).record.fields)[0].default.?;
    try std.testing.expectEqual(destination_modules[1], restored_default.module);
    try std.testing.expectEqual(@as(u32, 10), restored_default.expr_node);
}

test "function unit metadata validates the integer list template tag, its single module reference, and its item type" {
    try std.testing.expectEqual(@as(u8, 11), @backingInt(Tag.callable_eval_entry));
    try std.testing.expectEqual(@as(u8, 12), @backingInt(Tag.template_list_int));
    // An integer list template recipe has one module, a template ordinal,
    // and an item type byte. Unlike the callable-entry recipe, it has no
    // second module reference.
    var bytes = [_]u8{ 12, 0, 0, 0, 0, 23, 0, 0, 0, @backingInt(lower.ListIntItem.u32) };
    for (std.enums.values(lower.ListIntItem)) |item| {
        bytes[9] = @backingInt(item);
        var reader = MetadataReader{ .bytes = &bytes, .modules = 1 };
        try reader.worker();
        try std.testing.expectEqual(@as(usize, 0), reader.remaining());
    }
    for (0..bytes.len) |len| {
        var truncated = MetadataReader{ .bytes = bytes[0..len], .modules = 1 };
        try std.testing.expectError(error.InvalidMetadata, truncated.worker());
    }
    bytes[9] = @intCast(@typeInfo(lower.ListIntItem).@"enum".field_values.len);
    var invalid_item = MetadataReader{ .bytes = &bytes, .modules = 1 };
    try std.testing.expectError(error.InvalidMetadata, invalid_item.worker());
    bytes[9] = @backingInt(lower.ListIntItem.u32);
    bytes[1] = 1;
    var invalid = MetadataReader{ .bytes = &bytes, .modules = 1 };
    try std.testing.expectError(error.InvalidMetadata, invalid.worker());
}

test "function unit metadata validates both callable eval entry module references" {
    var bytes = [_]u8{ @backingInt(Tag.callable_eval_entry), 0, 0, 0, 0, 1, 0, 0, 0, 23, 0, 0, 0 };
    var reader = MetadataReader{ .bytes = &bytes, .modules = 2 };
    try reader.worker();
    try std.testing.expectEqual(@as(usize, 0), reader.remaining());
    for (0..bytes.len) |len| {
        var truncated = MetadataReader{ .bytes = bytes[0..len], .modules = 2 };
        try std.testing.expectError(error.InvalidMetadata, truncated.worker());
    }
    for ([_]usize{ 1, 5 }) |offset| {
        const original = bytes[offset];
        bytes[offset] = 2;
        var invalid = MetadataReader{ .bytes = &bytes, .modules = 2 };
        try std.testing.expectError(error.InvalidMetadata, invalid.worker());
        bytes[offset] = original;
    }
}

test "function unit metadata rejects truncated records and invalid type and module references" {
    const allocator = std.testing.allocator;
    var bytes: std.ArrayList(u8) = .empty;
    defer bytes.deinit(allocator);
    const Write = struct {
        fn word(out: *std.ArrayList(u8), value: u32) Allocator.Error!void {
            var encoded: [4]u8 = undefined;
            std.mem.writeInt(u32, &encoded, value, .little);
            try out.appendSlice(std.testing.allocator, &encoded);
        }
    };
    try Write.word(&bytes, 1); // one module
    try bytes.appendNTimes(allocator, 42, 32);
    try Write.word(&bytes, 1); // one type
    try bytes.append(allocator, @backingInt(std.meta.Tag(types.Node).empty_record));
    try Write.word(&bytes, 3); // descriptor -> override -> table -> descriptor
    try bytes.append(allocator, @backingInt(std.meta.Tag(CaptureNode).desc));
    const descriptor_ref = bytes.items.len;
    try Write.word(&bytes, 0); // its type
    try Write.word(&bytes, 1); // one scoped override
    try Write.word(&bytes, 0); // nominal type
    try bytes.append(allocator, 1);
    const scoped_ref = bytes.items.len;
    try Write.word(&bytes, 1); // forward callable reference
    try bytes.appendSlice(allocator, &.{ 0, 0 });
    for ([_]std.meta.Tag(CaptureNode){ .callable, .table }, [_]u32{ 2, 0 }) |tag, ref| {
        try bytes.append(allocator, @backingInt(tag));
        try Write.word(&bytes, 6);
        try bytes.append(allocator, @backingInt(Tag.structural));
        try Write.word(&bytes, 0);
        try bytes.append(allocator, @typeInfo(@FieldType(lower.WorkerKey, "structural")).@"enum".field_values[0]);
        try Write.word(&bytes, 1);
        try Write.word(&bytes, ref);
    }
    try Write.word(&bytes, 1); // one producer table
    try Write.word(&bytes, 1); // one descriptor input
    const origin_ref = bytes.items.len;
    try Write.word(&bytes, 0);
    try Write.word(&bytes, 1); // one capture-record descriptor
    try Write.word(&bytes, 3); // leading opaque words
    try Write.word(&bytes, 1); // one element descriptor
    try Write.word(&bytes, 0);
    try Write.word(&bytes, 1); // one callable
    const identity: LIR.ProcIdentity = .{ .bytes = @splat(19) };
    try bytes.appendSlice(allocator, &identity.bytes);
    try Write.word(&bytes, 23); // native image symbol
    try Write.word(&bytes, 1); // one capture
    try Write.word(&bytes, 0);
    try Write.word(&bytes, 0); // descriptors
    try Write.word(&bytes, 0); // dictionaries
    try Write.word(&bytes, 0); // table words
    try bytes.append(allocator, 0); // composed
    try Write.word(&bytes, 6); // structural worker: tag + module + operation
    try bytes.append(allocator, @backingInt(Tag.structural));
    const module_ref = bytes.items.len;
    try Write.word(&bytes, 0);
    try bytes.append(allocator, @typeInfo(@FieldType(lower.WorkerKey, "structural")).@"enum".field_values[0]);
    const view = try MetadataView.init(bytes.items);
    var functions = view.functions();
    try std.testing.expectEqual(CompiledFunction{ .identity = identity, .symbol = 23 }, functions.next().?);
    try std.testing.expectEqual(@as(?CompiledFunction, null), functions.next());
    for (0..bytes.items.len) |len| try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items[0..len]));
    bytes.items[origin_ref] = 3;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[origin_ref] = 0;
    bytes.items[descriptor_ref] = 1;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[descriptor_ref] = 0;
    bytes.items[scoped_ref] = 3;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[scoped_ref] = 1;
    bytes.items[module_ref] = 1;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[module_ref] = 0;
    try bytes.append(allocator, 0);
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
}

/// A worker's needs list (`tables.Needs`) as bytes another unit reads, in
/// the encoding of a stored function's recipe: its types and workers named
/// by module link identities, never by this lowering's ids or a checked
/// snapshot that can change while the native object stays reusable. Caller frees.
pub fn encodeNeeds(allocator: Allocator, program: *Program, needs: Tables.Needs) Allocator.Error![]const u8 {
    var encoder = Encoder{ .allocator = allocator, .program = program, .link_names = true };
    defer encoder.deinit();
    try encoder.word(needs.input_count);
    try encoder.word(needs.desc_count);
    try encoder.word(@intCast(needs.recipes.len));
    for (needs.recipes) |recipe| {
        try encoder.bytes.append(allocator, @backingInt(std.meta.activeTag(recipe)));
        switch (recipe) {
            .input => |index| try encoder.word(index),
            .desc => |desc| {
                try encoder.typeOf(desc.ty);
                try encoder.word(@intCast(desc.scoped.len));
                for (desc.scoped) |override| {
                    try encoder.typeOf(override.nominal);
                    for (override.methods) |method| {
                        try encoder.bytes.append(allocator, @intFromBool(method != null));
                        if (method) |operand| try encodeOperand(&encoder, operand);
                    }
                }
            },
            .args_desc => |args| {
                try encoder.word(args.leading);
                try encoder.bytes.append(allocator, @intFromBool(args.capture_record));
                try encodeOperands(&encoder, args.elems);
            },
            .callable => |callable| {
                try encoder.worker(callable.entry);
                try encodeOperands(&encoder, callable.captures);
            },
            .table => |table| {
                try encoder.worker(table.worker);
                try encodeOperands(&encoder, table.inputs);
            },
            .tag_index => |tag| {
                try encodeOperand(&encoder, tag.desc);
                try encoder.name(tag.name);
            },
            .field_index => |field| {
                try encodeOperand(&encoder, field.desc);
                try encoder.name(field.name);
            },
            .update_plan => |update| {
                try encodeOperand(&encoder, update.base);
                try encodeOperand(&encoder, update.result);
                try encoder.word(@intCast(update.names.len));
                for (update.names) |name| try encoder.name(name);
            },
            .adapt_plan => |plan| {
                try encodeOperand(&encoder, plan.from);
                try encodeOperand(&encoder, plan.to);
            },
            .names_block => |block| try encodeOperand(&encoder, block.desc),
        }
    }
    try encoder.word(@intCast(needs.slots.len));
    for (needs.slots) |slot| try encoder.word(@backingInt(slot));
    return try encoder.finish();
}

fn encodeOperands(encoder: *Encoder, operands: []const Tables.Operand) Allocator.Error!void {
    try encoder.word(@intCast(operands.len));
    for (operands) |operand| try encodeOperand(encoder, operand);
}

/// An operand: another recipe, or a static word, which a recipe holds
/// only as a closed type's descriptor or the null word.
fn encodeOperand(encoder: *Encoder, operand: Tables.Operand) Allocator.Error!void {
    switch (operand) {
        .recipe => |id| {
            try encoder.bytes.append(encoder.allocator, 0);
            try encoder.word(@backingInt(id));
        },
        .static => |word| switch (word) {
            .desc => |ty| {
                try encoder.bytes.append(encoder.allocator, 1);
                try encoder.typeOf(ty);
            },
            .zero => try encoder.bytes.append(encoder.allocator, 2),
            .scoped_desc, .callable, .table, .node, .int => invariant("a recipe holds a static word other than a closed type's descriptor or the null word"),
        },
    }
}

/// Read the needs list `encodeNeeds` wrote, its types and workers in this
/// lowering, allocated in the program's arena. Its bytes are this
/// compiler's: complete entries are admitted by `boxy_object_cache.decodeView`.
/// Link identities bind the records to the current checked module graph.
pub fn decodeNeeds(program: *Program, bytes: []const u8) Allocator.Error!Tables.Needs {
    const arena = program.arena.allocator();
    var decoder = Decoder{ .program = program, .link_names = true, .bytes = bytes };
    decoder.readModules();
    try decoder.readTable();
    defer program.allocator.free(decoder.table);
    const input_count = decoder.word();
    const desc_count = decoder.word();
    const recipes = try arena.alloc(Tables.Recipe, decoder.word());
    for (recipes) |*recipe| {
        const tag: std.meta.Tag(Tables.Recipe) = @fromBackingInt(@intCast(decoder.byte()));
        recipe.* = switch (tag) {
            .input => .{ .input = decoder.word() },
            .desc => blk: {
                const ty = decoder.typeOf();
                const scoped = try arena.alloc(Tables.ScopedOverride, decoder.word());
                for (scoped) |*override| {
                    override.* = .{ .nominal = decoder.typeOf(), .methods = undefined };
                    for (&override.methods) |*method| method.* = if (decoder.byte() != 0) try decodeOperand(&decoder) else null;
                }
                break :blk .{ .desc = .{ .ty = ty, .scoped = scoped } };
            },
            .args_desc => blk: {
                const leading = decoder.word();
                const capture_record = decoder.byte() != 0;
                break :blk .{ .args_desc = .{ .leading = leading, .elems = try decodeOperands(&decoder), .capture_record = capture_record } };
            },
            .callable => blk: {
                const entry = try decoder.worker();
                break :blk .{ .callable = .{ .entry = entry, .captures = try decodeOperands(&decoder) } };
            },
            .table => blk: {
                const worker = try decoder.worker();
                break :blk .{ .table = .{ .worker = worker, .inputs = try decodeOperands(&decoder) } };
            },
            .tag_index => blk: {
                const desc = try decodeOperand(&decoder);
                break :blk .{ .tag_index = .{ .desc = desc, .name = try decoder.name() } };
            },
            .field_index => blk: {
                const desc = try decodeOperand(&decoder);
                break :blk .{ .field_index = .{ .desc = desc, .name = try decoder.name() } };
            },
            .update_plan => blk: {
                const update_base = try decodeOperand(&decoder);
                const result = try decodeOperand(&decoder);
                const names = try arena.alloc(types.NameId, decoder.word());
                for (names) |*name| name.* = try decoder.name();
                break :blk .{ .update_plan = .{ .base = update_base, .result = result, .names = names } };
            },
            .adapt_plan => blk: {
                const from = try decodeOperand(&decoder);
                break :blk .{ .adapt_plan = .{ .from = from, .to = try decodeOperand(&decoder) } };
            },
            .names_block => .{ .names_block = .{ .desc = try decodeOperand(&decoder) } },
        };
    }
    const slots = try arena.alloc(Tables.RecipeId, decoder.word());
    for (slots) |*slot| slot.* = @fromBackingInt(decoder.word());
    return .{ .input_count = input_count, .desc_count = desc_count, .recipes = recipes, .slots = slots };
}

fn decodeOperands(decoder: *Decoder) Allocator.Error![]const Tables.Operand {
    const operands = try decoder.program.arena.allocator().alloc(Tables.Operand, decoder.word());
    for (operands) |*operand| operand.* = try decodeOperand(decoder);
    return operands;
}

fn decodeOperand(decoder: *Decoder) Allocator.Error!Tables.Operand {
    return switch (decoder.byte()) {
        0 => .{ .recipe = @fromBackingInt(decoder.word()) },
        1 => .{ .static = .{ .desc = decoder.typeOf() } },
        2 => .{ .static = .zero },
        else => invariant("an encoded needs list holds an operand of no kind"),
    };
}

/// Serialize the selected cyclic graph, then return only independent bytes.
/// Both temporary graphs are gone before the caller reads the result.
fn selectedCaptureMetadataForTest(allocator: Allocator, retained: *Retained) (Allocator.Error || error{TestExpectedEqual})![]const u8 {
    var scratch = std.heap.ArenaAllocator.init(allocator);
    defer scratch.deinit();
    var graph = CaptureGraphBuilder{ .retained = retained, .scratch = scratch.allocator() };
    defer graph.deinit();
    const roots = try graph.span(retained.program.static_descs.unfilled_tables.items[0].inputs);
    var selected = CaptureSelection{ .allocator = allocator, .scratch = scratch.allocator(), .source = graph.nodes.items };
    defer selected.deinit();
    const inputs = try selected.span(roots);
    // A descriptor's override captures a table which points back to both
    // itself and the descriptor. Shared roots remain shared after selection.
    try std.testing.expectEqual(@as(usize, 3), selected.nodes.items.len);
    try std.testing.expectEqualSlices(u32, &.{ 0, 2 }, inputs);
    try std.testing.expectEqual(@as(?u32, 1), selected.nodes.items[0].desc.scoped[0].methods[0]);
    try std.testing.expectEqualSlices(u32, &.{2}, selected.nodes.items[1].callable.captures);
    try std.testing.expectEqualSlices(u32, &.{ 0, 2 }, selected.nodes.items[2].table.inputs);
    var encoder = Encoder{ .allocator = allocator, .program = retained.program };
    defer encoder.deinit();
    try encodeCaptureGraph(&encoder, selected.nodes.items);
    try encoder.word(1);
    try encodeCaptureRefs(&encoder, inputs);
    try encoder.word(0); // capture records
    try encoder.word(0); // compiled functions
    return try encoder.finish();
}

fn captureSerializationScratchCase(allocator: Allocator) (Allocator.Error || error{ InvalidMetadata, TestExpectedEqual })!void {
    const setup = std.testing.allocator;
    // The test installs explicit producer records; no runtime code or
    // checked module is needed to encode this graph.
    var program: Program = undefined;
    program.allocator = setup;
    program.input = .{ .modules = undefined, .roots = undefined, .target_usize = .u64 };
    program.modules = .{ .allocator = setup };
    defer program.modules.deinit();
    var view: checked.ImportedModuleView = undefined;
    view.key = .{ .bytes = @splat(9) };
    view.module_identity.stable_hash = @splat(9);
    const module = try program.modules.add(view);
    program.arena = std.heap.ArenaAllocator.init(setup);
    defer program.arena.deinit();
    program.store = types.Store.init(setup);
    defer program.store.deinit();
    program.result = try LirProgram.Result.init(setup, .u64);
    defer program.result.deinit();
    program.static_descs = StaticDescs.init(setup, &program.store, &program.layouts, &program.result, .u64);
    defer program.static_descs.deinit();
    program.workers = .empty;
    defer program.workers.deinit(setup);
    program.worker_ids = .empty;
    defer program.worker_ids.deinit(setup);
    program.worklist = .empty;
    defer program.worklist.deinit(setup);
    const worker_id = try program.crashDictionaryWorker(module, .checked_error);
    const identity = program.result.store.getProcSpec(program.worker(worker_id).proc).identity;
    var retained = Retained{ .allocator = allocator, .program = &program };
    defer retained.workers.deinit(setup);
    try retained.workers.put(setup, identity, worker_id);
    const ty = try program.store.emptyRecord();
    const producer = &program.static_descs;
    try producer.scopes.ensureUnusedCapacity(setup, 2);
    producer.scopes.appendAssumeCapacity(&.{});
    producer.scopes.appendAssumeCapacity(try setup.dupe(StaticDescs.ScopedMethods, &.{.{
        .nominal = ty,
        .methods = .{ .{ .callable = 7 }, null, null },
    }}));
    const desc: StaticDescs.CaptureWord = .{ .scoped_desc = .{ .ty = ty, .scope = @fromBackingInt(1) } };
    try producer.unfilled_tables.ensureUnusedCapacity(setup, 1);
    producer.unfilled_tables.appendAssumeCapacity(.{
        .worker = worker_id,
        .inputs = try setup.dupe(StaticDescs.CaptureWord, &.{ desc, .{ .table = 5 } }),
        .reserved = undefined,
        .index = 0,
    });
    try producer.captured_tables.put(setup, 5, 0);
    try producer.captured_callables.ensureUnusedCapacity(setup, 1);
    producer.captured_callables.putAssumeCapacity(7, .{ .identity = identity, .words = try setup.dupe(StaticDescs.CaptureWord, &.{.{ .table = 5 }}) });
    try producer.captured_records.ensureUnusedCapacity(setup, 1);
    producer.captured_records.appendAssumeCapacity(.{ .leading = 2, .elems = try setup.dupe(StaticDescs.CaptureWord, &.{desc}) });

    // Failure injection covers serialization, after producer setup completes.
    program.allocator = allocator;
    for (0..3) |_| {
        const bytes = encodeMetadata(&retained, &program.result, &.{}) catch |err| {
            try std.testing.expectEqual(@as(usize, 0), program.arena.queryCapacity());
            return err;
        };
        defer allocator.free(bytes);
        // This is checked after the operation's local arena is destroyed.
        _ = try MetadataView.init(bytes);
        try std.testing.expectEqual(@as(usize, 0), program.arena.queryCapacity());
        const selected = selectedCaptureMetadataForTest(allocator, &retained) catch |err| {
            try std.testing.expectEqual(@as(usize, 0), program.arena.queryCapacity());
            return err;
        };
        defer allocator.free(selected);
        _ = try MetadataView.init(selected);
        try std.testing.expectEqual(@as(usize, 0), program.arena.queryCapacity());
    }
}

test "capture serialization scratch does not accumulate in a retained program" {
    try captureSerializationScratchCase(std.testing.allocator);
}

test "capture serialization scratch releases cyclic graphs on every allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, captureSerializationScratchCase, .{});
}
