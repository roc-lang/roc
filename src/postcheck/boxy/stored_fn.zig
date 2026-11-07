//! Stored function values of `.boxy` compile-time evaluation (design.md
//! "Stored Function Values").
//!
//! A function value under `.boxy` is an erased callable: a worker's code and
//! what the worker captured, which is values, the descriptors of its scope's
//! type variables, and dictionaries, themselves erased callables. A stored
//! function records exactly that: a recipe naming the worker, the types the
//! descriptors describe, and the captured values and dictionaries as nodes
//! of the constant store. Restoring it rebuilds the worker from the recipe
//! and embeds the captures as immutable data. No runtime constructor is emitted.
//!
//! The recipe is bytes the constant store keeps opaque but for its first
//! table: the modules it names, by checked key, each once, which the store
//! reads to name a stored function in an identity by its modules' link names
//! and to move it to the modules' current versions
//! (`ConstStore.recipeModuleKeys`). Then come a table of the types the recipe
//! names, generic where the worker's own types are, then the worker, then
//! the closed type each captured descriptor describes; a module in those is
//! an index into the first table.

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
        .crash_dictionary, .literal_thunk, .root_slot => .{ .values = &.{}, .descs = 0, .dicts = 0, .composed = true },
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
    /// Its values, then its dictionaries. Owned by the same allocator.
    captures: []const Capture,
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

        // The table's inputs: the descriptors of the types the value is at,
        // then its dictionaries, which the table holds as words.
        const table: ?[*]u8 = if (has_table) @ptrCast(@constCast(readWord(capture + offsets[shape.values.len]))) else null;
        const bound = try allocator.alloc(TypeId, shape.descs);
        defer allocator.free(bound);
        if (table) |address| {
            const table_words = switch (source) {
                .local => |id| (program.worker(id).needs orelse invariant("a captured table has no declared needs")).wordCount(),
                .imported => |foreign| foreign.function.table_words,
            };
            const offset = LIR.tableEntryOffset(table_words, program.static_descs.width.bytes());
            const origin = address[offset..][0..32];
            const index = std.mem.readInt(u32, address[offset + 32 ..][0..4], @import("builtin").cpu.arch.endian());
            if (std.mem.allEqual(u8, origin, 0)) {
                const tables = program.static_descs.unfilled_tables.items;
                if (index >= tables.len) invariant("a captured table has no producer record");
                const inputs = tables[index].inputs;
                if (inputs.len != shape.descs + shape.dicts) invariant("a captured table has the wrong input arity");
                for (bound, inputs[0..shape.descs]) |*ty, input| ty.* = descriptorType(input);
            } else {
                const metadata = try self.importMetadata(origin.*);
                if (index >= metadata.tables.len or metadata.tables[index].len != bound.len) invariant("a cached table has no matching producer types");
                @memcpy(bound, metadata.tables[index]);
            }
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
        try recipe.typeSpan(bound);
        // The dictionaries of a method whose scheme requires itself capture
        // one another. Record their closed types so restoration can reserve
        // the complete immutable graph before connecting its references.
        const recursive_dictionary = switch (source) {
            .local => |worker_id| switch (program.worker(worker_id).key) {
                .method_entry => |target| try program.dictionaryCapturesItself(target, bound),
                .lambda, .adapter, .structural, .structural_map, .crash_dictionary, .literal_thunk, .root_slot, .codec, .template, .default, .root, .static_data, .convert => false,
            },
            .imported => |foreign| try foreign.metadata.recursiveDictionary(program, foreign.function, bound),
        };
        try recipe.bytes.append(allocator, @intFromBool(recursive_dictionary));
        if (recursive_dictionary) return .{ .recipe = try recipe.finish(), .captures = &.{} };

        const captures = try allocator.alloc(Capture, shape.values.len + shape.dicts);
        errdefer allocator.free(captures);
        for (shape.values, captures[0..shape.values.len], offsets[0..shape.values.len]) |generic, *slot, offset| {
            const concrete = try program.substitute(allocator, generic, bound);
            slot.* = .{
                .plan = try const_schema.constPlanOf(program, concrete),
                .layout_idx = try program.layouts.layoutOf(concrete),
                .ptr = capture + offset,
            };
        }
        const dictionary_ty = try program.anyFunction();
        const word_bytes = program.static_descs.width.bytes();
        for (captures[shape.values.len..], shape.descs..) |*slot, index| {
            slot.* = .{
                .plan = try const_schema.constPlanOf(program, dictionary_ty),
                .layout_idx = try program.result.layouts.insertErasedCallable(),
                .ptr = table.? + LIR.tableEntryOffset(@intCast(index), word_bytes),
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
        const tables = try input.word();
        if (tables > input.remaining() / 4) return error.InvalidMetadata;
        for (0..tables) |_| try input.typeSpan();
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
            const identity: LIR.ProcIdentity = .{ .bytes = (self.input.take(32) catch unreachable)[0..32].* };
            const symbol = self.input.word() catch unreachable;
            const values = self.input.word() catch unreachable;
            _ = self.input.takeProduct(values, 4) catch unreachable;
            _ = self.input.take(13) catch unreachable;
            const len = self.input.word() catch unreachable;
            _ = self.input.take(len) catch unreachable;
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
                if (fields > self.remaining() / 13) return error.InvalidMetadata;
                for (0..fields) |_| {
                    try self.name();
                    try self.typeRef();
                    _ = try self.tag(types.FieldKind);
                    _ = try self.word();
                }
                try self.ext();
                const declared = try self.byte();
                if (declared > 1) return error.InvalidMetadata;
                if (declared == 1) {
                    try self.typeSpan();
                    const slots = try self.word();
                    _ = try self.takeProduct(slots, 4);
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

const ImportedMetadata = struct {
    modules: []const [32]u8,
    types: []const TypeId,
    tables: []const []const TypeId = &.{},
    functions: std.AutoHashMapUnmanaged(LIR.ProcIdentity, ImportedFunction) = .empty,

    fn reader(self: *const ImportedMetadata, program: *Program, bytes: []const u8) Decoder {
        return .{ .program = program, .link_names = true, .bytes = bytes, .modules = self.modules, .table = @constCast(self.types) };
    }

    fn recursiveDictionary(self: *const ImportedMetadata, program: *Program, function: ImportedFunction, bound: []const TypeId) Allocator.Error!bool {
        var input = self.reader(program, function.worker);
        if (input.byte() != @backingInt(Tag.method_entry)) return false;
        // Reconstitute the declared scope needed by the existing dictionary
        // dependency rule. This creates no executable body and runs no lowering.
        const target = try input.worker();
        return try program.dictionaryCapturesItself(target, bound);
    }

    fn load(program: *Program, bytes: []const u8) Allocator.Error!*ImportedMetadata {
        const arena = program.arena.allocator();
        var input = Decoder{ .program = program, .link_names = true, .bytes = bytes };
        input.readModules();
        try input.readTable();
        defer program.allocator.free(input.table);
        const result = try arena.create(ImportedMetadata);
        result.* = .{ .modules = input.modules, .types = try arena.dupe(TypeId, input.table) };
        const table_count = input.word();
        const tables = try arena.alloc([]const TypeId, table_count);
        for (tables) |*table| table.* = try input.typeSpan();
        result.tables = tables;
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

fn descriptorType(input: StaticDescs.CaptureWord) TypeId {
    return switch (input) {
        .desc => |ty| ty,
        .scoped_desc => |scoped| scoped.ty,
        .callable, .table, .node, .zero, .int => invariant("a table descriptor input has no type"),
    };
}

/// Record only callable workers the backend emitted and descriptor inputs of
/// the tables explicitly filled by their producer. Ordinary objects never request this table. `compiled`
/// is the backend producer's procedure list, not a guessed reachability scan.
pub fn encodeMetadata(retained: *Retained, result: *LirProgram.Result, compiled: []const CompiledFunction) Allocator.Error![]const u8 {
    const program = retained.program;
    program.result = result.*;
    defer result.* = program.result;
    const allocator = program.allocator;
    var encoder = Encoder{ .allocator = allocator, .program = program, .link_names = true };
    defer encoder.deinit();
    try encoder.word(@intCast(program.static_descs.unfilled_tables.items.len));
    for (program.static_descs.unfilled_tables.items) |table| {
        const count = program.descCount(table.worker);
        try encoder.word(@intCast(count));
        for (table.inputs[0..count]) |input| try encoder.typeOf(descriptorType(input));
    }
    const count_at = encoder.bytes.items.len;
    try encoder.word(0);
    var emitted: std.AutoHashMapUnmanaged(LIR.ProcIdentity, u32) = .empty;
    defer emitted.deinit(allocator);
    for (compiled) |function| try emitted.put(allocator, function.identity, function.symbol);
    var count: u32 = 0;
    // Lowering declares which workers can be function values. ARC may emit
    // additional direct-call ownership variants; those have no capture recipe
    // and do not belong in this table. Dead workers have no emitted symbol.
    for (program.workers.items, 0..) |record, index| {
        switch (record.key) {
            .template, .default, .root, .static_data, .convert => continue,
            .lambda, .adapter, .method_entry, .structural, .structural_map, .crash_dictionary, .literal_thunk, .root_slot, .codec => {},
        }
        const identity = program.result.store.getProcSpec(record.proc).identity;
        const symbol = emitted.get(identity) orelse continue;
        const id: WorkerId = @fromBackingInt(@intCast(index));
        const shape = captureShape(program, id);
        try encoder.bytes.appendSlice(allocator, &identity.bytes);
        try encoder.word(symbol);
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
                }
                try self.tableExt(record.ext);
                try self.table.append(self.allocator, @intFromBool(record.declared != null));
                if (record.declared) |declared| {
                    try self.tableTypes(store.span(declared.padding));
                    const slots = store.declaredSlots(declared);
                    try self.tableWord(@intCast(slots.len));
                    for (slots) |slot| try self.tableWord(slot);
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
                try self.tag(.template);
                try self.module(key.module);
                try self.word(@backingInt(key.template));
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
                }
                const remainder = self.ext();
                var declared: ?types.DeclaredOrderInput = null;
                if (self.byte() != 0) {
                    const padding = try self.typeSpan();
                    const slots = try program.arena.allocator().alloc(u32, self.word());
                    for (slots) |*slot| slot.* = self.word();
                    declared = .{ .padding = padding, .slots = slots };
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
    /// A dictionary composed again from `bound`, with no stored captures.
    recursive_dictionary: bool,
};

/// Read a stored function's recipe.
pub fn decode(b: *body.Builder, recipe: []const u8) Allocator.Error!Decoded {
    var decoder = Decoder{ .program = b.program, .bytes = recipe };
    decoder.readModules();
    try decoder.readTable();
    defer b.allocator.free(decoder.table);
    const worker = try decoder.worker();
    const bound = try decoder.typeSpan();
    return .{ .worker = worker, .bound = bound, .recursive_dictionary = decoder.byte() != 0 };
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
    try Write.word(&bytes, 1); // one producer table
    try Write.word(&bytes, 1); // one descriptor input
    const origin_ref = bytes.items.len;
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
    bytes.items[origin_ref] = 1;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[origin_ref] = 0;
    bytes.items[module_ref] = 1;
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
    bytes.items[module_ref] = 0;
    try bytes.append(allocator, 0);
    try std.testing.expectError(error.InvalidMetadata, MetadataView.init(bytes.items));
}

/// A worker's needs list (`tables.Needs`) as bytes another unit reads, in
/// the encoding of a stored function's recipe: its types and workers named
/// by their checked identities, never by this lowering's ids, so equal
/// needs are equal bytes. Caller frees.
pub fn encodeNeeds(allocator: Allocator, program: *Program, needs: Tables.Needs) Allocator.Error![]const u8 {
    var encoder = Encoder{ .allocator = allocator, .program = program };
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
/// compiler's: a cached entry's export records reach a lowering only once
/// their digest checks (`boxy_object_cache.decodeView`).
pub fn decodeNeeds(program: *Program, bytes: []const u8) Allocator.Error!Tables.Needs {
    const arena = program.arena.allocator();
    var decoder = Decoder{ .program = program, .bytes = bytes };
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
                break :blk .{ .args_desc = .{ .leading = leading, .elems = try decodeOperands(&decoder) } };
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
