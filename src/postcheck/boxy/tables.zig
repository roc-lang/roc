//! Type tables of `.boxy` workers.
//!
//! A worker whose scheme has hidden inputs (descriptors of its type
//! variables, dictionaries, literal thunks) takes one hidden argument: the
//! address of its type table, immutable static data that the lowering fills
//! for each instantiation the program reaches. A table's words are the
//! worker's inputs, in input order, followed by the words its body reads
//! that the inputs determine: descriptors of types written over its
//! variables, dictionaries, argument-record descriptors, and the tables of
//! the workers it calls.
//!
//! A worker's needs list says what each word of its table is, as a recipe
//! over the worker's own inputs. Filling a table evaluates each recipe with
//! the instantiation's inputs, which are static, so every word is static
//! data too. A word is an address of static data, or a field's or variant's
//! position at the instantiation (design.md "Type Tables"): a generic body
//! reaches a component of an open row by position, never by name. Recipes
//! name types by `TypeId` and workers by `WorkerId`, both local to one
//! lowering; a needs list another unit reads is encoded with types and
//! workers named by their checked identities.

const std = @import("std");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const static_descs_file = @import("static_descs.zig");
const StaticDescs = static_descs_file.StaticDescs;

const Allocator = std.mem.Allocator;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const TypeId = types.TypeId;
const NameId = types.NameId;
const WorkerId = lower.WorkerId;
const invariant = types.invariant;

/// The byte offset of word `index` of a table whose words are `ptr_bytes`
/// wide: the one accessor of the table format, shared with every consumer
/// of `LIR.DynOp.table_entry`.
pub const entryOffset = LIR.tableEntryOffset;

/// Names a recipe among one worker's recipes.
pub const RecipeId = enum(u32) { _ };

/// One word of a worker's table, over the worker's inputs.
pub const Recipe = union(enum) {
    /// Input word `index` of the worker's table.
    input: u32,
    /// The descriptor of `ty`, a type over the worker's variables that
    /// mentions one of them. `scoped` holds the override callables of the
    /// nominal types in its reach whose overrides only this worker's body
    /// can build (`ScopedOverride`).
    desc: struct { ty: TypeId, scoped: []const ScopedOverride = &.{} },
    /// The descriptor of an erased call's packed argument record: `leading`
    /// pointer-sized words, then values of the types whose descriptors
    /// `elems` are, in declared order.
    args_desc: struct { leading: u32, elems: []const Operand },
    /// An immortal erased callable of worker `entry` capturing `captures`.
    callable: struct { entry: WorkerId, captures: []const Operand },
    /// The table of `worker` at the inputs `inputs`.
    table: struct { worker: WorkerId, inputs: []const Operand },
    /// The discriminant of the tag `name` under the tag union the
    /// descriptor `desc` describes: an integer word.
    tag_index: struct { desc: Operand, name: NameId },
    /// The semantic index of the field `name` of the record the descriptor
    /// `desc` describes: an integer word.
    field_index: struct { desc: Operand, name: NameId },
    /// The update plan of a record update from a base the descriptor
    /// `base` describes to a result `result` describes, the update setting
    /// the fields `names` in that order (`StaticDescs.updatePlan`).
    update_plan: struct { base: Operand, result: Operand, names: []const NameId },
    /// The coercion plan from values the descriptor `from` describes to
    /// values `to` describes (`StaticDescs.adaptPlan`).
    adapt_plan: struct { from: Operand, to: Operand },
    /// The names block of the type the descriptor `desc` describes
    /// (`StaticDescs.namesWord`): a node, or the null word for a type with
    /// no label in its reach.
    names_block: struct { desc: Operand },
};

/// What a recipe reads: another recipe of the same worker, or a static word.
pub const Operand = union(enum) {
    recipe: RecipeId,
    static: StaticDescs.CaptureWord,
};

/// The override callables a descriptor holds for the nominal type `nominal`
/// (a type over the worker's variables), in `Methods` order, where no
/// static descriptor can hold them: a method declared in a function body,
/// or one whose requirements the worker's own dictionaries discharge.
pub const ScopedOverride = struct { nominal: TypeId, methods: [3]?Operand };

/// A worker's needs list: what each word of its table is.
pub const Needs = struct {
    /// How many inputs lead the table.
    input_count: u32,
    /// How many of the inputs are the descriptors of the worker's type
    /// variables, the leading ones, in variable order.
    desc_count: u32,
    /// The worker's recipes, which `slots` and one another name.
    recipes: []const Recipe,
    /// The recipe of table word `input_count + index`.
    slots: []const RecipeId,

    /// The recipe of table word `word`; null for an input.
    pub fn slotRecipe(self: Needs, word: u32) ?RecipeId {
        if (word < self.input_count) return null;
        const index = word - self.input_count;
        if (index >= self.slots.len) invariant("a table read names a word past its worker's needs");
        return self.slots[index];
    }

    /// The number of words of the worker's tables.
    pub fn wordCount(self: Needs) u32 {
        return self.input_count + @as(u32, @intCast(self.slots.len));
    }
};

/// The recipes one worker's lowering builds, one recipe per content: a
/// recipe built twice is one recipe. A recipe that may lead back to itself
/// (a dictionary whose requirements capture it, or the descriptor of a
/// nominal type whose override captures that descriptor) is reserved first,
/// named by the recipes it reaches, and given its content afterwards; a
/// reserved recipe given a content another recipe already has is that
/// recipe (`resolve`).
pub const Recipes = struct {
    arena: std.heap.ArenaAllocator,
    items: std.ArrayList(Entry) = .empty,
    interned: std.HashMapUnmanaged(Recipe, RecipeId, Context, std.hash_map.default_max_load_percentage) = .empty,

    const Entry = union(enum) {
        reserved,
        content: Recipe,
        /// The recipe is the recipe it names.
        same: RecipeId,
    };

    pub fn init(allocator: Allocator) Recipes {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Recipes) void {
        const allocator = self.arena.child_allocator;
        self.items.deinit(allocator);
        self.interned.deinit(allocator);
        self.arena.deinit();
    }

    /// The recipe `recipe`, interned by its content. Its slices are copied.
    pub fn intern(self: *Recipes, recipe: Recipe) Allocator.Error!RecipeId {
        const allocator = self.arena.child_allocator;
        if (self.interned.get(recipe)) |existing| return existing;
        const owned = try self.own(recipe);
        const id: RecipeId = @fromBackingInt(@intCast(self.items.items.len));
        try self.items.append(allocator, .{ .content = owned });
        try self.interned.put(allocator, owned, id);
        return id;
    }

    /// A recipe whose content `set` (or `setSame`) gives once the recipes
    /// it reaches exist.
    pub fn reserve(self: *Recipes) Allocator.Error!RecipeId {
        const id: RecipeId = @fromBackingInt(@intCast(self.items.items.len));
        try self.items.append(self.arena.child_allocator, .reserved);
        return id;
    }

    /// Give reserved recipe `id` its content: a callable, a table, or a
    /// descriptor, the kinds that can lead back to themselves. Where a
    /// recipe already has that content, `id` is that recipe.
    pub fn set(self: *Recipes, id: RecipeId, recipe: Recipe) Allocator.Error!void {
        std.debug.assert(self.items.items[@backingInt(id)] == .reserved);
        switch (recipe) {
            .callable, .table, .desc => {},
            .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => invariant("a reserved recipe is not a callable, a table, or a descriptor"),
        }
        if (self.interned.get(recipe)) |existing| {
            self.items.items[@backingInt(id)] = .{ .same = existing };
            return;
        }
        const owned = try self.own(recipe);
        self.items.items[@backingInt(id)] = .{ .content = owned };
        try self.interned.put(self.arena.child_allocator, owned, id);
    }

    /// Make reserved recipe `id` the recipe `target`.
    pub fn setSame(self: *Recipes, id: RecipeId, target: RecipeId) void {
        std.debug.assert(self.items.items[@backingInt(id)] == .reserved);
        const resolved = self.resolve(target);
        if (resolved == id) invariant("a reserved recipe was made itself");
        self.items.items[@backingInt(id)] = .{ .same = resolved };
    }

    /// The recipe `id` is: itself, or the recipe whose content it was given.
    pub fn resolve(self: *const Recipes, id: RecipeId) RecipeId {
        var current = id;
        while (true) switch (self.items.items[@backingInt(current)]) {
            .same => |next| current = next,
            .reserved, .content => return current,
        };
    }

    /// Recipe `id`'s content, or null while it is reserved and not yet set.
    pub fn peek(self: *const Recipes, id: RecipeId) ?Recipe {
        return switch (self.items.items[@backingInt(self.resolve(id))]) {
            .content => |recipe| recipe,
            .reserved => null,
            .same => unreachable,
        };
    }

    pub fn get(self: *const Recipes, id: RecipeId) Recipe {
        return self.peek(id) orelse invariant("a reserved recipe was read before it was set");
    }

    /// The needs list of a worker with `input_count` inputs, the first
    /// `desc_count` its variables' descriptors, whose table's remaining words
    /// are `slots`, allocated in `allocator`: the recipes the slots reach,
    /// numbered in the order a walk from the slots first reaches them, so
    /// equal lists are equal data.
    pub fn needs(self: *const Recipes, allocator: Allocator, scratch: Allocator, input_count: u32, desc_count: u32, slots: []const RecipeId) Allocator.Error!Needs {
        const unnumbered = std.math.maxInt(u32);
        const numbers = try scratch.alloc(u32, self.items.items.len);
        defer scratch.free(numbers);
        @memset(numbers, unnumbered);
        var order: std.ArrayList(RecipeId) = .empty;
        defer order.deinit(scratch);
        var pending: std.ArrayList(RecipeId) = .empty;
        defer pending.deinit(scratch);
        var operands: std.ArrayList(Operand) = .empty;
        defer operands.deinit(scratch);
        // Each slot's walk, depth first and in operand order.
        for (slots) |slot| {
            try pending.append(scratch, self.resolve(slot));
            while (pending.pop()) |id| {
                if (numbers[@backingInt(id)] != unnumbered) continue;
                numbers[@backingInt(id)] = @intCast(order.items.len);
                try order.append(scratch, id);
                operands.clearRetainingCapacity();
                try appendOperands(scratch, &operands, self.get(id));
                var index = operands.items.len;
                while (index > 0) {
                    index -= 1;
                    switch (operands.items[index]) {
                        .recipe => |next| {
                            const resolved = self.resolve(next);
                            if (numbers[@backingInt(resolved)] == unnumbered) try pending.append(scratch, resolved);
                        },
                        .static => {},
                    }
                }
            }
        }
        const renumber = struct {
            fn operand(recipes: *const Recipes, numbered: []const u32, value: Operand) Operand {
                return switch (value) {
                    .recipe => |id| .{ .recipe = @fromBackingInt(numbered[@backingInt(recipes.resolve(id))]) },
                    .static => value,
                };
            }
            fn all(alloc: Allocator, recipes: *const Recipes, numbered: []const u32, values: []const Operand) Allocator.Error![]const Operand {
                const out = try alloc.alloc(Operand, values.len);
                for (out, values) |*slot, value| slot.* = operand(recipes, numbered, value);
                return out;
            }
        };
        const recipes = try allocator.alloc(Recipe, order.items.len);
        for (recipes, order.items) |*recipe, id| recipe.* = switch (self.get(id)) {
            .input => |index| .{ .input = index },
            .desc => |desc| blk: {
                const scoped = try allocator.alloc(ScopedOverride, desc.scoped.len);
                for (scoped, desc.scoped) |*out, override| {
                    out.* = .{ .nominal = override.nominal, .methods = undefined };
                    for (&out.methods, override.methods) |*method, maybe| method.* = if (maybe) |value| renumber.operand(self, numbers, value) else null;
                }
                break :blk .{ .desc = .{ .ty = desc.ty, .scoped = scoped } };
            },
            .args_desc => |args| .{ .args_desc = .{ .leading = args.leading, .elems = try renumber.all(allocator, self, numbers, args.elems) } },
            .callable => |callable| .{ .callable = .{ .entry = callable.entry, .captures = try renumber.all(allocator, self, numbers, callable.captures) } },
            .table => |table| .{ .table = .{ .worker = table.worker, .inputs = try renumber.all(allocator, self, numbers, table.inputs) } },
            .tag_index => |tag| .{ .tag_index = .{ .desc = renumber.operand(self, numbers, tag.desc), .name = tag.name } },
            .field_index => |field| .{ .field_index = .{ .desc = renumber.operand(self, numbers, field.desc), .name = field.name } },
            .update_plan => |update| .{ .update_plan = .{ .base = renumber.operand(self, numbers, update.base), .result = renumber.operand(self, numbers, update.result), .names = try allocator.dupe(NameId, update.names) } },
            .adapt_plan => |plan| .{ .adapt_plan = .{ .from = renumber.operand(self, numbers, plan.from), .to = renumber.operand(self, numbers, plan.to) } },
            .names_block => |block| .{ .names_block = .{ .desc = renumber.operand(self, numbers, block.desc) } },
        };
        const numbered_slots = try allocator.alloc(RecipeId, slots.len);
        for (numbered_slots, slots) |*out, slot| out.* = @fromBackingInt(numbers[@backingInt(self.resolve(slot))]);
        return .{ .input_count = input_count, .desc_count = desc_count, .recipes = recipes, .slots = numbered_slots };
    }

    /// `recipe` with its slices copied into the arena.
    fn own(self: *Recipes, recipe: Recipe) Allocator.Error!Recipe {
        const arena = self.arena.allocator();
        return switch (recipe) {
            .input => recipe,
            .desc => |desc| .{ .desc = .{ .ty = desc.ty, .scoped = try arena.dupe(ScopedOverride, desc.scoped) } },
            .args_desc => |args| .{ .args_desc = .{ .leading = args.leading, .elems = try arena.dupe(Operand, args.elems) } },
            .callable => |callable| .{ .callable = .{ .entry = callable.entry, .captures = try arena.dupe(Operand, callable.captures) } },
            .table => |table| .{ .table = .{ .worker = table.worker, .inputs = try arena.dupe(Operand, table.inputs) } },
            .tag_index, .field_index, .adapt_plan, .names_block => recipe,
            .update_plan => |update| .{ .update_plan = .{ .base = update.base, .result = update.result, .names = try arena.dupe(NameId, update.names) } },
        };
    }

    const Context = struct {
        pub fn hash(_: Context, recipe: Recipe) u64 {
            var hasher = std.hash.Wyhash.init(0);
            hashRecipe(&hasher, recipe);
            return hasher.final();
        }

        pub fn eql(_: Context, a: Recipe, b: Recipe) bool {
            return recipesEqual(a, b);
        }
    };
};

/// Append the operands `recipe` reads, in order.
pub fn appendOperands(allocator: Allocator, out: *std.ArrayList(Operand), recipe: Recipe) Allocator.Error!void {
    switch (recipe) {
        .input => {},
        .desc => |desc| for (desc.scoped) |override| {
            for (override.methods) |method| if (method) |operand| try out.append(allocator, operand);
        },
        .args_desc => |args| try out.appendSlice(allocator, args.elems),
        .callable => |callable| try out.appendSlice(allocator, callable.captures),
        .table => |table| try out.appendSlice(allocator, table.inputs),
        .tag_index => |tag| try out.append(allocator, tag.desc),
        .field_index => |field| try out.append(allocator, field.desc),
        .update_plan => |update| try out.appendSlice(allocator, &.{ update.base, update.result }),
        .adapt_plan => |plan| try out.appendSlice(allocator, &.{ plan.from, plan.to }),
        .names_block => |block| try out.append(allocator, block.desc),
    }
}

/// Where an evaluation reads a worker's recipes: the recipes its lowering
/// is building, or its needs list.
pub const Source = union(enum) {
    building: *const Recipes,
    needs: *const Needs,

    fn get(self: Source, id: RecipeId) Recipe {
        return switch (self) {
            .building => |recipes| recipes.get(id),
            .needs => |needs| needs.recipes[@backingInt(id)],
        };
    }

    fn resolve(self: Source, id: RecipeId) RecipeId {
        return switch (self) {
            .building => |recipes| recipes.resolve(id),
            .needs => id,
        };
    }

    fn count(self: Source) usize {
        return switch (self) {
            .building => |recipes| recipes.items.items.len,
            .needs => |needs| needs.recipes.len,
        };
    }
};

/// The words a worker's recipes are at one instantiation, whose inputs are
/// static words: each recipe evaluated once, a group of recipes that lead
/// back to one another (`Recipes`) evaluated together. A cyclic group is
/// interned by its content program-wide, so equal groups are the same
/// static data wherever they are evaluated, and the tables keyed by their
/// words are too.
pub const Evaluation = struct {
    program: *lower.Program,
    allocator: Allocator,
    source: Source,
    inputs: []const StaticDescs.CaptureWord,
    /// How many leading inputs are the worker's variables' descriptors.
    desc_count: u32,
    words: std.ArrayList(?StaticDescs.CaptureWord) = .empty,
    /// The search's order of visit and low link of each recipe it visited,
    /// and whether it is on the search's stack.
    visit: std.ArrayList(u32) = .empty,
    low: std.ArrayList(u32) = .empty,
    on_stack: std.ArrayList(bool) = .empty,
    stack: std.ArrayList(RecipeId) = .empty,
    visited: u32 = 0,

    const unvisited = std.math.maxInt(u32);

    pub fn init(program: *lower.Program, source: Source, inputs: []const StaticDescs.CaptureWord, desc_count: u32) Evaluation {
        return .{ .program = program, .allocator = program.allocator, .source = source, .inputs = inputs, .desc_count = desc_count };
    }

    pub fn deinit(self: *Evaluation) void {
        self.words.deinit(self.allocator);
        self.visit.deinit(self.allocator);
        self.low.deinit(self.allocator);
        self.on_stack.deinit(self.allocator);
        self.stack.deinit(self.allocator);
    }

    /// The word of recipe `named` at this instantiation.
    pub fn word(self: *Evaluation, named: RecipeId) Allocator.Error!StaticDescs.CaptureWord {
        const id = self.source.resolve(named);
        try self.grow();
        if (self.words.items[@backingInt(id)]) |known| return known;
        try self.search(id);
        return self.words.items[@backingInt(id)] orelse invariant("a table recipe was searched but not evaluated");
    }

    /// The word of `operand`, whose recipe has been evaluated.
    fn operandWord(self: *const Evaluation, operand: Operand) StaticDescs.CaptureWord {
        return switch (operand) {
            .static => |static| static,
            .recipe => |id| self.words.items[@backingInt(self.source.resolve(id))] orelse invariant("a table recipe reads a recipe evaluated after it"),
        };
    }

    fn grow(self: *Evaluation) Allocator.Error!void {
        const count = self.source.count();
        const old = self.words.items.len;
        if (count == old) return;
        try self.words.resize(self.allocator, count);
        try self.visit.resize(self.allocator, count);
        try self.low.resize(self.allocator, count);
        try self.on_stack.resize(self.allocator, count);
        @memset(self.words.items[old..], null);
        @memset(self.visit.items[old..], unvisited);
        @memset(self.on_stack.items[old..], false);
    }

    const Frame = struct { id: RecipeId, operands: []const Operand, next: usize };

    /// Evaluate every recipe `root` reaches that has no word yet, a strongly
    /// connected group at a time, each after the groups it reads (Tarjan's
    /// search, with an explicit stack).
    fn search(self: *Evaluation, root: RecipeId) Allocator.Error!void {
        var frames: std.ArrayList(Frame) = .empty;
        defer {
            for (frames.items) |frame| self.allocator.free(frame.operands);
            frames.deinit(self.allocator);
        }
        try self.enter(&frames, root);
        while (frames.items.len != 0) {
            const top = &frames.items[frames.items.len - 1];
            if (top.next < top.operands.len) {
                const operand = top.operands[top.next];
                top.next += 1;
                const next = switch (operand) {
                    .recipe => |id| self.source.resolve(id),
                    .static => continue,
                };
                if (self.words.items[@backingInt(next)] != null) continue;
                if (self.visit.items[@backingInt(next)] == unvisited) {
                    try self.enter(&frames, next);
                } else if (self.on_stack.items[@backingInt(next)]) {
                    const from = @backingInt(top.id);
                    self.low.items[from] = @min(self.low.items[from], self.visit.items[@backingInt(next)]);
                }
                continue;
            }
            const done = frames.pop().?;
            self.allocator.free(done.operands);
            const id = @backingInt(done.id);
            if (frames.items.len != 0) {
                const parent = @backingInt(frames.items[frames.items.len - 1].id);
                self.low.items[parent] = @min(self.low.items[parent], self.low.items[id]);
            }
            if (self.low.items[id] != self.visit.items[id]) continue;
            // `done` heads a group: the stack's recipes from it on.
            const start = std.mem.findScalarLast(RecipeId, self.stack.items, done.id) orelse invariant("a searched recipe left the search's stack");
            const members = try self.allocator.dupe(RecipeId, self.stack.items[start..]);
            defer self.allocator.free(members);
            self.stack.shrinkRetainingCapacity(start);
            for (members) |member| self.on_stack.items[@backingInt(member)] = false;
            try self.evaluateGroup(members);
        }
    }

    fn enter(self: *Evaluation, frames: *std.ArrayList(Frame), id: RecipeId) Allocator.Error!void {
        var operands: std.ArrayList(Operand) = .empty;
        errdefer operands.deinit(self.allocator);
        try appendOperands(self.allocator, &operands, self.source.get(id));
        self.visit.items[@backingInt(id)] = self.visited;
        self.low.items[@backingInt(id)] = self.visited;
        self.visited += 1;
        self.on_stack.items[@backingInt(id)] = true;
        try self.stack.append(self.allocator, id);
        const owned = try operands.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(owned);
        try frames.append(self.allocator, .{ .id = id, .operands = owned, .next = 0 });
    }

    /// Evaluate the strongly connected group `members`.
    fn evaluateGroup(self: *Evaluation, members: []const RecipeId) Allocator.Error!void {
        if (members.len == 1 and !try self.readsItself(members[0])) {
            self.words.items[@backingInt(members[0])] = try self.evaluateOne(self.source.get(members[0]));
            return;
        }
        try self.evaluateCycle(members);
    }

    fn readsItself(self: *Evaluation, id: RecipeId) Allocator.Error!bool {
        var operands: std.ArrayList(Operand) = .empty;
        defer operands.deinit(self.allocator);
        try appendOperands(self.allocator, &operands, self.source.get(id));
        for (operands.items) |operand| switch (operand) {
            .recipe => |next| if (self.source.resolve(next) == id) return true,
            .static => {},
        };
        return false;
    }

    /// The word of `recipe`, every recipe it reads evaluated.
    fn evaluateOne(self: *Evaluation, recipe: Recipe) Allocator.Error!StaticDescs.CaptureWord {
        const program = self.program;
        return switch (recipe) {
            .input => |index| if (index < self.inputs.len) self.inputs[index] else invariant("a table recipe reads an input its instantiation lacks"),
            .desc => |desc| try self.descWord(desc.ty, desc.scoped),
            .args_desc => |args| blk: {
                const elems = try self.allocator.alloc(TypeId, args.elems.len);
                defer self.allocator.free(elems);
                var entries: std.ArrayList(StaticDescs.ScopedMethods) = .empty;
                defer entries.deinit(self.allocator);
                for (elems, args.elems) |*elem, operand| {
                    const elem_word = self.operandWord(operand);
                    elem.* = describedType(elem_word);
                    try self.appendScope(&entries, elem_word);
                }
                const scope = try program.static_descs.internScope(entries.items);
                break :blk .{ .node = (try program.static_descs.argsDescriptor(args.leading, elems, scope)).node };
            },
            .callable => |callable| blk: {
                const words = try self.operandWords(callable.captures);
                defer self.allocator.free(words);
                break :blk .{ .callable = (try program.static_descs.dictionary(program.worker(callable.entry).proc, words)).node };
            },
            .table => |table| blk: {
                const words = try self.operandWords(table.inputs);
                defer self.allocator.free(words);
                break :blk .{ .table = (try program.static_descs.table(table.worker, words)).node };
            },
            .tag_index => |tag| .{ .int = try program.static_descs.variantPosition(describedType(self.operandWord(tag.desc)), tag.name) },
            .field_index => |field| .{ .int = try program.static_descs.fieldPosition(describedType(self.operandWord(field.desc)), field.name) },
            .update_plan => |update| .{ .node = try program.static_descs.updatePlan(describedType(self.operandWord(update.base)), describedType(self.operandWord(update.result)), update.names) },
            .adapt_plan => |plan| .{ .node = try program.static_descs.adaptPlan(self.operandWord(plan.from), self.operandWord(plan.to)) },
            .names_block => |block| try program.static_descs.namesWord(self.operandWord(block.desc)),
        };
    }

    fn operandWords(self: *Evaluation, operands: []const Operand) Allocator.Error![]StaticDescs.CaptureWord {
        const words = try self.allocator.alloc(StaticDescs.CaptureWord, operands.len);
        for (words, operands) |*out, operand| out.* = self.operandWord(operand);
        return words;
    }

    /// The type a descriptor word describes.
    fn describedType(word_: StaticDescs.CaptureWord) TypeId {
        return switch (word_) {
            .desc => |ty| ty,
            .scoped_desc => |scoped| scoped.ty,
            .callable, .table, .node, .zero, .int => invariant("a type table's descriptor word is not a descriptor"),
        };
    }

    /// Append the override scope of descriptor word `word_`'s entries,
    /// merging those `entries` has.
    fn appendScope(self: *Evaluation, entries: *std.ArrayList(StaticDescs.ScopedMethods), word_: StaticDescs.CaptureWord) Allocator.Error!void {
        const scope = switch (word_) {
            .scoped_desc => |scoped| scoped.scope,
            .desc => return,
            .callable, .table, .node, .zero, .int => invariant("a type table's descriptor word is not a descriptor"),
        };
        for (self.program.static_descs.scopeEntries(scope)) |entry| try mergeEntry(self.allocator, entries, entry);
    }

    /// The descriptor word of the type `ty` over the worker's variables at
    /// this instantiation: the type with each variable at the type its
    /// descriptor input describes, in the override scope of those inputs and
    /// of `scoped`.
    fn descWord(self: *Evaluation, ty: TypeId, scoped: []const ScopedOverride) Allocator.Error!StaticDescs.CaptureWord {
        const program = self.program;
        if (self.desc_count > self.inputs.len) invariant("a table's instantiation lacks a variable's descriptor");
        const bound = try self.allocator.alloc(TypeId, self.desc_count);
        defer self.allocator.free(bound);
        var entries: std.ArrayList(StaticDescs.ScopedMethods) = .empty;
        defer entries.deinit(self.allocator);
        for (bound, self.inputs[0..self.desc_count]) |*slot, input| {
            slot.* = describedType(input);
            try self.appendScope(&entries, input);
        }
        const closed = try program.substitute(self.allocator, ty, bound);
        if (!program.store.isClosed(closed)) invariant("a table's descriptor recipe is open at its instantiation");
        for (scoped) |override| {
            var methods: [3]?StaticDescs.CaptureWord = .{ null, null, null };
            for (&methods, override.methods) |*method, maybe| method.* = if (maybe) |operand| self.operandWord(operand) else null;
            try mergeEntry(self.allocator, &entries, .{ .nominal = try program.substitute(self.allocator, override.nominal, bound), .methods = methods });
        }
        const scope = try program.static_descs.restrictScope(try program.static_descs.internScope(entries.items), closed);
        if (scope == .none) return .{ .desc = closed };
        return .{ .scoped_desc = .{ .ty = closed, .scope = scope } };
    }

    /// Evaluate the cyclic group `members`, from the group interned under
    /// its content if one was, otherwise reserving its callables, then
    /// evaluating its descriptors and tables, which read those callables,
    /// then filling the callables. The group's content is its smallest key
    /// over each member as head, so it is the same whichever member the
    /// search entered the group by.
    fn evaluateCycle(self: *Evaluation, members: []const RecipeId) Allocator.Error!void {
        const program = self.program;
        const static_descs = &program.static_descs;
        var order = try self.groupOrder(members, members[0]);
        defer self.allocator.free(order);
        var key: std.ArrayList(u8) = .empty;
        defer key.deinit(self.allocator);
        try self.groupKey(order, &key);
        var candidate_key: std.ArrayList(u8) = .empty;
        defer candidate_key.deinit(self.allocator);
        for (members[1..]) |head| {
            const candidate = try self.groupOrder(members, head);
            errdefer self.allocator.free(candidate);
            candidate_key.clearRetainingCapacity();
            try self.groupKey(candidate, &candidate_key);
            if (std.mem.order(u8, candidate_key.items, key.items) != .lt) {
                self.allocator.free(candidate);
                continue;
            }
            self.allocator.free(order);
            order = candidate;
            std.mem.swap(std.ArrayList(u8), &key, &candidate_key);
        }
        if (static_descs.group(key.items)) |words| {
            for (order, words) |member, member_word| self.words.items[@backingInt(member)] = member_word;
            return;
        }
        for (order) |member| switch (self.source.get(member)) {
            .callable => |callable| self.words.items[@backingInt(member)] = .{ .callable = try static_descs.reserveCallable(@intCast(callable.captures.len)) },
            .desc, .table => {},
            .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => invariant("only callables, descriptors, and tables lead back to themselves"),
        };
        for (order) |member| switch (self.source.get(member)) {
            .desc => |desc| self.words.items[@backingInt(member)] = try self.descWord(desc.ty, desc.scoped),
            .callable, .table, .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => {},
        };
        for (order) |member| switch (self.source.get(member)) {
            .table => |table| self.words.items[@backingInt(member)] = try self.evaluateOne(.{ .table = table }),
            .callable, .desc, .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => {},
        };
        for (order) |member| switch (self.source.get(member)) {
            .callable => |callable| {
                const words = try self.operandWords(callable.captures);
                defer self.allocator.free(words);
                try static_descs.fillCallable(self.words.items[@backingInt(member)].?.callable, program.worker(callable.entry).proc, words);
            },
            .desc, .table, .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => {},
        };
        const words = try self.allocator.alloc(StaticDescs.CaptureWord, order.len);
        defer self.allocator.free(words);
        for (words, order) |*out, member| out.* = self.words.items[@backingInt(member)].?;
        try static_descs.putGroup(key.items, words);
    }

    /// The members of a group in the order a walk from `head` first reaches
    /// them through its own members, in operand order.
    fn groupOrder(self: *Evaluation, members: []const RecipeId, head: RecipeId) Allocator.Error![]RecipeId {
        var order: std.ArrayList(RecipeId) = .empty;
        errdefer order.deinit(self.allocator);
        var pending: std.ArrayList(RecipeId) = .empty;
        defer pending.deinit(self.allocator);
        var operands: std.ArrayList(Operand) = .empty;
        defer operands.deinit(self.allocator);
        try pending.append(self.allocator, head);
        while (pending.pop()) |id| {
            if (std.mem.findScalar(RecipeId, order.items, id) != null) continue;
            try order.append(self.allocator, id);
            operands.clearRetainingCapacity();
            try appendOperands(self.allocator, &operands, self.source.get(id));
            var index = operands.items.len;
            while (index > 0) {
                index -= 1;
                const next = switch (operands.items[index]) {
                    .recipe => |next| self.source.resolve(next),
                    .static => continue,
                };
                if (std.mem.findScalar(RecipeId, members, next) != null and std.mem.findScalar(RecipeId, order.items, next) == null) try pending.append(self.allocator, next);
            }
        }
        if (order.items.len != members.len) invariant("a recipe group's member does not reach every member");
        return try order.toOwnedSlice(self.allocator);
    }

    /// The content of the group `order`: each member's kind and fields, an
    /// operand inside the group by its position in `order`, any other by
    /// its word, and a type at this instantiation.
    fn groupKey(self: *Evaluation, order: []const RecipeId, key: *std.ArrayList(u8)) Allocator.Error!void {
        const allocator = self.allocator;
        const program = self.program;
        const bound = try allocator.alloc(TypeId, self.desc_count);
        defer allocator.free(bound);
        for (bound, self.inputs[0..self.desc_count]) |*slot, input| slot.* = describedType(input);
        for (order) |member| {
            const recipe = self.source.get(member);
            try key.append(allocator, @backingInt(std.meta.activeTag(recipe)));
            switch (recipe) {
                .desc => |desc| {
                    try static_descs_file.appendKeyWord(allocator, key, @backingInt(try program.substitute(allocator, desc.ty, bound)));
                    // The variables' scopes are part of the descriptor.
                    for (self.inputs[0..self.desc_count]) |input| try static_descs_file.appendCaptureWordKey(allocator, key, input);
                    try static_descs_file.appendKeyWord(allocator, key, @intCast(desc.scoped.len));
                    for (desc.scoped) |override| {
                        try static_descs_file.appendKeyWord(allocator, key, @backingInt(try program.substitute(allocator, override.nominal, bound)));
                        for (override.methods) |method| {
                            if (method) |operand| try self.operandKey(order, operand, key) else try key.append(allocator, 2);
                        }
                    }
                },
                .callable => |callable| {
                    try static_descs_file.appendKeyWord(allocator, key, @backingInt(callable.entry));
                    try static_descs_file.appendKeyWord(allocator, key, @intCast(callable.captures.len));
                    for (callable.captures) |operand| try self.operandKey(order, operand, key);
                },
                .table => |table| {
                    try static_descs_file.appendKeyWord(allocator, key, @backingInt(table.worker));
                    try static_descs_file.appendKeyWord(allocator, key, @intCast(table.inputs.len));
                    for (table.inputs) |operand| try self.operandKey(order, operand, key);
                },
                .input, .args_desc, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => invariant("only callables, descriptors, and tables lead back to themselves"),
            }
        }
    }

    fn operandKey(self: *Evaluation, order: []const RecipeId, operand: Operand, key: *std.ArrayList(u8)) Allocator.Error!void {
        switch (operand) {
            .recipe => |id| if (std.mem.findScalar(RecipeId, order, self.source.resolve(id))) |position| {
                try key.append(self.allocator, 0);
                return try static_descs_file.appendKeyWord(self.allocator, key, @intCast(position));
            },
            .static => {},
        }
        try key.append(self.allocator, 1);
        try static_descs_file.appendCaptureWordKey(self.allocator, key, self.operandWord(operand));
    }
};

/// Add `entry` to the scope entries `entries`, which hold each nominal type
/// once: an entry for a nominal type already there gives it the same
/// overrides.
fn mergeEntry(allocator: Allocator, entries: *std.ArrayList(StaticDescs.ScopedMethods), entry: StaticDescs.ScopedMethods) Allocator.Error!void {
    for (entries.items) |existing| {
        if (existing.nominal != entry.nominal) continue;
        for (existing.methods, entry.methods) |a, b| {
            if ((a == null) != (b == null)) invariant("two override scopes give one nominal type different overrides");
            if (a) |word_| if (!StaticDescs.captureWordsEqual(word_, b.?)) invariant("two override scopes give one nominal type different overrides");
        }
        return;
    }
    try entries.append(allocator, entry);
}

fn hashWord(hasher: *std.hash.Wyhash, value: u32) void {
    hasher.update(std.mem.asBytes(&value));
}

fn hashRecipe(hasher: *std.hash.Wyhash, recipe: Recipe) void {
    hashWord(hasher, @backingInt(std.meta.activeTag(recipe)));
    switch (recipe) {
        .input => |index| hashWord(hasher, index),
        .desc => |desc| {
            hashWord(hasher, @backingInt(desc.ty));
            hashWord(hasher, @intCast(desc.scoped.len));
            for (desc.scoped) |override| {
                hashWord(hasher, @backingInt(override.nominal));
                for (override.methods) |method| {
                    if (method) |operand| hashOperand(hasher, operand) else hashWord(hasher, std.math.maxInt(u32));
                }
            }
        },
        .args_desc => |args| {
            hashWord(hasher, args.leading);
            hashOperands(hasher, args.elems);
        },
        .callable => |callable| {
            hashWord(hasher, @backingInt(callable.entry));
            hashOperands(hasher, callable.captures);
        },
        .table => |table| {
            hashWord(hasher, @backingInt(table.worker));
            hashOperands(hasher, table.inputs);
        },
        .tag_index => |tag| {
            hashOperand(hasher, tag.desc);
            hashWord(hasher, @backingInt(tag.name));
        },
        .field_index => |field| {
            hashOperand(hasher, field.desc);
            hashWord(hasher, @backingInt(field.name));
        },
        .update_plan => |update| {
            hashOperand(hasher, update.base);
            hashOperand(hasher, update.result);
            hashWord(hasher, @intCast(update.names.len));
            for (update.names) |name| hashWord(hasher, @backingInt(name));
        },
        .adapt_plan => |plan| {
            hashOperand(hasher, plan.from);
            hashOperand(hasher, plan.to);
        },
        .names_block => |block| hashOperand(hasher, block.desc),
    }
}

fn hashOperands(hasher: *std.hash.Wyhash, operands: []const Operand) void {
    hashWord(hasher, @intCast(operands.len));
    for (operands) |operand| hashOperand(hasher, operand);
}

fn hashOperand(hasher: *std.hash.Wyhash, operand: Operand) void {
    switch (operand) {
        .recipe => |id| {
            hashWord(hasher, 0);
            hashWord(hasher, @backingInt(id));
        },
        .static => |word| {
            hashWord(hasher, 1);
            StaticDescs.hashCaptureWord(hasher, word);
        },
    }
}

fn recipesEqual(a: Recipe, b: Recipe) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .input => |index| index == b.input,
        .desc => |desc| desc.ty == b.desc.ty and scopedEqual(desc.scoped, b.desc.scoped),
        .args_desc => |args| args.leading == b.args_desc.leading and operandsEqual(args.elems, b.args_desc.elems),
        .callable => |callable| callable.entry == b.callable.entry and operandsEqual(callable.captures, b.callable.captures),
        .table => |table| table.worker == b.table.worker and operandsEqual(table.inputs, b.table.inputs),
        .tag_index => |tag| tag.name == b.tag_index.name and operandEqual(tag.desc, b.tag_index.desc),
        .field_index => |field| field.name == b.field_index.name and operandEqual(field.desc, b.field_index.desc),
        .update_plan => |update| operandEqual(update.base, b.update_plan.base) and operandEqual(update.result, b.update_plan.result) and std.mem.eql(NameId, update.names, b.update_plan.names),
        .adapt_plan => |plan| operandEqual(plan.from, b.adapt_plan.from) and operandEqual(plan.to, b.adapt_plan.to),
        .names_block => |block| operandEqual(block.desc, b.names_block.desc),
    };
}

fn scopedEqual(a: []const ScopedOverride, b: []const ScopedOverride) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left.nominal != right.nominal) return false;
        for (left.methods, right.methods) |left_method, right_method| {
            if ((left_method == null) != (right_method == null)) return false;
            if (left_method) |operand| if (!operandEqual(operand, right_method.?)) return false;
        }
    }
    return true;
}

fn operandsEqual(a: []const Operand, b: []const Operand) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| if (!operandEqual(left, right)) return false;
    return true;
}

fn operandEqual(a: Operand, b: Operand) bool {
    return switch (a) {
        .recipe => |id| b == .recipe and b.recipe == id,
        .static => |word| b == .static and StaticDescs.captureWordsEqual(word, b.static),
    };
}

test "recipes are interned by content, reserved ones included" {
    var recipes = Recipes.init(std.testing.allocator);
    defer recipes.deinit();
    const ty: TypeId = @fromBackingInt(7);
    const desc = try recipes.intern(.{ .desc = .{ .ty = ty } });
    try std.testing.expectEqual(desc, try recipes.intern(.{ .desc = .{ .ty = ty } }));
    const input = try recipes.intern(.{ .input = 0 });
    const worker: WorkerId = @fromBackingInt(3);
    const table = try recipes.intern(.{ .table = .{ .worker = worker, .inputs = &.{ .{ .recipe = input }, .{ .recipe = desc } } } });
    try std.testing.expectEqual(table, try recipes.intern(.{ .table = .{ .worker = worker, .inputs = &.{ .{ .recipe = input }, .{ .recipe = desc } } } }));
    try std.testing.expect(table != try recipes.intern(.{ .table = .{ .worker = worker, .inputs = &.{ .{ .recipe = desc }, .{ .recipe = input } } } }));

    // A dictionary capturing itself: reserved, named by what it reaches,
    // then set.
    const head = try recipes.reserve();
    const inner = try recipes.intern(.{ .callable = .{ .entry = worker, .captures = &.{.{ .recipe = head }} } });
    try recipes.set(head, .{ .callable = .{ .entry = worker, .captures = &.{ .{ .recipe = inner }, .{ .static = .zero } } } });
    const other = try recipes.reserve();
    try recipes.set(other, .{ .callable = .{ .entry = worker, .captures = &.{ .{ .recipe = inner }, .{ .static = .zero } } } });
    // A reserved recipe given another's content is that recipe.
    try std.testing.expectEqual(head, recipes.resolve(other));
    try std.testing.expectEqual(head, recipes.resolve(head));
    const alias = try recipes.reserve();
    recipes.setSame(alias, other);
    try std.testing.expectEqual(head, recipes.resolve(alias));
    try std.testing.expectEqual(inner, recipes.get(alias).callable.captures[0].recipe);

    // A descriptor whose override captures it: reserved, then set.
    const nominal = try recipes.reserve();
    try std.testing.expect(recipes.peek(nominal) == null);
    const method = try recipes.intern(.{ .callable = .{ .entry = worker, .captures = &.{.{ .recipe = nominal }} } });
    try recipes.set(nominal, .{ .desc = .{ .ty = ty, .scoped = &.{.{ .nominal = ty, .methods = .{ null, null, .{ .recipe = method } } }} } });
    try std.testing.expectEqual(method, recipes.get(nominal).desc.scoped[0].methods[2].?.recipe);

    // The needs list holds what the slots reach, numbered by a walk from
    // them: the table, its input and descriptor, then the dictionary and
    // the callable it captures.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const needs = try recipes.needs(arena.allocator(), std.testing.allocator, 2, 1, &.{ table, alias });
    try std.testing.expectEqual(@as(u32, 4), needs.wordCount());
    try std.testing.expectEqual(@as(usize, 5), needs.recipes.len);
    try std.testing.expectEqual(@as(?RecipeId, null), needs.slotRecipe(1));
    try std.testing.expectEqual(@as(?RecipeId, @fromBackingInt(0)), needs.slotRecipe(2));
    try std.testing.expectEqual(@as(?RecipeId, @fromBackingInt(3)), needs.slotRecipe(3));
    try std.testing.expectEqual(@as(u32, 0), needs.recipes[1].input);
    try std.testing.expectEqual(@as(RecipeId, @fromBackingInt(3)), needs.recipes[4].callable.captures[0].recipe);
    try std.testing.expectEqual(@as(RecipeId, @fromBackingInt(4)), needs.recipes[3].callable.captures[0].recipe);
}

test "a cyclic group has the same words whichever member its evaluation enters by" {
    const allocator = std.testing.allocator;
    // An evaluation reads the program's allocator, store, layouts, result,
    // static data and workers; every other field stays unread.
    var program: lower.Program = undefined;
    program.allocator = allocator;
    program.store = types.Store.init(allocator);
    defer program.store.deinit();
    program.modules = .{ .allocator = allocator };
    defer program.modules.deinit();
    program.result = try LirProgram.Result.init(allocator, .u64);
    defer program.result.deinit();
    program.layouts = .{
        .allocator = allocator,
        .store = &program.store,
        .converter = .{ .store = &program.store, .modules = &program.modules },
        .layouts = &program.result.layouts,
    };
    defer program.layouts.deinit();
    program.static_descs = StaticDescs.init(allocator, &program.store, &program.layouts, &program.result, .u64);
    defer program.static_descs.deinit();
    program.workers = .empty;
    defer program.workers.deinit(allocator);

    const store = &program.result.store;
    const target = try store.addLocal(.{ .layout_idx = .u8 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = target } }, .test_fixture);
    const proc = try store.addProcSpec(.{ .name = .fromRaw(0), .identity = LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = try store.addLocalSpan(&.{target}), .body = ret, .ret_layout = .u8 }, .none);
    // The evaluation reads a callable's worker for its procedure alone.
    var entry: lower.Worker = undefined;
    entry.proc = proc;
    try program.workers.append(allocator, entry);

    // A dictionary capturing a table whose input is that dictionary.
    var recipes = Recipes.init(allocator);
    defer recipes.deinit();
    const callable = try recipes.reserve();
    const table = try recipes.intern(.{ .table = .{ .worker = @fromBackingInt(1), .inputs = &.{.{ .recipe = callable }} } });
    try recipes.set(callable, .{ .callable = .{ .entry = @fromBackingInt(0), .captures = &.{.{ .recipe = table }} } });

    var from_callable = Evaluation.init(&program, .{ .building = &recipes }, &.{}, 0);
    defer from_callable.deinit();
    const callable_word = try from_callable.word(callable);
    const table_word = try from_callable.word(table);

    var from_table = Evaluation.init(&program, .{ .building = &recipes }, &.{}, 0);
    defer from_table.deinit();
    try std.testing.expect(StaticDescs.captureWordsEqual(table_word, try from_table.word(table)));
    try std.testing.expect(StaticDescs.captureWordsEqual(callable_word, try from_table.word(callable)));
}

test "position and plan recipes evaluate to the instantiation's positions and plans" {
    const allocator = std.testing.allocator;
    // An evaluation of these recipes reads the program's allocator, store,
    // layouts, result, and static data; every other field stays unread.
    var program: lower.Program = undefined;
    program.allocator = allocator;
    program.store = types.Store.init(allocator);
    defer program.store.deinit();
    program.modules = .{ .allocator = allocator };
    defer program.modules.deinit();
    program.result = try LirProgram.Result.init(allocator, .u64);
    defer program.result.deinit();
    program.layouts = .{
        .allocator = allocator,
        .store = &program.store,
        .converter = .{ .store = &program.store, .modules = &program.modules },
        .layouts = &program.result.layouts,
    };
    defer program.layouts.deinit();
    program.static_descs = StaticDescs.init(allocator, &program.store, &program.layouts, &program.result, .u64);
    defer program.static_descs.deinit();
    const store = &program.store;
    const desc_format = @import("boxy_runtime").desc;

    const number = try store.primitive(.u64);
    const text = try store.primitive(.str);
    const a = try store.internName("A");
    const b = try store.internName("B");
    const c = try store.internName("C");
    const narrow = try store.intern(.{ .tag_union = .{ .tags = &.{ .{ .name = a, .payloads = &.{} }, .{ .name = c, .payloads = &.{number} } }, .ext = null } });
    const wide = try store.intern(.{ .tag_union = .{ .tags = &.{ .{ .name = a, .payloads = &.{} }, .{ .name = b, .payloads = &.{} }, .{ .name = c, .payloads = &.{number} } }, .ext = null } });
    const x = try store.internName("x");
    const y = try store.internName("y");
    const record = try store.intern(.{ .record = .{ .fields = &.{
        .{ .name = x, .ty = number, .kind = .inline_slot },
        .{ .name = y, .ty = text, .kind = .inline_slot },
    }, .ext = null } });

    var recipes = Recipes.init(allocator);
    defer recipes.deinit();
    const input = try recipes.intern(.{ .input = 0 });
    // A position over a static descriptor, and one over the worker's
    // variable, whose instantiation is the wider union.
    const static_tag = try recipes.intern(.{ .tag_index = .{ .desc = .{ .static = .{ .desc = narrow } }, .name = c } });
    const input_tag = try recipes.intern(.{ .tag_index = .{ .desc = .{ .recipe = input }, .name = c } });
    const field = try recipes.intern(.{ .field_index = .{ .desc = .{ .static = .{ .desc = record } }, .name = y } });
    const update = try recipes.intern(.{ .update_plan = .{ .base = .{ .static = .{ .desc = record } }, .result = .{ .static = .{ .desc = record } }, .names = &.{y} } });
    const widen = try recipes.intern(.{ .adapt_plan = .{ .from = .{ .static = .{ .desc = narrow } }, .to = .{ .recipe = input } } });

    var evaluation = Evaluation.init(&program, .{ .building = &recipes }, &.{.{ .desc = wide }}, 1);
    defer evaluation.deinit();
    try std.testing.expectEqual(@as(u32, 1), (try evaluation.word(static_tag)).int);
    try std.testing.expectEqual(@as(u32, 2), (try evaluation.word(input_tag)).int);
    try std.testing.expectEqual(@as(u32, 1), (try evaluation.word(field)).int);

    // The update takes `y` from its first value and copies `x` from the
    // base's slot for it.
    const update_node = program.static_descs.exports.items[(try evaluation.word(update)).node];
    const record_layout = program.result.layouts.getLayout(try program.layouts.layoutOf(record));
    const struct_idx = record_layout.getStruct().idx;
    for (0..2) |slot| {
        const entry = std.mem.readInt(u32, update_node.bytes[slot * 4 ..][0..4], .little);
        const semantic = program.result.layouts.getStructField(struct_idx, @intCast(slot)).index;
        if (semantic == 1) {
            try std.testing.expectEqual(desc_format.update_value_flag, entry);
        } else {
            try std.testing.expectEqual(@as(u32, @intCast(slot)), entry);
        }
    }

    // Widening `[A, C(U64)]` to `[A, B, C(U64)]` re-tags A to 0 and C to 2,
    // copying C's payload.
    const plan_node = program.static_descs.exports.items[(try evaluation.word(widen)).node];
    const plan_layout = desc_format.AdaptPlan.layout(.u64);
    try std.testing.expectEqual(@as(u8, @backingInt(desc_format.AdaptKind.retag)), plan_node.bytes[plan_layout.of(.kind)]);
    try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, plan_node.bytes[plan_layout.of(.count)..][0..4], .little));
    const entries_node = for (plan_node.relocations) |relocation| {
        if (relocation.offset == plan_layout.of(.entries)) break program.static_descs.exports.items[@backingInt(relocation.target.data_symbol)];
    } else return error.TestUnexpectedResult;
    const variant_layout = desc_format.AdaptVariant.layout(.u64);
    try std.testing.expectEqual(@as(u32, 0), std.mem.readInt(u32, entries_node.bytes[variant_layout.of(.discriminant)..][0..4], .little));
    try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, entries_node.bytes[variant_layout.size + variant_layout.of(.discriminant) ..][0..4], .little));
    // A has no payload plan; C's is a copy.
    try std.testing.expectEqual(@as(usize, 1), entries_node.relocations.len);
    try std.testing.expectEqual(variant_layout.size + variant_layout.of(.plan), entries_node.relocations[0].offset);
    const payload_plan = program.static_descs.exports.items[@backingInt(entries_node.relocations[0].target.data_symbol)];
    try std.testing.expectEqual(@as(u8, @backingInt(desc_format.AdaptKind.copy)), payload_plan.bytes[plan_layout.of(.kind)]);

    // A recipe over no input folds where it is lowered: a static position.
    var fold = Evaluation.init(&program, .{ .building = &recipes }, &.{}, 0);
    defer fold.deinit();
    try std.testing.expectEqual(@as(u32, 1), (try fold.word(static_tag)).int);
}

test "table words sit at consecutive pointer-sized offsets" {
    try std.testing.expectEqual(@as(u32, 0), entryOffset(0, 8));
    try std.testing.expectEqual(@as(u32, 24), entryOffset(3, 8));
    try std.testing.expectEqual(@as(u32, 12), entryOffset(3, 4));
}
