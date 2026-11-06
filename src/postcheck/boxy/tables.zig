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
//! data too. Recipes name types by `TypeId` and workers by `WorkerId`, both
//! local to one lowering; a needs list another unit reads is encoded with
//! types and workers named by their checked identities.

const std = @import("std");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const StaticDescs = @import("static_descs.zig").StaticDescs;

const Allocator = std.mem.Allocator;
const LIR = lir_core.LIR;
const TypeId = types.TypeId;
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
    /// pointer-sized words, then values of `elems`, in declared order.
    args_desc: struct { leading: u32, elems: []const TypeId },
    /// An immortal erased callable of worker `entry` capturing `captures`.
    callable: struct { entry: WorkerId, captures: []const Operand },
    /// The table of `worker` at the inputs `inputs`.
    table: struct { worker: WorkerId, inputs: []const Operand },
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

/// The recipes one worker's lowering builds. An acyclic recipe is interned
/// by its content, so a recipe built twice is one recipe. A recipe that may
/// lead back to itself (a dictionary whose requirements capture it, or the
/// descriptor of a nominal type whose override captures that descriptor)
/// is reserved first, named by the recipes it reaches, and set afterwards;
/// a reserved recipe is never interned.
pub const Recipes = struct {
    arena: std.heap.ArenaAllocator,
    items: std.ArrayList(?Recipe) = .empty,
    interned: std.HashMapUnmanaged(Recipe, RecipeId, Context, std.hash_map.default_max_load_percentage) = .empty,

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
        try self.items.append(allocator, owned);
        try self.interned.put(allocator, owned, id);
        return id;
    }

    /// A recipe whose content `set` gives once the recipes it reaches exist.
    pub fn reserve(self: *Recipes) Allocator.Error!RecipeId {
        const id: RecipeId = @fromBackingInt(@intCast(self.items.items.len));
        try self.items.append(self.arena.child_allocator, null);
        return id;
    }

    /// Give reserved recipe `id` its content: a callable, a table, or a
    /// descriptor, the kinds that can lead back to themselves.
    pub fn set(self: *Recipes, id: RecipeId, recipe: Recipe) Allocator.Error!void {
        const slot = &self.items.items[@backingInt(id)];
        std.debug.assert(slot.* == null);
        switch (recipe) {
            .callable, .table, .desc => {},
            .input, .args_desc => invariant("a reserved recipe is not a callable, a table, or a descriptor"),
        }
        slot.* = try self.own(recipe);
    }

    /// Recipe `id`'s content, or null while it is reserved and not yet set.
    pub fn peek(self: *const Recipes, id: RecipeId) ?Recipe {
        return self.items.items[@backingInt(id)];
    }

    pub fn get(self: *const Recipes, id: RecipeId) Recipe {
        return self.items.items[@backingInt(id)] orelse invariant("a reserved recipe was read before it was set");
    }

    /// The needs list of a worker with `input_count` inputs whose table's
    /// remaining words are `slots`. Every reserved recipe has been set.
    pub fn needs(self: *Recipes, input_count: u32, slots: []const RecipeId) Allocator.Error!Needs {
        const arena = self.arena.allocator();
        const recipes = try arena.alloc(Recipe, self.items.items.len);
        for (recipes, self.items.items) |*recipe, item| recipe.* = item orelse invariant("a reserved recipe was never set");
        return .{ .input_count = input_count, .recipes = recipes, .slots = try arena.dupe(RecipeId, slots) };
    }

    /// `recipe` with its slices copied into the arena.
    fn own(self: *Recipes, recipe: Recipe) Allocator.Error!Recipe {
        const arena = self.arena.allocator();
        return switch (recipe) {
            .input => recipe,
            .desc => |desc| .{ .desc = .{ .ty = desc.ty, .scoped = try arena.dupe(ScopedOverride, desc.scoped) } },
            .args_desc => |args| .{ .args_desc = .{ .leading = args.leading, .elems = try arena.dupe(TypeId, args.elems) } },
            .callable => |callable| .{ .callable = .{ .entry = callable.entry, .captures = try arena.dupe(Operand, callable.captures) } },
            .table => |table| .{ .table = .{ .worker = table.worker, .inputs = try arena.dupe(Operand, table.inputs) } },
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
            hashWord(hasher, @intCast(args.elems.len));
            for (args.elems) |ty| hashWord(hasher, @backingInt(ty));
        },
        .callable => |callable| {
            hashWord(hasher, @backingInt(callable.entry));
            hashOperands(hasher, callable.captures);
        },
        .table => |table| {
            hashWord(hasher, @backingInt(table.worker));
            hashOperands(hasher, table.inputs);
        },
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
        .args_desc => |args| args.leading == b.args_desc.leading and std.mem.eql(TypeId, args.elems, b.args_desc.elems),
        .callable => |callable| callable.entry == b.callable.entry and operandsEqual(callable.captures, b.callable.captures),
        .table => |table| table.worker == b.table.worker and operandsEqual(table.inputs, b.table.inputs),
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

test "recipes are interned by content and reserved recipes stay distinct" {
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
    try std.testing.expect(head != other);

    // A descriptor whose override captures it: reserved, then set.
    const nominal = try recipes.reserve();
    try std.testing.expect(recipes.peek(nominal) == null);
    const method = try recipes.intern(.{ .callable = .{ .entry = worker, .captures = &.{.{ .recipe = nominal }} } });
    try recipes.set(nominal, .{ .desc = .{ .ty = ty, .scoped = &.{.{ .nominal = ty, .methods = .{ null, null, .{ .recipe = method } } }} } });
    try std.testing.expectEqual(method, recipes.get(nominal).desc.scoped[0].methods[2].?.recipe);

    const needs = try recipes.needs(2, &.{ table, head });
    try std.testing.expectEqual(@as(u32, 4), needs.wordCount());
    try std.testing.expectEqual(@as(?RecipeId, null), needs.slotRecipe(1));
    try std.testing.expectEqual(@as(?RecipeId, head), needs.slotRecipe(3));
    try std.testing.expectEqual(head, needs.recipes[@backingInt(inner)].callable.captures[0].recipe);
}

test "table words sit at consecutive pointer-sized offsets" {
    try std.testing.expectEqual(@as(u32, 0), entryOffset(0, 8));
    try std.testing.expectEqual(@as(u32, 24), entryOffset(3, 8));
    try std.testing.expectEqual(@as(u32, 12), entryOffset(3, 4));
}
