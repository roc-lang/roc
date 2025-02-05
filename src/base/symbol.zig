const std = @import("std");
const base = @import("../base.zig");
const cols = @import("../collections.zig");
const problem = @import("../problem.zig");
const module = @import("module.zig");

pub const IdentId = packed struct(u32) {
    attributes: IdentAttributes,
    id: u29,
};

pub const Ident = struct {
    raw_text: []u8,
    attributes: IdentAttributes,
    problems: IdentProblems,

    pub fn for_text(text: []u8) Ident {
        return Ident{
            .raw_text = text,
            .attributes = IdentAttributes{},
            .problems = IdentProblems{},
        };
        // TODO: parse idents and their attributes/problems
    }
};

// this information could be built up during parsing
pub const IdentAttributes = packed struct(u3) {
    effectful: bool,
    ignored: bool,
    reassignable: bool,
};

// for example we detect two underscores in a row during parsing... we can make a problem and report
// it to the user later as a warning, but still allow the program to run
pub const IdentProblems = packed struct {
    // TODO: add more problem cases
    subsequent_underscores: bool,

    pub fn has_problems(self: *IdentProblems) bool {
        return self.subsequent_underscores;
    }
};

pub const IdentStore = struct {
    interner: cols.SmallStringInterner,
    regions: std.AutoHashMap(IdentId, base.Region),

    pub fn init(allocator: std.mem.Allocator) IdentStore {
        return IdentStore{
            .interner = cols.SmallStringInterner.init(allocator),
            .regions = std.AutoHashMap(u32, base.Region).init(allocator),
        };
    }

    pub fn deinit(self: *IdentStore) void {
        self.interner.deinit();
        self.regions.deinit();
    }

    pub fn insert(
        self: *IdentStore,
        ident: Ident,
        region: base.Region,
        problems: *std.ArrayList(problem.Problem),
    ) IdentId {
        if (ident.problems.has_problems()) {
            problems.push(.IdentIssue{ .problems = ident.problems, .region = region });
        }

        const id = self.interner.insert(ident.raw_text);
        self.regions.put(id.id, region);

        return IdentId{ .attributes = ident.attibutes, .id = @as(u29, id.id) };
    }

    pub fn getText(self: *IdentStore, ident_id: IdentId) []u8 {
        return self.interner.get(cols.SmallStringId{ .id = @as(u32, ident_id.id) });
    }

    pub fn getRegion(self: *IdentStore, ident_id: IdentId) base.Region {
        return self.regions.get(ident_id).?;
    }
};

pub const Symbol = struct {
    ident_id: IdentId,
    module_id: module.ModuleId,
};

pub const SymbolStore = struct {
    // One per moduleId, so we can use ModuleId as index
    ident_stores: std.ArrayList(IdentStore),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SymbolStore {
        return SymbolStore{
            .ident_stores = std.ArrayList(IdentStore).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SymbolStore) void {
        for (self.ident_stores.items) |ident_store| {
            ident_store.deinit();
        }

        self.ident_stores.deinit();
    }

    pub fn insert(
        self: *SymbolStore,
        ident: Ident,
        region: base.Region,
        module_id: base.ModuleId,
        problems: *std.ArrayList(problem.Problem),
    ) Symbol {
        while (@as(u32, self.ident_stores.items.len) <= module_id.id) {
            self.ident_stores.append(IdentStore.init(self.allocator)) catch cols.exit_on_oom;
        }

        const ident_store = self.ident_stores.items[@as(usize, module_id.id)];
        const ident_id = ident_store.insert(ident, region, problems);

        return Symbol{ .ident_id = ident_id, .module_id = module_id };
    }

    pub fn getText(self: *SymbolStore, symbol: Symbol) []u8 {
        const ident_store = self.ident_stores.items[@as(usize, symbol.module_id.id)];
        return ident_store.getText(symbol.ident_id);
    }

    pub fn getRegion(self: *SymbolStore, symbol: Symbol) base.Region {
        const ident_store = self.ident_stores.items[@as(usize, symbol.module_id.id)];
        return ident_store.getRegion(symbol.ident_id);
    }
};
