const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

const Region = base.Region;
const Ident = base.Ident;
const Module = base.Module;
const Problem = @import("../../problem.zig").Problem;

const Scope = @This();

env: *base.ModuleEnv,
// Enable when these are implemented:
//
// /// The custom alias for this file if one has been defined.
custom_alias: ?collections.SafeList(Ident.Idx).Idx,
//
/// Identifiers/aliases that are in scope, and defined in the current module
levels: Level.List,
allocator: std.mem.Allocator,

pub fn init(
    env: *base.ModuleEnv,
    builtin_aliases: []Alias,
    imported_idents: []Ident.Idx,
    allocator: std.mem.Allocator,
) Scope {
    var scope = Scope{
        .env = env,
        // .aliases = builtin_aliases.cloneWithAllocator(allocator) catch exitOnOom(),
        .custom_alias = null,
        .levels = Level.List.init(allocator),
        .allocator = allocator,
    };

    scope.enterLevel();

    var base_level_idents = scope.levels.items.items(.idents)[0];
    for (imported_idents) |imported| {
        base_level_idents.append(imported) catch exitOnOom();
    }

    var base_level_aliases = scope.levels.items.items(.aliases)[0];
    for (builtin_aliases) |builtin_alias| {
        base_level_aliases.append(builtin_alias) catch exitOnOom();
    }

    return scope;
}

pub fn enterLevel(self: *Scope) void {
    _ = self.levels.append(Level.init(self.allocator));
}

pub fn exitLevel(self: *Scope) void {
    if (self.levels.len() <= 1) {
        self.env.problems.append(Problem.Compiler.Canonicalize.make(.ExitedTopScopeLevel));
    } else {
        self.levels.items.pop();
    }
}

const ContainsIdent = union(enum) {
    InScope: Ident.Idx,
    NotInScope: Ident.Idx,
    NotPresent,
};

pub const LookupResult = union(enum) {
    InScope: Ident.Idx,
    Problem: Problem,
};

pub fn lookup(self: *Scope, ident: Ident.Idx) ContainsIdent {
    if (self.containsIdent(ident)) |ident_in_scope| {
        return ContainsIdent{ .InScope = ident_in_scope };
    }

    var options_in_scope = collections.SafeList(Ident).init(self.allocator);
    var all_idents_in_scope = self.iterIdentsInScope();
    while (all_idents_in_scope.next()) |ident_in_scope| {
        options_in_scope.append(ident_in_scope);
    }

    const problem = Problem.Canonicalize.make(.{ .LookupNotInScope = .{
        .ident = ident,
        .options_in_scope = options_in_scope,
    } });
    self.env.problems.append(problem);

    return LookupResult{ .Problem = problem };
}

pub fn containsIdent(self: *Scope, ident: Ident.Idx) ?Ident.Idx {
    var idents_in_scope = self.iterIdentsInScope();
    while (idents_in_scope.next()) |ident_in_scope| {
        if (self.env.idents.identsHaveSameText(ident, ident_in_scope)) {
            return ident_in_scope;
        }
    }

    return null;
}

pub fn introduce(self: *Scope, ident: Ident.Idx) Ident.Idx {
    if (self.containsIdent(ident)) |ident_in_scope| {
        const problem = Problem.Canonicalize.make(.{ .UnqualifiedAlreadyInScope = .{
            .original_ident = ident_in_scope,
            .shadow = ident,
        } });

        self.env.problems.append(problem);
        // TODO: is this correct for shadows?
        return ident;
    }

    const last_level = self.levels.getLast();
    last_level.append(ident);

    return ident;
}

/// Generates a unique ident like "1" or "5" in the home module.
///
/// This is used, for example, during canonicalization of an Expr::Closure
/// to generate a unique ident to refer to that closure.
pub fn genUnique(self: *Scope) Ident.Idx {
    const unique_idx = self.env.idents.genUnique(Region.zero());

    // There is always at least one level in
    const last_level = self.levels.getLast();
    last_level.append(unique_idx);

    return unique_idx;
}

const IdentsInScopeIterator = struct {
    scope: *Scope,
    level_index: usize,
    prior_ident_index: usize,

    fn empty(scope: *Scope) IdentsInScopeIterator {
        return IdentsInScopeIterator{
            .scope = scope,
            .level_index = 0,
            .prior_ident_index = 0,
        };
    }

    pub fn next(self: *IdentsInScopeIterator) ?Ident.Idx {
        if (self.prior_ident_index == 0) {
            return null;
        }

        const level = self.scope.levels.items[self.level_index];
        const next_ident = level.items[self.prior_ident_index - 1];

        self.prior_ident_index -= 1;

        if (self.prior_ident_index == 0) {
            self.level_index -|= 1;

            while (self.level_index > 0 and
                self.scope.levels.items[self.level_index].items.len == 0)
            {
                self.level_index -= 1;
            }
        }

        return next_ident;
    }
};

fn iterIdentsInScope(self: *Scope) IdentsInScopeIterator {
    if (self.levels.len() == 0) {
        return IdentsInScopeIterator.empty(self);
    }

    var level_index = self.levels.items.len -| 1;
    while (level_index > 0 and self.levels.items[level_index].items.len == 0) {
        level_index -= 1;
    }

    const prior_ident_index = self.levels.items[level_index].items.len;

    return IdentsInScopeIterator{
        .scope = self,
        .level_index = level_index,
        .prior_ident_index = prior_ident_index,
    };
}

pub const Level = struct {
    idents: std.ArrayList(Ident.Idx),
    aliases: std.ArrayList(Alias),

    pub fn init(allocator: std.mem.Allocator) Level {
        return Level{
            .idents = std.ArrayList(Ident.Idx).init(allocator),
            .aliases = std.ArrayList(Alias).init(allocator),
        };
    }

    pub fn deinit(self: *Level) void {
        self.idents.deinit();
        self.aliases.deinit();
    }

    pub const List = collections.SafeMultiList(@This());
};

pub const Alias = struct {
    name: Ident.Idx,
    kind: Kind,

    pub const Kind = enum {
        Custom,
        Structural,
    };
};
