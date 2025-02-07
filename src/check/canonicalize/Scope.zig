const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const Region = base.Region;
const Ident = base.Ident;
const Module = base.Module;
const ModuleIdent = base.ModuleIdent;
const Problem = @import("../../problem.zig").Problem;
const exitOnOom = collections.exitOnOom;

const Scope = @This();

env: *base.ModuleEnv,
/// The type aliases currently in scope
custom_alias: ?Ident.Idx,
/// Identifiers that are in scope, and defined in the current module
levels: Level.List,
early_returns: ?EarlyReturn.List,
allocator: std.mem.Allocator,

pub const EarlyReturn = struct {
    type_var: base.TypeVar,
    region: base.Region,
    kind: Kind,

    pub const Kind = enum {
        Return,
        Try,
    };

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
};

pub fn init(
    env: *base.ModuleEnv,
    builtin_aliases: []Alias,
    imported_module_idents: []base.ModuleIdent,
    allocator: std.mem.Allocator,
) Scope {
    const scope = Scope{
        .env = env,
        .aliases = builtin_aliases.cloneWithAllocator(allocator) catch exitOnOom(),
        .custom_alias = null,
        .levels = Level.List.init(allocator),
        .early_returns = EarlyReturn.List.init(allocator),
        .allocator = allocator,
    };

    scope.enterLevel();

    const base_level_idents = scope.levels.items.items(.idents)[0];
    for (imported_module_idents) |imported| {
        base_level_idents.append(imported);
    }

    const base_level_aliases = scope.levels.items.items(.aliases)[0];
    for (builtin_aliases) |builtin_alias| {
        base_level_aliases.append(builtin_alias);
    }

    return scope;
}

pub fn enterLevel(self: *Scope) void {
    self.levels.append(Level.init(self.allocator));
}

pub fn exitLevel(self: *Scope) void {
    if (self.levels.len() <= 1) {
        self.env.problems.append(Problem.Compiler.Canonicalize.make(.ExitedTopScopeLevel));
    } else {
        self.levels.items.pop();
    }
}

const ContainsIdent = union(enum) {
    InScope: ModuleIdent,
    NotInScope: Ident.Idx,
    NotPresent,
};

pub const LookupResult = union(enum) {
    InScope: ModuleIdent,
    Problem: Problem,
};

pub fn lookup(self: *Scope, ident: Ident.Idx) ContainsIdent {
    if (self.containsIdent(ident)) |ident_in_scope| {
        return ContainsIdent{ .InScope = ident_in_scope };
    }

    var options_in_scope = collections.SafeList(ModuleIdent);
    var all_idents_in_scope = self.iterIdentsInScope();
    while (all_idents_in_scope.next()) |ident_in_scope| {
        options_in_scope.append(ident_in_scope);
    }

    const problem = Problem.Canonicalize.make(.LookupNotInScope{
        .ident = ident,
        .options_in_scope = options_in_scope,
    });
    self.env.problems.append(problem);

    return LookupResult{ .Problem = problem };
}

pub fn containsIdent(self: *Scope, ident: Ident.Idx) ?ModuleIdent {
    var idents_in_scope = self.iterIdentsInScope();
    while (idents_in_scope.next()) |ident_in_scope| {
        if (self.env.idents.identsHaveSameText(ident, ident_in_scope)) {
            return ident_in_scope;
        }
    }

    return null;
}

pub fn introduce(self: *Scope, ident: Ident.Idx) ModuleIdent {
    if (self.containsIdent(ident)) |ident_in_scope| {
        const problem = Problem.Canonicalize.make(.UnqualifiedAlreadyInScope{
            .original_ident = ident_in_scope,
            .shadow = ident,
        });

        self.env.problems.append(problem);
        // TODO: is this correct for shadows?
        return ident.in_home_module();
    }

    const last_level = self.levels.getLast();
    last_level.append(ident.in_home_module());

    return ident.in_home_module();
}

/// Generates a unique ident like "1" or "5" in the home module.
///
/// This is used, for example, during canonicalization of an Expr::Closure
/// to generate a unique ident to refer to that closure.
pub fn genUnique(self: *Scope) ModuleIdent {
    const unique_idx = self.env.idents.genUnique(Region.zero()).in_home_module();

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

    pub fn next(self: *IdentsInScopeIterator) ?ModuleIdent {
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
    idents: std.ArrayList(ModuleIdent),
    aliases: std.ArrayList(Alias),

    pub fn init(allocator: std.mem.Allocator) Level {
        return Level{
            .idents = std.ArrayList(ModuleIdent).init(allocator),
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
    name: ModuleIdent,
    kind: Kind,

    pub const Kind = enum {
        Custom,
        Structural,
    };
};
