//! Tests for cross-module type checking functionality.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
const can = @import("can");
const Check = @import("../Check.zig");

const CIR = can.CIR;
const Var = types_mod.Var;
const Content = types_mod.Content;
const Ident = base.Ident;
const testing = std.testing;
const ModuleEnv = can.ModuleEnv;
const problem = @import("../problem.zig");
const snapshot = @import("../snapshot.zig");
const occurs = @import("../occurs.zig");
const ProblemStore = problem.Store;
const SnapshotStore = snapshot.Store;
const UnifierScratch = @import("../unify.zig").Scratch;
const OccursScratch = occurs.Scratch;
const unify = @import("../unify.zig").unify;

// All tests in this file are disabled because they use old APIs that no longer exist.
// They were using methods like addTypeSlotAndTypeVar and addExprAndTypeVar which
// were part of the old Store-based architecture that has been removed.
// These tests need to be rewritten to use the new CIR architecture.
