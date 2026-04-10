const can = @import("can");
const types = @import("types");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

pub const Modules = struct {
    all_module_envs: []const *const ModuleEnv,

    pub fn init(all_module_envs: []const *const ModuleEnv) Modules {
        return .{ .all_module_envs = all_module_envs };
    }

    pub fn module(self: @This(), module_idx: u32) Module {
        return .{
            .module_idx = module_idx,
            .env = self.all_module_envs[module_idx],
        };
    }

    pub fn env(self: @This(), module_idx: u32) *const ModuleEnv {
        return self.all_module_envs[module_idx];
    }
};

pub const Module = struct {
    module_idx: u32,
    env: *const ModuleEnv,

    pub fn def(self: @This(), idx: CIR.Def.Idx) Def {
        const data = self.env.store.getDef(idx);
        return .{
            .module_idx = self.module_idx,
            .env = self.env,
            .idx = idx,
            .data = data,
            .pattern = self.pattern(data.pattern),
            .expr = self.expr(data.expr),
        };
    }

    pub fn expr(self: @This(), idx: CIR.Expr.Idx) Expr {
        return .{
            .module_idx = self.module_idx,
            .env = self.env,
            .idx = idx,
            .data = self.env.store.getExpr(idx),
            .solved_var = ModuleEnv.varFrom(idx),
        };
    }

    pub fn pattern(self: @This(), idx: CIR.Pattern.Idx) Pattern {
        return .{
            .module_idx = self.module_idx,
            .env = self.env,
            .idx = idx,
            .data = self.env.store.getPattern(idx),
            .solved_var = ModuleEnv.varFrom(idx),
        };
    }

    pub fn typeAnnoVar(self: @This(), idx: CIR.TypeAnno.Idx) Var {
        _ = self;
        return ModuleEnv.varFrom(idx);
    }

    pub fn getStatement(self: @This(), idx: CIR.Statement.Idx) CIR.Statement {
        return self.env.store.getStatement(idx);
    }

    pub fn getRecordField(self: @This(), idx: CIR.RecordField.Idx) CIR.RecordField {
        return self.env.store.getRecordField(idx);
    }

    pub fn getRecordDestruct(self: @This(), idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
        return self.env.store.getRecordDestruct(idx);
    }

    pub fn getIfBranch(self: @This(), idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
        return self.env.store.getIfBranch(idx);
    }

    pub fn getMatchBranch(self: @This(), idx: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
        return self.env.store.getMatchBranch(idx);
    }

    pub fn getMatchBranchPattern(self: @This(), idx: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
        return self.env.store.getMatchBranchPattern(idx);
    }

    pub fn sliceExpr(self: @This(), span: CIR.Expr.Span) []CIR.Expr.Idx {
        return self.env.store.sliceExpr(span);
    }

    pub fn slicePatterns(self: @This(), span: CIR.Pattern.Span) []CIR.Pattern.Idx {
        return self.env.store.slicePatterns(span);
    }

    pub fn sliceStatements(self: @This(), span: CIR.Statement.Span) []CIR.Statement.Idx {
        return self.env.store.sliceStatements(span);
    }

    pub fn sliceRecordFields(self: @This(), span: CIR.RecordField.Span) []CIR.RecordField.Idx {
        return self.env.store.sliceRecordFields(span);
    }

    pub fn sliceRecordDestructs(self: @This(), span: CIR.Pattern.RecordDestruct.Span) []CIR.Pattern.RecordDestruct.Idx {
        return self.env.store.sliceRecordDestructs(span);
    }

    pub fn sliceIfBranches(self: @This(), span: CIR.Expr.IfBranch.Span) []CIR.Expr.IfBranch.Idx {
        return self.env.store.sliceIfBranches(span);
    }

    pub fn matchBranchSlice(self: @This(), span: CIR.Expr.Match.Branch.Span) []CIR.Expr.Match.Branch.Idx {
        return self.env.store.matchBranchSlice(span);
    }

    pub fn sliceMatchBranchPatterns(self: @This(), span: CIR.Expr.Match.BranchPattern.Span) []CIR.Expr.Match.BranchPattern.Idx {
        return self.env.store.sliceMatchBranchPatterns(span);
    }
};

pub const Def = struct {
    module_idx: u32,
    env: *const ModuleEnv,
    idx: CIR.Def.Idx,
    data: CIR.Def,
    pattern: Pattern,
    expr: Expr,

    pub fn module(self: @This()) Module {
        return .{ .module_idx = self.module_idx, .env = self.env };
    }
};

pub const Expr = struct {
    module_idx: u32,
    env: *const ModuleEnv,
    idx: CIR.Expr.Idx,
    data: CIR.Expr,
    solved_var: Var,

    pub fn module(self: @This()) Module {
        return .{ .module_idx = self.module_idx, .env = self.env };
    }
};

pub const Pattern = struct {
    module_idx: u32,
    env: *const ModuleEnv,
    idx: CIR.Pattern.Idx,
    data: CIR.Pattern,
    solved_var: Var,

    pub fn module(self: @This()) Module {
        return .{ .module_idx = self.module_idx, .env = self.env };
    }
};
