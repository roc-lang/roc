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
