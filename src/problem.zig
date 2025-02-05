const base = @import("base.zig");

const Problem = union(enum) {
    CompilerProblem: CompilerProblem,
    IdentProblems: struct {
        problems: base.IdentProblems,
        region: base.Region,
    },
};

const CompilerProblem = union(enum) {
    SpecializeTypes: SpecializeTypesProblem,
    LiftFunctions: LiftFunctionsProblem,
    SolveFunctions: SolveFunctionsProblem,
    SpecializeFunctions: SpecializeFunctionsProblem,
    LowerIr: LowerIrProblem,
    ReferenceCount: ReferenceCountProblem,
};

const SpecializeTypesProblem = enum {};
const LiftFunctionsProblem = enum {};
const SolveFunctionsProblem = enum {};
const SpecializeFunctionsProblem = enum {};
const LowerIrProblem = enum {};
const ReferenceCountProblem = enum {};
