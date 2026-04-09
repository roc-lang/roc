const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const type_mod = @import("type.zig");

const CIR = can.CIR;
const Var = types.Var;

pub const TypeKey = struct {
    module_idx: u32,
    var_: Var,
};

pub const ExprKey = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

pub const PatternTypeKey = struct {
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
};

pub const RecordDestructKey = struct {
    module_idx: u32,
    destruct_idx: CIR.Pattern.RecordDestruct.Idx,
};

pub const ExplicitCallFact = struct {
    fn_var: Var,
    result_var: Var,
    arg_vars: []Var,
};

pub const ExplicitCallTypeFact = struct {
    fn_ty: type_mod.TypeId,
    result_ty: type_mod.TypeId,
    arg_tys: []type_mod.TypeId,
    applied_result_tys: []type_mod.TypeId,
};

pub const ExplicitFunctionFact = struct {
    arg_vars: []Var,
    synthetic_unit_arg: bool,
    node_ret_var: Var,
    final_ret_var: Var,
};

pub const ExplicitFunctionTypeFact = struct {
    arg_tys: []type_mod.TypeId,
    synthetic_unit_arg: bool,
    node_ret_ty: type_mod.TypeId,
    final_ret_ty: type_mod.TypeId,
};

pub const ExprSourceFunctionFact = struct {
    seed_var: Var,
    arity: usize,
};

pub const ArithmeticBinopFact = struct {
    operand_var: Var,
    result_var: Var,
};

pub const ArithmeticBinopTypeFact = struct {
    operand_ty: type_mod.TypeId,
};

pub const Mutable = struct {
    allocator: std.mem.Allocator,
    function_facts: std.AutoHashMap(TypeKey, ExplicitFunctionFact),
    function_type_facts: std.AutoHashMap(TypeKey, ExplicitFunctionTypeFact),
    collected_expr_facts: std.AutoHashMap(ExprKey, void),
    expr_type_facts: std.AutoHashMap(ExprKey, type_mod.TypeId),
    expr_result_var_facts: std.AutoHashMap(ExprKey, Var),
    expr_source_function_facts: std.AutoHashMap(ExprKey, ExprSourceFunctionFact),
    call_facts: std.AutoHashMap(ExprKey, ExplicitCallFact),
    call_type_facts: std.AutoHashMap(ExprKey, ExplicitCallTypeFact),
    pattern_type_facts: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
    pattern_source_type_facts: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
    expr_field_index_facts: std.AutoHashMap(ExprKey, u16),
    expr_tag_discriminant_facts: std.AutoHashMap(ExprKey, u16),
    pattern_tag_discriminant_facts: std.AutoHashMap(PatternTypeKey, u16),
    pattern_list_elem_type_facts: std.AutoHashMap(PatternTypeKey, type_mod.TypeId),
    record_destruct_field_index_facts: std.AutoHashMap(RecordDestructKey, u16),
    arithmetic_binop_facts: std.AutoHashMap(ExprKey, ArithmeticBinopFact),
    arithmetic_binop_type_facts: std.AutoHashMap(ExprKey, ArithmeticBinopTypeFact),

    pub fn init(allocator: std.mem.Allocator) Mutable {
        return .{
            .allocator = allocator,
            .function_facts = std.AutoHashMap(TypeKey, ExplicitFunctionFact).init(allocator),
            .function_type_facts = std.AutoHashMap(TypeKey, ExplicitFunctionTypeFact).init(allocator),
            .collected_expr_facts = std.AutoHashMap(ExprKey, void).init(allocator),
            .expr_type_facts = std.AutoHashMap(ExprKey, type_mod.TypeId).init(allocator),
            .expr_result_var_facts = std.AutoHashMap(ExprKey, Var).init(allocator),
            .expr_source_function_facts = std.AutoHashMap(ExprKey, ExprSourceFunctionFact).init(allocator),
            .call_facts = std.AutoHashMap(ExprKey, ExplicitCallFact).init(allocator),
            .call_type_facts = std.AutoHashMap(ExprKey, ExplicitCallTypeFact).init(allocator),
            .pattern_type_facts = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
            .pattern_source_type_facts = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
            .expr_field_index_facts = std.AutoHashMap(ExprKey, u16).init(allocator),
            .expr_tag_discriminant_facts = std.AutoHashMap(ExprKey, u16).init(allocator),
            .pattern_tag_discriminant_facts = std.AutoHashMap(PatternTypeKey, u16).init(allocator),
            .pattern_list_elem_type_facts = std.AutoHashMap(PatternTypeKey, type_mod.TypeId).init(allocator),
            .record_destruct_field_index_facts = std.AutoHashMap(RecordDestructKey, u16).init(allocator),
            .arithmetic_binop_facts = std.AutoHashMap(ExprKey, ArithmeticBinopFact).init(allocator),
            .arithmetic_binop_type_facts = std.AutoHashMap(ExprKey, ArithmeticBinopTypeFact).init(allocator),
        };
    }

    pub fn deinit(self: *Mutable) void {
        self.arithmetic_binop_type_facts.deinit();
        self.arithmetic_binop_facts.deinit();
        self.record_destruct_field_index_facts.deinit();
        self.pattern_list_elem_type_facts.deinit();
        self.pattern_tag_discriminant_facts.deinit();
        self.expr_tag_discriminant_facts.deinit();
        self.expr_field_index_facts.deinit();
        self.pattern_source_type_facts.deinit();
        self.pattern_type_facts.deinit();
        var call_iter = self.call_facts.valueIterator();
        while (call_iter.next()) |fact| {
            self.allocator.free(fact.arg_vars);
        }
        self.call_facts.deinit();
        var call_type_iter = self.call_type_facts.valueIterator();
        while (call_type_iter.next()) |fact| {
            self.allocator.free(fact.arg_tys);
            self.allocator.free(fact.applied_result_tys);
        }
        self.call_type_facts.deinit();
        self.expr_source_function_facts.deinit();
        var function_iter = self.function_facts.valueIterator();
        while (function_iter.next()) |fact| {
            self.allocator.free(fact.arg_vars);
        }
        self.function_facts.deinit();
        var function_type_iter = self.function_type_facts.valueIterator();
        while (function_type_iter.next()) |fact| {
            self.allocator.free(fact.arg_tys);
        }
        self.function_type_facts.deinit();
        self.collected_expr_facts.deinit();
        self.expr_result_var_facts.deinit();
        self.expr_type_facts.deinit();
    }
};
