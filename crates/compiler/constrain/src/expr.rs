#![allow(clippy::too_many_arguments)]

use std::ops::Range;

use crate::builtins::{
    empty_list_type, float_literal, int_literal, list_type, num_literal, single_quote_literal,
};
use crate::pattern::{constrain_pattern, PatternState};
use roc_can::annotation::IntroducedVariables;
use roc_can::constraint::{
    Constraint, Constraints, ExpectEffectfulReason, ExpectedTypeIndex, FxCallKind, FxExpectation,
    Generalizable, OpportunisticResolve, TypeOrVar,
};
use roc_can::def::{Def, DefKind};
use roc_can::exhaustive::{sketch_pattern_to_rows, sketch_when_branches, ExhaustiveContext};
use roc_can::expected::Expected::{self, *};
use roc_can::expected::PExpected;
use roc_can::expr::Expr::{self, *};
use roc_can::expr::{
    AnnotatedMark, ClosureData, DeclarationTag, Declarations, DestructureDef, ExpectLookup, Field,
    FunctionDef, OpaqueWrapFunctionData, StructAccessorData, WhenBranch,
};
use roc_can::pattern::Pattern;
use roc_can::traverse::symbols_introduced_from_pattern;
use roc_collections::all::{HumanIndex, MutMap, SendMap};
use roc_collections::VecMap;
use roc_module::ident::{IdentSuffix, Lowercase};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{IllegalCycleMark, Variable};
use roc_types::types::Type::{self, *};
use roc_types::types::{
    AliasKind, AnnotationSource, Category, EarlyReturnKind, IndexOrField, OptAbleType, PReason,
    Reason, RecordField, TypeExtension, TypeTag, Types,
};
use soa::{Index, Slice};

/// This is for constraining Defs
#[derive(Default, Debug)]
struct Info {
    pub vars: Vec<Loc<Variable>>,
    pub constraints: Vec<Constraint>,
    pub def_types: VecMap<Symbol, Loc<TypeOrVar>>,
}

impl Info {
    pub fn with_capacity(capacity: usize) -> Self {
        Info {
            vars: Vec::with_capacity(capacity),
            constraints: Vec::with_capacity(capacity),
            def_types: VecMap::default(),
        }
    }
}

pub struct Env {
    /// for example `a` in the annotation `identity : a -> a`, we add it to this
    /// map so that expressions within that annotation can share these vars.
    pub rigids: MutMap<Lowercase, Variable>,
    pub resolutions_to_make: Vec<OpportunisticResolve>,
    pub home: ModuleId,
    /// The enclosing function's fx var to be unified with inner calls
    pub fx_expectation: Option<FxExpectation>,
}

impl Env {
    pub fn new(home: ModuleId) -> Self {
        Self {
            rigids: MutMap::default(),
            resolutions_to_make: Vec::new(),
            home,
            fx_expectation: None,
        }
    }

    pub fn with_fx_expectation<F, T>(
        &mut self,
        fx_var: Variable,
        ann_region: Option<Region>,
        f: F,
    ) -> T
    where
        F: FnOnce(&mut Env) -> T,
    {
        let prev = self.fx_expectation.take();

        self.fx_expectation = Some(FxExpectation { fx_var, ann_region });

        let result = f(self);

        self.fx_expectation = prev;

        result
    }
}

fn constrain_untyped_args(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    closure_type: Type,
    return_type: Type,
    fx_type: Type,
) -> (Vec<Variable>, PatternState, Type) {
    let mut vars = Vec::with_capacity(arguments.len());
    let mut pattern_types = Vec::with_capacity(arguments.len());

    let mut pattern_state = PatternState::default();

    for (pattern_var, annotated_mark, loc_pattern) in arguments {
        // Untyped args don't need exhaustiveness checking because they are the source of truth!
        let _ = annotated_mark;

        let pattern_type = Variable(*pattern_var);
        let pattern_type_index = constraints.push_variable(*pattern_var);
        let pattern_expected =
            constraints.push_pat_expected_type(PExpected::NoExpectation(pattern_type_index));

        pattern_types.push(pattern_type);

        constrain_pattern(
            types,
            constraints,
            env,
            &loc_pattern.value,
            loc_pattern.region,
            pattern_expected,
            &mut pattern_state,
        );

        vars.push(*pattern_var);
    }

    let function_type = Type::Function(
        pattern_types,
        Box::new(closure_type),
        Box::new(return_type),
        Box::new(fx_type),
    );

    (vars, pattern_state, function_type)
}

fn constrain_untyped_closure(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    region: Region,
    expected: ExpectedTypeIndex,
    fn_var: Variable,
    closure_var: Variable,
    ret_var: Variable,
    fx_var: Variable,
    early_returns: &[(Variable, Region, EarlyReturnKind)],
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    loc_body_expr: &Loc<Expr>,
    captured_symbols: &[(Symbol, Variable)],
    name: Symbol,
) -> Constraint {
    let closure_type = Type::Variable(closure_var);
    let return_type = Type::Variable(ret_var);
    let return_type_index = constraints.push_variable(ret_var);
    let fx_type = Type::Variable(fx_var);
    let (mut vars, pattern_state, function_type) = constrain_untyped_args(
        types,
        constraints,
        env,
        arguments,
        closure_type,
        return_type,
        fx_type,
    );

    vars.push(ret_var);
    vars.push(fx_var);
    vars.push(closure_var);
    vars.push(fn_var);

    let return_type_index = constraints.push_expected_type(ForReason(
        Reason::FunctionOutput,
        return_type_index,
        loc_body_expr.region,
    ));

    let returns_constraint = env.with_fx_expectation(fx_var, None, |env| {
        constrain_function_return(
            types,
            constraints,
            env,
            loc_body_expr,
            early_returns,
            return_type_index,
            ret_var,
            false,
        )
    });

    // make sure the captured symbols are sorted!
    debug_assert_eq!(captured_symbols.to_vec(), {
        let mut copy = captured_symbols.to_vec();
        copy.sort();
        copy
    });

    let closure_constraint = constrain_closure_size(
        types,
        constraints,
        name,
        region,
        fn_var,
        captured_symbols,
        closure_var,
        &mut vars,
    );

    let pattern_state_constraints = constraints.and_constraint(pattern_state.constraints);

    let function_type = {
        let typ = types.from_old_type(&function_type);
        constraints.push_type(types, typ)
    };

    let cons = [
        constraints.let_constraint(
            [],
            pattern_state.vars.into_iter().map(Loc::at_zero),
            pattern_state.headers,
            pattern_state_constraints,
            returns_constraint,
            Generalizable(true),
        ),
        constraints.and_constraint(pattern_state.delayed_fx_suffix_constraints),
        constraints.equal_types_with_storage(
            function_type,
            expected,
            Category::Lambda,
            region,
            fn_var,
        ),
        closure_constraint,
        constraints.flex_to_pure(fx_var),
    ];

    constraints.exists_many(vars, cons)
}

pub fn constrain_function_return(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    body_expr: &Loc<Expr>,
    early_returns: &[(Variable, Region, EarlyReturnKind)],
    return_type_expected: ExpectedTypeIndex,
    ret_var: Variable,
    should_attach_res_constraints: bool,
) -> Constraint {
    let return_con = constrain_expr(
        types,
        constraints,
        env,
        body_expr.region,
        &body_expr.value,
        return_type_expected,
    );

    let mut return_constraints = Vec::with_capacity(early_returns.len() + 1);
    let mut return_type_vars = Vec::with_capacity(early_returns.len() + 1);
    return_constraints.push(return_con);
    return_type_vars.push(ret_var);

    for (early_return_variable, early_return_region, early_return_kind) in early_returns {
        let early_return_con = constraints.equal_types_var(
            *early_return_variable,
            return_type_expected,
            Category::Return(*early_return_kind),
            *early_return_region,
        );

        return_constraints.push(early_return_con);
        return_type_vars.push(*early_return_variable);
    }

    let returns_constraint = constraints.exists_many(return_type_vars, return_constraints);

    if should_attach_res_constraints {
        attach_resolution_constraints(constraints, env, returns_constraint)
    } else {
        returns_constraint
    }
}

pub fn constrain_expr(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    region: Region,
    expr: &Expr,
    expected: ExpectedTypeIndex,
) -> Constraint {
    match expr {
        &Int(var, precision, _, _, bound) => {
            int_literal(types, constraints, var, precision, expected, region, bound)
        }
        &Num(var, _, _, bound) => num_literal(types, constraints, var, expected, region, bound),
        &Float(var, precision, _, _, bound) => {
            float_literal(types, constraints, var, precision, expected, region, bound)
        }
        EmptyRecord => constrain_empty_record(types, constraints, region, expected),
        Expr::Record { record_var, fields } => {
            if fields.is_empty() {
                constrain_empty_record(types, constraints, region, expected)
            } else {
                let mut field_types = SendMap::default();
                let mut field_vars = Vec::with_capacity(fields.len());

                // Constraints need capacity for each field
                // + 1 for the record itself + 1 for record var
                let mut rec_constraints = Vec::with_capacity(2 + fields.len());

                for (label, field) in fields {
                    let field_var = field.var;
                    let loc_field_expr = &field.loc_expr;
                    let (field_type, field_con) =
                        constrain_field(types, constraints, env, field_var, loc_field_expr);

                    let field_con = match label.suffix() {
                        IdentSuffix::None => {
                            let check_field_con =
                                constraints.fx_record_field_unsuffixed(field_var, field.region);
                            constraints.and_constraint([field_con, check_field_con])
                        }
                        IdentSuffix::Bang => field_con,
                    };

                    field_vars.push(field_var);
                    field_types.insert(label.clone(), RecordField::Required(field_type));

                    rec_constraints.push(field_con);
                }

                let record_type = {
                    let typ =
                        types.from_old_type(&Type::Record(field_types, TypeExtension::Closed));
                    constraints.push_type(types, typ)
                };

                let record_con = constraints.equal_types_with_storage(
                    record_type,
                    expected,
                    Category::Record,
                    region,
                    *record_var,
                );

                rec_constraints.push(record_con);
                field_vars.push(*record_var);

                let and_constraint = constraints.and_constraint(rec_constraints);
                constraints.exists(field_vars, and_constraint)
            }
        }
        Expr::Tuple { tuple_var, elems } => {
            let mut elem_types = VecMap::with_capacity(elems.len());
            let mut elem_vars = Vec::with_capacity(elems.len());

            // Constraints need capacity for each elem
            // + 1 for the tuple itself + 1 for tuple var
            let mut tuple_constraints = Vec::with_capacity(2 + elems.len());

            for (i, (elem_var, loc_expr)) in elems.iter().enumerate() {
                let elem_type = constraints.push_variable(*elem_var);
                let elem_expected = constraints.push_expected_type(NoExpectation(elem_type));
                let elem_con = constrain_expr(
                    types,
                    constraints,
                    env,
                    loc_expr.region,
                    &loc_expr.value,
                    elem_expected,
                );

                elem_vars.push(*elem_var);
                elem_types.insert(i, Variable(*elem_var));

                tuple_constraints.push(elem_con);
            }

            let tuple_type = {
                let typ = types.from_old_type(&Type::Tuple(elem_types, TypeExtension::Closed));
                constraints.push_type(types, typ)
            };

            let tuple_con = constraints.equal_types_with_storage(
                tuple_type,
                expected,
                Category::Tuple,
                region,
                *tuple_var,
            );

            tuple_constraints.push(tuple_con);
            elem_vars.push(*tuple_var);

            let and_constraint = constraints.and_constraint(tuple_constraints);
            constraints.exists(elem_vars, and_constraint)
        }
        RecordUpdate {
            record_var,
            ext_var,
            symbol,
            updates,
        } => {
            let mut fields: SendMap<Lowercase, RecordField<Type>> = SendMap::default();
            let mut vars = Vec::with_capacity(updates.len() + 2);
            let mut cons = Vec::with_capacity(updates.len() + 1);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) = constrain_field_update(
                    types,
                    constraints,
                    env,
                    var,
                    loc_expr.region,
                    field_name.clone(),
                    &loc_expr,
                );
                fields.insert(field_name, RecordField::Required(tipe));
                vars.push(var);
                cons.push(con);
            }

            let fields_type = {
                let typ = types.from_old_type(&Type::Record(
                    fields,
                    TypeExtension::from_non_annotation_type(Type::Variable(*ext_var)),
                ));
                constraints.push_type(types, typ)
            };
            let record_type = {
                let typ = types.from_old_type(&Type::Variable(*record_var));
                constraints.push_type(types, typ)
            };

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_type_expected = constraints.push_expected_type(NoExpectation(fields_type));
            let fields_con = constraints.equal_types_var(
                *record_var,
                fields_type_expected,
                Category::Record,
                region,
            );
            let expected_record = expected;
            let record_con =
                constraints.equal_types_var(*record_var, expected_record, Category::Record, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let record_being_updated_expectation = constraints.push_expected_type(ForReason(
                Reason::RecordUpdateKeys(
                    *symbol,
                    updates
                        .iter()
                        .map(|(key, field)| (key.clone(), field.region))
                        .collect(),
                ),
                record_type,
                region,
            ));

            let con = constraints.lookup(*symbol, record_being_updated_expectation, region);

            // ensure constraints are solved in this order, gives better errors
            cons.insert(0, fields_con);
            cons.insert(1, con);
            cons.insert(2, record_con);

            let and_constraint = constraints.and_constraint(cons);
            constraints.exists(vars, and_constraint)
        }
        Str(_) => {
            let str_index = constraints.push_type(types, Types::STR);
            let expected_index = expected;
            constraints.equal_types(str_index, expected_index, Category::Str, region)
        }
        IngestedFile(file_path, bytes, var) => {
            let index = constraints.push_variable(*var);
            let eq_con = constraints.equal_types(
                index,
                expected,
                Category::IngestedFile(file_path.clone()),
                region,
            );
            let ingested_con = constraints.ingested_file(index, file_path.clone(), bytes.clone());

            // First resolve the type variable with the eq_con then try to ingest a file into the correct type.
            let and_constraint = constraints.and_constraint(vec![eq_con, ingested_con]);
            constraints.exists([*var], and_constraint)
        }
        SingleQuote(num_var, precision_var, _, bound) => single_quote_literal(
            types,
            constraints,
            *num_var,
            *precision_var,
            expected,
            region,
            *bound,
        ),
        List {
            elem_var,
            loc_elems,
        } => {
            if loc_elems.is_empty() {
                let elem_type_index = {
                    let typ = types.from_old_type(&empty_list_type(*elem_var));
                    constraints.push_type(types, typ)
                };
                let eq = constraints.equal_types(elem_type_index, expected, Category::List, region);
                constraints.exists(vec![*elem_var], eq)
            } else {
                let list_elem_type = Type::Variable(*elem_var);
                let list_elem_type_index = constraints.push_variable(*elem_var);
                let mut list_constraints = Vec::with_capacity(1 + loc_elems.len());

                for (index, loc_elem) in loc_elems.iter().enumerate() {
                    let elem_expected = constraints.push_expected_type(ForReason(
                        Reason::ElemInList {
                            index: HumanIndex::zero_based(index),
                        },
                        list_elem_type_index,
                        loc_elem.region,
                    ));
                    let constraint = constrain_expr(
                        types,
                        constraints,
                        env,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    list_constraints.push(constraint);
                }

                let elem_type_index = {
                    let typ = types.from_old_type(&list_type(list_elem_type));
                    constraints.push_type(types, typ)
                };
                list_constraints.push(constraints.equal_types(
                    elem_type_index,
                    expected,
                    Category::List,
                    region,
                ));

                let and_constraint = constraints.and_constraint(list_constraints);
                constraints.exists([*elem_var], and_constraint)
            }
        }
        Call(boxed, loc_args, called_via) => {
            let (fn_var, loc_fn, closure_var, ret_var, fx_var) = &**boxed;
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let opt_symbol = if let Var(symbol, _) | AbilityMember(symbol, _, _) = loc_fn.value {
                Some(symbol)
            } else {
                None
            };

            let fn_type_index = constraints.push_variable(*fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = constraints.push_expected_type(NoExpectation(fn_type_index));

            let fn_reason = Reason::FnCall {
                name: opt_symbol,
                arity: loc_args.len() as u8,
                called_via: *called_via,
            };

            let fn_con = constrain_expr(
                types,
                constraints,
                env,
                loc_fn.region,
                &loc_fn.value,
                fn_expected,
            );

            // The function's return type
            let ret_type = Variable(*ret_var);

            // The function's effect type
            let fx_type = Variable(*fx_var);

            // type of values captured in the closure
            let closure_type = Variable(*closure_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);
            vars.push(*closure_var);
            vars.push(*fx_var);

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let arg_type = Variable(*arg_var);
                let arg_type_index = constraints.push_variable(*arg_var);

                let reason = Reason::FnArg {
                    name: opt_symbol,
                    arg_index: HumanIndex::zero_based(index),
                    called_via: *called_via,
                };
                let expected_arg =
                    constraints.push_expected_type(ForReason(reason, arg_type_index, region));
                let arg_con = constrain_expr(
                    types,
                    constraints,
                    env,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_index = {
                let arguments = types.from_old_type_slice(arg_types.iter());
                let lambda_set = types.from_old_type(&closure_type);
                let ret = types.from_old_type(&ret_type);
                let fx = types.from_old_type(&fx_type);
                let typ = types.function(arguments, lambda_set, ret, fx);
                constraints.push_type(types, typ)
            };
            let expected_fn_type =
                constraints.push_expected_type(ForReason(fn_reason, expected_fn_index, region));

            let expected_final_type = expected;

            let category = Category::CallResult(opt_symbol, *called_via);

            let and_cons = [
                fn_con,
                constraints.equal_types_var(*fn_var, expected_fn_type, category.clone(), fn_region),
                constraints.and_constraint(arg_cons),
                constraints.equal_types_var(
                    *ret_var,
                    expected_final_type,
                    category.clone(),
                    region,
                ),
                constraints.fx_call(
                    *fx_var,
                    FxCallKind::Call(opt_symbol),
                    region,
                    env.fx_expectation,
                ),
            ];

            let and_constraint = constraints.and_constraint(and_cons);
            constraints.exists(vars, and_constraint)
        }
        Expr::Crash { msg, ret_var } => {
            let str_index = constraints.push_type(types, Types::STR);
            let expected_msg = constraints.push_expected_type(Expected::ForReason(
                Reason::CrashArg,
                str_index,
                msg.region,
            ));

            let msg_is_str = constrain_expr(
                types,
                constraints,
                env,
                msg.region,
                &msg.value,
                expected_msg,
            );
            let magic = constraints.equal_types_var(*ret_var, expected, Category::Crash, region);

            let and = constraints.and_constraint([msg_is_str, magic]);

            constraints.exists([*ret_var], and)
        }
        Var(symbol, variable)
        | ParamsVar {
            symbol,
            var: variable,
            ..
        } => {
            // Save the expectation in the variable, then lookup the symbol's type in the environment
            let expected_type = *constraints[expected].get_type_ref();
            let store_expected = constraints.store(expected_type, *variable, file!(), line!());

            let lookup_constr = constraints.lookup(*symbol, expected, region);

            constraints.and_constraint([store_expected, lookup_constr])
        }
        ImportParams(module_id, region, Some((var, params))) => {
            let index = constraints.push_variable(*var);
            let expected_params = constraints.push_expected_type(Expected::ForReason(
                Reason::ImportParams(*module_id),
                index,
                *region,
            ));
            let expr_con =
                constrain_expr(types, constraints, env, *region, params, expected_params);
            let params_con = constraints.import_params(Some(index), *module_id, *region);
            let expr_and_params = constraints.and_constraint([expr_con, params_con]);
            constraints.exists([*var], expr_and_params)
        }
        ImportParams(module_id, region, None) => {
            constraints.import_params(None, *module_id, *region)
        }
        &AbilityMember(symbol, specialization_id, specialization_var) => {
            // Save the expectation in the `specialization_var` so we know what to specialize, then
            // lookup the member in the environment.
            let expected_type = *constraints[expected].get_type_ref();
            let store_expected =
                constraints.store(expected_type, specialization_var, file!(), line!());

            let stored_index = constraints.push_variable(specialization_var);
            let stored_specialization_var =
                constraints.push_expected_type(Expected::NoExpectation(stored_index));

            let lookup_constr = constraints.lookup(symbol, stored_specialization_var, region);

            // Make sure we attempt to resolve the specialization, if we can.
            if let Some(specialization_id) = specialization_id {
                env.resolutions_to_make.push(OpportunisticResolve {
                    specialization_variable: specialization_var,
                    member: symbol,
                    specialization_id,
                });
            }

            constraints.and_constraint([store_expected, lookup_constr])
        }
        Closure(ClosureData {
            function_type: fn_var,
            closure_type: closure_var,
            return_type: ret_var,
            fx_type: fx_var,
            early_returns,
            arguments,
            loc_body: boxed,
            captured_symbols,
            name,
            ..
        }) => {
            // shared code with function defs without an annotation
            constrain_untyped_closure(
                types,
                constraints,
                env,
                region,
                expected,
                *fn_var,
                *closure_var,
                *ret_var,
                *fx_var,
                early_returns,
                arguments,
                boxed,
                captured_symbols,
                *name,
            )
        }

        Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let expected_bool = {
                let bool_type = constraints.push_variable(Variable::BOOL);
                constraints.push_expected_type(Expected::ForReason(
                    Reason::ExpectCondition,
                    bool_type,
                    loc_condition.region,
                ))
            };

            let cond_con = constrain_expr(
                types,
                constraints,
                env,
                loc_condition.region,
                &loc_condition.value,
                expected_bool,
            );

            let continuation_con = constrain_expr(
                types,
                constraints,
                env,
                loc_continuation.region,
                &loc_continuation.value,
                expected,
            );

            // + 2 for cond_con and continuation_con
            let mut all_constraints = Vec::with_capacity(lookups_in_cond.len() + 2);

            all_constraints.push(cond_con);
            all_constraints.push(continuation_con);

            let mut vars = Vec::with_capacity(lookups_in_cond.len());

            for ExpectLookup {
                symbol,
                var,
                ability_info: _,
            } in lookups_in_cond.iter()
            {
                vars.push(*var);

                let var_index = constraints.push_variable(*var);
                let store_into = constraints.push_expected_type(NoExpectation(var_index));

                all_constraints.push(constraints.lookup(*symbol, store_into, Region::zero()));
            }

            constraints.exists_many(vars, all_constraints)
        }

        Dbg {
            source_location: _,
            source: _,
            loc_message,
            loc_continuation,
            variable,
            symbol: _,
        } => {
            let dbg_type = constraints.push_variable(*variable);
            let expected_dbg = constraints.push_expected_type(Expected::NoExpectation(dbg_type));

            let message_con = constrain_expr(
                types,
                constraints,
                env,
                loc_message.region,
                &loc_message.value,
                expected_dbg,
            );

            let continuation_con = constrain_expr(
                types,
                constraints,
                env,
                loc_continuation.region,
                &loc_continuation.value,
                expected,
            );

            constraints.exists_many([*variable], [message_con, continuation_con])
        }

        Try {
            result_expr,
            result_var,
            return_var,
            ok_payload_var,
            err_payload_var,
            err_ext_var,
            kind,
        } => {
            let result_var_index = constraints.push_variable(*result_var);
            let result_expected_type = constraints.push_expected_type(ForReason(
                Reason::TryResult,
                result_var_index,
                result_expr.region,
            ));
            let result_constraint = constrain_expr(
                types,
                constraints,
                env,
                result_expr.region,
                &result_expr.value,
                result_expected_type,
            );

            let try_target_constraint = constraints.try_target(
                result_var_index,
                *ok_payload_var,
                *err_payload_var,
                result_expr.region,
                *kind,
            );

            let return_type_index = constraints.push_variable(*return_var);
            let expected_return_value = constraints.push_expected_type(ForReason(
                Reason::TryResult,
                return_type_index,
                result_expr.region,
            ));

            let try_failure_type_index = {
                let typ = types.from_old_type(&Type::TagUnion(
                    vec![("Err".into(), vec![Type::Variable(*err_payload_var)])],
                    TypeExtension::from_non_annotation_type(Type::Variable(*err_ext_var)),
                ));
                constraints.push_type(types, typ)
            };
            let try_failure_constraint = constraints.equal_types(
                try_failure_type_index,
                expected_return_value,
                Category::TryFailure,
                region,
            );

            let ok_type_index = constraints.push_variable(*ok_payload_var);
            let try_success_constraint =
                constraints.equal_types(ok_type_index, expected, Category::TrySuccess, region);

            constraints.exists_many(
                [
                    *return_var,
                    *result_var,
                    *ok_payload_var,
                    *err_payload_var,
                    *err_ext_var,
                ],
                [
                    result_constraint,
                    try_target_constraint,
                    try_failure_constraint,
                    try_success_constraint,
                ],
            )
        }

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let expect_bool = |constraints: &mut Constraints, region| {
                let bool_type = constraints.push_variable(Variable::BOOL);
                constraints.push_expected_type(Expected::ForReason(
                    Reason::IfCondition,
                    bool_type,
                    region,
                ))
            };
            let mut branch_cons = Vec::with_capacity(2 * branches.len() + 3);

            // TODO why does this cond var exist? is it for error messages?
            let first_cond_region = branches[0].0.region;
            let expected_bool = expect_bool(constraints, first_cond_region);
            let cond_var_is_bool_con = constraints.equal_types_var(
                *cond_var,
                expected_bool,
                Category::If,
                first_cond_region,
            );

            branch_cons.push(cond_var_is_bool_con);

            let expected = constraints[expected].clone();
            match expected {
                FromAnnotation(name, arity, ann_source, tipe) => {
                    let num_branches = branches.len() + 1;
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let expected_bool = expect_bool(constraints, loc_cond.region);
                        let cond_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expected_bool,
                        );

                        let expected_then = constraints.push_expected_type(FromAnnotation(
                            name.clone(),
                            arity,
                            AnnotationSource::TypedIfBranch {
                                index: HumanIndex::zero_based(index),
                                num_branches,
                                region: ann_source.region(),
                            },
                            tipe,
                        ));

                        let then_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_body.region,
                            &loc_body.value,
                            expected_then,
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }

                    let expected_else = constraints.push_expected_type(FromAnnotation(
                        name,
                        arity,
                        AnnotationSource::TypedIfBranch {
                            index: HumanIndex::zero_based(branches.len()),
                            num_branches,
                            region: ann_source.region(),
                        },
                        tipe,
                    ));
                    let else_con = constrain_expr(
                        types,
                        constraints,
                        env,
                        final_else.region,
                        &final_else.value,
                        expected_else,
                    );

                    let expected_result_type = constraints.push_expected_type(NoExpectation(tipe));

                    let ast_con = constraints.equal_types_var(
                        *branch_var,
                        expected_result_type,
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    );

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    constraints.exists_many([*cond_var, *branch_var], branch_cons)
                }
                _ => {
                    let branch_var_index = constraints.push_variable(*branch_var);

                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let expected_bool = expect_bool(constraints, loc_cond.region);
                        let cond_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expected_bool,
                        );

                        let expected_then = constraints.push_expected_type(ForReason(
                            Reason::IfBranch {
                                index: HumanIndex::zero_based(index),
                                total_branches: branches.len(),
                            },
                            branch_var_index,
                            loc_body.region,
                        ));
                        let then_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_body.region,
                            &loc_body.value,
                            expected_then,
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }
                    let expected_else = constraints.push_expected_type(ForReason(
                        Reason::IfBranch {
                            index: HumanIndex::zero_based(branches.len()),
                            total_branches: branches.len() + 1,
                        },
                        branch_var_index,
                        final_else.region,
                    ));
                    let else_con = constrain_expr(
                        types,
                        constraints,
                        env,
                        final_else.region,
                        &final_else.value,
                        expected_else,
                    );

                    let expected = constraints.push_expected_type(expected);

                    branch_cons.push(constraints.equal_types_var(
                        *branch_var,
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    ));
                    branch_cons.push(else_con);

                    constraints.exists_many([*cond_var, *branch_var], branch_cons)
                }
            }
        }
        When {
            cond_var: real_cond_var,
            expr_var,
            loc_cond,
            branches,
            branches_cond_var,
            exhaustive,
            ..
        } => {
            let branches_cond_var = *branches_cond_var;
            let branches_cond_index = constraints.push_variable(branches_cond_var);

            let body_var = *expr_var;
            let body_type_index = constraints.push_variable(body_var);

            let branches_region = {
                debug_assert!(!branches.is_empty());
                Region::span_across(&loc_cond.region, &branches.last().unwrap().value.region)
            };

            let branch_expr_reason = |expected: &Expected<TypeOrVar>, index| match expected {
                FromAnnotation(name, arity, ann_source, _typ) => {
                    // NOTE deviation from elm.
                    //
                    // in elm, `_typ` is used, but because we have this `expr_var` too
                    // and need to constrain it, this is what works and gives better error messages
                    FromAnnotation(
                        name.clone(),
                        *arity,
                        AnnotationSource::TypedWhenBranch {
                            index,
                            region: ann_source.region(),
                        },
                        body_type_index,
                    )
                }

                _ => ForReason(Reason::WhenBranch { index }, body_type_index, region),
            };

            // Our goal is to constrain and introduce variables in all pattern when branch patterns before
            // looking at their bodies.
            //
            //   pat1 -> body1
            //   *^^^    +~~~~
            //   pat2 -> body2
            //   *^^^    +~~~~
            //
            //   * solve first
            //   + solve second
            //
            // For a single pattern/body pair, we must introduce variables and symbols defined in the
            // pattern before solving the body, since those definitions are effectively let-bound.
            //
            // But also, we'd like to solve all branch pattern constraints in one swoop before looking at
            // the bodies, because the patterns may have presence constraints that expect to be built up
            // together.
            //
            // For this reason, we distinguish the two - and introduce variables in the branch patterns
            // as part of the pattern constraint, solving all of those at once, and then solving the body
            // constraints.
            let mut pattern_vars = Vec::with_capacity(branches.len());
            let mut pattern_headers = SendMap::default();
            let mut pattern_cons = Vec::with_capacity(branches.len() + 2);
            let mut delayed_is_open_constraints = Vec::with_capacity(2);
            let mut body_cons = Vec::with_capacity(branches.len());

            for (index, when_branch) in branches.iter().enumerate() {
                let expected_pattern = |sub_pattern, sub_region| {
                    PExpected::ForReason(
                        PReason::WhenMatch {
                            index: HumanIndex::zero_based(index),
                            sub_pattern,
                        },
                        branches_cond_index,
                        sub_region,
                    )
                };

                let ConstrainedBranch {
                    vars: new_pattern_vars,
                    headers: new_pattern_headers,
                    pattern_constraints,
                    is_open_constrains,
                    body_constraints,
                } = constrain_when_branch_help(
                    types,
                    constraints,
                    env,
                    region,
                    when_branch,
                    expected_pattern,
                    branch_expr_reason(&constraints[expected], HumanIndex::zero_based(index)),
                );

                pattern_vars.extend(new_pattern_vars);

                if cfg!(debug_assertions) {
                    let intersection: Vec<_> = pattern_headers
                        .keys()
                        .filter(|k| new_pattern_headers.contains_key(k))
                        .collect();

                    debug_assert!(
                        intersection.is_empty(),
                        "Two patterns introduce the same symbols - that's a bug!\n{intersection:?}"
                    );
                }

                pattern_headers.extend(new_pattern_headers);
                pattern_cons.push(pattern_constraints);
                delayed_is_open_constraints.extend(is_open_constrains);

                body_cons.push(body_constraints);
            }

            // Deviation: elm adds another layer of And nesting
            //
            // Record the original conditional expression's constraint.
            // Each branch's pattern must have the same type
            // as the condition expression did.
            //
            // The return type of each branch must equal the return type of
            // the entire when-expression.

            // Layer on the "is-open" constraints at the very end, after we know what the branch
            // types are supposed to look like without open-ness.
            let is_open_constr = constraints.and_constraint(delayed_is_open_constraints);
            pattern_cons.push(is_open_constr);

            // After solving the condition variable with what's expected from the branch patterns,
            // check it against the condition expression.
            //
            // First, solve the condition type.
            let real_cond_var = *real_cond_var;
            let real_cond_type = constraints.push_variable(real_cond_var);
            let expected_real_cond =
                constraints.push_expected_type(Expected::NoExpectation(real_cond_type));
            let cond_constraint = constrain_expr(
                types,
                constraints,
                env,
                loc_cond.region,
                &loc_cond.value,
                expected_real_cond,
            );
            pattern_cons.push(cond_constraint);

            // Now check the condition against the type expected by the branches.
            let sketched_rows = sketch_when_branches(branches_region, branches);
            let expected_by_branches = constraints.push_expected_type(Expected::ForReason(
                Reason::WhenBranches,
                branches_cond_index,
                branches_region,
            ));
            let cond_matches_branches_constraint = constraints.exhaustive(
                real_cond_var,
                loc_cond.region,
                Ok((loc_cond.value.category(), expected_by_branches)),
                sketched_rows,
                ExhaustiveContext::BadCase,
                *exhaustive,
            );
            pattern_cons.push(cond_matches_branches_constraint);

            // Solve all the pattern constraints together, introducing variables in the pattern as
            // need be before solving the bodies.
            let pattern_constraints = constraints.and_constraint(pattern_cons);
            let body_constraints = constraints.and_constraint(body_cons);
            let when_body_con = constraints.let_constraint(
                [],
                pattern_vars.into_iter().map(Loc::at_zero),
                pattern_headers,
                pattern_constraints,
                body_constraints,
                // Never generalize identifiers introduced in branch-patterns
                Generalizable(false),
            );

            let result_con =
                constraints.equal_types_var(body_var, expected, Category::When, region);

            let total_cons = [when_body_con, result_con];
            let branch_constraints = constraints.and_constraint(total_cons);

            constraints.exists(
                [branches_cond_var, real_cond_var, *expr_var],
                branch_constraints,
            )
        }
        RecordAccess {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let ext_var = *ext_var;
            let ext_type = Type::Variable(ext_var);
            let field_var = *field_var;
            let field_type = Type::Variable(field_var);

            let mut rec_field_types = SendMap::default();

            let label = field.clone();
            rec_field_types.insert(label, RecordField::Demanded(field_type));

            let record_type = {
                let typ = types.from_old_type(&Type::Record(
                    rec_field_types,
                    TypeExtension::from_non_annotation_type(ext_type),
                ));
                constraints.push_type(types, typ)
            };
            let record_expected = constraints.push_expected_type(NoExpectation(record_type));

            let category = Category::RecordAccess(field.clone());

            let record_con =
                constraints.equal_types_var(*record_var, record_expected, category.clone(), region);

            let expected_record = constraints.push_expected_type(NoExpectation(record_type));
            let constraint = constrain_expr(
                types,
                constraints,
                env,
                region,
                &loc_expr.value,
                expected_record,
            );

            let eq = constraints.equal_types_var(field_var, expected, category, region);
            constraints.exists_many(
                [*record_var, field_var, ext_var],
                [constraint, eq, record_con],
            )
        }
        RecordAccessor(StructAccessorData {
            name: closure_name,
            function_var,
            field,
            record_var,
            closure_var,
            ext_var,
            field_var,
        }) => {
            let ext_var = *ext_var;
            let ext_type = Variable(ext_var);
            let field_var = *field_var;
            let field_type = Variable(field_var);

            let record_type = match field {
                IndexOrField::Field(field) => {
                    let mut field_types = SendMap::default();
                    let label = field.clone();
                    field_types.insert(label, RecordField::Demanded(field_type.clone()));
                    Type::Record(
                        field_types,
                        TypeExtension::from_non_annotation_type(ext_type),
                    )
                }
                IndexOrField::Index(index) => {
                    let mut field_types = VecMap::with_capacity(1);
                    field_types.insert(*index, field_type.clone());
                    Type::Tuple(
                        field_types,
                        TypeExtension::from_non_annotation_type(ext_type),
                    )
                }
            };

            let record_type_index = {
                let typ = types.from_old_type(&record_type);
                constraints.push_type(types, typ)
            };

            let category = Category::Accessor(field.clone());

            let record_expected = constraints.push_expected_type(NoExpectation(record_type_index));
            let record_con =
                constraints.equal_types_var(*record_var, record_expected, category.clone(), region);

            let expected_lambda_set = {
                let lambda_set_ty = {
                    let typ = types.from_old_type(&Type::ClosureTag {
                        name: *closure_name,
                        captures: vec![],
                        ambient_function: *function_var,
                    });
                    constraints.push_type(types, typ)
                };
                constraints.push_expected_type(NoExpectation(lambda_set_ty))
            };

            let closure_type = Type::Variable(*closure_var);

            let function_type_index = {
                let typ = types.from_old_type(&Type::Function(
                    vec![record_type],
                    Box::new(closure_type),
                    Box::new(field_type),
                    Box::new(Type::Variable(Variable::PURE)),
                ));
                constraints.push_type(types, typ)
            };

            let cons = [
                constraints.equal_types_var(
                    *closure_var,
                    expected_lambda_set,
                    category.clone(),
                    region,
                ),
                constraints.equal_types(function_type_index, expected, category.clone(), region),
                {
                    let store_fn_var_index = constraints.push_variable(*function_var);
                    let store_fn_var_expected =
                        constraints.push_expected_type(NoExpectation(store_fn_var_index));
                    constraints.equal_types(
                        function_type_index,
                        store_fn_var_expected,
                        category,
                        region,
                    )
                },
                record_con,
            ];

            constraints.exists_many(
                [*record_var, *function_var, *closure_var, field_var, ext_var],
                cons,
            )
        }
        TupleAccess {
            tuple_var,
            ext_var,
            elem_var,
            loc_expr,
            index,
        } => {
            let mut tup_elem_types = VecMap::with_capacity(1);

            let ext_var = *ext_var;
            let ext_type = Variable(ext_var);

            let elem_var = *elem_var;
            let elem_type = Type::Variable(elem_var);

            tup_elem_types.insert(*index, elem_type);

            let tuple_type = {
                let typ = types.from_old_type(&Type::Tuple(
                    tup_elem_types,
                    TypeExtension::from_non_annotation_type(ext_type),
                ));
                constraints.push_type(types, typ)
            };
            let tuple_expected = constraints.push_expected_type(NoExpectation(tuple_type));

            let category = Category::TupleAccess(*index);

            let tuple_con =
                constraints.equal_types_var(*tuple_var, tuple_expected, category.clone(), region);

            let expected_tuple = constraints.push_expected_type(NoExpectation(tuple_type));
            let constraint = constrain_expr(
                types,
                constraints,
                env,
                region,
                &loc_expr.value,
                expected_tuple,
            );

            let eq = constraints.equal_types_var(elem_var, expected, category, region);
            constraints.exists_many([*tuple_var, elem_var, ext_var], [constraint, eq, tuple_con])
        }
        LetRec(defs, loc_ret, cycle_mark) => {
            let body_con = constrain_expr(
                types,
                constraints,
                env,
                loc_ret.region,
                &loc_ret.value,
                expected,
            );

            constrain_recursive_defs(types, constraints, env, defs, body_con, *cycle_mark)
        }
        LetNonRec(def, loc_ret) => {
            let mut stack = Vec::with_capacity(1);

            let mut loc_ret = loc_ret;

            stack.push(def);

            while let LetNonRec(def, new_loc_ret) = &loc_ret.value {
                stack.push(def);
                loc_ret = new_loc_ret;
            }

            let mut body_con = constrain_expr(
                types,
                constraints,
                env,
                loc_ret.region,
                &loc_ret.value,
                expected,
            );

            while let Some(def) = stack.pop() {
                body_con = match def.kind {
                    DefKind::Let => constrain_let_def(types, constraints, env, def, body_con, None),
                    DefKind::Stmt(fx_var) => {
                        constrain_stmt_def(types, constraints, env, def, body_con, fx_var)
                    }
                    DefKind::Ignored(fx_var) => {
                        constrain_let_def(types, constraints, env, def, body_con, Some(fx_var))
                    }
                };
            }

            body_con
        }
        Return {
            return_value,
            return_var,
        } => {
            let return_type_index = constraints.push_variable(*return_var);
            let expected_return_value = constraints.push_expected_type(ForReason(
                Reason::FunctionOutput,
                return_type_index,
                return_value.region,
            ));

            let return_con = constrain_expr(
                types,
                constraints,
                env,
                return_value.region,
                &return_value.value,
                expected_return_value,
            );

            constraints.exists([*return_var], return_con)
        }
        Tag {
            tag_union_var: variant_var,
            ext_var,
            name,
            arguments,
        } => {
            // +2 because we push all the arguments, plus variant_var and ext_var
            let num_vars = arguments.len() + 2;
            let mut vars = Vec::with_capacity(num_vars);
            let mut payload_types = Vec::with_capacity(arguments.len());
            let mut arg_cons = Vec::with_capacity(arguments.len());

            for (var, loc_expr) in arguments {
                let var_index = constraints.push_variable(*var);
                let expected_arg = constraints.push_expected_type(NoExpectation(var_index));
                let arg_con = constrain_expr(
                    types,
                    constraints,
                    env,
                    loc_expr.region,
                    &loc_expr.value,
                    expected_arg,
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                payload_types.push(Type::Variable(*var));
            }

            let tag_union_type = {
                let typ = types.from_old_type(&Type::TagUnion(
                    vec![(name.clone(), payload_types)],
                    TypeExtension::from_non_annotation_type(Type::Variable(*ext_var)),
                ));
                constraints.push_type(types, typ)
            };

            let union_con = constraints.equal_types_with_storage(
                tag_union_type,
                expected,
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
                *variant_var,
            );

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);

            constraints.exists_many(vars, arg_cons)
        }
        ZeroArgumentTag {
            variant_var,
            ext_var,
            name,
            closure_name,
        } => {
            let function_or_tag_union = {
                let typ = types.from_old_type(&Type::FunctionOrTagUnion(
                    name.clone(),
                    *closure_name,
                    TypeExtension::from_non_annotation_type(Type::Variable(*ext_var)),
                ));
                constraints.push_type(types, typ)
            };
            let union_con = constraints.equal_types_with_storage(
                function_or_tag_union,
                expected,
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: 0,
                },
                region,
                *variant_var,
            );

            constraints.exists_many([*variant_var, *ext_var], [union_con])
        }
        OpaqueRef {
            opaque_var,
            name,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => {
            let (arg_var, arg_loc_expr) = &**argument;
            let arg_type = Type::Variable(*arg_var);
            let arg_type_index = constraints.push_variable(*arg_var);

            let opaque_type = {
                let typ = types.from_old_type(&Type::Alias {
                    symbol: *name,
                    type_arguments: type_arguments
                        .iter()
                        .map(|v| OptAbleType {
                            typ: Type::Variable(v.var),
                            opt_abilities: v.opt_abilities.clone(),
                        })
                        .collect(),
                    lambda_set_variables: lambda_set_variables.clone(),
                    infer_ext_in_output_types: vec![],
                    actual: Box::new(arg_type),
                    kind: AliasKind::Opaque,
                });
                constraints.push_type(types, typ)
            };

            // Constrain the argument
            let expected_arg =
                constraints.push_expected_type(Expected::NoExpectation(arg_type_index));
            let arg_con = constrain_expr(
                types,
                constraints,
                env,
                arg_loc_expr.region,
                &arg_loc_expr.value,
                expected_arg,
            );

            // Link the entire wrapped opaque type (with the now-constrained argument) to the
            // expected type
            let opaque_con = constraints.equal_types_with_storage(
                opaque_type,
                expected,
                Category::OpaqueWrap(*name),
                region,
                *opaque_var,
            );

            // Link the entire wrapped opaque type (with the now-constrained argument) to the type
            // variables of the opaque type
            // TODO: better expectation here
            let link_type_variables_con = {
                let specialized_type_index = {
                    let typ = types.from_old_type(specialized_def_type);
                    constraints.push_type(types, typ)
                };
                let expected_index =
                    constraints.push_expected_type(Expected::NoExpectation(specialized_type_index));
                constraints.equal_types(
                    arg_type_index,
                    expected_index,
                    Category::OpaqueArg,
                    arg_loc_expr.region,
                )
            };

            let mut vars = vec![*arg_var, *opaque_var];
            // Also add the fresh variables we created for the type argument and lambda sets
            vars.extend(type_arguments.iter().map(|v| v.var));
            vars.extend(lambda_set_variables.iter().map(|v| {
                v.0.expect_variable("all lambda sets should be fresh variables here")
            }));

            constraints.exists_many(vars, [arg_con, opaque_con, link_type_variables_con])
        }
        OpaqueWrapFunction(OpaqueWrapFunctionData {
            opaque_name,
            opaque_var,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
            function_name,
            function_var,
            argument_var,
            closure_var,
        }) => {
            let argument_type = Type::Variable(*argument_var);

            let opaque_type = {
                let typ = types.from_old_type(&Type::Alias {
                    symbol: *opaque_name,
                    type_arguments: type_arguments
                        .iter()
                        .map(|v| OptAbleType {
                            typ: Type::Variable(v.var),
                            opt_abilities: v.opt_abilities.clone(),
                        })
                        .collect(),
                    lambda_set_variables: lambda_set_variables.clone(),
                    infer_ext_in_output_types: vec![],
                    actual: Box::new(argument_type.clone()),
                    kind: AliasKind::Opaque,
                });
                constraints.push_type(types, typ)
            };

            let expected_opaque_type = constraints.push_expected_type(NoExpectation(opaque_type));

            // Tie the opaque type to the opaque_var
            let opaque_con = constraints.equal_types_var(
                *opaque_var,
                expected_opaque_type,
                Category::OpaqueWrap(*opaque_name),
                region,
            );

            // Tie the type of the value wrapped by the opaque to the opaque's type variables.
            let link_type_variables_con = {
                let arg_type_index = {
                    let typ = types.from_old_type(&argument_type);
                    constraints.push_type(types, typ)
                };
                let specialized_type_index = {
                    let typ = types.from_old_type(specialized_def_type);
                    constraints.push_type(types, typ)
                };
                let expected_specialized =
                    constraints.push_expected_type(Expected::NoExpectation(specialized_type_index));
                constraints.equal_types(
                    arg_type_index,
                    expected_specialized,
                    Category::OpaqueArg,
                    region,
                )
            };

            let lambda_set = {
                let lambda_set_index = {
                    let typ = types.from_old_type(&Type::ClosureTag {
                        name: *function_name,
                        captures: vec![],
                        ambient_function: *function_var,
                    });
                    constraints.push_type(types, typ)
                };
                constraints.push_expected_type(NoExpectation(lambda_set_index))
            };

            let closure_type = Type::Variable(*closure_var);

            let opaque_type = Type::Variable(*opaque_var);

            let expected_function_type = {
                let fn_type = {
                    let typ = types.from_old_type(&Type::Function(
                        vec![argument_type],
                        Box::new(closure_type),
                        Box::new(opaque_type),
                        Box::new(Type::Variable(Variable::PURE)),
                    ));
                    constraints.push_type(types, typ)
                };
                constraints.push_expected_type(NoExpectation(fn_type))
            };

            let cons = [
                opaque_con,
                link_type_variables_con,
                constraints.equal_types_var(
                    *closure_var,
                    lambda_set,
                    Category::OpaqueWrap(*opaque_name),
                    region,
                ),
                constraints.equal_types_var(
                    *function_var,
                    expected_function_type,
                    Category::OpaqueWrap(*opaque_name),
                    region,
                ),
                constraints.equal_types_var(
                    *function_var,
                    expected,
                    Category::OpaqueWrap(*opaque_name),
                    region,
                ),
            ];

            let mut vars = vec![*argument_var, *opaque_var];

            // Also add the fresh variables we created for the type argument and lambda sets
            vars.extend(type_arguments.iter().map(|v| v.var));
            vars.extend(lambda_set_variables.iter().map(|v| {
                v.0.expect_variable("all lambda sets should be fresh variables here")
            }));

            vars.extend([*function_var, *closure_var]);

            constraints.exists_many(vars, cons)
        }

        RunLowLevel { args, ret_var, op } => {
            // This is a modified version of what we do for function calls.

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |constraints: &mut Constraints, index, arg_type: TypeOrVar, arg| {
                let reason = Reason::LowLevelOpArg {
                    op: *op,
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg =
                    constraints.push_expected_type(ForReason(reason, arg_type, Region::zero()));
                let arg_con =
                    constrain_expr(types, constraints, env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);
                let arg_var_index = constraints.push_variable(*arg_var);

                add_arg(constraints, index, arg_var_index, arg);
            }

            let category = Category::LowLevelOpResult(*op);

            // Deviation: elm uses an additional And here
            let eq = constraints.equal_types_var(*ret_var, expected, category, region);
            arg_cons.push(eq);
            constraints.exists_many(vars, arg_cons)
        }
        ForeignCall {
            args,
            ret_var,
            foreign_symbol,
        } => {
            // This is a modified version of what we do for function calls.

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |constraints: &mut Constraints, index, arg_type: TypeOrVar, arg| {
                let reason = Reason::ForeignCallArg {
                    foreign_symbol: foreign_symbol.clone(),
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg =
                    constraints.push_expected_type(ForReason(reason, arg_type, Region::zero()));
                let arg_con =
                    constrain_expr(types, constraints, env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);
                let arg_var_index = constraints.push_variable(*arg_var);

                add_arg(constraints, index, arg_var_index, arg);
            }

            let category = Category::ForeignCall;

            // Deviation: elm uses an additional And here
            let eq = constraints.equal_types_var(*ret_var, expected, category, region);
            arg_cons.push(eq);
            constraints.exists_many(vars, arg_cons)
        }
        RuntimeError(_) => {
            // Runtime Errors are always going to crash, so they don't introduce any new
            // constraints.
            // Instead, trivially equate the expected type to itself. This will never yield
            // unification errors but it will catch errors in type translation, including ability
            // obligations.
            let trivial_type = *constraints[expected].get_type_ref();
            constraints.equal_types(trivial_type, expected, Category::Unknown, region)
        }
    }
}

fn constrain_function_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    index: usize,
    function_def_index: Index<Loc<FunctionDef>>,
    body_con: Constraint,
) -> Constraint {
    let loc_expr = &declarations.expressions[index];
    let loc_symbol = declarations.symbols[index];
    let expr_var = declarations.variables[index];
    let expr_var_index = constraints.push_variable(expr_var);
    let opt_annotation = &declarations.annotations[index];

    let loc_function_def = &declarations.function_bodies[function_def_index.index()];
    let function_def = &loc_function_def.value;

    match opt_annotation {
        Some(annotation) => {
            let loc_pattern = Loc::at(loc_symbol.region, Pattern::Identifier(loc_symbol.value));
            let loc_body_expr = loc_expr;

            let arity = annotation.signature.arity();
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let InstantiateRigids {
                signature,
                new_rigid_variables,
                new_infer_variables,
                has_explicit_inference_variables: _,
            } = instantiate_rigids_simple(
                types,
                &annotation.signature,
                &annotation.introduced_variables,
                &mut ftv,
            );

            let signature_index = constraints.push_type(types, signature);

            let (arg_types, _signature_closure_type, ret_type, fx_type) = match types[signature] {
                TypeTag::Function(signature_closure_type, ret_type, fx_type) => (
                    types.get_type_arguments(signature),
                    signature_closure_type,
                    ret_type,
                    fx_type,
                ),
                _ => {
                    // aliases, or just something weird

                    let def_pattern_state = {
                        let mut def_pattern_state = PatternState::default();

                        def_pattern_state.headers.insert(
                            loc_symbol.value,
                            Loc {
                                region: loc_function_def.region,
                                // todo can we use Type::Variable(expr_var) here?
                                value: signature_index,
                            },
                        );

                        // TODO see if we can get away with not adding this constraint at all
                        def_pattern_state.vars.push(expr_var);
                        let annotation_expected = FromAnnotation(
                            loc_pattern.clone(),
                            arity,
                            AnnotationSource::TypedBody {
                                region: annotation.region,
                            },
                            signature_index,
                        );

                        {
                            let expected_index =
                                constraints.push_expected_type(annotation_expected);
                            def_pattern_state.constraints.push(constraints.equal_types(
                                expr_var_index,
                                expected_index,
                                Category::Storage(std::file!(), std::line!()),
                                Region::span_across(&annotation.region, &loc_body_expr.region),
                            ));
                        }

                        def_pattern_state
                    };

                    let annotation_expected = constraints.push_expected_type(FromAnnotation(
                        loc_pattern,
                        arity,
                        AnnotationSource::TypedBody {
                            region: annotation.region,
                        },
                        signature_index,
                    ));

                    let ret_constraint = constrain_untyped_closure(
                        types,
                        constraints,
                        env,
                        loc_function_def.region,
                        annotation_expected,
                        expr_var,
                        function_def.closure_type,
                        function_def.return_type,
                        function_def.fx_type,
                        &function_def.early_returns,
                        &function_def.arguments,
                        loc_body_expr,
                        &function_def.captured_symbols,
                        loc_symbol.value,
                    );

                    let ret_constraint =
                        attach_resolution_constraints(constraints, env, ret_constraint);

                    let cons = [
                        ret_constraint,
                        // Store type into AST vars. We use Store so errors aren't reported twice
                        constraints.store(signature_index, expr_var, std::file!(), std::line!()),
                    ];
                    let expr_con = constraints.and_constraint(cons);

                    return constrain_function_def_make_constraint(
                        constraints,
                        new_rigid_variables,
                        new_infer_variables,
                        expr_con,
                        body_con,
                        def_pattern_state,
                    );
                }
            };

            let env = &mut Env {
                home: env.home,
                rigids: ftv,
                resolutions_to_make: vec![],
                fx_expectation: Some(FxExpectation {
                    fx_var: function_def.fx_type,
                    ann_region: Some(annotation.region),
                }),
            };

            let region = loc_function_def.region;

            let mut argument_pattern_state = PatternState {
                headers: VecMap::default(),
                vars: Vec::with_capacity(function_def.arguments.len()),
                constraints: Vec::with_capacity(1),
                delayed_is_open_constraints: vec![],
                delayed_fx_suffix_constraints: Vec::with_capacity(function_def.arguments.len()),
            };
            let mut vars = Vec::with_capacity(argument_pattern_state.vars.capacity() + 1);
            let closure_var = function_def.closure_type;

            let ret_type_index = constraints.push_type(types, ret_type);
            let fx_type_index = constraints.push_type(types, fx_type);

            vars.push(function_def.return_type);
            vars.push(function_def.closure_type);
            vars.push(function_def.fx_type);

            let mut def_pattern_state = PatternState::default();

            def_pattern_state.headers.insert(
                loc_symbol.value,
                Loc {
                    region: loc_function_def.region,
                    // NOTE: we MUST use `expr_var` here so that the correct type variable is
                    // associated with the function. We prefer this to the annotation type, because the
                    // annotation type may be instantiated into a fresh type variable that is
                    // disassociated fromt the rest of the program.
                    // Below, we'll check that the function actually matches the annotation.
                    value: expr_var_index,
                },
            );

            // TODO see if we can get away with not adding this constraint at all
            def_pattern_state.vars.push(expr_var);
            let annotation_expected = {
                constraints.push_expected_type(FromAnnotation(
                    loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody {
                        region: annotation.region,
                    },
                    signature_index,
                ))
            };

            {
                let expr_type_index = constraints.push_variable(expr_var);
                def_pattern_state.constraints.push(constraints.equal_types(
                    expr_type_index,
                    annotation_expected,
                    Category::Storage(std::file!(), std::line!()),
                    Region::span_across(&annotation.region, &loc_body_expr.region),
                ));
            }

            constrain_typed_function_arguments_simple(
                types,
                constraints,
                env,
                loc_symbol.value,
                &mut def_pattern_state,
                &mut argument_pattern_state,
                &function_def.arguments,
                arg_types,
            );

            let closure_constraint = constrain_closure_size(
                types,
                constraints,
                loc_symbol.value,
                region,
                expr_var,
                &function_def.captured_symbols,
                closure_var,
                &mut vars,
            );

            let return_type_annotation_expected = constraints.push_expected_type(FromAnnotation(
                loc_pattern,
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                ret_type_index,
            ));

            let solved_fn_type = {
                // TODO(types-soa) optimize for Variable
                let pattern_types = types.from_old_type_slice(
                    function_def.arguments.iter().map(|a| Type::Variable(a.0)),
                );
                let lambda_set = types.from_old_type(&Type::Variable(function_def.closure_type));
                let ret_var = types.from_old_type(&Type::Variable(function_def.return_type));
                let fx_var = types.from_old_type(&Type::Variable(function_def.fx_type));

                let fn_type = types.function(pattern_types, lambda_set, ret_var, fx_var);
                constraints.push_type(types, fn_type)
            };

            let returns_constraint =
                env.with_fx_expectation(function_def.fx_type, Some(annotation.region), |env| {
                    constrain_function_return(
                        types,
                        constraints,
                        env,
                        loc_body_expr,
                        &function_def.early_returns,
                        return_type_annotation_expected,
                        function_def.return_type,
                        true,
                    )
                });

            vars.push(expr_var);

            let defs_constraint = constraints.and_constraint(argument_pattern_state.constraints);

            let cons = [
                // Store fx type first so errors are reported at call site
                constraints.store(
                    fx_type_index,
                    function_def.fx_type,
                    std::file!(),
                    std::line!(),
                ),
                constraints.let_constraint(
                    [],
                    argument_pattern_state.vars.into_iter().map(Loc::at_zero),
                    argument_pattern_state.headers,
                    defs_constraint,
                    returns_constraint,
                    // This is a syntactic function, it can be generalized
                    Generalizable(true),
                ),
                // Store the inferred ret var into the function type now, so that
                // when we check that the solved function type matches the annotation, we can
                // display the fully inferred return variable.
                constraints.store(
                    ret_type_index,
                    function_def.return_type,
                    std::file!(),
                    std::line!(),
                ),
                // Now, check the solved function type matches the annotation.
                constraints.equal_types(
                    solved_fn_type,
                    annotation_expected,
                    Category::Lambda,
                    region,
                ),
                // Check argument suffixes against usage
                constraints.and_constraint(argument_pattern_state.delayed_fx_suffix_constraints),
                // Finally put the solved closure type into the dedicated def expr variable.
                constraints.store(signature_index, expr_var, std::file!(), std::line!()),
                closure_constraint,
                constraints.flex_to_pure(function_def.fx_type),
            ];

            let expr_con = constraints.exists_many(vars, cons);

            constrain_function_def_make_constraint(
                constraints,
                new_rigid_variables,
                new_infer_variables,
                expr_con,
                body_con,
                def_pattern_state,
            )
        }
        None => {
            let expr_type = constraints.push_variable(expr_var);

            let expected_expr = constraints.push_expected_type(NoExpectation(expr_type));
            let expr_con = constrain_untyped_closure(
                types,
                constraints,
                env,
                loc_function_def.region,
                expected_expr,
                expr_var,
                function_def.closure_type,
                function_def.return_type,
                function_def.fx_type,
                &function_def.early_returns,
                &function_def.arguments,
                loc_expr,
                &function_def.captured_symbols,
                loc_symbol.value,
            );

            let expr_con = attach_resolution_constraints(constraints, env, expr_con);

            constrain_value_def_make_constraint(
                constraints,
                vec![],
                vec![],
                expr_con,
                body_con,
                loc_symbol,
                expr_var,
                expr_type,
                Generalizable(true), // this is a syntactic function
            )
        }
    }
}

fn constrain_destructure_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    index: usize,
    destructure_def_index: Index<DestructureDef>,
    body_con: Constraint,
) -> Constraint {
    let loc_expr = &declarations.expressions[index];
    let expr_var = declarations.variables[index];
    let expr_var_index = constraints.push_variable(expr_var);
    let opt_annotation = &declarations.annotations[index];

    let destructure_def = &declarations.destructs[destructure_def_index.index()];
    let loc_pattern = &destructure_def.loc_pattern;

    let mut def_pattern_state =
        constrain_def_pattern(types, constraints, env, loc_pattern, expr_var_index);

    def_pattern_state.vars.push(expr_var);

    match opt_annotation {
        Some(annotation) => {
            let arity = 1;
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let InstantiateRigids {
                signature,
                new_rigid_variables,
                new_infer_variables,
                has_explicit_inference_variables: _,
            } = instantiate_rigids(
                types,
                constraints,
                &annotation.signature,
                &annotation.introduced_variables,
                loc_pattern,
                &mut ftv,
                &mut def_pattern_state.headers,
                IsRecursiveDef::No,
            );

            let env = &mut Env {
                home: env.home,
                rigids: ftv,
                resolutions_to_make: vec![],
                fx_expectation: env.fx_expectation,
            };

            let signature_index = constraints.push_type(types, signature);

            let annotation_expected = constraints.push_expected_type(FromAnnotation(
                loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                signature_index,
            ));

            // This will fill in inference variables in the `signature` as well, so that we can
            // then take the signature as the source-of-truth without having to worry about
            // incompleteness.
            let ret_constraint = constrain_expr(
                types,
                constraints,
                env,
                loc_expr.region,
                &loc_expr.value,
                annotation_expected,
            );

            let cons = [
                ret_constraint,
                // Store type into AST vars. We use Store so errors aren't reported twice
                constraints.store(signature_index, expr_var, std::file!(), std::line!()),
            ];
            let expr_con = constraints.and_constraint(cons);

            constrain_function_def_make_constraint(
                constraints,
                new_rigid_variables,
                new_infer_variables,
                expr_con,
                body_con,
                def_pattern_state,
            )
        }
        None => {
            let expr_type = constraints.push_variable(expr_var);

            let expected_type = constraints.push_expected_type(NoExpectation(expr_type));
            let expr_con = constrain_expr(
                types,
                constraints,
                env,
                loc_expr.region,
                &loc_expr.value,
                expected_type,
            );

            constrain_function_def_make_constraint(
                constraints,
                vec![],
                vec![],
                expr_con,
                body_con,
                def_pattern_state,
            )
        }
    }
}

fn constrain_value_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    index: usize,
    body_con: Constraint,
) -> Constraint {
    let loc_expr = &declarations.expressions[index];
    let loc_symbol = declarations.symbols[index];
    let expr_var = declarations.variables[index];
    let opt_annotation = &declarations.annotations[index];

    let generalizable = Generalizable(is_generalizable_expr(&loc_expr.value));

    match opt_annotation {
        Some(annotation) => {
            let arity = 1;
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let InstantiateRigids {
                signature,
                new_rigid_variables,
                new_infer_variables,
                has_explicit_inference_variables: _,
            } = instantiate_rigids_simple(
                types,
                &annotation.signature,
                &annotation.introduced_variables,
                &mut ftv,
            );

            let env = &mut Env {
                home: env.home,
                rigids: ftv,
                resolutions_to_make: vec![],
                fx_expectation: env.fx_expectation,
            };

            let loc_pattern = Loc::at(loc_symbol.region, Pattern::Identifier(loc_symbol.value));

            let signature_index = constraints.push_type(types, signature);

            let annotation_expected = constraints.push_expected_type(FromAnnotation(
                loc_pattern,
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                signature_index,
            ));

            // This will fill in inference variables in the `signature` as well, so that we can
            // then take the signature as the source-of-truth without having to worry about
            // incompleteness.
            let ret_constraint = constrain_expr(
                types,
                constraints,
                env,
                loc_expr.region,
                &loc_expr.value,
                annotation_expected,
            );
            let ret_constraint = attach_resolution_constraints(constraints, env, ret_constraint);

            let cons = [
                ret_constraint,
                // Store type into AST vars. We use Store so errors aren't reported twice
                constraints.store(signature_index, expr_var, std::file!(), std::line!()),
            ];
            let expr_con = constraints.and_constraint(cons);

            constrain_value_def_make_constraint(
                constraints,
                new_rigid_variables,
                new_infer_variables,
                expr_con,
                body_con,
                loc_symbol,
                expr_var,
                signature_index,
                generalizable,
            )
        }
        None => {
            let expr_type = constraints.push_variable(expr_var);

            let expected_type = constraints.push_expected_type(NoExpectation(expr_type));
            let expr_con = constrain_expr(
                types,
                constraints,
                env,
                loc_expr.region,
                &loc_expr.value,
                expected_type,
            );

            let expr_con = attach_resolution_constraints(constraints, env, expr_con);

            constrain_value_def_make_constraint(
                constraints,
                vec![],
                vec![],
                expr_con,
                body_con,
                loc_symbol,
                expr_var,
                expr_type,
                generalizable,
            )
        }
    }
}

struct ConstrainedBranch {
    vars: Vec<Variable>,
    headers: VecMap<Symbol, Loc<TypeOrVar>>,
    pattern_constraints: Constraint,
    is_open_constrains: Vec<Constraint>,
    body_constraints: Constraint,
}

/// Constrain a when branch, returning (variables in pattern, symbols introduced in pattern, pattern constraint, body constraint).
/// We want to constraint all pattern constraints in a "when" before body constraints.
#[inline(always)]
fn constrain_when_branch_help(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: impl Fn(HumanIndex, Region) -> PExpected<TypeOrVar>,
    expr_expected: Expected<TypeOrVar>,
) -> ConstrainedBranch {
    let expr_expected = constraints.push_expected_type(expr_expected);
    let ret_constraint = constrain_expr(
        types,
        constraints,
        env,
        when_branch.value.region,
        &when_branch.value.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: VecMap::default(),
        vars: Vec::with_capacity(2),
        constraints: Vec::with_capacity(2),
        delayed_is_open_constraints: Vec::new(),
        delayed_fx_suffix_constraints: Vec::new(),
    };

    for (i, loc_pattern) in when_branch.patterns.iter().enumerate() {
        let pattern_expected = constraints.push_pat_expected_type(pattern_expected(
            HumanIndex::zero_based(i),
            loc_pattern.pattern.region,
        ));

        let mut partial_state = PatternState::default();
        constrain_pattern(
            types,
            constraints,
            env,
            &loc_pattern.pattern.value,
            loc_pattern.pattern.region,
            pattern_expected,
            &mut partial_state,
        );

        state.vars.extend(partial_state.vars);
        state.constraints.extend(partial_state.constraints);
        state
            .delayed_is_open_constraints
            .extend(partial_state.delayed_is_open_constraints);
        state
            .delayed_fx_suffix_constraints
            .extend(partial_state.delayed_fx_suffix_constraints);

        if i == 0 {
            state.headers.extend(partial_state.headers);
        } else {
            // Make sure the bound variables in the patterns on the same branch agree in their types.
            for (sym, all_branches_bound_typ) in state.headers.iter() {
                if let Some(this_bound_typ) = partial_state.headers.get(sym) {
                    let whole_typ = all_branches_bound_typ.value;
                    let this_typ = constraints
                        .push_expected_type(Expected::NoExpectation(this_bound_typ.value));

                    state.constraints.push(constraints.equal_types(
                        whole_typ,
                        this_typ,
                        Category::When,
                        this_bound_typ.region,
                    ));
                }

                // If the pattern doesn't bind all symbols introduced in the branch we'll have
                // reported a canonicalization error, but still might reach here; that's okay.
            }

            // Add any variables this pattern binds that the other patterns don't bind.
            // This will already have been reported as an error, but we still might be able to
            // solve their types.
            for (sym, ty) in partial_state.headers {
                if !state.headers.contains_key(&sym) {
                    state.headers.insert(sym, ty);
                }
            }
        }
    }

    let (pattern_constraints, delayed_is_open_constraints, body_constraints) =
        if let Some(loc_guard) = &when_branch.guard {
            let bool_index = constraints.push_variable(Variable::BOOL);
            let expected_guard = constraints.push_expected_type(Expected::ForReason(
                Reason::WhenGuard,
                bool_index,
                loc_guard.region,
            ));

            let guard_constraint = constrain_expr(
                types,
                constraints,
                env,
                region,
                &loc_guard.value,
                expected_guard,
            );

            // must introduce the headers from the pattern before constraining the guard
            let delayed_is_open_constraints = state.delayed_is_open_constraints;
            let state_constraints = constraints.and_constraint(state.constraints);
            let inner = constraints.let_constraint(
                [],
                [],
                [],
                guard_constraint,
                ret_constraint,
                // Never generalize identifiers introduced in branch guards
                Generalizable(false),
            );

            (state_constraints, delayed_is_open_constraints, inner)
        } else {
            let delayed_is_open_constraints = state.delayed_is_open_constraints;
            let state_constraints = constraints.and_constraint(state.constraints);
            (
                state_constraints,
                delayed_is_open_constraints,
                ret_constraint,
            )
        };

    ConstrainedBranch {
        vars: state.vars,
        headers: state.headers,
        pattern_constraints,
        is_open_constrains: delayed_is_open_constraints,
        body_constraints,
    }
}

fn constrain_field(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    field_var: Variable,
    loc_expr: &Loc<Expr>,
) -> (Type, Constraint) {
    let field_type = constraints.push_variable(field_var);
    let field_expected = constraints.push_expected_type(NoExpectation(field_type));
    let constraint = constrain_expr(
        types,
        constraints,
        env,
        loc_expr.region,
        &loc_expr.value,
        field_expected,
    );

    (Variable(field_var), constraint)
}

#[inline(always)]
fn constrain_empty_record(
    types: &mut Types,
    constraints: &mut Constraints,
    region: Region,
    expected: ExpectedTypeIndex,
) -> Constraint {
    let record_type_index = constraints.push_type(types, Types::EMPTY_RECORD);
    constraints.equal_types(record_type_index, expected, Category::Record, region)
}

fn add_host_annotation(
    types: &mut Types,
    constraints: &mut Constraints,
    host_exposed_annotation: Option<&(Variable, roc_can::def::Annotation)>,
    constraint: Constraint,
) -> Constraint {
    if let Some((var, ann)) = host_exposed_annotation {
        let host_annotation = {
            let type_index = types.from_old_type(&ann.signature);
            constraints.push_type(types, type_index)
        };

        let store_constr = constraints.store(host_annotation, *var, file!(), line!());

        constraints.and_constraint([store_constr, constraint])
    } else {
        constraint
    }
}

/// Constrain top-level module declarations
#[inline(always)]
pub fn constrain_decls(
    types: &mut Types,
    constraints: &mut Constraints,
    home: ModuleId,
    declarations: &Declarations,
) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;

    let mut env = Env {
        home,
        rigids: MutMap::default(),
        resolutions_to_make: vec![],
        fx_expectation: None,
    };

    debug_assert_eq!(declarations.declarations.len(), declarations.symbols.len());

    let mut index = 0;
    while index < declarations.len() {
        // Clear the rigids from the previous iteration.
        // rigids are not shared between top-level definitions
        env.rigids.clear();

        use roc_can::expr::DeclarationTag::*;
        let tag = declarations.declarations[index];

        match tag {
            Value => {
                constraint = constrain_value_def(
                    types,
                    constraints,
                    &mut env,
                    declarations,
                    index,
                    constraint,
                );

                constraint = add_host_annotation(
                    types,
                    constraints,
                    declarations.host_exposed_annotations.get(&index),
                    constraint,
                );
            }
            Function(function_def_index) => {
                constraint = constrain_function_def(
                    types,
                    constraints,
                    &mut env,
                    declarations,
                    index,
                    function_def_index,
                    constraint,
                );

                constraint = add_host_annotation(
                    types,
                    constraints,
                    declarations.host_exposed_annotations.get(&index),
                    constraint,
                );
            }
            Recursive(_) | TailRecursive(_) => {
                constraint = add_host_annotation(
                    types,
                    constraints,
                    declarations.host_exposed_annotations.get(&index),
                    constraint,
                );

                // for the type it does not matter that a recursive call is a tail call
                constraint = constrain_recursive_declarations(
                    types,
                    constraints,
                    &mut env,
                    declarations,
                    index..index + 1,
                    constraint,
                    IllegalCycleMark::empty(),
                );
            }
            Destructure(destructure_def_index) => {
                constraint = constrain_destructure_def(
                    types,
                    constraints,
                    &mut env,
                    declarations,
                    index,
                    destructure_def_index,
                    constraint,
                );
            }
            MutualRecursion { length, cycle_mark } => {
                // the next `length` defs belong to this group
                let length = length as usize;

                constraint = constrain_recursive_declarations(
                    types,
                    constraints,
                    &mut env,
                    declarations,
                    index + 1..index + 1 + length,
                    constraint,
                    cycle_mark,
                );

                index += length;
            }
            Expectation => {
                let loc_expr = &declarations.expressions[index];

                let bool_type = constraints.push_variable(Variable::BOOL);
                let expected = constraints.push_expected_type(Expected::ForReason(
                    Reason::ExpectCondition,
                    bool_type,
                    loc_expr.region,
                ));

                let expect_constraint = constrain_expr(
                    types,
                    constraints,
                    &mut env,
                    loc_expr.region,
                    &loc_expr.value,
                    expected,
                );

                constraint = constraints.let_constraint(
                    [],
                    [],
                    [],
                    expect_constraint,
                    constraint,
                    Generalizable(false),
                )
            }
        }

        index += 1;
    }

    // this assert make the "root" of the constraint wasn't dropped
    debug_assert!(constraints.contains_save_the_environment(&constraint));

    constraint
}

pub(crate) fn constrain_def_pattern(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    loc_pattern: &Loc<Pattern>,
    expr_type: TypeOrVar,
) -> PatternState {
    let pattern_expected = constraints.push_pat_expected_type(PExpected::NoExpectation(expr_type));

    let mut state = PatternState {
        headers: VecMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
        delayed_is_open_constraints: vec![],
        delayed_fx_suffix_constraints: vec![],
    };

    constrain_pattern(
        types,
        constraints,
        env,
        &loc_pattern.value,
        loc_pattern.region,
        pattern_expected,
        &mut state,
    );

    state
}

/// Generate constraints for a definition with a type signature
fn constrain_typed_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    body_con: Constraint,
    annotation: &roc_can::def::Annotation,
) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type_index = constraints.push_variable(expr_var);

    let mut def_pattern_state =
        constrain_def_pattern(types, constraints, env, &def.loc_pattern, expr_type_index);

    def_pattern_state.vars.push(expr_var);

    let arity = annotation.signature.arity();
    let rigids = &env.rigids;
    let mut ftv = rigids.clone();

    let InstantiateRigids {
        signature,
        new_rigid_variables,
        new_infer_variables,
        has_explicit_inference_variables: _,
    } = instantiate_rigids(
        types,
        constraints,
        &annotation.signature,
        &annotation.introduced_variables,
        &def.loc_pattern,
        &mut ftv,
        &mut def_pattern_state.headers,
        IsRecursiveDef::No,
    );

    let env = &mut Env {
        home: env.home,
        resolutions_to_make: vec![],
        rigids: ftv,
        fx_expectation: env.fx_expectation,
    };

    let signature_index = constraints.push_type(types, signature);

    let annotation_expected = constraints.push_expected_type(FromAnnotation(
        def.loc_pattern.clone(),
        arity,
        AnnotationSource::TypedBody {
            region: annotation.region,
        },
        signature_index,
    ));

    def_pattern_state.constraints.push(constraints.equal_types(
        expr_type_index,
        annotation_expected,
        Category::Storage(std::file!(), std::line!()),
        Region::span_across(&annotation.region, &def.loc_expr.region),
    ));

    // when a def is annotated, and its body is a closure, treat this
    // as a named function (in elm terms) for error messages.
    //
    // This means we get errors like "the first argument of `f` is weird"
    // instead of the more generic "something is wrong with the body of `f`"
    match (&def.loc_expr.value, types[signature]) {
        (
            Closure(ClosureData {
                function_type: fn_var,
                closure_type: closure_var,
                return_type: ret_var,
                early_returns,
                fx_type: fx_var,
                captured_symbols,
                arguments,
                loc_body,
                name,
                ..
            }),
            TypeTag::Function(_signature_closure_type, ret_type, fx_type),
        ) => {
            let arg_types = types.get_type_arguments(signature);

            // NOTE if we ever have problems with the closure, the ignored `_closure_type`
            // is probably a good place to start the investigation!

            let region = def.loc_expr.region;

            let loc_body_expr = &**loc_body;
            let mut argument_pattern_state = PatternState {
                headers: VecMap::default(),
                vars: Vec::with_capacity(arguments.len()),
                constraints: Vec::with_capacity(1),
                delayed_is_open_constraints: vec![],
                delayed_fx_suffix_constraints: Vec::with_capacity(arguments.len()),
            };
            let mut vars = Vec::with_capacity(argument_pattern_state.vars.capacity() + 1);
            let ret_var = *ret_var;
            let closure_var = *closure_var;
            let fx_var = *fx_var;
            let ret_type_index = constraints.push_type(types, ret_type);
            let fx_type_index = constraints.push_type(types, fx_type);

            vars.push(ret_var);
            vars.push(closure_var);
            vars.push(fx_var);

            constrain_typed_function_arguments(
                types,
                constraints,
                env,
                def,
                &mut def_pattern_state,
                &mut argument_pattern_state,
                arguments,
                arg_types,
            );

            let closure_constraint = constrain_closure_size(
                types,
                constraints,
                *name,
                region,
                *fn_var,
                captured_symbols,
                closure_var,
                &mut vars,
            );

            let solved_fn_type = {
                // TODO(types-soa) optimize for Variable
                let arg_types =
                    types.from_old_type_slice(arguments.iter().map(|a| Type::Variable(a.0)));
                let lambda_set = types.from_old_type(&Type::Variable(closure_var));
                let ret_var = types.from_old_type(&Type::Variable(ret_var));
                let fx_var = types.from_old_type(&Type::Variable(fx_var));

                let fn_type = types.function(arg_types, lambda_set, ret_var, fx_var);
                constraints.push_type(types, fn_type)
            };

            let return_type = constraints.push_expected_type(FromAnnotation(
                def.loc_pattern.clone(),
                arguments.len(),
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                ret_type_index,
            ));

            let returns_constraint =
                env.with_fx_expectation(fx_var, Some(annotation.region), |env| {
                    constrain_function_return(
                        types,
                        constraints,
                        env,
                        loc_body_expr,
                        early_returns,
                        return_type,
                        ret_var,
                        true,
                    )
                });

            vars.push(*fn_var);
            let defs_constraint = constraints.and_constraint(argument_pattern_state.constraints);

            let cons = [
                // Store fx type first so errors are reported at call site
                constraints.store(fx_type_index, fx_var, std::file!(), std::line!()),
                constraints.let_constraint(
                    [],
                    argument_pattern_state.vars.into_iter().map(Loc::at_zero),
                    argument_pattern_state.headers,
                    defs_constraint,
                    returns_constraint,
                    // This is a syntactic function, it can be generalized
                    Generalizable(true),
                ),
                // Check argument suffixes against usage
                constraints.and_constraint(argument_pattern_state.delayed_fx_suffix_constraints),
                // Store the inferred ret var into the function type now, so that
                // when we check that the solved function type matches the annotation, we can
                // display the fully inferred return variable.
                constraints.store(ret_type_index, ret_var, std::file!(), std::line!()),
                // Now, check the solved function type matches the annotation.
                constraints.equal_types(
                    solved_fn_type,
                    annotation_expected,
                    Category::Lambda,
                    region,
                ),
                // Finally put the solved closure type into the dedicated def expr variables.
                constraints.store(signature_index, *fn_var, std::file!(), std::line!()),
                constraints.store(signature_index, expr_var, std::file!(), std::line!()),
                closure_constraint,
                constraints.flex_to_pure(fx_var),
            ];

            let expr_con = constraints.exists_many(vars, cons);

            constrain_def_make_constraint(
                constraints,
                new_rigid_variables.into_iter(),
                new_infer_variables.into_iter(),
                expr_con,
                body_con,
                def_pattern_state,
                Generalizable(true),
            )
        }

        _ => {
            let annotation_expected = constraints.push_expected_type(FromAnnotation(
                def.loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                expr_type_index,
            ));

            let ret_constraint = constrain_expr(
                types,
                constraints,
                env,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            );
            let expr_con = attach_resolution_constraints(constraints, env, ret_constraint);

            let generalizable = Generalizable(is_generalizable_expr(&def.loc_expr.value));

            constrain_def_make_constraint(
                constraints,
                new_rigid_variables.into_iter(),
                new_infer_variables.into_iter(),
                expr_con,
                body_con,
                def_pattern_state,
                generalizable,
            )
        }
    }
}

fn constrain_typed_function_arguments(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    def_pattern_state: &mut PatternState,
    argument_pattern_state: &mut PatternState,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    arg_types: Slice<TypeTag>,
) {
    // ensure type matches the one in the annotation
    let opt_label = if let Pattern::Identifier(label) = def.loc_pattern.value {
        Some(label)
    } else {
        None
    };

    let it = arguments.iter().zip(arg_types).enumerate();
    for (index, ((pattern_var, annotated_mark, loc_pattern), ann)) in it {
        let pattern_var_index = constraints.push_variable(*pattern_var);
        let ann_index = constraints.push_type(types, ann);

        if loc_pattern.value.surely_exhaustive() {
            // OPT: we don't need to perform any type-level exhaustiveness checking.
            // Check instead only that the pattern unifies with the annotation type.
            let pattern_expected = constraints.push_pat_expected_type(PExpected::ForReason(
                PReason::TypedArg {
                    index: HumanIndex::zero_based(index),
                    opt_name: opt_label,
                },
                ann_index,
                loc_pattern.region,
            ));

            constrain_pattern(
                types,
                constraints,
                env,
                &loc_pattern.value,
                loc_pattern.region,
                pattern_expected,
                argument_pattern_state,
            );

            {
                // NOTE: because we perform an equality with part of the signature
                // this constraint must be to the def_pattern_state's constraints
                def_pattern_state.vars.push(*pattern_var);

                let ann_expected =
                    constraints.push_expected_type(Expected::NoExpectation(ann_index));
                let pattern_con = constraints.equal_types_var(
                    *pattern_var,
                    ann_expected,
                    Category::Storage(std::file!(), std::line!()),
                    loc_pattern.region,
                );

                def_pattern_state.constraints.push(pattern_con);
            }
        } else {
            // We need to check the types, and run exhaustiveness checking.
            let &AnnotatedMark {
                annotation_var,
                exhaustive,
            } = annotated_mark;

            def_pattern_state.vars.push(*pattern_var);
            def_pattern_state.vars.push(annotation_var);

            {
                // First, solve the type that the pattern is expecting to match in this
                // position.
                let pattern_expected =
                    constraints.push_pat_expected_type(PExpected::NoExpectation(pattern_var_index));
                constrain_pattern(
                    types,
                    constraints,
                    env,
                    &loc_pattern.value,
                    loc_pattern.region,
                    pattern_expected,
                    argument_pattern_state,
                );
            }

            {
                // Store the actual type in a variable.
                let ann_expected =
                    constraints.push_expected_type(Expected::NoExpectation(ann_index));
                argument_pattern_state
                    .constraints
                    .push(constraints.equal_types_var(
                        annotation_var,
                        ann_expected,
                        Category::Storage(file!(), line!()),
                        Region::zero(),
                    ));
            }

            {
                // let pattern_expected = PExpected::ForReason(
                //     PReason::TypedArg {
                //         index: HumanIndex::zero_based(index),
                //         opt_name: opt_label,
                //     },
                //     ann.clone(),
                //     loc_pattern.region,
                // );

                // Exhaustiveness-check the type in the pattern against what the
                // annotation wants.
                let sketched_rows = sketch_pattern_to_rows(loc_pattern.region, &loc_pattern.value);
                let category = loc_pattern.value.category();
                let expected = constraints.push_pat_expected_type(PExpected::ForReason(
                    PReason::TypedArg {
                        index: HumanIndex::zero_based(index),
                        opt_name: opt_label,
                    },
                    pattern_var_index,
                    loc_pattern.region,
                ));
                let exhaustive_constraint = constraints.exhaustive(
                    annotation_var,
                    loc_pattern.region,
                    Err((category, expected)),
                    sketched_rows,
                    ExhaustiveContext::BadArg,
                    exhaustive,
                );
                argument_pattern_state
                    .constraints
                    .push(exhaustive_constraint)
            }
        }
    }

    // There may be argument idents left over that don't line up with the function arity.
    // Add their patterns' symbols in so that they are present in the env, even though this will
    // wind up a type error.
    if arguments.len() > arg_types.len() {
        for (pattern_var, _annotated_mark, loc_pattern) in &arguments[arg_types.len()..] {
            let pattern_var_index = constraints.push_variable(*pattern_var);

            def_pattern_state.vars.push(*pattern_var);

            let pattern_expected =
                constraints.push_pat_expected_type(PExpected::NoExpectation(pattern_var_index));
            constrain_pattern(
                types,
                constraints,
                env,
                &loc_pattern.value,
                loc_pattern.region,
                pattern_expected,
                argument_pattern_state,
            );
        }
    }
}

fn constrain_typed_function_arguments_simple(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    symbol: Symbol,
    def_pattern_state: &mut PatternState,
    argument_pattern_state: &mut PatternState,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    arg_types: Slice<TypeTag>,
) {
    let it = arguments.iter().zip(arg_types).enumerate();
    for (index, ((pattern_var, annotated_mark, loc_pattern), ann)) in it {
        let pattern_var_index = constraints.push_variable(*pattern_var);
        let ann_index = constraints.push_type(types, ann);

        if loc_pattern.value.surely_exhaustive() {
            // OPT: we don't need to perform any type-level exhaustiveness checking.
            // Check instead only that the pattern unifies with the annotation type.
            let pattern_expected = constraints.push_pat_expected_type(PExpected::ForReason(
                PReason::TypedArg {
                    index: HumanIndex::zero_based(index),
                    opt_name: Some(symbol),
                },
                ann_index,
                loc_pattern.region,
            ));

            constrain_pattern(
                types,
                constraints,
                env,
                &loc_pattern.value,
                loc_pattern.region,
                pattern_expected,
                argument_pattern_state,
            );

            {
                // NOTE: because we perform an equality with part of the signature
                // this constraint must be to the def_pattern_state's constraints
                def_pattern_state.vars.push(*pattern_var);

                let ann_expected =
                    constraints.push_expected_type(Expected::NoExpectation(ann_index));
                let pattern_con = constraints.equal_types_var(
                    *pattern_var,
                    ann_expected,
                    Category::Storage(std::file!(), std::line!()),
                    loc_pattern.region,
                );

                def_pattern_state.constraints.push(pattern_con);
            }
        } else {
            // We need to check the types, and run exhaustiveness checking.
            let &AnnotatedMark {
                annotation_var,
                exhaustive,
            } = annotated_mark;

            def_pattern_state.vars.push(*pattern_var);
            def_pattern_state.vars.push(annotation_var);

            {
                // First, solve the type that the pattern is expecting to match in this
                // position.
                let pattern_expected =
                    constraints.push_pat_expected_type(PExpected::NoExpectation(pattern_var_index));
                constrain_pattern(
                    types,
                    constraints,
                    env,
                    &loc_pattern.value,
                    loc_pattern.region,
                    pattern_expected,
                    argument_pattern_state,
                );
            }

            {
                // Store the actual type in a variable.
                let expected_annotation =
                    constraints.push_expected_type(Expected::NoExpectation(ann_index));
                argument_pattern_state
                    .constraints
                    .push(constraints.equal_types_var(
                        annotation_var,
                        expected_annotation,
                        Category::Storage(file!(), line!()),
                        Region::zero(),
                    ));
            }

            {
                // Exhaustiveness-check the type in the pattern against what the
                // annotation wants.
                let sketched_rows = sketch_pattern_to_rows(loc_pattern.region, &loc_pattern.value);
                let category = loc_pattern.value.category();
                let expected = constraints.push_pat_expected_type(PExpected::ForReason(
                    PReason::TypedArg {
                        index: HumanIndex::zero_based(index),
                        opt_name: Some(symbol),
                    },
                    pattern_var_index,
                    loc_pattern.region,
                ));
                let exhaustive_constraint = constraints.exhaustive(
                    annotation_var,
                    loc_pattern.region,
                    Err((category, expected)),
                    sketched_rows,
                    ExhaustiveContext::BadArg,
                    exhaustive,
                );
                argument_pattern_state
                    .constraints
                    .push(exhaustive_constraint)
            }
        }
    }

    // There may be argument idents left over that don't line up with the function arity.
    // Add their patterns' symbols in so that they are present in the env, even though this will
    // wind up a type error.
    if arguments.len() > arg_types.len() {
        for (pattern_var, _annotated_mark, loc_pattern) in &arguments[arg_types.len()..] {
            let pattern_var_index = constraints.push_variable(*pattern_var);

            def_pattern_state.vars.push(*pattern_var);

            let pattern_expected =
                constraints.push_pat_expected_type(PExpected::NoExpectation(pattern_var_index));
            constrain_pattern(
                types,
                constraints,
                env,
                &loc_pattern.value,
                loc_pattern.region,
                pattern_expected,
                argument_pattern_state,
            );
        }
    }
}

#[inline(always)]
fn attach_resolution_constraints(
    constraints: &mut Constraints,
    env: &mut Env,
    constraint: Constraint,
) -> Constraint {
    let resolution_constrs =
        constraints.and_constraint(env.resolutions_to_make.drain(..).map(Constraint::Resolve));
    constraints.and_constraint([constraint, resolution_constrs])
}

fn constrain_let_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    body_con: Constraint,
    ignored_fx_var: Option<Variable>,
) -> Constraint {
    match &def.annotation {
        Some(annotation) => constrain_typed_def(types, constraints, env, def, body_con, annotation),
        None => {
            let expr_var = def.expr_var;
            let expr_type_index = constraints.push_variable(expr_var);

            let mut def_pattern_state =
                constrain_def_pattern(types, constraints, env, &def.loc_pattern, expr_type_index);

            def_pattern_state.vars.push(expr_var);
            // no annotation, so no extra work with rigids

            let expected = constraints.push_expected_type(NoExpectation(expr_type_index));

            let expr_con = match ignored_fx_var {
                None => constrain_expr(
                    types,
                    constraints,
                    env,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    expected,
                ),
                Some(fx_var) => {
                    let expr_con = env.with_fx_expectation(fx_var, None, |env| {
                        constrain_expr(
                            types,
                            constraints,
                            env,
                            def.loc_expr.region,
                            &def.loc_expr.value,
                            expected,
                        )
                    });

                    let effectful_constraint = if def.loc_expr.value.contains_any_early_returns() {
                        // If the statement has early returns, it doesn't need to be effectful to
                        // potentially affect the output of the containing function
                        Constraint::True
                    } else {
                        // If there are no early returns, it must be effectful or else it's dead code
                        Constraint::ExpectEffectful(
                            fx_var,
                            ExpectEffectfulReason::Ignored,
                            def.loc_pattern.region,
                        )
                    };

                    let enclosing_fx_constraint = constraints.fx_call(
                        fx_var,
                        FxCallKind::Ignored,
                        def.loc_pattern.region,
                        env.fx_expectation,
                    );

                    constraints.and_constraint([
                        expr_con,
                        enclosing_fx_constraint,
                        effectful_constraint,
                    ])
                }
            };
            let expr_con = attach_resolution_constraints(constraints, env, expr_con);

            let generalizable = Generalizable(is_generalizable_expr(&def.loc_expr.value));

            constrain_def_make_constraint(
                constraints,
                std::iter::empty(),
                std::iter::empty(),
                expr_con,
                body_con,
                def_pattern_state,
                generalizable,
            )
        }
    }
}

fn constrain_stmt_def(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    body_con: Constraint,
    fx_var: Variable,
) -> Constraint {
    let region = def.loc_expr.region;

    // Try to extract the fn name and region if the stmt is a call to a named function
    let (fn_name, error_region) = if let Expr::Call(boxed, _, _) = &def.loc_expr.value {
        let loc_fn_expr = &boxed.1;

        match loc_fn_expr.value {
            Var(symbol, _) | ParamsVar { symbol, .. } => (Some(symbol), loc_fn_expr.region),
            _ => (None, def.loc_expr.region),
        }
    } else {
        (None, def.loc_expr.region)
    };

    // Statement expressions must return an empty record
    let empty_record_index = constraints.push_type(types, Types::EMPTY_RECORD);
    let expect_empty_record = constraints.push_expected_type(ForReason(
        Reason::Stmt(fn_name),
        empty_record_index,
        error_region,
    ));

    let expr_con = env.with_fx_expectation(fx_var, None, |env| {
        constrain_expr(
            types,
            constraints,
            env,
            region,
            &def.loc_expr.value,
            expect_empty_record,
        )
    });

    let expr_con = attach_resolution_constraints(constraints, env, expr_con);

    let generalizable = Generalizable(is_generalizable_expr(&def.loc_expr.value));

    let body_con = constraints.let_constraint(
        std::iter::empty(),
        std::iter::empty(),
        std::iter::empty(),
        expr_con,
        body_con,
        generalizable,
    );

    let effectful_constraint = if def.loc_expr.value.contains_any_early_returns() {
        // If the statement has early returns, it doesn't need to be effectful to
        // potentially affect the output of the containing function
        Constraint::True
    } else {
        // If there are no early returns, it must be effectful or else it's dead code
        Constraint::ExpectEffectful(fx_var, ExpectEffectfulReason::Stmt, region)
    };

    let fx_call_kind = match fn_name {
        None => FxCallKind::Stmt,
        Some(name) => FxCallKind::Call(Some(name)),
    };

    // We have to unify the stmt fx with the enclosing fx
    // since we used the former to constrain the expr.
    let enclosing_fx_constraint =
        constraints.fx_call(fx_var, fx_call_kind, error_region, env.fx_expectation);

    constraints.and_constraint([body_con, effectful_constraint, enclosing_fx_constraint])
}

/// Create a let-constraint for a non-recursive def.
/// Recursive defs should always use `constrain_recursive_defs`.
pub(crate) fn constrain_def_make_constraint(
    constraints: &mut Constraints,
    annotation_rigid_variables: impl Iterator<Item = Loc<Variable>>,
    annotation_infer_variables: impl Iterator<Item = Loc<Variable>>,
    def_expr_con: Constraint,
    after_def_con: Constraint,
    def_pattern_state: PatternState,
    generalizable: Generalizable,
) -> Constraint {
    let all_flex_variables =
        (def_pattern_state.vars.into_iter().map(Loc::at_zero)).chain(annotation_infer_variables);

    let pattern_constraints = constraints.and_constraint(def_pattern_state.constraints);
    let def_pattern_and_body_con = constraints.and_constraint([pattern_constraints, def_expr_con]);

    constraints.let_constraint(
        annotation_rigid_variables,
        all_flex_variables,
        def_pattern_state.headers,
        def_pattern_and_body_con,
        after_def_con,
        generalizable,
    )
}

fn constrain_value_def_make_constraint(
    constraints: &mut Constraints,
    new_rigid_variables: Vec<Loc<Variable>>,
    new_infer_variables: Vec<Loc<Variable>>,
    expr_con: Constraint,
    body_con: Constraint,
    symbol: Loc<Symbol>,
    expr_var: Variable,
    expr_type: TypeOrVar,
    generalizable: Generalizable,
) -> Constraint {
    let def_con = constraints.let_constraint(
        [],
        new_infer_variables,
        [], // empty, because our functions have no arguments!
        Constraint::True,
        expr_con,
        generalizable,
    );

    let headers = [(symbol.value, Loc::at(symbol.region, expr_type))];

    constraints.let_constraint(
        new_rigid_variables,
        [Loc::at(symbol.region, expr_var)],
        headers,
        def_con,
        body_con,
        generalizable,
    )
}

fn constrain_function_def_make_constraint(
    constraints: &mut Constraints,
    new_rigid_variables: Vec<Loc<Variable>>,
    new_infer_variables: Vec<Loc<Variable>>,
    expr_con: Constraint,
    body_con: Constraint,
    def_pattern_state: PatternState,
) -> Constraint {
    let and_constraint = constraints.and_constraint(def_pattern_state.constraints);

    let def_con = constraints.let_constraint(
        [],
        new_infer_variables,
        [], // empty, because our functions have no arguments!
        and_constraint,
        expr_con,
        Generalizable(true),
    );

    constraints.let_constraint(
        new_rigid_variables,
        def_pattern_state.vars.into_iter().map(Loc::at_zero),
        def_pattern_state.headers,
        def_con,
        body_con,
        Generalizable(true),
    )
}

fn constrain_closure_size(
    types: &mut Types,
    constraints: &mut Constraints,
    name: Symbol,
    region: Region,
    ambient_function: Variable,
    captured_symbols: &[(Symbol, Variable)],
    closure_var: Variable,
    variables: &mut Vec<Variable>,
) -> Constraint {
    debug_assert!(variables.iter().any(|s| *s == closure_var));

    let mut captured_types = Vec::with_capacity(captured_symbols.len());
    let mut captured_symbols_constraints = Vec::with_capacity(captured_symbols.len());

    for (symbol, var) in captured_symbols {
        // make sure the variable is registered
        variables.push(*var);

        // this symbol is captured, so it must be part of the closure type
        captured_types.push(Type::Variable(*var));

        // make the variable equal to the looked-up type of symbol
        let store_var_index = constraints.push_variable(*var);
        let store_into = constraints.push_expected_type(Expected::NoExpectation(store_var_index));
        captured_symbols_constraints.push(constraints.lookup(*symbol, store_into, Region::zero()));
    }

    let finalizer = {
        // pick a more efficient representation if we don't actually capture anything
        let closure_type = {
            let typ = types.from_old_type(&Type::ClosureTag {
                name,
                captures: captured_types,
                ambient_function,
            });
            constraints.push_type(types, typ)
        };
        let clos_type = constraints.push_expected_type(NoExpectation(closure_type));
        constraints.equal_types_var(closure_var, clos_type, Category::ClosureSize, region)
    };

    captured_symbols_constraints.push(finalizer);

    constraints.and_constraint(captured_symbols_constraints)
}

pub struct InstantiateRigids {
    pub signature: Index<TypeTag>,
    pub new_rigid_variables: Vec<Loc<Variable>>,
    pub new_infer_variables: Vec<Loc<Variable>>,
    /// Whether the annotation has explicit inference variables `_`.
    /// Annotations with inference variables are handled specially during typechecking of mutually recursive defs,
    /// because they are not guaranteed to be generalized (XREF(rec-def-strategy)).
    /// Ideally, this special-casing would be removed in the future.
    pub has_explicit_inference_variables: bool,
}

#[derive(PartialEq, Eq)]
enum IsRecursiveDef {
    Yes,
    No,
}

fn instantiate_rigids(
    types: &mut Types,
    constraints: &mut Constraints,
    annotation: &Type,
    introduced_vars: &IntroducedVariables,
    loc_pattern: &Loc<Pattern>,
    ftv: &mut MutMap<Lowercase, Variable>, // rigids defined before the current annotation
    headers: &mut VecMap<Symbol, Loc<TypeOrVar>>,
    is_recursive_def: IsRecursiveDef,
) -> InstantiateRigids {
    let mut new_rigid_variables: Vec<Loc<Variable>> = vec![];
    let mut new_infer_variables: Vec<Loc<Variable>> = vec![];

    let mut generate_fresh_ann = |types: &mut Types| {
        let mut annotation = annotation.clone();

        let mut rigid_substitution: MutMap<Variable, Variable> = MutMap::default();
        for named in introduced_vars.iter_named() {
            use std::collections::hash_map::Entry::*;

            match ftv.entry(named.name().clone()) {
                Occupied(occupied) => {
                    let existing_rigid = occupied.get();
                    rigid_substitution.insert(named.variable(), *existing_rigid);
                }
                Vacant(vacant) => {
                    // It's possible to use this rigid in nested defs
                    vacant.insert(named.variable());
                    new_rigid_variables.push(Loc::at(named.first_seen(), named.variable()));
                }
            }
        }

        // wildcards are always freshly introduced in this annotation
        new_rigid_variables.extend(introduced_vars.wildcards.iter().copied());

        // lambda set vars are always freshly introduced in this annotation
        new_rigid_variables.extend(introduced_vars.lambda_sets.iter().map(|&v| Loc::at_zero(v)));

        // ext-infer vars are always freshly introduced in this annotation
        new_infer_variables.extend(
            introduced_vars
                .infer_ext_in_output
                .iter()
                .map(|&v| Loc::at_zero(v)),
        );

        let has_explicit_inference_variables = !introduced_vars.inferred.is_empty();
        new_infer_variables.extend(introduced_vars.inferred.iter().copied());

        // Instantiate rigid variables
        if !rigid_substitution.is_empty() {
            annotation.substitute_variables(&rigid_substitution);
        }

        (
            types.from_old_type(&annotation),
            has_explicit_inference_variables,
        )
    };

    let (signature, has_explicit_inference_variables) = generate_fresh_ann(types);
    {
        // If this is a recursive def, we must also generate a fresh annotation to be used as the
        // type annotation that will be used in the first def headers introduced during the solving
        // of the recursive definition.
        //
        // That is, this annotation serves as step (1) of XREF(rec-def-strategy). We don't want to
        // link to the final annotation, since it may be incomplete (or incorrect, see step (1)).
        // So, we generate a fresh annotation here, and return a separate fresh annotation below;
        // the latter annotation is the one used to construct the finalized type.
        let annotation_index = if is_recursive_def == IsRecursiveDef::Yes {
            generate_fresh_ann(types).0
        } else {
            signature
        };

        let loc_annotation_ref = Loc::at(loc_pattern.region, annotation_index);
        if let Pattern::Identifier(symbol) = loc_pattern.value {
            let annotation_index = constraints.push_type(types, annotation_index);
            headers.insert(symbol, Loc::at(loc_pattern.region, annotation_index));
        } else if let Some(new_headers) = crate::pattern::headers_from_annotation(
            types,
            constraints,
            &loc_pattern.value,
            &loc_annotation_ref,
        ) {
            headers.extend(new_headers)
        }
    }

    InstantiateRigids {
        signature,
        new_rigid_variables,
        new_infer_variables,
        has_explicit_inference_variables,
    }
}

fn instantiate_rigids_simple(
    types: &mut Types,
    annotation: &Type,
    introduced_vars: &IntroducedVariables,
    ftv: &mut MutMap<Lowercase, Variable>, // rigids defined before the current annotation
) -> InstantiateRigids {
    let mut annotation = annotation.clone();
    let mut new_rigid_variables: Vec<Loc<Variable>> = Vec::new();

    let mut rigid_substitution: MutMap<Variable, Variable> = MutMap::default();
    for named in introduced_vars.iter_named() {
        use std::collections::hash_map::Entry::*;

        match ftv.entry(named.name().clone()) {
            Occupied(occupied) => {
                let existing_rigid = occupied.get();
                rigid_substitution.insert(named.variable(), *existing_rigid);
            }
            Vacant(vacant) => {
                // It's possible to use this rigid in nested defs
                vacant.insert(named.variable());
                new_rigid_variables.push(Loc::at(named.first_seen(), named.variable()));
            }
        }
    }

    // wildcards are always freshly introduced in this annotation
    new_rigid_variables.extend(introduced_vars.wildcards.iter().copied());

    // lambda set vars are always freshly introduced in this annotation
    new_rigid_variables.extend(introduced_vars.lambda_sets.iter().map(|&v| Loc::at_zero(v)));

    let has_explicit_inference_variables = !introduced_vars.inferred.is_empty();
    let mut new_infer_variables: Vec<Loc<Variable>> = introduced_vars.inferred.clone();

    // ext-infer vars are always freshly introduced in this annotation
    new_infer_variables.extend(
        introduced_vars
            .infer_ext_in_output
            .iter()
            .map(|&v| Loc::at_zero(v)),
    );

    // Instantiate rigid variables
    if !rigid_substitution.is_empty() {
        annotation.substitute_variables(&rigid_substitution);
    }

    InstantiateRigids {
        signature: types.from_old_type(&annotation),
        new_rigid_variables,
        new_infer_variables,
        has_explicit_inference_variables,
    }
}

fn constrain_recursive_declarations(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    range: Range<usize>,
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    rec_defs_help_simple(
        types,
        constraints,
        env,
        declarations,
        range,
        body_con,
        cycle_mark,
    )
}

fn constraint_recursive_function(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    index: usize,
    function_def_index: Index<Loc<FunctionDef>>,
    rigid_info: &mut Info,
    flex_info: &mut Info,
) {
    let loc_expr = &declarations.expressions[index];
    let loc_symbol = declarations.symbols[index];
    let expr_var = declarations.variables[index];
    let opt_annotation = &declarations.annotations[index];

    let loc_function_def = &declarations.function_bodies[function_def_index.index()];
    let function_def = &loc_function_def.value;

    match opt_annotation {
        None => {
            let expr_type_index = constraints.push_variable(expr_var);

            let expected_expr = constraints.push_expected_type(NoExpectation(expr_type_index));
            let expr_con = constrain_untyped_closure(
                types,
                constraints,
                env,
                loc_function_def.region,
                expected_expr,
                expr_var,
                function_def.closure_type,
                function_def.return_type,
                function_def.fx_type,
                &function_def.early_returns,
                &function_def.arguments,
                loc_expr,
                &function_def.captured_symbols,
                loc_symbol.value,
            );

            let expr_con = attach_resolution_constraints(constraints, env, expr_con);
            let def_con = expr_con;

            flex_info.vars.push(Loc::at_zero(expr_var));
            flex_info.constraints.push(def_con);
            flex_info.def_types.insert(
                loc_symbol.value,
                Loc::at(loc_symbol.region, expr_type_index),
            );
        }

        Some(annotation) => {
            let arity = annotation.signature.arity();
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let InstantiateRigids {
                signature,
                new_rigid_variables,
                new_infer_variables,
                has_explicit_inference_variables: _,
            } = instantiate_rigids_simple(
                types,
                &annotation.signature,
                &annotation.introduced_variables,
                &mut ftv,
            );

            let loc_pattern = Loc::at(loc_symbol.region, Pattern::Identifier(loc_symbol.value));

            let signature_index = constraints.push_type(types, signature);

            let annotation_expected = constraints.push_expected_type(FromAnnotation(
                loc_pattern,
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                signature_index,
            ));

            let (arg_types, _signature_closure_type, ret_type, fx_type) = match types[signature] {
                TypeTag::Function(signature_closure_type, ret_type, fx_type) => (
                    types.get_type_arguments(signature),
                    signature_closure_type,
                    ret_type,
                    fx_type,
                ),
                _ => todo!("TODO {:?}", (loc_symbol, types[signature])),
            };

            let region = loc_function_def.region;

            let loc_body_expr = loc_expr;
            let mut argument_pattern_state = PatternState {
                headers: VecMap::default(),
                vars: Vec::with_capacity(function_def.arguments.len()),
                constraints: Vec::with_capacity(1),
                delayed_is_open_constraints: vec![],
                delayed_fx_suffix_constraints: Vec::with_capacity(function_def.arguments.len()),
            };
            let mut vars = Vec::with_capacity(argument_pattern_state.vars.capacity() + 1);
            let ret_var = function_def.return_type;
            let fx_var = function_def.fx_type;
            let closure_var = function_def.closure_type;
            let ret_type_index = constraints.push_type(types, ret_type);
            let fx_type_index = constraints.push_type(types, fx_type);

            vars.push(ret_var);
            vars.push(closure_var);
            vars.push(fx_var);

            let mut def_pattern_state = PatternState::default();

            def_pattern_state.headers.insert(
                loc_symbol.value,
                Loc {
                    region,
                    // TODO coalesce with other `signature_index`.
                    // This doesn't yet work; needs investigation as to why.
                    // My guess is that when types SoA lands, this might just resolve itself, since
                    // types will be composed from variables to begin with.
                    value: {
                        let typ =
                            types.clone_with_variable_substitutions(signature, &Default::default());
                        constraints.push_type(types, typ)
                    },
                },
            );

            constrain_typed_function_arguments_simple(
                types,
                constraints,
                env,
                loc_symbol.value,
                &mut def_pattern_state,
                &mut argument_pattern_state,
                &function_def.arguments,
                arg_types,
            );

            let pattern_types = types
                .from_old_type_slice(function_def.arguments.iter().map(|a| Type::Variable(a.0)));

            let closure_constraint = constrain_closure_size(
                types,
                constraints,
                loc_symbol.value,
                region,
                expr_var,
                &function_def.captured_symbols,
                closure_var,
                &mut vars,
            );

            let fn_type = {
                // TODO(types-soa) optimize for Variable
                let lambda_set = types.from_old_type(&Type::Variable(closure_var));
                let typ = types.function(pattern_types, lambda_set, ret_type, fx_type);
                constraints.push_type(types, typ)
            };

            let returns_constraint =
                env.with_fx_expectation(fx_var, Some(annotation.region), |env| {
                    let expected = constraints.push_expected_type(ForReason(
                        Reason::FunctionOutput,
                        ret_type_index,
                        region,
                    ));

                    constrain_function_return(
                        types,
                        constraints,
                        env,
                        loc_body_expr,
                        &function_def.early_returns,
                        expected,
                        ret_var,
                        true,
                    )
                });

            vars.push(expr_var);

            let state_constraints = constraints.and_constraint(argument_pattern_state.constraints);
            let cons = [
                constraints.store(fx_type_index, fx_var, std::file!(), std::line!()),
                constraints.let_constraint(
                    [],
                    argument_pattern_state.vars.into_iter().map(Loc::at_zero),
                    argument_pattern_state.headers,
                    state_constraints,
                    returns_constraint,
                    // Syntactic function can be generalized
                    Generalizable(true),
                ),
                // Check argument suffixes against usage
                constraints.and_constraint(argument_pattern_state.delayed_fx_suffix_constraints),
                constraints.equal_types(fn_type, annotation_expected, Category::Lambda, region),
                // "fn_var is equal to the closure's type" - fn_var is used in code gen
                // Store type into AST vars. We use Store so errors aren't reported twice
                constraints.store(signature_index, expr_var, std::file!(), std::line!()),
                constraints.store(ret_type_index, ret_var, std::file!(), std::line!()),
                closure_constraint,
                constraints.flex_to_pure(fx_var),
            ];

            let and_constraint = constraints.and_constraint(cons);
            let def_con = constraints.exists(vars, and_constraint);

            rigid_info.vars.extend(&new_rigid_variables);
            flex_info.vars.extend(&new_infer_variables);

            rigid_info.constraints.push({
                // Solve the body of the recursive function, making sure it lines up with the
                // signature.
                //
                // This happens when we're checking that the def of a recursive function actually
                // aligns with what the (mutually-)recursive signature says, so finish
                // generalization of the function.
                let rigids = new_rigid_variables;
                let flex_pattern_vars = def_pattern_state.vars.into_iter().map(Loc::at_zero);
                let flex = flex_pattern_vars.chain(new_infer_variables);

                constraints.let_constraint(
                    rigids,
                    flex,
                    // Although we will have already introduced the headers of the def in the
                    // outermost scope when we introduced the rigid variables, we now re-introduce
                    // them to force an occurs check and appropriate fixing, since we might end up
                    // inferring recursive types at inference variable points. E.g.
                    //
                    //   f : _ -> _
                    //   f = \F c -> F (List.map f c)
                    //
                    // TODO: I (Ayaz) believe we can considerably simplify all this.
                    def_pattern_state.headers.clone(),
                    def_con,
                    Constraint::True,
                    Generalizable(true),
                )
            });
            rigid_info.def_types.extend(def_pattern_state.headers);
        }
    }
}

pub fn rec_defs_help_simple(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    declarations: &Declarations,
    range: Range<usize>,
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    let length = range.end - range.start;

    // We partition recursive defs into three buckets:
    //   rigid: those with fully-elaborated type annotations (no inference vars), e.g. a -> b
    //   hybrid: those with type annotations containing an inference variable, e.g. _ -> b
    //   flex: those without a type annotation
    let mut rigid_info = Info::with_capacity(length);
    let mut hybrid_and_flex_info = Info::with_capacity(length);

    let mut loc_symbols = Vec::with_capacity(length);
    let mut expr_regions = Vec::with_capacity(length);

    // Rec defs are generalizable only if everything in the cycle is generalizable.
    let generalizable = {
        let generalizable = range.clone().all(|i| match declarations.declarations[i] {
            DeclarationTag::Value => {
                let loc_expr = &declarations.expressions[i];
                is_generalizable_expr(&loc_expr.value)
            }
            _ => true, // this must be a function
        });
        Generalizable(generalizable)
    };

    for index in range {
        // Clear the rigids from the previous iteration.
        // rigids are not shared between top-level definitions
        env.rigids.clear();

        let loc_symbol = declarations.symbols[index];
        loc_symbols.push((loc_symbol.value, loc_symbol.region));

        match declarations.declarations[index] {
            DeclarationTag::Value => {
                let expr_var = declarations.variables[index];
                let expr_var_index = constraints.push_variable(expr_var);
                let opt_annotation = &declarations.annotations[index];

                let loc_expr = &declarations.expressions[index];
                let expr_region = loc_expr.region;
                expr_regions.push(expr_region);

                match opt_annotation {
                    None => {
                        let expected =
                            constraints.push_expected_type(NoExpectation(expr_var_index));
                        let expr_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_expr.region,
                            &loc_expr.value,
                            expected,
                        );
                        let expr_con = attach_resolution_constraints(constraints, env, expr_con);

                        let def_con = expr_con;

                        hybrid_and_flex_info
                            .vars
                            .push(Loc::at(expr_region, expr_var));
                        hybrid_and_flex_info.constraints.push(def_con);
                        hybrid_and_flex_info
                            .def_types
                            .insert(loc_symbol.value, Loc::at(loc_symbol.region, expr_var_index));
                    }
                    Some(annotation) => {
                        let arity = annotation.signature.arity();
                        let rigids = &env.rigids;
                        let mut ftv = rigids.clone();

                        let InstantiateRigids {
                            signature,
                            new_rigid_variables,
                            new_infer_variables,
                            has_explicit_inference_variables,
                        } = instantiate_rigids_simple(
                            types,
                            &annotation.signature,
                            &annotation.introduced_variables,
                            &mut ftv,
                        );

                        let loc_pattern =
                            Loc::at(loc_symbol.region, Pattern::Identifier(loc_symbol.value));

                        let is_hybrid = has_explicit_inference_variables;

                        hybrid_and_flex_info.vars.extend(new_infer_variables);

                        let signature_index = constraints.push_type(types, signature);

                        let annotation_expected = FromAnnotation(
                            loc_pattern.clone(),
                            arity,
                            AnnotationSource::TypedBody {
                                region: annotation.region,
                            },
                            signature_index,
                        );

                        let expected = constraints.push_expected_type(annotation_expected);

                        let ret_constraint = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_expr.region,
                            &loc_expr.value,
                            expected,
                        );
                        let ret_constraint =
                            attach_resolution_constraints(constraints, env, ret_constraint);

                        let cons = [
                            ret_constraint,
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            constraints.store(
                                signature_index,
                                expr_var,
                                std::file!(),
                                std::line!(),
                            ),
                        ];
                        let def_con = constraints.and_constraint(cons);

                        let loc_type = Loc::at(loc_symbol.region, signature_index);
                        if is_hybrid {
                            hybrid_and_flex_info.vars.extend(&new_rigid_variables);
                            hybrid_and_flex_info.constraints.push(def_con);
                            hybrid_and_flex_info
                                .def_types
                                .insert(loc_symbol.value, loc_type);
                        } else {
                            rigid_info.vars.extend(&new_rigid_variables);

                            rigid_info.constraints.push(constraints.let_constraint(
                                new_rigid_variables,
                                [Loc::at(expr_region, expr_var)],
                                [], // no headers introduced (at this level)
                                def_con,
                                Constraint::True,
                                generalizable,
                            ));
                            rigid_info.def_types.insert(loc_symbol.value, loc_type);
                        }
                    }
                }
            }
            DeclarationTag::Recursive(f_index) | DeclarationTag::TailRecursive(f_index) => {
                expr_regions.push(declarations.function_bodies[f_index.index()].region);

                constraint_recursive_function(
                    types,
                    constraints,
                    env,
                    declarations,
                    index,
                    f_index,
                    &mut rigid_info,
                    &mut hybrid_and_flex_info,
                );
            }
            _ => unreachable!(),
        }
    }

    // NB(rec-def-strategy) Strategy for recursive defs:
    //
    // 1. Let-generalize all rigid annotations. These are the source of truth we'll solve
    //    everything else with. If there are circular type errors here, they will be caught
    //    during the let-generalization.
    //
    // 2. Introduce all symbols of the flex + hybrid defs, but don't generalize them yet.
    //    Now, solve those defs' bodies. This way, when checking something like
    //      f = \x -> f [x]
    //    we introduce `f: b -> c`, then constrain the call `f [x]`,
    //    forcing `b -> c ~ List b -> c` and correctly picking up a recursion error.
    //    Had we generalized `b -> c`, the call `f [x]` would have been generalized, and this
    //    error would not be found.
    //
    //    - This works just as well for mutually recursive defs.
    //    - For hybrid defs, we also ensure solved types agree with what the
    //      elaborated parts of their type annotations demand.
    //
    // 3. Now properly let-generalize the flex + hybrid defs, since we now know their types and
    //    that they don't have circular type errors.
    //
    // 4. Solve the bodies of the typed body defs, and check that they agree the types of the type
    //    annotation.
    //
    // 5. Solve the rest of the program that happens after this recursive def block.

    // 2. Solve untyped defs without generalization of their symbols.
    let untyped_body_constraints = constraints.and_constraint(hybrid_and_flex_info.constraints);
    let untyped_def_symbols_constr = constraints.let_constraint(
        [],
        [],
        hybrid_and_flex_info.def_types.clone(),
        Constraint::True,
        untyped_body_constraints,
        generalizable,
    );

    // an extra constraint that propagates information to the solver to check for invalid recursion
    // and generate a good error message there.
    let cycle_constraint = constraints.check_cycle(loc_symbols, expr_regions, cycle_mark);

    // 4 + 5. Solve the typed body defs, and the rest of the program.
    let typed_body_constraints = constraints.and_constraint(rigid_info.constraints);
    let typed_body_and_final_constr =
        constraints.and_constraint([typed_body_constraints, cycle_constraint, body_con]);

    // 3. Properly generalize untyped defs after solving them.
    let inner = constraints.let_constraint(
        [],
        hybrid_and_flex_info.vars,
        hybrid_and_flex_info.def_types,
        untyped_def_symbols_constr,
        typed_body_and_final_constr,
        generalizable,
    );

    // 1. Let-generalize annotations we know.
    constraints.let_constraint(
        rigid_info.vars,
        [],
        rigid_info.def_types,
        Constraint::True,
        inner,
        generalizable,
    )
}

/// A let-bound expression is generalizable if it is
///   - a syntactic function under an opaque wrapper
///   - a number literal under an opaque wrapper
fn is_generalizable_expr(mut expr: &Expr) -> bool {
    loop {
        match expr {
            Num(..) | Int(..) | Float(..) => return true,
            Closure(_) => return true,
            RecordAccessor(_) => {
                // RecordAccessor functions `.field` are equivalent to closures `\r -> r.field`, no need to weaken them.
                return true;
            }
            OpaqueWrapFunction(_) => {
                // Opaque wrapper functions `@Q` are equivalent to closures `\x -> @Q x`, no need to weaken them.
                return true;
            }
            RuntimeError(roc_problem::can::RuntimeError::NoImplementation)
            | RuntimeError(roc_problem::can::RuntimeError::NoImplementationNamed { .. }) => {
                // Allow generalization of signatures with no implementation
                return true;
            }
            OpaqueRef { argument, .. } => expr = &argument.1.value,
            ImportParams(_, _, Some((_, params))) => expr = params,
            ImportParams(_, _, None) => return false,
            Str(_)
            | IngestedFile(..)
            | List { .. }
            | SingleQuote(_, _, _, _)
            | When { .. }
            | If { .. }
            | LetRec(_, _, _)
            | LetNonRec(_, _)
            | Call(_, _, _)
            | RunLowLevel { .. }
            | ForeignCall { .. }
            | EmptyRecord
            | Expr::Record { .. }
            | Expr::Tuple { .. }
            | Crash { .. }
            | RecordAccess { .. }
            | TupleAccess { .. }
            | RecordUpdate { .. }
            | Expect { .. }
            | Dbg { .. }
            | Try { .. }
            | Return { .. }
            | RuntimeError(..)
            | ZeroArgumentTag { .. }
            | Tag { .. }
            | AbilityMember(..)
            | Var(..)
            | ParamsVar { .. } => return false,
        }
    }
}

fn constrain_recursive_defs(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    defs: &[Def],
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    rec_defs_help(types, constraints, env, defs, body_con, cycle_mark)
}

fn rec_defs_help(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    defs: &[Def],
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    // We partition recursive defs into three buckets:
    //   rigid: those with fully-elaborated type annotations (no inference vars), e.g. a -> b
    //   hybrid: those with type annotations containing an inference variable, e.g. _ -> b
    //   flex: those without a type annotation
    let mut rigid_info = Info::with_capacity(defs.len());
    let mut hybrid_and_flex_info = Info::with_capacity(defs.len());

    // Rec defs are generalizable only if everything in the cycle is generalizable.
    let generalizable = {
        let generalizable = defs
            .iter()
            .all(|d| is_generalizable_expr(&d.loc_expr.value));
        Generalizable(generalizable)
    };

    for def in defs {
        let expr_var = def.expr_var;
        let expr_type_index = constraints.push_variable(expr_var);

        let mut def_pattern_state =
            constrain_def_pattern(types, constraints, env, &def.loc_pattern, expr_type_index);

        def_pattern_state.vars.push(expr_var);

        match &def.annotation {
            None => {
                let expected = constraints.push_expected_type(NoExpectation(expr_type_index));
                let expr_con = constrain_expr(
                    types,
                    constraints,
                    env,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    expected,
                );
                let expr_con = attach_resolution_constraints(constraints, env, expr_con);

                let def_con = expr_con;

                hybrid_and_flex_info
                    .vars
                    .extend(def_pattern_state.vars.into_iter().map(Loc::at_zero));
                hybrid_and_flex_info.constraints.push(def_con);
                hybrid_and_flex_info
                    .def_types
                    .extend(def_pattern_state.headers);
            }

            Some(annotation) => {
                let arity = annotation.signature.arity();
                let mut ftv = env.rigids.clone();

                let InstantiateRigids {
                    signature,
                    new_rigid_variables,
                    new_infer_variables,
                    has_explicit_inference_variables,
                } = instantiate_rigids(
                    types,
                    constraints,
                    &annotation.signature,
                    &annotation.introduced_variables,
                    &def.loc_pattern,
                    &mut ftv,
                    &mut def_pattern_state.headers,
                    IsRecursiveDef::Yes,
                );

                let is_hybrid = has_explicit_inference_variables;

                hybrid_and_flex_info.vars.extend(&new_infer_variables);

                let signature_index = constraints.push_type(types, signature);

                let annotation_expected = FromAnnotation(
                    def.loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody {
                        region: annotation.region,
                    },
                    signature_index,
                );

                // when a def is annotated, and it's body is a closure, treat this
                // as a named function (in elm terms) for error messages.
                //
                // This means we get errors like "the first argument of `f` is weird"
                // instead of the more generic "something is wrong with the body of `f`"
                match (&def.loc_expr.value, types[signature]) {
                    (
                        Closure(ClosureData {
                            function_type: fn_var,
                            closure_type: closure_var,
                            return_type: ret_var,
                            early_returns,
                            fx_type: fx_var,
                            captured_symbols,
                            arguments,
                            loc_body,
                            name,
                            ..
                        }),
                        TypeTag::Function(_closure_type, ret_type, fx_type),
                    ) => {
                        // NOTE if we ever have trouble with closure type unification, the ignored
                        // `_closure_type` here is a good place to start investigating

                        let arg_types = types.get_type_arguments(signature);

                        let expected = annotation_expected;
                        let region = def.loc_expr.region;

                        let loc_body_expr = &**loc_body;
                        let mut argument_pattern_state = PatternState {
                            headers: VecMap::default(),
                            vars: Vec::with_capacity(arguments.len()),
                            constraints: Vec::with_capacity(1),
                            delayed_is_open_constraints: vec![],
                            delayed_fx_suffix_constraints: Vec::with_capacity(arguments.len()),
                        };
                        let mut vars =
                            Vec::with_capacity(argument_pattern_state.vars.capacity() + 1);
                        let ret_var = *ret_var;
                        let fx_var = *fx_var;
                        let closure_var = *closure_var;
                        let ret_type_index = constraints.push_type(types, ret_type);
                        let fx_type_index = constraints.push_type(types, fx_type);

                        vars.push(ret_var);
                        vars.push(closure_var);
                        vars.push(fx_var);

                        constrain_typed_function_arguments(
                            types,
                            constraints,
                            env,
                            def,
                            &mut def_pattern_state,
                            &mut argument_pattern_state,
                            arguments,
                            arg_types,
                        );
                        let pattern_types = types
                            .from_old_type_slice(arguments.iter().map(|a| Type::Variable(a.0)));

                        let closure_constraint = constrain_closure_size(
                            types,
                            constraints,
                            *name,
                            region,
                            *fn_var,
                            captured_symbols,
                            closure_var,
                            &mut vars,
                        );

                        let fn_type_index = {
                            // TODO(types-soa) optimize for variable
                            let lambda_set = types.from_old_type(&Type::Variable(closure_var));
                            let typ = types.function(pattern_types, lambda_set, ret_type, fx_type);
                            constraints.push_type(types, typ)
                        };
                        let returns_constraint =
                            env.with_fx_expectation(fx_var, Some(annotation.region), |env| {
                                let return_type_expected = constraints.push_expected_type(
                                    ForReason(Reason::FunctionOutput, ret_type_index, region),
                                );

                                constrain_function_return(
                                    types,
                                    constraints,
                                    env,
                                    loc_body_expr,
                                    early_returns,
                                    return_type_expected,
                                    ret_var,
                                    true,
                                )
                            });

                        vars.push(*fn_var);

                        let state_constraints =
                            constraints.and_constraint(argument_pattern_state.constraints);
                        let expected_index = constraints.push_expected_type(expected);
                        let cons = [
                            // Store fx type first so errors are reported at call site
                            constraints.store(fx_type_index, fx_var, std::file!(), std::line!()),
                            constraints.let_constraint(
                                [],
                                argument_pattern_state.vars.into_iter().map(Loc::at_zero),
                                argument_pattern_state.headers,
                                state_constraints,
                                returns_constraint,
                                generalizable,
                            ),
                            // Check argument suffixes against usage
                            constraints.and_constraint(
                                argument_pattern_state.delayed_fx_suffix_constraints,
                            ),
                            constraints.equal_types(
                                fn_type_index,
                                expected_index,
                                Category::Lambda,
                                region,
                            ),
                            // "fn_var is equal to the closure's type" - fn_var is used in code gen
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            constraints.store(signature_index, *fn_var, std::file!(), std::line!()),
                            constraints.store(
                                signature_index,
                                expr_var,
                                std::file!(),
                                std::line!(),
                            ),
                            constraints.store(ret_type_index, ret_var, std::file!(), std::line!()),
                            closure_constraint,
                            constraints.flex_to_pure(fx_var),
                        ];

                        let and_constraint = constraints.and_constraint(cons);
                        let def_con = constraints.exists(vars, and_constraint);

                        if is_hybrid {
                            // TODO this is not quite right, types that are purely rigid should not
                            // be stored as hybrid!
                            hybrid_and_flex_info.vars.extend(&new_rigid_variables);
                            hybrid_and_flex_info.constraints.push(def_con);
                            hybrid_and_flex_info
                                .def_types
                                .extend(def_pattern_state.headers);
                        } else {
                            rigid_info.vars.extend(&new_rigid_variables);

                            let rigids = new_rigid_variables;
                            let flex_pattern_vars =
                                def_pattern_state.vars.into_iter().map(Loc::at_zero);
                            let flex = flex_pattern_vars.chain(new_infer_variables);

                            rigid_info.constraints.push(constraints.let_constraint(
                                rigids,
                                flex,
                                [], // no headers introduced (at this level)
                                def_con,
                                Constraint::True,
                                generalizable,
                            ));
                            rigid_info.def_types.extend(def_pattern_state.headers);
                        }
                    }
                    _ => {
                        let expected = constraints.push_expected_type(annotation_expected);

                        let ret_constraint = constrain_expr(
                            types,
                            constraints,
                            env,
                            def.loc_expr.region,
                            &def.loc_expr.value,
                            expected,
                        );
                        let ret_constraint =
                            attach_resolution_constraints(constraints, env, ret_constraint);

                        let cons = [
                            ret_constraint,
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            constraints.store(
                                signature_index,
                                expr_var,
                                std::file!(),
                                std::line!(),
                            ),
                        ];
                        let def_con = constraints.and_constraint(cons);

                        if is_hybrid {
                            hybrid_and_flex_info.vars.extend(&new_rigid_variables);
                            hybrid_and_flex_info.constraints.push(def_con);
                            hybrid_and_flex_info
                                .def_types
                                .extend(def_pattern_state.headers);
                        } else {
                            rigid_info.vars.extend(&new_rigid_variables);
                            let flex_vars = def_pattern_state.vars.into_iter().map(Loc::at_zero);

                            rigid_info.constraints.push(constraints.let_constraint(
                                new_rigid_variables,
                                flex_vars,
                                [], // no headers introduced (at this level)
                                def_con,
                                Constraint::True,
                                generalizable,
                            ));
                            rigid_info.def_types.extend(def_pattern_state.headers);
                        }
                    }
                }
            }
        }
    }

    // NB(rec-def-strategy) Strategy for recursive defs:
    //
    // 1. Let-generalize all rigid annotations. These are the source of truth we'll solve
    //    everything else with. If there are circular type errors here, they will be caught
    //    during the let-generalization.
    //
    // 2. Introduce all symbols of the flex + hybrid defs, but don't generalize them yet.
    //    Now, solve those defs' bodies. This way, when checking something like
    //      f = \x -> f [x]
    //    we introduce `f: b -> c`, then constrain the call `f [x]`,
    //    forcing `b -> c ~ List b -> c` and correctly picking up a recursion error.
    //    Had we generalized `b -> c`, the call `f [x]` would have been generalized, and this
    //    error would not be found.
    //
    //    - This works just as well for mutually recursive defs.
    //    - For hybrid defs, we also ensure solved types agree with what the
    //      elaborated parts of their type annotations demand.
    //
    // 3. Now properly let-generalize the flex + hybrid defs, since we now know their types and
    //    that they don't have circular type errors.
    //
    // 4. Solve the bodies of the typed body defs, and check that they agree the types of the type
    //    annotation.
    //
    // 5. Solve the rest of the program that happens after this recursive def block.

    // 2. Solve untyped defs without generalization of their symbols.
    let untyped_body_constraints = constraints.and_constraint(hybrid_and_flex_info.constraints);
    let untyped_def_symbols_constr = constraints.let_constraint(
        [],
        [],
        hybrid_and_flex_info.def_types.clone(),
        Constraint::True,
        untyped_body_constraints,
        generalizable,
    );

    // an extra constraint that propagates information to the solver to check for invalid recursion
    // and generate a good error message there.
    let (loc_symbols, expr_regions): (Vec<_>, Vec<_>) = defs
        .iter()
        .flat_map(|def| {
            symbols_introduced_from_pattern(&def.loc_pattern)
                .map(move |loc_symbol| ((loc_symbol.value, loc_symbol.region), def.loc_expr.region))
        })
        .unzip();

    let cycle_constraint = constraints.check_cycle(loc_symbols, expr_regions, cycle_mark);

    let typed_body_constraints = constraints.and_constraint(rigid_info.constraints);
    let typed_body_and_final_constr =
        constraints.and_constraint([typed_body_constraints, cycle_constraint, body_con]);

    // 3. Properly generalize untyped defs after solving them.
    let inner = constraints.let_constraint(
        [],
        hybrid_and_flex_info.vars,
        hybrid_and_flex_info.def_types,
        untyped_def_symbols_constr,
        // 4 + 5. Solve the typed body defs, and the rest of the program.
        typed_body_and_final_constr,
        generalizable,
    );

    // 1. Let-generalize annotations we know.
    constraints.let_constraint(
        rigid_info.vars,
        [],
        rigid_info.def_types,
        Constraint::True,
        inner,
        generalizable,
    )
}

#[inline(always)]
fn constrain_field_update(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Loc<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = constraints.push_variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = constraints.push_expected_type(ForReason(reason, field_type, region));
    let con = constrain_expr(
        types,
        constraints,
        env,
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    (var, Variable(var), con)
}
