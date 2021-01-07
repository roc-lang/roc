use crate::builtins::{num_floatingpoint, num_integer, num_num};
use crate::expr::{exists, Info};
use roc_can::annotation::IntroducedVariables;
use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::LetConstraint;
use roc_can::def::{Declaration, Def};
use roc_can::expected::{Expected, PExpected};
use roc_can::expr::{Expr, Field, WhenBranch};
use roc_can::pattern::{DestructType, Pattern, RecordDestruct};
use roc_collections::all::{ImMap, ImSet, Index, SendMap};
use roc_module::ident::{Ident, Lowercase};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::boolean_algebra::Bool;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::AnnotationSource::{self, *};
use roc_types::types::Type::{self, *};
use roc_types::types::{Category, PReason, Reason, RecordField};
use roc_uniq::builtins::{attr_type, empty_list_type, list_type, str_type};
use roc_uniq::sharing::{self, FieldAccess, Mark, Usage, VarUsage};

pub struct Env {
    /// Whenever we encounter a user-defined type variable (a "rigid" var for short),
    /// for example `u` and `a` in the annotation `identity : Attr u a -> Attr u a`, we add it to this
    /// map so that expressions within that annotation can share these vars.
    pub rigids: ImMap<Lowercase, (Variable, Variable)>,
    pub home: ModuleId,
}

pub fn constrain_declaration(
    home: ModuleId,
    var_store: &mut VarStore,
    region: Region,
    loc_expr: &Located<Expr>,
    _declared_idents: &ImMap<Ident, (Symbol, Region)>,
    expected: Expected<Type>,
) -> Constraint {
    // TODO this means usage is local to individual declarations.
    // Should be per-module in the future!
    let mut var_usage = VarUsage::default();

    sharing::annotate_usage(&loc_expr.value, &mut var_usage);

    let mut applied_usage_constraint = ImSet::default();
    constrain_expr(
        &Env {
            rigids: ImMap::default(),
            home,
        },
        var_store,
        &var_usage,
        &mut applied_usage_constraint,
        region,
        &loc_expr.value,
        expected,
    )
}

/// Constrain top-level module declarations
#[inline(always)]
pub fn constrain_decls(
    home: ModuleId,
    decls: &[Declaration],
    var_store: &mut VarStore,
) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;

    // perform usage analysis on the whole file
    let mut var_usage = VarUsage::default();

    for decl in decls.iter().rev() {
        // NOTE: rigids are empty because they are not shared between top-level definitions
        match decl {
            Declaration::Declare(def) | Declaration::Builtin(def) => {
                sharing::annotate_usage(&def.loc_expr.value, &mut var_usage);
            }
            Declaration::DeclareRec(defs) => {
                for def in defs {
                    sharing::annotate_usage(&def.loc_expr.value, &mut var_usage);
                }
            }
            Declaration::InvalidCycle(_, _) => {
                // any usage of a value defined in an invalid cycle will blow up
                // so for the analysis usage by such values doesn't count
                continue;
            }
        }
    }

    let mut env = Env {
        home,
        rigids: ImMap::default(),
    };

    for decl in decls.iter().rev() {
        // clear the set of rigids from the previous iteration.
        // rigids are not shared between top-level definitions.
        env.rigids.clear();

        match decl {
            Declaration::Declare(def) | Declaration::Builtin(def) => {
                constraint = exists(
                    Vec::new(),
                    constrain_def(
                        &env,
                        var_store,
                        &var_usage,
                        &mut ImSet::default(),
                        def,
                        constraint,
                    ),
                );
            }
            Declaration::DeclareRec(defs) => {
                constraint = exists(
                    Vec::new(),
                    constrain_recursive_defs(
                        &env,
                        var_store,
                        &var_usage,
                        &mut ImSet::default(),
                        defs,
                        constraint,
                    ),
                );
            }
            Declaration::InvalidCycle(_, _) => {
                // invalid cycles give a canonicalization error. we skip them here.
                continue;
            }
        }
    }

    constraint
}

pub struct PatternState {
    pub headers: SendMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

fn constrain_pattern(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    state: &mut PatternState,
    pattern: &Located<Pattern>,
    expected: PExpected<Type>,
) {
    use roc_can::pattern::Pattern::*;
    use roc_types::types::PatternCategory;

    let region = pattern.region;

    match &pattern.value {
        Identifier(symbol) => {
            state.headers.insert(
                *symbol,
                Located {
                    region: pattern.region,
                    value: expected.get_type(),
                },
            );
        }

        NumLiteral(inner_var, _) => {
            let (inner_uvar, num_var, num_type) = unique_unbound_num(*inner_var, var_store);
            state.constraints.push(exists(
                vec![num_var, inner_uvar, *inner_var],
                Constraint::Pattern(pattern.region, PatternCategory::Num, num_type, expected),
            ));
        }

        IntLiteral(inner_var, _) => {
            let (a, b, c, num_type) = unique_int(var_store);

            state.constraints.push(exists(
                vec![*inner_var, a, b, c],
                Constraint::Pattern(pattern.region, PatternCategory::Int, num_type, expected),
            ));
        }
        FloatLiteral(inner_var, _) => {
            let (a, b, c, num_type) = unique_float(var_store);

            state.constraints.push(exists(
                vec![*inner_var, a, b, c],
                Constraint::Pattern(pattern.region, PatternCategory::Float, num_type, expected),
            ));
        }

        StrLiteral(_) => {
            let uniq_var = var_store.fresh();
            state.constraints.push(exists(
                vec![uniq_var],
                Constraint::Pattern(
                    pattern.region,
                    PatternCategory::Str,
                    str_type(Bool::variable(uniq_var)),
                    expected,
                ),
            ));
        }

        RecordDestructure {
            whole_var,
            ext_var,
            destructs,
        } => {
            // TODO if a subpattern doesn't bind any identifiers, it doesn't count for uniqueness
            let mut pattern_uniq_vars = Vec::with_capacity(destructs.len());

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut field_types: SendMap<Lowercase, RecordField<Type>> = SendMap::default();
            for Located {
                value:
                    RecordDestruct {
                        var,
                        label,
                        symbol,
                        typ,
                    },
                ..
            } in destructs
            {
                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pat_type = attr_type(Bool::variable(pat_uniq_var), Type::Variable(*var));
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(&symbol) {
                    state
                        .headers
                        .insert(*symbol, Located::at(pattern.region, pat_type.clone()));
                }

                let field_type = match typ {
                    DestructType::Guard(guard_var, loc_guard) => {
                        state.constraints.push(Constraint::Pattern(
                            pattern.region,
                            PatternCategory::PatternGuard,
                            Type::Variable(*guard_var),
                            PExpected::NoExpectation(pat_type.clone()),
                        ));
                        state.vars.push(*guard_var);
                        constrain_pattern(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            state,
                            loc_guard,
                            expected,
                        );

                        RecordField::Demanded(pat_type)
                    }
                    DestructType::Optional(expr_var, loc_expr) => {
                        let expr_expected = Expected::ForReason(
                            Reason::RecordDefaultField(label.clone()),
                            pat_type.clone(),
                            loc_expr.region,
                        );

                        state.constraints.push(Constraint::Pattern(
                            region,
                            PatternCategory::PatternDefault,
                            Type::Variable(*expr_var),
                            PExpected::ForReason(
                                PReason::OptionalField,
                                pat_type.clone(),
                                loc_expr.region,
                            ),
                        ));

                        state.vars.push(*expr_var);

                        let expr_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_expr.region,
                            &loc_expr.value,
                            expr_expected,
                        );
                        state.constraints.push(expr_con);

                        RecordField::Optional(pat_type)
                    }
                    DestructType::Required => {
                        // No extra constraints necessary.
                        RecordField::Demanded(pat_type)
                    }
                };

                field_types.insert(label.clone(), field_type);

                state.vars.push(*var);
            }

            let record_uniq_type = {
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                state.vars.extend(pattern_uniq_vars.clone());
                Bool::container(empty_var, pattern_uniq_vars)
            };

            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(ext_type)),
            );

            let whole_con = Constraint::Eq(
                Type::Variable(*whole_var),
                Expected::NoExpectation(record_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = Constraint::Pattern(
                region,
                PatternCategory::Record,
                Type::Variable(*whole_var),
                expected,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }

        AppliedTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => {
            // TODO if a subpattern doesn't bind any identifiers, it doesn't count for uniqueness
            let mut argument_types = Vec::with_capacity(arguments.len());
            let mut pattern_uniq_vars = Vec::with_capacity(arguments.len());

            for (pattern_var, loc_pattern) in arguments {
                state.vars.push(*pattern_var);

                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pattern_type =
                    attr_type(Bool::variable(pat_uniq_var), Type::Variable(*pattern_var));
                argument_types.push(pattern_type.clone());

                let expected = PExpected::NoExpectation(pattern_type);
                constrain_pattern(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    state,
                    loc_pattern,
                    expected,
                );
            }

            let tag_union_uniq_type = {
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                state.vars.extend(pattern_uniq_vars.clone());
                Bool::container(empty_var, pattern_uniq_vars)
            };
            let union_type = attr_type(
                tag_union_uniq_type,
                Type::TagUnion(
                    vec![(tag_name.clone(), argument_types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
            );

            let whole_con = Constraint::Eq(
                Type::Variable(*whole_var),
                Expected::NoExpectation(union_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let tag_con = Constraint::Pattern(
                region,
                PatternCategory::Ctor(tag_name.clone()),
                Type::Variable(*whole_var),
                expected,
            );

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);

            state.constraints.push(whole_con);
            state.constraints.push(tag_con);
        }

        Underscore | Shadowed(_, _) | MalformedPattern(_, _) | UnsupportedPattern(_) => {
            // no constraints
        }
    }
}

fn unique_unbound_num(inner_var: Variable, var_store: &mut VarStore) -> (Variable, Variable, Type) {
    let num_uvar = var_store.fresh();
    let inner_uvar = var_store.fresh();

    let val_type = Type::Variable(inner_var);
    let val_utype = attr_type(Bool::variable(inner_uvar), val_type);

    let num_utype = num_num(val_utype);
    let num_type = attr_type(Bool::variable(num_uvar), num_utype);

    (inner_uvar, num_uvar, num_type)
}

fn unique_int(var_store: &mut VarStore) -> (Variable, Variable, Variable, Type) {
    let num_uvar1 = var_store.fresh();
    let num_uvar2 = var_store.fresh();
    let num_uvar3 = var_store.fresh();
    let num_uvar4 = var_store.fresh();

    let integer = num_integer(Type::Variable(num_uvar4));
    let attr_int = attr_type(Bool::variable(num_uvar2), integer);
    let num = num_num(attr_int);
    let attr_num = attr_type(Bool::variable(num_uvar3), num);

    (num_uvar1, num_uvar2, num_uvar3, attr_num)
}

fn unique_float(var_store: &mut VarStore) -> (Variable, Variable, Variable, Type) {
    let num_uvar1 = var_store.fresh();
    let num_uvar2 = var_store.fresh();
    let num_uvar3 = var_store.fresh();
    let num_uvar4 = var_store.fresh();

    let fp = num_floatingpoint(Type::Variable(num_uvar4));
    let attr_fp = attr_type(Bool::variable(num_uvar2), fp);
    let num = num_num(attr_fp);
    let attr_num = attr_type(Bool::variable(num_uvar3), num);

    (num_uvar1, num_uvar2, num_uvar3, attr_num)
}

pub fn constrain_expr(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    pub use roc_can::expr::Expr::*;

    match expr {
        Num(inner_var, _) => {
            let var = var_store.fresh();
            let (inner_uvar, num_var, num_type) = unique_unbound_num(*inner_var, var_store);

            exists(
                vec![var, *inner_var, inner_uvar, num_var],
                And(vec![
                    Eq(
                        Type::Variable(var),
                        Expected::ForReason(Reason::NumLiteral, num_type, region),
                        Category::Num,
                        region,
                    ),
                    Eq(Type::Variable(var), expected, Category::Num, region),
                ]),
            )
        }
        Int(var, _, _) => {
            let (a, b, c, num_type) = unique_int(var_store);

            exists(
                vec![*var, a, b, c],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(Reason::IntLiteral, num_type, region),
                        Category::Int,
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, Category::Int, region),
                ]),
            )
        }
        Float(var, _, _) => {
            let (a, b, c, num_type) = unique_float(var_store);

            exists(
                vec![*var, a, b, c],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(Reason::FloatLiteral, num_type, region),
                        Category::Float,
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, Category::Float, region),
                ]),
            )
        }
        Str(_) => {
            let uniq_type = var_store.fresh();
            let inferred = str_type(Bool::variable(uniq_type));

            exists(
                vec![uniq_type],
                Eq(inferred, expected, Category::Str, region),
            )
        }
        EmptyRecord => {
            let uniq_type = var_store.fresh();

            exists(
                vec![uniq_type],
                Eq(
                    attr_type(Bool::variable(uniq_type), EmptyRec),
                    expected,
                    Category::Record,
                    region,
                ),
            )
        }
        Record {
            record_var, fields, ..
        } => {
            // NOTE: canonicalization guarantees at least one field
            // zero fields generates an EmptyRecord
            let mut field_types = SendMap::default();
            let mut field_vars = Vec::with_capacity(fields.len());
            field_vars.push(*record_var);

            // Constraints need capacity for each field + 1 for the record itself + 1 for ext
            let mut constraints = Vec::with_capacity(2 + fields.len());

            for (label, ref field) in fields.iter() {
                let field_var = var_store.fresh();
                let field_type = Variable(field_var);
                let field_expected = Expected::NoExpectation(field_type.clone());
                let loc_expr = &*field.loc_expr;
                let field_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    loc_expr.region,
                    &loc_expr.value,
                    field_expected,
                );

                field_vars.push(field_var);
                field_types.insert(label.clone(), RecordField::Required(field_type));

                constraints.push(field_con);
            }

            let record_uniq_var = var_store.fresh();
            field_vars.push(record_uniq_var);
            let record_type = attr_type(
                Bool::variable(record_uniq_var),
                Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(Type::EmptyRec),
                ),
            );
            let record_con = Eq(record_type, expected.clone(), Category::Record, region);
            let ext_con = Eq(
                Type::Variable(*record_var),
                expected,
                Category::Record,
                region,
            );

            constraints.push(record_con);
            constraints.push(ext_con);

            exists(field_vars, And(constraints))
        }
        Tag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            let mut vars = Vec::with_capacity(arguments.len());
            let mut types = Vec::with_capacity(arguments.len());
            let mut arg_cons = Vec::with_capacity(arguments.len());

            for (var, loc_expr) in arguments {
                let arg_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    loc_expr.region,
                    &loc_expr.value,
                    Expected::NoExpectation(Type::Variable(*var)),
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                types.push(Type::Variable(*var));
            }

            let uniq_var = var_store.fresh();

            let union_type = attr_type(
                Bool::variable(uniq_var),
                Type::TagUnion(
                    vec![(name.clone(), types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
            );

            let union_con = Eq(
                union_type,
                expected.clone(),
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
            );
            let ast_con = Eq(
                Type::Variable(*variant_var),
                expected,
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
            );

            vars.push(uniq_var);
            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        List {
            list_var,
            elem_var,
            loc_elems,
        } => {
            let uniq_var = var_store.fresh();
            if loc_elems.is_empty() {
                let inferred = empty_list_type(Bool::variable(uniq_var), *elem_var);
                exists(
                    vec![*elem_var, uniq_var],
                    Eq(inferred, expected, Category::List, region),
                )
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let entry_type = Type::Variable(*elem_var);
                let mut constraints = Vec::with_capacity(1 + loc_elems.len());

                for (index, loc_elem) in loc_elems.iter().enumerate() {
                    let elem_expected = Expected::ForReason(
                        Reason::ElemInList {
                            index: Index::zero_based(index),
                        },
                        entry_type.clone(),
                        region,
                    );
                    let constraint = constrain_expr(
                        env,
                        var_store,
                        var_usage,
                        applied_usage_constraint,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(constraint);
                }

                let inferred = list_type(Bool::variable(uniq_var), entry_type);
                constraints.push(Eq(inferred, expected.clone(), Category::List, region));

                let stored = Type::Variable(*list_var);
                constraints.push(Eq(
                    stored,
                    expected,
                    Category::Storage(std::file!(), std::line!()),
                    region,
                ));

                exists(vec![*elem_var, *list_var, uniq_var], And(constraints))
            }
        }
        Var(symbol) => {
            let usage = var_usage.get_usage(*symbol);

            constrain_var(
                var_store,
                applied_usage_constraint,
                *symbol,
                usage,
                region,
                expected,
            )
        }
        Closure {
            function_type: fn_var,
            return_type: ret_var,
            closure_type: closure_var,
            closure_ext_var,
            recursive: recursion,
            arguments,
            loc_body: boxed,
            captured_symbols,
            name,
            ..
        } => {
            use roc_can::expr::Recursive;

            let loc_body_expr = &**boxed;
            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(arguments.len()),
                constraints: Vec::with_capacity(1),
            };
            let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
            let mut pattern_types = Vec::with_capacity(state.vars.capacity());
            let ret_var = *ret_var;
            let ret_type = Type::Variable(ret_var);
            let closure_var = *closure_var;
            let closure_ext_var = *closure_ext_var;

            vars.push(ret_var);
            vars.push(closure_var);
            vars.push(closure_ext_var);
            vars.push(*fn_var);

            for (pattern_var, loc_pattern) in arguments {
                let pattern_type = Type::Variable(*pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

                pattern_types.push(pattern_type);

                constrain_pattern(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    &mut state,
                    loc_pattern,
                    pattern_expected,
                );

                vars.push(*pattern_var);
            }

            let fn_uniq_type;
            if let Recursive::NotRecursive = recursion {
                let fn_uniq_var = var_store.fresh();
                vars.push(fn_uniq_var);
                fn_uniq_type = Bool::variable(fn_uniq_var);
            } else {
                // recursive definitions MUST be Shared
                fn_uniq_type = Bool::shared()
            }

            let body_type = Expected::NoExpectation(ret_type.clone());
            let ret_constraint = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            let defs_constraint = And(state.constraints);

            let closure_constraint = constrain_closure_size(
                *name,
                region,
                captured_symbols,
                closure_var,
                closure_ext_var,
                &mut vars,
            );

            let fn_type = attr_type(
                fn_uniq_type,
                Type::Function(
                    pattern_types,
                    Box::new(Type::Variable(closure_var)),
                    Box::new(ret_type),
                ),
            );

            exists(
                vars,
                And(vec![
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: state.vars,
                        def_types: state.headers,
                        defs_constraint,
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected type"
                    Eq(fn_type.clone(), expected, Category::Lambda, region),
                    // "fn_var is equal to the closure's type" - fn_var is used in code gen
                    Eq(
                        Type::Variable(*fn_var),
                        Expected::NoExpectation(fn_type),
                        Category::Lambda,
                        region,
                    ),
                    closure_constraint,
                ]),
            )
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, fn_expr, closure_var, ret_var) = &**boxed;
            let fn_type = Variable(*fn_var);
            let ret_type = Variable(*ret_var);
            let closure_type = Variable(*closure_var);
            let fn_expected = Expected::NoExpectation(fn_type.clone());
            let fn_region = fn_expr.region;

            let opt_symbol = if let Var(symbol) = fn_expr.value {
                Some(symbol)
            } else {
                None
            };

            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);
            vars.push(*closure_var);

            // Canonicalize the function expression and its arguments
            let fn_con = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                fn_region,
                &fn_expr.value,
                fn_expected,
            );

            let fn_reason = Reason::FnCall {
                name: opt_symbol,
                arity: loc_args.len() as u8,
            };

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);

                let reason = Reason::FnArg {
                    name: opt_symbol,
                    arg_index: Index::zero_based(index),
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_uniq_type = var_store.fresh();
            vars.push(expected_uniq_type);
            let expected_fn_type = Expected::ForReason(
                fn_reason,
                attr_type(
                    Bool::variable(expected_uniq_type),
                    Function(
                        arg_types,
                        Box::new(closure_type),
                        Box::new(ret_type.clone()),
                    ),
                ),
                region,
            );

            exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(
                        fn_type,
                        expected_fn_type,
                        Category::CallResult(opt_symbol),
                        fn_region,
                    ),
                    And(arg_cons),
                    Eq(ret_type, expected, Category::CallResult(opt_symbol), region),
                ]),
            )
        }
        RunLowLevel { op, args, ret_var } => {
            // This is a modified version of what we do for function calls.

            let ret_type = Variable(*ret_var);
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            // Canonicalize the function expression and its arguments

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            for (index, (arg_var, arg_expr)) in args.iter().enumerate() {
                let arg_type = Variable(*arg_var);

                let reason = Reason::LowLevelOpArg {
                    op: *op,
                    arg_index: Index::zero_based(index),
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    Region::zero(),
                    arg_expr,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_uniq_type = var_store.fresh();
            vars.push(expected_uniq_type);

            exists(
                vars,
                And(vec![
                    And(arg_cons),
                    Eq(ret_type, expected, Category::LowLevelOpResult(*op), region),
                ]),
            )
        }
        ForeignCall {
            foreign_symbol,
            args,
            ret_var,
        } => {
            // This is a modified version of what we do for function calls.

            let ret_type = Variable(*ret_var);
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            // Canonicalize the function expression and its arguments

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            for (index, (arg_var, arg_expr)) in args.iter().enumerate() {
                let arg_type = Variable(*arg_var);

                let reason = Reason::ForeignCallArg {
                    foreign_symbol: foreign_symbol.clone(),
                    arg_index: Index::zero_based(index),
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    Region::zero(),
                    arg_expr,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_uniq_type = var_store.fresh();
            vars.push(expected_uniq_type);

            let category = Category::ForeignCall;

            exists(
                vars,
                And(vec![
                    And(arg_cons),
                    Eq(ret_type, expected, category, region),
                ]),
            )
        }
        LetRec(defs, loc_ret, var) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let body_con = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );

            exists(
                vec![*var],
                And(vec![
                    constrain_recursive_defs(
                        env,
                        var_store,
                        var_usage,
                        applied_usage_constraint,
                        defs,
                        body_con,
                    ),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(
                        Type::Variable(*var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        loc_ret.region,
                    ),
                ]),
            )
        }
        LetNonRec(def, loc_ret, var) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let body_con = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );

            exists(
                vec![*var],
                And(vec![
                    constrain_def(
                        env,
                        var_store,
                        var_usage,
                        applied_usage_constraint,
                        def,
                        body_con,
                    ),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(
                        Type::Variable(*var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        loc_ret.region,
                    ),
                ]),
            )
        }
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            // TODO use Bool alias here, so we don't allocate this type every time
            let bool_type = Type::Variable(Variable::BOOL);
            let mut branch_cons = Vec::with_capacity(2 * branches.len() + 2);
            let mut cond_uniq_vars = Vec::with_capacity(branches.len() + 2);

            // TODO why does this cond var exist? is it for error messages?
            let cond_uniq_var = var_store.fresh();
            cond_uniq_vars.push(cond_uniq_var);
            let cond_var_is_bool_con = Eq(
                Type::Variable(*cond_var),
                Expected::ForReason(
                    Reason::IfCondition,
                    attr_type(Bool::variable(cond_uniq_var), bool_type.clone()),
                    region,
                ),
                Category::If,
                Region::zero(),
            );

            branch_cons.push(cond_var_is_bool_con);

            match expected {
                Expected::FromAnnotation(name, arity, _, tipe) => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_uniq_var = var_store.fresh();
                        let expect_bool = Expected::ForReason(
                            Reason::IfCondition,
                            attr_type(Bool::variable(cond_uniq_var), bool_type.clone()),
                            region,
                        );
                        cond_uniq_vars.push(cond_uniq_var);

                        let cond_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool,
                        );

                        let then_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_body.region,
                            &loc_body.value,
                            Expected::FromAnnotation(
                                name.clone(),
                                arity,
                                AnnotationSource::TypedIfBranch {
                                    index: Index::zero_based(index),
                                    num_branches: branches.len(),
                                },
                                tipe.clone(),
                            ),
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }
                    let else_con = constrain_expr(
                        env,
                        var_store,
                        var_usage,
                        applied_usage_constraint,
                        final_else.region,
                        &final_else.value,
                        Expected::FromAnnotation(
                            name,
                            arity,
                            AnnotationSource::TypedIfBranch {
                                index: Index::zero_based(branches.len()),
                                num_branches: branches.len(),
                            },
                            tipe.clone(),
                        ),
                    );

                    let ast_con = Eq(
                        Type::Variable(*branch_var),
                        Expected::NoExpectation(tipe),
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    );

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    cond_uniq_vars.push(*cond_var);
                    cond_uniq_vars.push(*branch_var);

                    exists(cond_uniq_vars, And(branch_cons))
                }
                _ => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_uniq_var = var_store.fresh();
                        let expect_bool = Expected::ForReason(
                            Reason::IfCondition,
                            attr_type(Bool::variable(cond_uniq_var), bool_type.clone()),
                            region,
                        );
                        cond_uniq_vars.push(cond_uniq_var);

                        let cond_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool,
                        );

                        let then_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_body.region,
                            &loc_body.value,
                            Expected::ForReason(
                                Reason::IfBranch {
                                    index: Index::zero_based(index),
                                    total_branches: branches.len(),
                                },
                                Type::Variable(*branch_var),
                                region,
                            ),
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }
                    let else_con = constrain_expr(
                        env,
                        var_store,
                        var_usage,
                        applied_usage_constraint,
                        final_else.region,
                        &final_else.value,
                        Expected::ForReason(
                            Reason::IfBranch {
                                index: Index::zero_based(branches.len()),
                                total_branches: branches.len(),
                            },
                            Type::Variable(*branch_var),
                            region,
                        ),
                    );

                    branch_cons.push(Eq(
                        Type::Variable(*branch_var),
                        expected,
                        Category::If,
                        region,
                    ));
                    branch_cons.push(else_con);

                    cond_uniq_vars.push(*cond_var);
                    cond_uniq_vars.push(*branch_var);

                    exists(cond_uniq_vars, And(branch_cons))
                }
            }
        }
        When {
            cond_var,
            expr_var,
            loc_cond,
            branches,
            ..
        } => {
            let cond_var = *cond_var;
            let cond_type = Variable(cond_var);
            let expr_con = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                region,
                &loc_cond.value,
                Expected::NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);
            constraints.push(expr_con);

            match &expected {
                Expected::FromAnnotation(name, arity, _, typ) => {
                    constraints.push(Eq(
                        Type::Variable(*expr_var),
                        expected.clone(),
                        Category::When,
                        region,
                    ));

                    for (index, when_branch) in branches.iter().enumerate() {
                        let pattern_region =
                            Region::across_all(when_branch.patterns.iter().map(|v| &v.region));

                        let branch_con = constrain_when_branch(
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            env,
                            region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.clone(),
                                pattern_region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                *arity,
                                TypedWhenBranch {
                                    index: Index::zero_based(index),
                                },
                                typ.clone(),
                            ),
                        );

                        constraints.push(
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            branch_con,
                        );
                    }
                }

                _ => {
                    let branch_type = Variable(*expr_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, when_branch) in branches.iter().enumerate() {
                        let pattern_region =
                            Region::across_all(when_branch.patterns.iter().map(|v| &v.region));
                        let branch_con = constrain_when_branch(
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            env,
                            region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.clone(),
                                pattern_region,
                            ),
                            Expected::ForReason(
                                Reason::WhenBranch {
                                    index: Index::zero_based(index),
                                },
                                branch_type.clone(),
                                region,
                            ),
                        );

                        branch_cons.push(branch_con);
                    }

                    constraints.push(And(vec![
                        // Each branch's pattern must have the same type
                        // as the condition expression did.
                        And(branch_cons),
                        // The return type of each branch must equal
                        // the return type of the entire case-expression.
                        Eq(branch_type, expected, Category::When, region),
                    ]))
                }
            }

            exists(vec![cond_var, *expr_var], And(constraints))
        }

        Update {
            record_var,
            ext_var,
            symbol,
            updates,
        } => {
            let mut fields: SendMap<Lowercase, RecordField<Type>> = SendMap::default();
            let mut vars = Vec::with_capacity(updates.len() + 2);
            let mut cons = Vec::with_capacity(updates.len() + 3);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) = constrain_field_update(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    var,
                    region,
                    field_name.clone(),
                    &loc_expr,
                );
                fields.insert(field_name, RecordField::Required(tipe));
                vars.push(var);
                cons.push(con);
            }

            let uniq_var = var_store.fresh();
            vars.push(uniq_var);

            let fields_type = attr_type(
                Bool::variable(uniq_var),
                Type::Record(fields, Box::new(Type::Variable(*ext_var))),
            );
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(
                record_type.clone(),
                Expected::NoExpectation(fields_type),
                Category::Record,
                region,
            );
            let record_con = Eq(record_type.clone(), expected, Category::Record, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = Lookup(
                *symbol,
                Expected::ForReason(
                    Reason::RecordUpdateKeys(
                        *symbol,
                        updates
                            .iter()
                            .map(|(key, field)| (key.clone(), field.region))
                            .collect(),
                    ),
                    record_type,
                    region,
                ),
                region,
            );

            cons.push(con);
            cons.push(fields_con);
            cons.push(record_con);

            exists(vars, And(cons))
        }

        Access {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let mut field_types = SendMap::default();

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::variable(field_uniq_var);
            let field_type = attr_type(field_uniq_type, Type::Variable(*field_var));

            field_types.insert(field.clone(), RecordField::Demanded(field_type.clone()));

            let record_uniq_var = var_store.fresh();
            let record_uniq_type = Bool::container(record_uniq_var, vec![field_uniq_var]);
            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(Type::Variable(*ext_var))),
            );

            let category = Category::Access(field.clone());

            let record_expected = Expected::NoExpectation(record_type);
            let record_con = Eq(
                Type::Variable(*record_var),
                record_expected.clone(),
                category.clone(),
                region,
            );

            let inner_constraint = constrain_expr(
                env,
                var_store,
                var_usage,
                applied_usage_constraint,
                loc_expr.region,
                &loc_expr.value,
                record_expected,
            );

            exists(
                vec![
                    *record_var,
                    *field_var,
                    *ext_var,
                    field_uniq_var,
                    record_uniq_var,
                ],
                And(vec![
                    Eq(field_type, expected, category, region),
                    inner_constraint,
                    record_con,
                ]),
            )
        }

        Accessor {
            function_var,
            field,
            record_var,
            closure_var,
            field_var,
            ext_var,
        } => {
            let mut field_types = SendMap::default();

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::variable(field_uniq_var);
            let field_type = attr_type(field_uniq_type, Type::Variable(*field_var));

            field_types.insert(field.clone(), RecordField::Demanded(field_type.clone()));

            let record_uniq_var = var_store.fresh();
            let record_uniq_type = Bool::container(record_uniq_var, vec![field_uniq_var]);
            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(Type::Variable(*ext_var))),
            );

            let category = Category::Accessor(field.clone());

            let record_expected = Expected::NoExpectation(record_type.clone());
            let record_con = Eq(
                Type::Variable(*record_var),
                record_expected,
                category.clone(),
                region,
            );

            let fn_uniq_var = var_store.fresh();
            let closure_type = Type::Variable(*closure_var);
            let fn_type = attr_type(
                Bool::variable(fn_uniq_var),
                Type::Function(
                    vec![record_type],
                    Box::new(closure_type),
                    Box::new(field_type),
                ),
            );

            exists(
                vec![
                    *record_var,
                    *function_var,
                    *closure_var,
                    *field_var,
                    *ext_var,
                    fn_uniq_var,
                    field_uniq_var,
                    record_uniq_var,
                ],
                And(vec![
                    Eq(fn_type.clone(), expected, category.clone(), region),
                    Eq(
                        fn_type,
                        Expected::NoExpectation(Variable(*function_var)),
                        category,
                        region,
                    ),
                    record_con,
                ]),
            )
        }
        RuntimeError(_) => True,
    }
}

fn constrain_var(
    var_store: &mut VarStore,
    applied_usage_constraint: &mut ImSet<Symbol>,
    symbol_for_lookup: Symbol,
    usage: Option<&Usage>,
    region: Region,
    expected: Expected<Type>,
) -> Constraint {
    use sharing::Mark::*;
    use sharing::Usage::*;

    match usage {
        None | Some(Simple(Shared)) => {
            // the variable is used/consumed more than once, so it must be Shared
            let val_var = var_store.fresh();
            let uniq_var = var_store.fresh();

            let val_type = Variable(val_var);
            let uniq_type = Bool::variable(uniq_var);

            let attr_type = attr_type(uniq_type.clone(), val_type);

            exists(
                vec![val_var, uniq_var],
                And(vec![
                    Lookup(symbol_for_lookup, expected.clone(), region),
                    Eq(attr_type, expected, Category::Uniqueness, region),
                    Eq(
                        Type::Boolean(uniq_type),
                        Expected::NoExpectation(Type::Boolean(Bool::shared())),
                        Category::Uniqueness,
                        region,
                    ),
                ]),
            )
        }
        Some(Simple(Unique)) | Some(Simple(Seen)) => {
            // no additional constraints, keep uniqueness unbound
            Lookup(symbol_for_lookup, expected, region)
        }
        Some(Usage::RecordAccess(_, _))
        | Some(Usage::RecordUpdate(_, _))
        | Some(Usage::ApplyAccess(_, _))
        | Some(Usage::ApplyUpdate(_, _)) => {
            applied_usage_constraint.insert(symbol_for_lookup);

            let mut variables = Vec::new();
            let (record_bool, inner_type) =
                constrain_by_usage(&usage.expect("wut"), var_store, &mut variables);

            let record_type = attr_type(record_bool, inner_type);

            // NOTE breaking the expectation up like this REALLY matters!
            let new_expected = Expected::NoExpectation(record_type.clone());
            exists(
                variables,
                And(vec![
                    Lookup(symbol_for_lookup, new_expected, region),
                    Eq(record_type, expected, Category::Uniqueness, region),
                ]),
            )
        }
    }
}

fn constrain_by_usage(
    usage: &Usage,
    var_store: &mut VarStore,
    introduced: &mut Vec<Variable>,
) -> (Bool, Type) {
    use Usage::*;

    match usage {
        Simple(Mark::Shared) => {
            let var = var_store.fresh();

            introduced.push(var);

            (Bool::Shared, Type::Variable(var))
        }
        Simple(Mark::Seen) | Simple(Mark::Unique) => {
            let var = var_store.fresh();
            let uvar = var_store.fresh();

            introduced.push(var);
            introduced.push(uvar);

            (Bool::container(uvar, vec![]), Type::Variable(var))
        }
        Usage::RecordAccess(mark, fields) => {
            let (record_bool, ext_type) = constrain_by_usage(&Simple(*mark), var_store, introduced);

            constrain_by_usage_record(fields, record_bool, ext_type, introduced, var_store)
        }
        Usage::RecordUpdate(_, fields) => {
            let record_uvar = var_store.fresh();
            introduced.push(record_uvar);

            let record_bool = Bool::variable(record_uvar);

            let ext_var = var_store.fresh();
            let ext_type = Type::Variable(ext_var);
            introduced.push(ext_var);

            constrain_by_usage_record(fields, record_bool, ext_type, introduced, var_store)
        }
        Usage::ApplyAccess(mark, fields) => {
            let (list_bool, _ext_type) = constrain_by_usage(&Simple(*mark), var_store, introduced);

            // TODO reconsier this for multi-value applies
            let field_usage = fields.get(0).expect("no LIST_ELEM key");

            let (elem_bool, elem_type) = constrain_by_usage(field_usage, var_store, introduced);

            match list_bool {
                Bool::Shared => (
                    Bool::Shared,
                    Type::Apply(Symbol::LIST_LIST, vec![attr_type(Bool::Shared, elem_type)]),
                ),
                Bool::Container(list_uvar, list_mvars) => {
                    debug_assert!(list_mvars.is_empty());

                    match elem_bool {
                        Bool::Shared => (
                            Bool::variable(list_uvar),
                            Type::Apply(
                                Symbol::LIST_LIST,
                                vec![attr_type(Bool::Shared, elem_type)],
                            ),
                        ),
                        Bool::Container(cvar, mvars) => {
                            debug_assert!(mvars.is_empty());
                            (
                                Bool::container(list_uvar, vec![cvar]),
                                Type::Apply(
                                    Symbol::LIST_LIST,
                                    vec![attr_type(Bool::container(cvar, mvars), elem_type)],
                                ),
                            )
                        }
                    }
                }
            }
        }

        Usage::ApplyUpdate(_, fields) => {
            let list_uvar = var_store.fresh();
            introduced.push(list_uvar);

            // TODO reconsier this for multi-value applies
            let field_usage = fields.get(0).expect("no LIST_ELEM key");

            let (elem_bool, elem_type) = constrain_by_usage(field_usage, var_store, introduced);

            match elem_bool {
                Bool::Shared => (
                    Bool::variable(list_uvar),
                    Type::Apply(Symbol::LIST_LIST, vec![attr_type(Bool::Shared, elem_type)]),
                ),
                Bool::Container(cvar, mvars) => {
                    debug_assert!(mvars.is_empty());
                    (
                        Bool::container(list_uvar, vec![cvar]),
                        Type::Apply(
                            Symbol::LIST_LIST,
                            vec![attr_type(Bool::container(cvar, mvars), elem_type)],
                        ),
                    )
                }
            }
        }
    }
}

fn constrain_by_usage_record(
    fields: &FieldAccess,
    record_bool: Bool,
    ext_type: Type,
    introduced: &mut Vec<Variable>,
    var_store: &mut VarStore,
) -> (Bool, Type) {
    let mut field_types = SendMap::default();

    match record_bool {
        _ if fields.is_empty() => (record_bool, Type::Record(field_types, Box::new(ext_type))),
        Bool::Shared => {
            for (lowercase, nested_usage) in fields.clone().into_iter() {
                let (_, nested_type) = constrain_by_usage(&nested_usage, var_store, introduced);

                let field_type = attr_type(Bool::Shared, nested_type);

                // In expressions, it's only possible to do record access on
                // Required fields.
                field_types.insert(lowercase.clone(), RecordField::Required(field_type));
            }

            (
                Bool::Shared,
                Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(ext_type),
                ),
            )
        }
        Bool::Container(record_uniq_var, mvars) => {
            debug_assert!(mvars.is_empty());

            let mut uniq_vars = Vec::with_capacity(fields.len());

            for (lowercase, nested_usage) in fields.clone().into_iter() {
                let (nested_bool, nested_type) =
                    constrain_by_usage(&nested_usage, var_store, introduced);

                let field_type = match nested_bool {
                    Bool::Container(uvar, atoms) => {
                        for atom in &atoms {
                            uniq_vars.push(*atom);
                        }
                        uniq_vars.push(uvar);
                        attr_type(Bool::Container(uvar, atoms), nested_type)
                    }
                    Bool::Shared => attr_type(Bool::Shared, nested_type),
                };

                // In expressions, it's only possible to do record access on
                // Required fields.
                field_types.insert(lowercase.clone(), RecordField::Required(field_type));
            }

            (
                Bool::container(record_uniq_var, uniq_vars),
                Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(ext_type),
                ),
            )
        }
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
// NOTE enabling the inline pragma can blow the stack in debug mode
// #[inline(always)]
fn constrain_when_branch(
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    env: &Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        region,
        &when_branch.value.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    for loc_pattern in &when_branch.patterns {
        // mutates the state, so return value is not used
        constrain_pattern(
            env,
            var_store,
            var_usage,
            applied_usage_constraint,
            &mut state,
            &loc_pattern,
            pattern_expected.clone(),
        );
    }

    if let Some(loc_guard) = &when_branch.guard {
        let guard_uniq_var = var_store.fresh();

        let bool_type = attr_type(
            Bool::variable(guard_uniq_var),
            Type::Variable(Variable::BOOL),
        );

        let guard_constraint = constrain_expr(
            env,
            var_store,
            var_usage,
            applied_usage_constraint,
            loc_guard.region,
            &loc_guard.value,
            Expected::ForReason(Reason::WhenGuard, bool_type, loc_guard.region),
        );

        Constraint::Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint: Constraint::Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: vec![guard_uniq_var],
                def_types: SendMap::default(),
                defs_constraint: guard_constraint,
                ret_constraint,
            })),
        }))
    } else {
        Constraint::Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint,
        }))
    }
}

fn constrain_def_pattern(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    loc_pattern: &Located<Pattern>,
    expr_type: Type,
) -> PatternState {
    // Exclude the current ident from shadowable_idents; you can't shadow yourself!
    // (However, still include it in scope, because you *can* recursively refer to yourself.)
    let pattern_expected = PExpected::NoExpectation(expr_type);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    constrain_pattern(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        &mut state,
        loc_pattern,
        pattern_expected,
    );

    state
}

/// Turn e.g. `Int` into `Attr.Attr * Int`
fn annotation_to_attr_type(
    var_store: &mut VarStore,
    ann: &Type,
    rigids: &mut ImSet<Variable>,
    change_var_kind: bool,
) -> (Vec<Variable>, Type) {
    use roc_types::types::Type::*;

    match ann {
        Variable(var) => {
            if change_var_kind {
                let uvar = var_store.fresh();
                rigids.insert(uvar);
                (
                    vec![],
                    attr_type(Bool::variable(uvar), Type::Variable(*var)),
                )
            } else {
                (vec![], Type::Variable(*var))
            }
        }

        Boolean(_) | Erroneous(_) => (vec![], ann.clone()),
        EmptyRec | EmptyTagUnion => {
            let uniq_var = var_store.fresh();
            (
                vec![uniq_var],
                attr_type(Bool::variable(uniq_var), ann.clone()),
            )
        }

        Function(arguments, closure, result) => {
            let uniq_var = var_store.fresh();
            let (mut arg_vars, args_lifted) =
                annotation_to_attr_type_many(var_store, arguments, rigids, change_var_kind);

            // NOTE: we don't lift the closure var!
            // their uniqueness will never matter (it's a phantom parameter)
            // and not lifting makes code reuse possible

            let (result_vars, result_lifted) =
                annotation_to_attr_type(var_store, result, rigids, change_var_kind);

            arg_vars.extend(result_vars);
            arg_vars.push(uniq_var);

            match **closure {
                Type::Variable(c) => arg_vars.push(c),
                _ => unreachable!("closure must contain a type variable"),
            }

            (
                arg_vars,
                attr_type(
                    Bool::variable(uniq_var),
                    Type::Function(args_lifted, closure.clone(), Box::new(result_lifted)),
                ),
            )
        }

        Apply(Symbol::ATTR_ATTR, args) => {
            let uniq_type = args[0].clone();

            // A rigid behind an attr has already been lifted, don't do it again!
            let (result_vars, result_lifted) = match args[1] {
                Type::Variable(_) => match uniq_type {
                    Type::Boolean(Bool::Container(urigid, _)) => (vec![urigid], args[1].clone()),
                    _ => (vec![], args[1].clone()),
                },
                _ => annotation_to_attr_type(var_store, &args[1], rigids, change_var_kind),
            };

            let result = Apply(Symbol::ATTR_ATTR, vec![uniq_type, result_lifted]);

            (result_vars, result)
        }

        Apply(symbol, args) => {
            let uniq_var = var_store.fresh();

            let (mut arg_vars, args_lifted) =
                annotation_to_attr_type_many(var_store, args, rigids, change_var_kind);
            let result = attr_type(Bool::variable(uniq_var), Type::Apply(*symbol, args_lifted));

            arg_vars.push(uniq_var);

            (arg_vars, result)
        }

        Record(fields, ext_type) => {
            let uniq_var = var_store.fresh();
            let mut vars = Vec::with_capacity(fields.len());
            let mut lifted_fields = SendMap::default();

            for (label, field) in fields.clone() {
                use RecordField::*;

                let lifted_field = match field {
                    Demanded(tipe) => {
                        let (new_vars, lifted_field) =
                            annotation_to_attr_type(var_store, &tipe, rigids, change_var_kind);

                        vars.extend(new_vars);

                        Demanded(lifted_field)
                    }
                    Required(tipe) => {
                        let (new_vars, lifted_field) =
                            annotation_to_attr_type(var_store, &tipe, rigids, change_var_kind);

                        vars.extend(new_vars);

                        Required(lifted_field)
                    }
                    Optional(tipe) => {
                        let (new_vars, lifted_field) =
                            annotation_to_attr_type(var_store, &tipe, rigids, change_var_kind);

                        vars.extend(new_vars);

                        Optional(lifted_field)
                    }
                };

                lifted_fields.insert(label, lifted_field);
            }

            vars.push(uniq_var);

            (
                vars,
                attr_type(
                    Bool::variable(uniq_var),
                    Type::Record(lifted_fields, ext_type.clone()),
                ),
            )
        }

        TagUnion(tags, ext_type) => {
            let uniq_var = var_store.fresh();
            let mut vars = Vec::with_capacity(tags.len());
            let mut lifted_tags = Vec::with_capacity(tags.len());

            for (tag, fields) in tags {
                let (new_vars, lifted_fields) =
                    annotation_to_attr_type_many(var_store, fields, rigids, change_var_kind);
                vars.extend(new_vars);
                lifted_tags.push((tag.clone(), lifted_fields));
            }

            vars.push(uniq_var);

            (
                vars,
                attr_type(
                    Bool::variable(uniq_var),
                    Type::TagUnion(lifted_tags, ext_type.clone()),
                ),
            )
        }
        RecursiveTagUnion(rec_var, tags, ext_type) => {
            // In the case of
            //
            //      [ Cons a (List a), Nil ] as List a
            //
            // We need to lift it to
            //
            //      Attr u ([ Cons a (Attr u (List a)), Nil ] as List a)
            //
            // So the `u` of the whole recursive tag union is the same as the one used in the recursion

            let uniq_var = var_store.fresh();
            let mut vars = Vec::with_capacity(tags.len());
            let mut lifted_tags = Vec::with_capacity(tags.len());

            let mut substitutions = ImMap::default();
            substitutions.insert(
                *rec_var,
                attr_type(Bool::variable(uniq_var), Type::Variable(*rec_var)),
            );

            for (tag, fields) in tags {
                let (new_vars, mut lifted_fields) =
                    annotation_to_attr_type_many(var_store, fields, rigids, change_var_kind);
                vars.extend(new_vars);

                for f in lifted_fields.iter_mut() {
                    f.substitute(&substitutions);
                }

                lifted_tags.push((tag.clone(), lifted_fields));
            }

            vars.push(uniq_var);

            let result = attr_type(
                Bool::variable(uniq_var),
                Type::RecursiveTagUnion(*rec_var, lifted_tags, ext_type.clone()),
            );

            (vars, result)
        }

        Alias(symbol, fields, actual) => {
            let (mut actual_vars, lifted_actual) =
                annotation_to_attr_type(var_store, actual, rigids, change_var_kind);

            if let Type::Apply(attr_symbol, args) = lifted_actual {
                debug_assert!(attr_symbol == Symbol::ATTR_ATTR);

                let uniq_type = args[0].clone();
                let actual_type = args[1].clone();

                let mut new_fields = Vec::with_capacity(fields.len());
                for (name, tipe) in fields {
                    let (lifted_vars, lifted) =
                        annotation_to_attr_type(var_store, tipe, rigids, change_var_kind);

                    actual_vars.extend(lifted_vars);

                    new_fields.push((name.clone(), lifted));
                }

                let alias = Type::Alias(*symbol, new_fields, Box::new(actual_type));

                (
                    actual_vars,
                    crate::builtins::builtin_type(Symbol::ATTR_ATTR, vec![uniq_type, alias]),
                )
            } else {
                panic!("lifted type is not Attr")
            }
        }
        HostExposedAlias {
            name: symbol,
            arguments: fields,
            actual_var,
            actual,
        } => {
            let (mut actual_vars, lifted_actual) =
                annotation_to_attr_type(var_store, actual, rigids, change_var_kind);

            if let Type::Apply(attr_symbol, args) = lifted_actual {
                debug_assert!(attr_symbol == Symbol::ATTR_ATTR);

                let uniq_type = args[0].clone();
                let actual_type = args[1].clone();

                let mut new_fields = Vec::with_capacity(fields.len());
                for (name, tipe) in fields {
                    let (lifted_vars, lifted) =
                        annotation_to_attr_type(var_store, tipe, rigids, change_var_kind);

                    actual_vars.extend(lifted_vars);

                    new_fields.push((name.clone(), lifted));
                }

                let alias = Type::HostExposedAlias {
                    name: *symbol,
                    arguments: new_fields,
                    actual_var: *actual_var,
                    actual: Box::new(actual_type),
                };

                (
                    actual_vars,
                    crate::builtins::builtin_type(Symbol::ATTR_ATTR, vec![uniq_type, alias]),
                )
            } else {
                panic!("lifted type is not Attr")
            }
        }
    }
}

fn annotation_to_attr_type_many(
    var_store: &mut VarStore,
    anns: &[Type],
    rigids: &mut ImSet<Variable>,
    change_var_kind: bool,
) -> (Vec<Variable>, Vec<Type>) {
    anns.iter()
        .fold((Vec::new(), Vec::new()), |(mut vars, mut types), value| {
            let (new_vars, tipe) =
                annotation_to_attr_type(var_store, value, rigids, change_var_kind);
            vars.extend(new_vars);
            types.push(tipe);

            (vars, types)
        })
}

fn constrain_def(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    def: &Def,
    body_con: Constraint,
) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut pattern_state = constrain_def_pattern(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        &def.loc_pattern,
        expr_type.clone(),
    );

    pattern_state.vars.push(expr_var);

    let mut new_rigids = Vec::new();

    let expr_con = match &def.annotation {
        Some(annotation) => {
            let arity = annotation.signature.arity();
            let mut ftv = env.rigids.clone();

            let signature = instantiate_rigids(
                var_store,
                &annotation.signature,
                &annotation.introduced_variables,
                &mut new_rigids,
                &mut ftv,
                &def.loc_pattern,
                &mut pattern_state.headers,
            );

            let annotation_expected = Expected::FromAnnotation(
                def.loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                signature,
            );

            pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
                Category::Storage(std::file!(), std::line!()),
                Region::zero(),
            ));

            constrain_expr(
                &Env {
                    rigids: ftv,
                    home: env.home,
                },
                var_store,
                var_usage,
                applied_usage_constraint,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            )
        }
        None => constrain_expr(
            env,
            var_store,
            var_usage,
            applied_usage_constraint,
            def.loc_expr.region,
            &def.loc_expr.value,
            Expected::NoExpectation(expr_type),
        ),
    };

    Let(Box::new(LetConstraint {
        rigid_vars: new_rigids,
        flex_vars: pattern_state.vars,
        def_types: pattern_state.headers,
        defs_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),        // always empty
            flex_vars: Vec::new(),         // empty, because our functions have no arguments
            def_types: SendMap::default(), // empty, because our functions have no arguments!
            defs_constraint: And(pattern_state.constraints),
            ret_constraint: expr_con,
        })),
        ret_constraint: body_con,
    }))
}

fn constrain_closure_size(
    name: Symbol,
    region: Region,
    captured_symbols: &[(Symbol, Variable)],
    closure_var: Variable,
    closure_ext_var: Variable,
    variables: &mut Vec<Variable>,
) -> Constraint {
    debug_assert!(variables.iter().any(|s| *s == closure_var));
    debug_assert!(variables.iter().any(|s| *s == closure_ext_var));

    let mut tag_arguments = Vec::with_capacity(captured_symbols.len());
    let mut captured_symbols_constraints = Vec::with_capacity(captured_symbols.len());

    for (symbol, var) in captured_symbols {
        // make sure the variable is registered
        variables.push(*var);

        // this symbol is captured, so it must be part of the closure type
        tag_arguments.push(Type::Variable(*var));

        // make the variable equal to the looked-up type of symbol
        captured_symbols_constraints.push(Constraint::Lookup(
            *symbol,
            Expected::NoExpectation(Type::Variable(*var)),
            Region::zero(),
        ));
    }

    let tag_name = roc_module::ident::TagName::Closure(name);
    let closure_type = Type::TagUnion(
        vec![(tag_name, tag_arguments)],
        Box::new(Type::Variable(closure_ext_var)),
    );

    let finalizer = Eq(
        Type::Variable(closure_var),
        Expected::NoExpectation(closure_type),
        Category::ClosureSize,
        region,
    );

    captured_symbols_constraints.push(finalizer);

    Constraint::And(captured_symbols_constraints)
}

fn instantiate_rigids(
    var_store: &mut VarStore,
    annotation: &Type,
    introduced_vars: &IntroducedVariables,
    new_rigids: &mut Vec<Variable>,
    ftv: &mut ImMap<Lowercase, (Variable, Variable)>,
    loc_pattern: &Located<Pattern>,
    headers: &mut SendMap<Symbol, Located<Type>>,
) -> Type {
    let unlifed_annotation = annotation.clone();
    let mut annotation = annotation.clone();

    let mut rigid_substitution: ImMap<Variable, Type> = ImMap::default();

    for (name, var) in introduced_vars.var_by_name.iter() {
        if let Some((existing_rigid, existing_uvar)) = ftv.get(&name) {
            rigid_substitution.insert(
                *var,
                attr_type(
                    Bool::variable(*existing_uvar),
                    Type::Variable(*existing_rigid),
                ),
            );
        } else {
            // possible use this rigid in nested def's
            let uvar = var_store.fresh();
            ftv.insert(name.clone(), (*var, uvar));

            new_rigids.push(*var);
        }
    }

    // Instantiate rigid variables
    if !rigid_substitution.is_empty() {
        annotation.substitute(&rigid_substitution);
    }

    let mut new_uniqueness_rigids = ImSet::default();
    let (mut uniq_vars, annotation) =
        annotation_to_attr_type(var_store, &annotation, &mut new_uniqueness_rigids, true);

    if let Pattern::Identifier(symbol) = loc_pattern.value {
        headers.insert(symbol, Located::at(loc_pattern.region, annotation.clone()));
    } else if let Some(new_headers) = crate::pattern::headers_from_annotation(
        &loc_pattern.value,
        &Located::at(loc_pattern.region, unlifed_annotation),
    ) {
        for (k, v) in new_headers {
            let (new_uniq_vars, attr_annotation) =
                annotation_to_attr_type(var_store, &v.value, &mut new_uniqueness_rigids, true);

            uniq_vars.extend(new_uniq_vars);

            headers.insert(k, Located::at(loc_pattern.region, attr_annotation));
        }
    }

    new_rigids.extend(uniq_vars);
    new_rigids.extend(introduced_vars.wildcards.iter().cloned());
    new_rigids.extend(new_uniqueness_rigids);

    annotation
}

fn constrain_recursive_defs(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    defs: &[Def],
    body_con: Constraint,
) -> Constraint {
    rec_defs_help(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        defs,
        body_con,
        Info::with_capacity(defs.len()),
        Info::with_capacity(defs.len()),
    )
}

#[allow(clippy::too_many_arguments)]
pub fn rec_defs_help(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    defs: &[Def],
    body_con: Constraint,
    mut rigid_info: Info,
    mut flex_info: Info,
) -> Constraint {
    for def in defs {
        let expr_var = def.expr_var;
        let expr_type = Type::Variable(expr_var);

        let pattern_expected = PExpected::NoExpectation(expr_type.clone());

        let mut pattern_state = PatternState {
            headers: SendMap::default(),
            vars: flex_info.vars.clone(),
            constraints: Vec::with_capacity(1),
        };

        pattern_state.vars.push(expr_var);

        constrain_pattern(
            env,
            var_store,
            var_usage,
            applied_usage_constraint,
            &mut pattern_state,
            &def.loc_pattern,
            pattern_expected,
        );

        // TODO see where aliases should go
        let mut new_rigids = Vec::new();
        match &def.annotation {
            None => {
                let expr_con = constrain_expr(
                    env,
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    Expected::NoExpectation(expr_type),
                );

                // TODO investigate if this let can be safely removed
                let def_con = Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: Vec::new(), // empty because Roc function defs have no args
                    def_types: SendMap::default(), // empty because Roc function defs have no args
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                flex_info.vars = pattern_state.vars;
                flex_info.constraints.push(def_con);
                flex_info.def_types.extend(pattern_state.headers);
            }

            Some(annotation) => {
                let arity = annotation.signature.arity();
                let mut ftv = env.rigids.clone();
                let signature = instantiate_rigids(
                    var_store,
                    &annotation.signature,
                    &annotation.introduced_variables,
                    &mut new_rigids,
                    &mut ftv,
                    &def.loc_pattern,
                    &mut pattern_state.headers,
                );
                let annotation_expected = Expected::FromAnnotation(
                    def.loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody {
                        region: annotation.region,
                    },
                    signature.clone(),
                );
                let expr_con = constrain_expr(
                    &Env {
                        rigids: ftv,
                        home: env.home,
                    },
                    var_store,
                    var_usage,
                    applied_usage_constraint,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    annotation_expected.clone(),
                );

                // ensure expected type unifies with annotated type
                let storage_con = Eq(
                    expr_type,
                    annotation_expected,
                    Category::Storage(std::file!(), std::line!()),
                    def.loc_expr.region,
                );

                // TODO investigate if this let can be safely removed
                let def_con = Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: Vec::new(), // empty because Roc function defs have no args
                    def_types: SendMap::default(), // empty because Roc function defs have no args
                    defs_constraint: storage_con,
                    ret_constraint: expr_con,
                }));

                rigid_info.vars.extend(&new_rigids);
                rigid_info.constraints.push(Let(Box::new(LetConstraint {
                    rigid_vars: new_rigids,
                    flex_vars: pattern_state.vars,
                    def_types: SendMap::default(), // no headers introduced (at this level)
                    defs_constraint: def_con,
                    ret_constraint: True,
                })));
                rigid_info.def_types.extend(pattern_state.headers);
            }
        }
    }

    Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        def_types: rigid_info.def_types,
        defs_constraint: True,
        ret_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: flex_info.vars,
            def_types: flex_info.def_types.clone(),
            defs_constraint: Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: Vec::new(),
                def_types: flex_info.def_types,
                defs_constraint: True,
                ret_constraint: And(flex_info.constraints),
            })),
            ret_constraint: And(vec![And(rigid_info.constraints), body_con]),
        })),
    }))
}

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn constrain_field_update(
    env: &Env,
    var_store: &mut VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Located<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = Expected::ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    (var, field_type, con)
}
