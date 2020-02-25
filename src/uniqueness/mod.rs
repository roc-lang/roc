use crate::can::def::{Declaration, Def};
use crate::can::expr::Expr;
use crate::can::expr::Field;
use crate::can::ident::{Ident, Lowercase, TagName};
use crate::can::pattern::{Pattern, RecordDestruct};
use crate::collections::{ImMap, ImSet, SendMap};
use crate::constrain::expr::{exists, exists_with_aliases, Info};
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::Alias;
use crate::types::AnnotationSource::{self, *};
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self};
use crate::types::LetConstraint;
use crate::types::PExpected::{self};
use crate::types::PReason::{self};
use crate::types::Reason;
use crate::types::Type::{self, *};
use crate::uniqueness::boolean_algebra::{Atom, Bool};
use crate::uniqueness::builtins::{attr_type, list_type, str_type};
use crate::uniqueness::sharing::{FieldAccess, ReferenceCount, VarUsage};

pub use crate::can::expr::Expr::*;

pub mod boolean_algebra;
pub mod builtins;
pub mod sharing;

pub struct Env {
    /// Whenever we encounter a user-defined type variable (a "rigid" var for short),
    /// for example `u` and `a` in the annotation `identity : Attr u a -> Attr u a`, we add it to this
    /// map so that expressions within that annotation can share these vars.
    pub rigids: ImMap<Lowercase, (Variable, Variable)>,
    pub home: ModuleId,
}

pub fn constrain_declaration(
    home: ModuleId,
    var_store: &VarStore,
    region: Region,
    loc_expr: Located<Expr>,
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
    aliases: SendMap<Symbol, Alias>,
) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;

    let var_store = VarStore::default();
    let var_usage = VarUsage::default();

    for decl in decls.iter().rev() {
        // NOTE: rigids are empty because they are not shared between top-level definitions
        match decl {
            Declaration::Declare(def) => {
                constraint = exists_with_aliases(
                    aliases.clone(),
                    Vec::new(),
                    constrain_def(
                        &Env {
                            home,
                            rigids: ImMap::default(),
                        },
                        &var_store,
                        &var_usage,
                        &mut ImSet::default(),
                        def,
                        constraint,
                    ),
                );
            }
            Declaration::DeclareRec(defs) => {
                constraint = exists_with_aliases(
                    aliases.clone(),
                    Vec::new(),
                    constrain_recursive_defs(
                        &Env {
                            home,
                            rigids: ImMap::default(),
                        },
                        &var_store,
                        &var_usage,
                        &mut ImSet::default(),
                        defs,
                        constraint,
                    ),
                );
            }
            Declaration::InvalidCycle(_, _) => panic!("TODO handle invalid cycle"),
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
    var_store: &VarStore,
    state: &mut PatternState,
    pattern: &Located<Pattern>,
    expected: PExpected<Type>,
) {
    use crate::can::pattern::Pattern::*;
    use crate::types::PatternCategory;

    match &pattern.value {
        Identifier(symbol) => {
            state.headers.insert(
                symbol.clone(),
                Located {
                    region: pattern.region,
                    value: expected.get_type(),
                },
            );
        }

        IntLiteral(_) => {
            let (num_uvar, int_uvar, num_type) = unique_int(var_store);
            state.constraints.push(exists(
                vec![num_uvar, int_uvar],
                Constraint::Pattern(pattern.region, PatternCategory::Int, num_type, expected),
            ));
        }
        FloatLiteral(_) => {
            let (num_uvar, float_uvar, num_type) = unique_float(var_store);
            state.constraints.push(exists(
                vec![num_uvar, float_uvar],
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

        RecordDestructure(ext_var, patterns) => {
            // TODO if a subpattern doesn't bind any identifiers, it doesn't count for uniqueness
            let mut pattern_uniq_vars = Vec::with_capacity(patterns.len());

            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut field_types: SendMap<Lowercase, Type> = SendMap::default();
            for Located {
                value:
                    RecordDestruct {
                        var,
                        label,
                        symbol,
                        guard,
                    },
                ..
            } in patterns
            {
                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pat_type = attr_type(Bool::variable(pat_uniq_var), Type::Variable(*var));
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(&symbol) {
                    state.headers.insert(
                        symbol.clone(),
                        Located::at(pattern.region, pat_type.clone()),
                    );
                }

                field_types.insert(label.clone(), pat_type.clone());

                if let Some((guard_var, loc_guard)) = guard {
                    state.constraints.push(Eq(
                        Type::Variable(*guard_var),
                        Expected::NoExpectation(pat_type.clone()),
                        pattern.region,
                    ));
                    state.vars.push(*guard_var);
                    constrain_pattern(var_store, state, loc_guard, expected);
                }

                state.vars.push(*var);
            }

            let record_uniq_type = {
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                state.vars.extend(pattern_uniq_vars.clone());
                Bool::with_free(
                    empty_var,
                    pattern_uniq_vars.into_iter().map(Atom::Variable).collect(),
                )
            };

            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(ext_type)),
            );
            let record_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Record,
                record_type,
                expected,
            );

            state.constraints.push(record_con);
        }

        AppliedTag(ext_var, symbol, patterns) => {
            // TODO if a subpattern doesn't bind any identifiers, it doesn't count for uniqueness
            let mut argument_types = Vec::with_capacity(patterns.len());
            let mut pattern_uniq_vars = Vec::with_capacity(patterns.len());

            for (pattern_var, loc_pattern) in patterns {
                state.vars.push(*pattern_var);

                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pattern_type =
                    attr_type(Bool::variable(pat_uniq_var), Type::Variable(*pattern_var));
                argument_types.push(pattern_type.clone());

                let expected = PExpected::NoExpectation(pattern_type);
                constrain_pattern(var_store, state, loc_pattern, expected);
            }

            let tag_union_uniq_type = {
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                state.vars.extend(pattern_uniq_vars.clone());
                Bool::with_free(
                    empty_var,
                    pattern_uniq_vars.into_iter().map(Atom::Variable).collect(),
                )
            };
            let union_type = attr_type(
                tag_union_uniq_type,
                Type::TagUnion(
                    vec![(symbol.clone(), argument_types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
            );

            let tag_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Ctor(symbol.clone()),
                union_type,
                expected,
            );

            state.vars.push(*ext_var);
            state.constraints.push(tag_con);
        }

        Underscore | Shadowed(_, _) | UnsupportedPattern(_) => {
            // no constraints
        }
    }
}

fn unique_num(var_store: &VarStore, symbol: Symbol) -> (Variable, Variable, Type) {
    let num_uvar = var_store.fresh();
    let val_uvar = var_store.fresh();

    let val_type = Type::Apply(symbol, Vec::new());
    let val_utype = attr_type(Bool::variable(val_uvar), val_type);

    let num_utype = Type::Apply(Symbol::NUM_NUM, vec![val_utype]);
    let num_type = attr_type(Bool::variable(num_uvar), num_utype);

    (num_uvar, val_uvar, num_type)
}

fn unique_int(var_store: &VarStore) -> (Variable, Variable, Type) {
    unique_num(var_store, Symbol::INT_INTEGER)
}

fn unique_float(var_store: &VarStore) -> (Variable, Variable, Type) {
    unique_num(var_store, Symbol::FLOAT_FLOATINGPOINT)
}

pub fn constrain_expr(
    env: &Env,
    var_store: &VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    pub use crate::can::expr::Expr::*;

    match expr {
        Int(var, _) => {
            let (num_uvar, int_uvar, num_type) = unique_int(var_store);

            exists(
                vec![*var, num_uvar, int_uvar],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(Reason::IntLiteral, num_type, region),
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, region),
                ]),
            )
        }
        Float(var, _) => {
            let (num_uvar, float_uvar, num_type) = unique_float(var_store);
            exists(
                vec![*var, num_uvar, float_uvar],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(Reason::FloatLiteral, num_type, region),
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, region),
                ]),
            )
        }
        BlockStr(_) | Str(_) => {
            let uniq_type = var_store.fresh();
            let inferred = str_type(Bool::variable(uniq_type));

            exists(vec![uniq_type], Eq(inferred, expected, region))
        }
        EmptyRecord => {
            let uniq_type = var_store.fresh();

            exists(
                vec![uniq_type],
                Eq(
                    attr_type(Bool::variable(uniq_type), EmptyRec),
                    expected,
                    region,
                ),
            )
        }
        Record(variable, fields) => {
            // NOTE: canonicalization guarantees at least one field
            // zero fields generates an EmptyRecord
            let mut field_types = SendMap::default();
            let mut field_vars = Vec::with_capacity(fields.len());
            field_vars.push(*variable);

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
                field_types.insert(label.clone(), field_type);

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
            let record_con = Eq(record_type, expected.clone(), region);
            let ext_con = Eq(Type::Variable(*variable), expected, region);

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

            let union_con = Eq(union_type, expected.clone(), region);
            let ast_con = Eq(Type::Variable(*variant_var), expected, region);

            vars.push(uniq_var);
            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        List {
            entry_var,
            loc_elems,
        } => {
            let uniq_var = var_store.fresh();
            if loc_elems.is_empty() {
                let inferred = builtins::empty_list_type(Bool::variable(uniq_var), *entry_var);
                exists(vec![*entry_var, uniq_var], Eq(inferred, expected, region))
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let entry_type = Type::Variable(*entry_var);
                let mut constraints = Vec::with_capacity(1 + loc_elems.len());

                for loc_elem in loc_elems.iter() {
                    let elem_expected =
                        Expected::ForReason(Reason::ElemInList, entry_type.clone(), region);
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
                constraints.push(Eq(inferred, expected, region));

                exists(vec![*entry_var, uniq_var], And(constraints))
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
        Closure(fn_var, _symbol, recursion, args, boxed) => {
            use crate::can::expr::Recursive;

            let (loc_body_expr, ret_var) = &**boxed;
            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(args.len()),
                constraints: Vec::with_capacity(1),
            };
            let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
            let mut pattern_types = Vec::with_capacity(state.vars.capacity());
            let ret_var = *ret_var;
            let ret_type = Type::Variable(ret_var);

            vars.push(ret_var);
            vars.push(*fn_var);

            for (pattern_var, loc_pattern) in args {
                let pattern_type = Type::Variable(*pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

                pattern_types.push(pattern_type);

                constrain_pattern(var_store, &mut state, loc_pattern, pattern_expected);

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

            let fn_type = attr_type(
                fn_uniq_type,
                Type::Function(pattern_types, Box::new(ret_type.clone())),
            );
            let body_type = Expected::NoExpectation(ret_type);
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

            exists(
                vars,
                And(vec![
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: state.vars,
                        def_types: state.headers,
                        def_aliases: SendMap::default(),
                        defs_constraint,
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected type"
                    Eq(fn_type.clone(), expected, region),
                    // "fn_var is equal to the closure's type" - fn_var is used in code gen
                    Eq(
                        Type::Variable(*fn_var),
                        Expected::NoExpectation(fn_type),
                        region,
                    ),
                ]),
            )
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, fn_expr, ret_var) = &**boxed;
            let fn_type = Variable(*fn_var);
            let ret_type = Variable(*ret_var);
            let fn_expected = Expected::NoExpectation(fn_type.clone());
            let fn_region = fn_expr.region;

            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);

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

            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);

                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
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
                    Function(arg_types, Box::new(ret_type.clone())),
                ),
                region,
            );

            exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, region),
                ]),
            )
        }
        LetRec(defs, loc_ret, var, unlifted_aliases) => {
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

            let mut aliases = unlifted_aliases.clone();
            aliases_to_attr_type(var_store, &mut aliases);

            exists_with_aliases(
                aliases,
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
                    Eq(Type::Variable(*var), expected, loc_ret.region),
                ]),
            )
        }
        LetNonRec(def, loc_ret, var, unlifted_aliases) => {
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

            let mut aliases = unlifted_aliases.clone();
            aliases_to_attr_type(var_store, &mut aliases);

            exists_with_aliases(
                aliases,
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
                    Eq(Type::Variable(*var), expected, loc_ret.region),
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
            let bool_type = Type::TagUnion(
                vec![
                    (TagName::Global("True".into()), vec![]),
                    (TagName::Global("False".into()), vec![]),
                ],
                Box::new(Type::EmptyTagUnion),
            );

            let mut branch_cons = Vec::with_capacity(2 * branches.len() + 2);
            let mut cond_uniq_vars = Vec::with_capacity(branches.len() + 2);

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

                        let cond_con = Eq(
                            Type::Variable(*cond_var),
                            expect_bool.clone(),
                            loc_cond.region,
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
                                AnnotationSource::TypedIfBranch(index + 1),
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
                            AnnotationSource::TypedIfBranch(branches.len() + 1),
                            tipe.clone(),
                        ),
                    );

                    let ast_con = Eq(
                        Type::Variable(*branch_var),
                        Expected::NoExpectation(tipe),
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

                        let cond_con = Eq(
                            Type::Variable(*cond_var),
                            expect_bool.clone(),
                            loc_cond.region,
                        );
                        let then_con = constrain_expr(
                            env,
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            loc_body.region,
                            &loc_body.value,
                            Expected::ForReason(
                                Reason::IfBranch { index: index + 1 },
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
                                index: branches.len() + 1,
                            },
                            Type::Variable(*branch_var),
                            region,
                        ),
                    );

                    branch_cons.push(Eq(Type::Variable(*branch_var), expected, region));
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
                    constraints.push(Eq(Type::Variable(*expr_var), expected.clone(), region));

                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            env,
                            region,
                            &loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                *arity,
                                TypedWhenBranch(index),
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

                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            var_store,
                            var_usage,
                            applied_usage_constraint,
                            env,
                            region,
                            &loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::ForReason(
                                Reason::WhenBranch { index },
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
                        Eq(branch_type, expected, region),
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
            let mut fields: SendMap<Lowercase, Type> = SendMap::default();
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
                fields.insert(field_name, tipe);
                vars.push(var);
                cons.push(con);
            }

            let uniq_var = var_store.fresh();
            vars.push(uniq_var);

            let fields_type = attr_type(
                Bool::variable(uniq_var),
                Type::Record(fields.clone(), Box::new(Type::Variable(*ext_var))),
            );
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(
                record_type.clone(),
                Expected::NoExpectation(fields_type),
                region,
            );
            let record_con = Eq(record_type.clone(), expected, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = Lookup(
                *symbol,
                Expected::ForReason(
                    Reason::RecordUpdateKeys(*symbol, fields),
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
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let mut field_types = SendMap::default();

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::variable(field_uniq_var);
            let field_type = attr_type(field_uniq_type, Type::Variable(*field_var));

            field_types.insert(field.clone(), field_type.clone());

            let record_uniq_var = var_store.fresh();
            let record_uniq_type =
                Bool::with_free(record_uniq_var, vec![Atom::Variable(field_uniq_var)]);
            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(Type::Variable(*ext_var))),
            );

            let record_expected = Expected::NoExpectation(record_type);
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
                vec![*field_var, *ext_var, field_uniq_var, record_uniq_var],
                And(vec![Eq(field_type, expected, region), inner_constraint]),
            )
        }

        Accessor {
            field,
            field_var,
            ext_var,
        } => {
            let mut field_types = SendMap::default();

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::variable(field_uniq_var);
            let field_type = attr_type(field_uniq_type, Type::Variable(*field_var));

            field_types.insert(field.clone(), field_type.clone());

            let record_uniq_var = var_store.fresh();
            let record_uniq_type =
                Bool::with_free(record_uniq_var, vec![Atom::Variable(field_uniq_var)]);
            let record_type = attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(Type::Variable(*ext_var))),
            );

            let fn_uniq_var = var_store.fresh();
            let fn_type = attr_type(
                Bool::variable(fn_uniq_var),
                Type::Function(vec![record_type], Box::new(field_type)),
            );

            exists(
                vec![
                    *field_var,
                    *ext_var,
                    fn_uniq_var,
                    field_uniq_var,
                    record_uniq_var,
                ],
                And(vec![Eq(fn_type, expected, region)]),
            )
        }
        RuntimeError(_) => True,
    }
}

fn constrain_var(
    var_store: &VarStore,
    applied_usage_constraint: &mut ImSet<Symbol>,
    symbol_for_lookup: Symbol,
    usage: Option<&ReferenceCount>,
    region: Region,
    expected: Expected<Type>,
) -> Constraint {
    use sharing::ReferenceCount::*;
    match usage {
        Some(Shared) => {
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
                    Eq(attr_type, expected, region),
                    Eq(
                        Type::Boolean(uniq_type),
                        Expected::NoExpectation(Type::Boolean(Bool::shared())),
                        region,
                    ),
                ]),
            )
        }
        Some(Unique) => {
            // no additional constraints, keep uniqueness unbound
            Lookup(symbol_for_lookup, expected, region)
        }
        Some(ReferenceCount::Access(field_access))
        | Some(ReferenceCount::Update(_, field_access)) => {
            applied_usage_constraint.insert(symbol_for_lookup.clone());

            let mut variables = Vec::new();
            let (free, rest, inner_type) =
                constrain_field_access(var_store, &field_access, &mut variables);

            let record_type = attr_type(Bool::with_free(free, rest), inner_type);

            // NOTE breaking the expectation up like this REALLY matters!
            let new_expected = Expected::NoExpectation(record_type.clone());
            exists(
                variables,
                And(vec![
                    Lookup(symbol_for_lookup, new_expected, region),
                    Eq(record_type, expected, region),
                ]),
            )
        }

        Some(other) => panic!("some other rc value: {:?}", other),
        None => panic!("symbol not analyzed"),
    }
}

fn constrain_field_access(
    var_store: &VarStore,
    field_access: &FieldAccess,
    field_vars: &mut Vec<Variable>,
) -> (Variable, Vec<Atom>, Type) {
    use sharing::ReferenceCount::Shared;

    let mut field_types = SendMap::default();
    let mut uniq_vars = Vec::new();

    for (field, (rc, nested)) in field_access.fields.clone() {
        // handle nested fields
        let field_type = if nested.is_empty() {
            // generate constraint for this field
            let field_var = var_store.fresh();
            field_vars.push(field_var);

            if rc == Shared {
                attr_type(Bool::shared(), Variable(field_var))
            } else {
                // TODO don't generate constraint when field is possible unique?
                let uniq_var = var_store.fresh();
                field_vars.push(uniq_var);
                uniq_vars.push(Atom::Variable(uniq_var));
                attr_type(Bool::variable(uniq_var), Variable(field_var))
            }
        } else {
            let (inner_free, inner_rest, inner_type) =
                constrain_field_access(var_store, &nested, field_vars);

            if rc == Shared {
                attr_type(Bool::shared(), inner_type)
            } else {
                uniq_vars.push(Atom::Variable(inner_free));
                uniq_vars.extend(inner_rest.clone());
                attr_type(Bool::with_free(inner_free, inner_rest), inner_type)
            }
        };
        field_types.insert(field.into(), field_type);
    }

    let record_uniq_var = var_store.fresh();
    let record_ext_var = var_store.fresh();
    field_vars.push(record_uniq_var);
    field_vars.push(record_ext_var);

    (
        record_uniq_var,
        uniq_vars,
        Type::Record(
            field_types,
            // TODO can we avoid doing Box::new on every single one of these?
            // For example, could we have a single lazy_static global Box they
            // could all share?
            Box::new(Variable(record_ext_var)),
        ),
    )
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn constrain_when_branch(
    var_store: &VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    env: &Env,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(
        env,
        var_store,
        var_usage,
        applied_usage_constraint,
        region,
        &loc_expr.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    // mutates the state, so return value is not used
    constrain_pattern(var_store, &mut state, &loc_pattern, pattern_expected);

    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        def_aliases: SendMap::default(),
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint,
    }))
}

fn constrain_def_pattern(
    var_store: &VarStore,
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

    constrain_pattern(var_store, &mut state, loc_pattern, pattern_expected);

    state
}

/// Turn e.g. `Int` into `Attr.Attr * Int`
fn annotation_to_attr_type(
    var_store: &VarStore,
    ann: &Type,
    rigids: &mut ImMap<Variable, Variable>,
    change_var_kind: bool,
) -> (Vec<Variable>, Type) {
    use crate::types::Type::*;

    match ann {
        Variable(var) => {
            if change_var_kind {
                if let Some(uvar) = rigids.get(var) {
                    (
                        vec![],
                        attr_type(Bool::variable(*uvar), Type::Variable(*var)),
                    )
                } else {
                    let uvar = var_store.fresh();
                    rigids.insert(*var, uvar);
                    (
                        vec![],
                        attr_type(Bool::variable(uvar), Type::Variable(*var)),
                    )
                }
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

        Function(arguments, result) => {
            let uniq_var = var_store.fresh();
            let (mut arg_vars, args_lifted) =
                annotation_to_attr_type_many(var_store, arguments, rigids, change_var_kind);
            let (result_vars, result_lifted) =
                annotation_to_attr_type(var_store, result, rigids, change_var_kind);

            arg_vars.extend(result_vars);
            arg_vars.push(uniq_var);

            (
                arg_vars,
                attr_type(
                    Bool::variable(uniq_var),
                    Type::Function(args_lifted, Box::new(result_lifted)),
                ),
            )
        }

        Apply(Symbol::ATTR_ATTR, args) => {
            let uniq_type = args[0].clone();

            // A rigid behind an attr has already been lifted, don't do it again!
            let (result_vars, result_lifted) = match args[1] {
                Type::Variable(_) => match uniq_type {
                    Type::Boolean(Bool(Atom::Variable(urigid), _)) => {
                        (vec![urigid], args[1].clone())
                    }
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

            for (label, tipe) in fields.clone() {
                let (new_vars, lifted_field) =
                    annotation_to_attr_type(var_store, &tipe, rigids, change_var_kind);
                vars.extend(new_vars);
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
                    Type::RecursiveTagUnion(*rec_var, lifted_tags, ext_type.clone()),
                ),
            )
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
                    crate::constrain::builtins::builtin_type(
                        Symbol::ATTR_ATTR,
                        vec![uniq_type, alias],
                    ),
                )
            } else {
                panic!("lifted type is not Attr")
            }
        }
    }
}

fn annotation_to_attr_type_many(
    var_store: &VarStore,
    anns: &[Type],
    rigids: &mut ImMap<Variable, Variable>,
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

fn aliases_to_attr_type(var_store: &VarStore, aliases: &mut SendMap<Symbol, Alias>) {
    for alias in aliases.iter_mut() {
        // ensure
        //
        // Identity a : [ Identity a ]
        //
        // does not turn into
        //
        // Identity a : [ Identity (Attr u a) ]
        //
        // That would give a double attr wrapper on the type arguments.
        // The `change_var_kind` flag set to false ensures type variables remain of kind *
        let (_, new) = annotation_to_attr_type(var_store, &alias.typ, &mut ImMap::default(), false);

        // remove the outer Attr, because when this occurs in a signature it'll already be wrapped in one
        match new {
            Type::Apply(Symbol::ATTR_ATTR, args) => {
                alias.typ = args[1].clone();
            }
            _ => unreachable!(),
        }
    }
}

fn constrain_def(
    env: &Env,
    var_store: &VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    def: &Def,
    body_con: Constraint,
) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut pattern_state = constrain_def_pattern(var_store, &def.loc_pattern, expr_type.clone());

    pattern_state.vars.push(expr_var);

    let mut def_aliases = SendMap::default();
    let mut new_rigids = Vec::new();

    let expr_con = match &def.annotation {
        Some((annotation, free_vars, ann_def_aliases)) => {
            def_aliases = ann_def_aliases.clone();
            let arity = annotation.arity();
            let mut ftv = env.rigids.clone();
            let annotation = instantiate_rigids(
                var_store,
                annotation,
                &free_vars,
                &mut new_rigids,
                &mut ftv,
                &def.loc_pattern,
                &mut pattern_state.headers,
            );

            let annotation_expected = Expected::FromAnnotation(
                def.loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody,
                annotation,
            );

            pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
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

    // Lift aliases to Attr types
    aliases_to_attr_type(var_store, &mut def_aliases);

    Let(Box::new(LetConstraint {
        rigid_vars: new_rigids,
        flex_vars: pattern_state.vars,
        def_types: pattern_state.headers,
        def_aliases,
        defs_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),        // always empty
            flex_vars: Vec::new(),         // empty, because our functions have no arguments
            def_types: SendMap::default(), // empty, because our functions have no arguments!
            def_aliases: SendMap::default(),
            defs_constraint: And(pattern_state.constraints),
            ret_constraint: expr_con,
        })),
        ret_constraint: body_con,
    }))
}

fn instantiate_rigids(
    var_store: &VarStore,
    annotation: &Type,
    free_vars: &SendMap<Lowercase, Variable>,
    new_rigids: &mut Vec<Variable>,
    ftv: &mut ImMap<Lowercase, (Variable, Variable)>,
    loc_pattern: &Located<Pattern>,
    headers: &mut SendMap<Symbol, Located<Type>>,
) -> Type {
    let unlifed_annotation = annotation.clone();
    let mut annotation = annotation.clone();

    let mut rigid_substitution: ImMap<Variable, Type> = ImMap::default();

    for (name, var) in free_vars {
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

    let mut new_rigid_pairs = ImMap::default();
    let (mut uniq_vars, annotation) =
        annotation_to_attr_type(var_store, &annotation, &mut new_rigid_pairs, true);

    if let Pattern::Identifier(symbol) = loc_pattern.value {
        headers.insert(symbol, Located::at(loc_pattern.region, annotation.clone()));
    } else if let Some(new_headers) = crate::constrain::pattern::headers_from_annotation(
        &loc_pattern.value,
        &Located::at(loc_pattern.region, unlifed_annotation),
    ) {
        for (k, v) in new_headers {
            let (new_uniq_vars, attr_annotation) =
                annotation_to_attr_type(var_store, &v.value, &mut new_rigid_pairs, true);

            uniq_vars.extend(new_uniq_vars);

            headers.insert(k, Located::at(loc_pattern.region, attr_annotation));
        }
    }

    new_rigids.extend(uniq_vars);

    for (_, v) in new_rigid_pairs {
        new_rigids.push(v);
    }

    annotation
}

fn constrain_recursive_defs(
    env: &Env,
    var_store: &VarStore,
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
    var_store: &VarStore,
    var_usage: &VarUsage,
    applied_usage_constraint: &mut ImSet<Symbol>,
    defs: &[Def],
    body_con: Constraint,
    mut rigid_info: Info,
    mut flex_info: Info,
) -> Constraint {
    let mut def_aliases = SendMap::default();
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
            var_store,
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
                    def_aliases: SendMap::default(),
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                flex_info.vars = pattern_state.vars;
                flex_info.constraints.push(def_con);
                flex_info.def_types.extend(pattern_state.headers);
            }

            Some((annotation, free_vars, ann_def_aliases)) => {
                for (symbol, alias) in ann_def_aliases.clone() {
                    def_aliases.insert(symbol, alias);
                }
                let arity = annotation.arity();
                let mut ftv = env.rigids.clone();
                let annotation = instantiate_rigids(
                    var_store,
                    annotation,
                    &free_vars,
                    &mut new_rigids,
                    &mut ftv,
                    &def.loc_pattern,
                    &mut pattern_state.headers,
                );
                let annotation_expected = Expected::FromAnnotation(
                    def.loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody,
                    annotation.clone(),
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
                    Expected::NoExpectation(expr_type.clone()),
                );

                // ensure expected type unifies with annotated type
                rigid_info.constraints.push(Eq(
                    expr_type,
                    annotation_expected.clone(),
                    def.loc_expr.region,
                ));

                // TODO investigate if this let can be safely removed
                let def_con = Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: Vec::new(), // empty because Roc function defs have no args
                    def_types: SendMap::default(), // empty because Roc function defs have no args
                    def_aliases: SendMap::default(),
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                rigid_info.vars.extend(&new_rigids);
                // because of how in Roc headers point to variables, we must include the pattern var here
                rigid_info.vars.extend(pattern_state.vars);
                rigid_info.constraints.push(Let(Box::new(LetConstraint {
                    rigid_vars: new_rigids,
                    flex_vars: Vec::new(),         // no flex vars introduced
                    def_types: SendMap::default(), // no headers introduced (at this level)
                    def_aliases: SendMap::default(),
                    defs_constraint: def_con,
                    ret_constraint: True,
                })));
                rigid_info.def_types.extend(pattern_state.headers);
            }
        }
    }

    // list aliases to Attr types
    aliases_to_attr_type(var_store, &mut def_aliases);

    Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        def_types: rigid_info.def_types,
        def_aliases,
        defs_constraint: True,
        ret_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: flex_info.vars,
            def_types: flex_info.def_types.clone(),
            def_aliases: SendMap::default(),
            defs_constraint: Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: Vec::new(),
                def_types: flex_info.def_types,
                def_aliases: SendMap::default(),
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
    var_store: &VarStore,
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
