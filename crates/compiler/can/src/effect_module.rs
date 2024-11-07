use crate::def::{Def, DefKind};
use crate::expr::{AnnotatedMark, ClosureData, Expr, Recursive};
use crate::pattern::Pattern;
use crate::scope::Scope;
use roc_collections::SendMap;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{LambdaSet, OptAbleVar, Type};

pub fn build_host_exposed_def(
    scope: &mut Scope,
    symbol: Symbol,
    ident: &str,
    var_store: &mut VarStore,
    annotation: crate::annotation::Annotation,
) -> Def {
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    let mut arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)> = Vec::new();
    let mut linked_symbol_arguments: Vec<(Variable, Expr)> = Vec::new();
    let mut captured_symbols: Vec<(Symbol, Variable)> = Vec::new();

    let crate::annotation::Annotation {
        introduced_variables,
        typ,
        aliases,
        ..
    } = annotation;

    let def_body = {
        match typ.shallow_structural_dealias() {
            Type::Function(args, _, _, fx) if **fx == Type::Pure => {
                for i in 0..args.len() {
                    let name = format!("closure_arg_{ident}_{i}");

                    let arg_symbol = {
                        let ident = name.clone().into();
                        scope.introduce(ident, Region::zero()).unwrap()
                    };

                    let arg_var = var_store.fresh();

                    arguments.push((
                        arg_var,
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(Pattern::Identifier(arg_symbol)),
                    ));

                    captured_symbols.push((arg_symbol, arg_var));
                    linked_symbol_arguments.push((arg_var, Expr::Var(arg_symbol, arg_var)));
                }

                let foreign_symbol_name = format!("roc_fx_{ident}");
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let task_closure_symbol = {
                    let name = format!("task_closure_{ident}");

                    let ident = name.into();
                    scope.introduce(ident, Region::zero()).unwrap()
                };

                let task_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    fx_type: var_store.fresh(),
                    early_returns: vec![],
                    name: task_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(empty_record_pattern(var_store)),
                    )],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                let (specialized_def_type, type_arguments, lambda_set_variables) =
                    build_fresh_opaque_variables(var_store);
                let body = Expr::OpaqueRef {
                    opaque_var: var_store.fresh(),
                    name: Symbol::TASK_TASK,
                    argument: Box::new((var_store.fresh(), Loc::at_zero(task_closure))),
                    specialized_def_type,
                    type_arguments,
                    lambda_set_variables,
                };

                Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    fx_type: var_store.fresh(),
                    early_returns: vec![],
                    name: symbol,
                    captured_symbols: std::vec::Vec::new(),
                    recursive: Recursive::NotRecursive,
                    arguments,
                    loc_body: Box::new(Loc::at_zero(body)),
                })
            }
            Type::Function(args, _, _, fx) if **fx == Type::Effectful => {
                for i in 0..args.len() {
                    let name = format!("{ident}_arg_{i}");

                    let arg_symbol = {
                        let ident = name.clone().into();
                        scope.introduce(ident, Region::zero()).unwrap()
                    };

                    let arg_var = var_store.fresh();

                    arguments.push((
                        arg_var,
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(Pattern::Identifier(arg_symbol)),
                    ));

                    linked_symbol_arguments.push((arg_var, Expr::Var(arg_symbol, arg_var)));
                }

                let ident_without_bang = ident.trim_end_matches('!');
                let foreign_symbol_name = format!("roc_fx_{ident_without_bang}");
                let foreign_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    fx_type: var_store.fresh(),
                    early_returns: vec![],
                    name: symbol,
                    captured_symbols: std::vec::Vec::new(),
                    recursive: Recursive::NotRecursive,
                    arguments,
                    loc_body: Box::new(Loc::at_zero(foreign_call)),
                })
            }
            _ => {
                // not a function

                let foreign_symbol_name = format!("roc_fx_{ident}");
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let task_closure_symbol = {
                    let name = format!("task_closure_{ident}");

                    let ident = name.into();
                    scope.introduce(ident, Region::zero()).unwrap()
                };

                let task_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    fx_type: var_store.fresh(),
                    early_returns: vec![],
                    name: task_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(empty_record_pattern(var_store)),
                    )],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                let (specialized_def_type, type_arguments, lambda_set_variables) =
                    build_fresh_opaque_variables(var_store);
                Expr::OpaqueRef {
                    opaque_var: var_store.fresh(),
                    name: Symbol::TASK_TASK,
                    argument: Box::new((var_store.fresh(), Loc::at_zero(task_closure))),
                    specialized_def_type,
                    type_arguments,
                    lambda_set_variables,
                }
            }
        }
    };

    let def_annotation = crate::def::Annotation {
        signature: typ,
        introduced_variables,
        aliases,
        region: Region::zero(),
    };

    Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(def_body),
        expr_var,
        pattern_vars,
        annotation: Some(def_annotation),
        kind: DefKind::Let,
    }
}

fn build_fresh_opaque_variables(
    var_store: &mut VarStore,
) -> (Box<Type>, Vec<OptAbleVar>, Vec<LambdaSet>) {
    let closure_var = var_store.fresh();

    let ok_var = var_store.fresh();
    let err_var = var_store.fresh();
    let result_var = var_store.fresh();
    let fx_var = var_store.fresh();

    let actual = Type::Function(
        vec![Type::EmptyRec],
        Box::new(Type::Variable(closure_var)),
        Box::new(Type::Variable(result_var)),
        Box::new(Type::Variable(fx_var)),
    );

    let type_arguments = vec![
        OptAbleVar {
            var: ok_var,
            opt_abilities: None,
        },
        OptAbleVar {
            var: err_var,
            opt_abilities: None,
        },
    ];
    let lambda_set_variables = vec![roc_types::types::LambdaSet(Type::Variable(closure_var))];

    (Box::new(actual), type_arguments, lambda_set_variables)
}

#[inline(always)]
fn empty_record_pattern(var_store: &mut VarStore) -> Pattern {
    Pattern::RecordDestructure {
        whole_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        destructs: vec![],
    }
}
