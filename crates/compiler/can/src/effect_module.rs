use crate::def::{Def, DefKind};
use crate::expr::{AnnotatedMark, ClosureData, Expr, Recursive};
use crate::pattern::Pattern;
use crate::scope::Scope;
use roc_collections::SendMap;
use roc_module::symbol::Symbol;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

pub fn build_host_exposed_def(
    scope: &mut Scope,
    symbol: Symbol,
    region: Region,
    ident: &str,
    var_store: &mut VarStore,
    problems: &mut Vec<Problem>,
    annotation: crate::annotation::Annotation,
) -> Def {
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    let mut arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)> = Vec::new();
    let mut linked_symbol_arguments: Vec<(Variable, Expr)> = Vec::new();

    let crate::annotation::Annotation {
        introduced_variables,
        typ,
        aliases,
        ..
    } = annotation;

    let foreign_call = match typ.shallow_structural_dealias() {
        Type::Function(args, _, _, _) => {
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

            Expr::ForeignCall {
                foreign_symbol: foreign_symbol_name.into(),
                args: linked_symbol_arguments,
                ret_var: var_store.fresh(),
            }
        }
        _ => {
            let runtime_error = RuntimeError::NonFunctionHostedAnnotation(region);
            problems.push(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    };

    let def_body = Expr::Closure(ClosureData {
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
    });

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
