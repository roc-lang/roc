#![allow(clippy::manual_map)]

use crate::env::Env;
use crate::scope::Scope;
use roc_module::called_via::CalledVia;
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{Collection, Defs, Pattern, ValueDef};
use roc_region::all::{Loc, Region};

/// Desugars a `dbg expr` expression into a statement block that prints and returns the
/// value produced by `expr`. Essentially:
/// (
///     tmpVar = expr
///     LowLevelDbg (Inspect.to_str tmpVar)
///     tmpVar
/// )
fn desugar_dbg_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    expr: &'a Loc<Expr<'a>>,
    outer_region: Region,
) -> &'a Expr<'a> {
    let region = expr.region;

    // tmpVar = expr
    let ident = env.arena.alloc(scope.gen_unique_symbol_name().to_string());

    let value_def = ValueDef::Body(
        env.arena.alloc(Loc {
            value: Pattern::Identifier { ident },
            region,
        }),
        expr,
    );

    let defs = env.arena.alloc(Defs::default());
    defs.push_value_def(value_def, region, &[], &[]);

    // tmpVar
    let tmp_var = env.arena.alloc(Loc {
        value: Var {
            module_name: "",
            ident,
        },
        region,
    });

    // LowLevelDbg
    let dbg_stmt = env.arena.alloc(Loc {
        value: *desugar_dbg_stmt(env, tmp_var, tmp_var),
        region: outer_region,
    });

    env.arena.alloc(Defs(defs, dbg_stmt))
}

/// Build a desugared `dbg {}` expression to act as a placeholder when the AST
/// is invalid.
pub fn desugar_invalid_dbg_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    outer_region: Region,
) -> &'a Expr<'a> {
    let placeholder_expr = env.arena.alloc(Loc {
        value: Record(Collection::empty()),
        region: outer_region,
    });

    desugar_dbg_expr(env, scope, placeholder_expr, outer_region)
}

/// Desugars a `dbg x` statement into essentially `Inspect.to_str x |> LowLevelDbg`
fn desugar_dbg_stmt<'a>(
    env: &mut Env<'a>,
    condition: &'a Loc<Expr<'a>>,
    continuation: &'a Loc<Expr<'a>>,
) -> &'a Expr<'a> {
    let region = condition.region;

    let inspect_fn = Var {
        module_name: ModuleName::INSPECT,
        ident: "to_str",
    };
    let loc_inspect_fn_var = env.arena.alloc(Loc {
        value: inspect_fn,
        region,
    });
    let inspect_args = &*env.arena.alloc([condition]);

    let dbg_str = env.arena.alloc(Loc {
        value: Apply(loc_inspect_fn_var, inspect_args, CalledVia::Space),
        region,
    });

    let line_col = env.line_info().convert_pos(region.start());

    let dbg_src = env
        .src
        .split_at(region.start().offset as usize)
        .1
        .split_at((region.end().offset - region.start().offset) as usize)
        .0;

    let module_path_str = env.module_path.to_string_lossy();

    // |> LowLevelDbg
    env.arena.alloc(LowLevelDbg(
        env.arena.alloc((
            &*env
                .arena
                .alloc_str(&format!("{}:{}", module_path_str, line_col.line + 1)),
            &*env.arena.alloc_str(dbg_src),
        )),
        dbg_str,
        continuation,
    ))
}
