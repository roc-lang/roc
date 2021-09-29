use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use roc_parse::{parser::SyntaxError, pattern::PatternType};
use roc_region::all::Region;

use crate::lang::{
    core::{expr::expr_to_expr2::loc_expr_to_expr2, pattern::to_pattern2},
    env::Env,
    scope::Scope,
};

use super::def2::Def2;

pub fn defs_to_defs2<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    scope: &mut Scope,
    parsed_defs: &'a BumpVec<roc_region::all::Loc<roc_parse::ast::Def<'a>>>,
    region: Region,
) -> Vec<Def2> {
    parsed_defs
        .iter()
        .map(|loc| def_to_def2(arena, env, scope, &loc.value, region))
        .collect()
}

pub fn def_to_def2<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    scope: &mut Scope,
    parsed_def: &'a roc_parse::ast::Def<'a>,
    region: Region,
) -> Def2 {
    use roc_parse::ast::Def::*;

    match parsed_def {
        SpaceBefore(inner_def, _) => def_to_def2(arena, env, scope, inner_def, region),
        SpaceAfter(inner_def, _) => def_to_def2(arena, env, scope, inner_def, region),
        Body(&loc_pattern, &loc_expr) => {
            // TODO loc_pattern use identifier
            let expr2 = loc_expr_to_expr2(arena, loc_expr, env, scope, region).0;
            let expr_id = env.pool.add(expr2);

            use roc_parse::ast::Pattern::*;

            match loc_pattern.value {
                Identifier(_) => {
                    let (_, pattern2) = to_pattern2(
                        env,
                        scope,
                        PatternType::TopLevelDef,
                        &loc_pattern.value,
                        region,
                    );
                    let pattern_id = env.pool.add(pattern2);

                    // TODO support with annotation
                    Def2::ValueDef {
                        identifier_id: pattern_id,
                        expr_id,
                    }
                }
                other => {
                    unimplemented!(
                        "I don't yet know how to convert the pattern {:?} into an expr2",
                        other
                    )
                }
            }
        }
        other => {
            unimplemented!(
                "I don't know how to make an expr2 from this def yet: {:?}",
                other
            )
        }
    }
}

pub fn str_to_def2<'a>(
    arena: &'a Bump,
    input: &'a str,
    env: &mut Env<'a>,
    scope: &mut Scope,
    region: Region,
) -> Result<Vec<Def2>, SyntaxError<'a>> {
    match roc_parse::test_helpers::parse_defs_with(arena, input.trim()) {
        Ok(vec_loc_def) => Ok(defs_to_defs2(
            arena,
            env,
            scope,
            arena.alloc(vec_loc_def),
            region,
        )),
        Err(fail) => Err(fail),
    }
}
