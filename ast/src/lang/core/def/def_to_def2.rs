use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;
use roc_parse::pattern::PatternType;
use roc_region::all::Region;

use crate::lang::{core::pattern::to_pattern2, env::Env, scope::Scope};

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
        .map(|loc| to_def2_from_def(arena, env, scope, &loc.value, region))
        .collect()
}

pub fn to_def2_from_def<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    scope: &mut Scope,
    parsed_def: &'a roc_parse::ast::Def<'a>,
    region: Region,
) -> Def2 {
    use roc_parse::ast::Def::*;

    match parsed_def {
        SpaceBefore(inner_def, _) => to_def2_from_def(arena, env, scope, inner_def, region),
        SpaceAfter(inner_def, _) => to_def2_from_def(arena, env, scope, inner_def, region),
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