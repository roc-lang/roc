use std::path::PathBuf;

use bumpalo::Bump;
use roc_module::symbol::ModuleId;
use roc_parse::{
    ast::{Collection, Defs, Pattern},
    header::HeaderType,
};
use roc_problem::can::Problem;
use roc_region::all::{LineInfo, Loc, Region};

use crate::{
    desugar::{desugar_defs_node_values, desugar_record_destructures},
    env::SoloEnv,
    scope::SoloScope,
};

#[derive(Debug)]
pub struct SoloCanOutput<'a> {
    pub scope: SoloScope,
    pub loc_defs: Defs<'a>,
    pub solo_home: ModuleId,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    pub src: &'a str,

    /// Lazily calculated line info. This data is only needed if the code contains calls to `dbg`,
    /// otherwise we can leave it as `None` and never pay the cost of scanning the source an extra
    /// time.
    pub lazy_line_info: &'a mut Option<LineInfo>,

    pub module_params: Option<(Region, Collection<'a, Loc<Pattern<'a>>>)>,
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
pub fn solo_canonicalize_module_defs<'a>(
    arena: &'a Bump,
    header_type: HeaderType<'a>,
    loc_defs: &'a mut Defs<'a>,
    module_path: PathBuf,
    src: &'a str,
) -> SoloCanOutput<'a> {
    let mut scope = SoloScope::new();
    let mut env = SoloEnv::new(arena, src, arena.alloc(module_path));

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.

    desugar_defs_node_values(&mut env, &mut scope, loc_defs);

    let module_params = header_type.get_params().as_ref().map(
        |roc_parse::header::ModuleParams {
             pattern,
             before_arrow: _,
             after_arrow: _,
         }| {
            let desugared_patterns =
                desugar_record_destructures(&mut env, &mut scope, pattern.value);

            (pattern.region, desugared_patterns)
        },
    );

    SoloCanOutput {
        scope,
        solo_home: env.solo_home,
        loc_defs: loc_defs.clone(),
        problems: env.problems,
        src: env.src,
        lazy_line_info: env.lazy_line_info,
        module_params,
    }
}
