use std::cell::RefCell;

use roc_module::symbol::Interns;
use roc_mono::{
    ir::ProcLayout,
    layout::{CapturesNiche, LayoutCache},
};
use roc_parse::ast::Expr;
use roc_repl_eval::eval::{jit_to_ast, ToAstProblem};
use roc_target::TargetInfo;
use roc_types::subs::{Subs, Variable};

mod app;
pub mod run;

use app::{ExpectMemory, ExpectReplApp};

#[allow(clippy::too_many_arguments)]
pub fn get_values<'a>(
    target_info: TargetInfo,
    arena: &'a bumpalo::Bump,
    subs: &Subs,
    interns: &'a Interns,
    start: *const u8,
    start_offset: usize,
    variables: &[Variable],
) -> Result<(usize, Vec<Expr<'a>>), ToAstProblem> {
    let mut result = Vec::with_capacity(variables.len());

    let memory = ExpectMemory {
        start,
        bytes_read: RefCell::new(0),
    };

    let app = ExpectReplApp {
        memory: arena.alloc(memory),
        offset: start_offset,
    };

    let app = arena.alloc(app);

    for variable in variables {
        let expr = {
            let variable = *variable;

            let content = subs.get_content_without_compacting(variable);

            let mut layout_cache = LayoutCache::new(target_info);
            let layout = layout_cache.from_var(arena, variable, subs).unwrap();

            let proc_layout = ProcLayout {
                arguments: &[],
                result: layout,
                captures_niche: CapturesNiche::no_niche(),
            };

            let element = jit_to_ast(
                arena,
                app,
                "expect_repl_main_fn",
                proc_layout,
                content,
                subs,
                interns,
                target_info,
            )?;

            element
        };

        result.push(expr);
    }

    Ok((app.offset, result))
}
