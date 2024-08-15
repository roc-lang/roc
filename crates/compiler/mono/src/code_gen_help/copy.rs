use roc_module::symbol::{IdentIds, Symbol};

use crate::ir::{Expr, Stmt};
use crate::layout::{InLayout, Layout, STLayoutInterner};

use super::{CodeGenHelp, Context};

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

pub fn copy_indirect<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    _ctx: &mut Context<'a>,
    _layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> Stmt<'a> {
    let arena = root.arena;
    let unit = root.create_symbol(ident_ids, "unit");
    let loaded = root.create_symbol(ident_ids, "loaded");
    Stmt::Let(
        loaded,
        Expr::ptr_load(arena.alloc(ARG_2)),
        layout,
        arena.alloc(
            //
            Stmt::Let(
                unit,
                Expr::ptr_store(arena.alloc([ARG_1, loaded])),
                Layout::UNIT,
                arena.alloc(
                    //
                    Stmt::Ret(unit),
                ),
            ),
        ),
    )
}
