
use crate::ui::ui_error::UIResult;
use super::text_pos::TextPos;
use super::selection::{ValidSelection};
use winit::event::{ModifiersState};
use super::selection::validate_selection;


#[derive(Debug)]
pub struct CaretWSelect {
    pub caret_pos: TextPos,
    pub selection_opt: Option<ValidSelection>,
}


fn mk_some_sel(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<ValidSelection>> {
    Ok(
        Some(
            validate_selection(start_pos, end_pos)?
        )
    )
}

impl Default for CaretWSelect {
    fn default() -> Self {
        Self {
            caret_pos: TextPos {
                line: 0,
                column: 0
            },
            selection_opt: None
        }
    }
}

impl CaretWSelect {

    pub fn move_caret_w_mods(&mut self, new_pos: TextPos, mods: &ModifiersState) -> UIResult<()> {
        let caret_pos = self.caret_pos;
    
        // one does not simply move the caret
        let valid_sel_opt = 
            if new_pos != caret_pos {
                if mods.shift() {
                    if let Some(old_sel) = self.selection_opt {
                        if new_pos < old_sel.start_pos {
                            if caret_pos > old_sel.start_pos {
                                mk_some_sel(
                                    new_pos,
                                    old_sel.start_pos
                                )?
                            } else {
                                mk_some_sel(
                                    new_pos,
                                    old_sel.end_pos
                                )?
                            }
                        } else if new_pos > old_sel.end_pos {
                            if caret_pos < old_sel.end_pos {
                                mk_some_sel(
                                    old_sel.end_pos,
                                    new_pos
                                )?
                            } else {
                                mk_some_sel(
                                    old_sel.start_pos,
                                    new_pos
                                )?
                            }
                        } else if new_pos > caret_pos {
                            mk_some_sel(
                                new_pos,
                                old_sel.end_pos
                            )?
                        } else if new_pos < caret_pos {
                            mk_some_sel(
                                old_sel.start_pos,
                                new_pos
                            )?
                        } else {
                            // TODO should this return none?
                            None
                        }
                    } else if new_pos < self.caret_pos {
                            mk_some_sel(
                                new_pos,
                                caret_pos
                            )?
                    } else {
                        mk_some_sel(
                            caret_pos,
                            new_pos
                        )?
                    }
                } else {
                    None
                }
            } else {
                self.selection_opt
            };
    
        self.caret_pos = new_pos;
        self.selection_opt = valid_sel_opt;

        Ok(())
    }

}
