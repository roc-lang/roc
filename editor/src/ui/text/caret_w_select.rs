#![allow(dead_code)]

use super::selection::validate_selection;
use super::selection::Selection;
use super::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::window::keyboard_input::Modifiers;

#[derive(Debug, Copy, Clone)]
pub struct CaretWSelect {
    pub caret_pos: TextPos,
    pub selection_opt: Option<Selection>,
}

pub enum CaretPos {
    Start,
    Exact(TextPos),
    End,
}

fn mk_some_sel(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<Selection>> {
    if start_pos == end_pos {
        Ok(None)
    } else {
        Ok(Some(validate_selection(start_pos, end_pos)?))
    }
}

impl Default for CaretWSelect {
    fn default() -> Self {
        Self {
            caret_pos: TextPos { line: 0, column: 0 },
            selection_opt: None,
        }
    }
}

impl CaretWSelect {
    pub fn new(caret_pos: TextPos, selection_opt: Option<Selection>) -> Self {
        Self {
            caret_pos,
            selection_opt,
        }
    }

    pub fn move_caret_w_mods(&self, new_pos: TextPos, mods: &Modifiers) -> UIResult<CaretWSelect> {
        let old_caret_pos = self.caret_pos;

        // one does not simply move the caret
        let valid_sel_opt = if mods.shift {
            if new_pos != old_caret_pos {
                if let Some(old_sel) = self.selection_opt {
                    if new_pos < old_sel.start_pos {
                        if old_caret_pos > old_sel.start_pos {
                            mk_some_sel(new_pos, old_sel.start_pos)?
                        } else {
                            mk_some_sel(new_pos, old_sel.end_pos)?
                        }
                    } else if new_pos > old_sel.end_pos {
                        if old_caret_pos < old_sel.end_pos {
                            mk_some_sel(old_sel.end_pos, new_pos)?
                        } else {
                            mk_some_sel(old_sel.start_pos, new_pos)?
                        }
                    } else if new_pos > old_caret_pos {
                        mk_some_sel(new_pos, old_sel.end_pos)?
                    } else if new_pos < old_caret_pos {
                        mk_some_sel(old_sel.start_pos, new_pos)?
                    } else {
                        None
                    }
                } else if new_pos < self.caret_pos {
                    mk_some_sel(new_pos, old_caret_pos)?
                } else {
                    mk_some_sel(old_caret_pos, new_pos)?
                }
            } else {
                self.selection_opt
            }
        } else {
            None
        };

        Ok(CaretWSelect::new(new_pos, valid_sel_opt))
    }
}

// VIEW
// ----
use crate::graphics::primitives::rect::Rect;
use crate::ui::theme::UITheme;

pub fn make_caret_rect_from_pos(
    caret_pos: TextPos,
    glyph_dim_rect: &Rect,
    ui_theme: &UITheme,
) -> Rect {
    let caret_x =
        glyph_dim_rect.top_left_coords.x + glyph_dim_rect.width * (caret_pos.column as f32);

    let caret_y =
        glyph_dim_rect.top_left_coords.y + (caret_pos.line as f32) * glyph_dim_rect.height;

    make_caret_rect(caret_x, caret_y, glyph_dim_rect, ui_theme)
}

pub fn make_caret_rect(
    caret_x: f32,
    caret_y: f32,
    glyph_dim_rect: &Rect,
    ui_theme: &UITheme,
) -> Rect {
    Rect {
        top_left_coords: (caret_x, caret_y).into(),
        height: glyph_dim_rect.height,
        width: glyph_dim_rect.width / 6.0,
        color: ui_theme.caret,
    }
}

pub fn make_selection_rect(
    sel_rect_x: f32,
    sel_rect_y: f32,
    width: f32,
    glyph_dim_rect: &Rect,
    ui_theme: &UITheme,
) -> Rect {
    Rect {
        top_left_coords: (sel_rect_x, sel_rect_y).into(),
        height: glyph_dim_rect.height,
        width,
        color: ui_theme.select_highlight,
    }
}

#[cfg(test)]
pub mod test_caret_w_select {
    use crate::ui::text::caret_w_select::CaretWSelect;
    use crate::ui::text::selection::validate_selection;
    use crate::ui::text::text_pos::TextPos;
    use crate::ui::ui_error::OutOfBounds;
    use crate::ui::ui_error::UIResult;
    use crate::ui::util::slice_get;
    use core::cmp::Ordering;
    use pest::Parser;
    use snafu::OptionExt;
    use std::{collections::HashMap, slice::SliceIndex};

    #[derive(Parser)]
    #[grammar = "../tests/selection.pest"]
    pub struct LineParser;

    // Retrieve selection and position from formatted string
    pub fn convert_dsl_to_selection(lines: &[String]) -> Result<CaretWSelect, String> {
        let lines_str: String = lines.join("\n");

        let parsed = LineParser::parse(Rule::linesWithSelect, &lines_str)
            .expect("Selection test DSL parsing failed");

        let mut caret_opt: Option<(usize, usize)> = None;
        let mut sel_start_opt: Option<(usize, usize)> = None;
        let mut sel_end_opt: Option<(usize, usize)> = None;
        let mut line_nr = 0;
        let mut col_nr = 0;

        for line in parsed {
            for elt in line.into_inner() {
                match elt.as_rule() {
                    Rule::optCaret => {
                        if elt.as_span().as_str() == "┃" {
                            if caret_opt.is_some() {
                                return Err(
                                    "Multiple carets found, there should be only one".to_owned()
                                );
                            } else {
                                caret_opt = Some((line_nr, col_nr));
                            }
                        }
                    }
                    Rule::optSelStart => {
                        if sel_start_opt.is_some() {
                            if elt.as_span().as_str() == "❮" {
                                return Err("Found start of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "❮" {
                            sel_start_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::optSelEnd => {
                        if sel_end_opt.is_some() {
                            if elt.as_span().as_str() == "❯" {
                                return Err("Found end of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "❯" {
                            sel_end_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::text => {
                        let split_str = elt
                            .as_span()
                            .as_str()
                            .split('\n')
                            .into_iter()
                            .collect::<Vec<&str>>();

                        if split_str.len() > 1 {
                            line_nr += split_str.len() - 1;
                            col_nr = 0
                        }
                        if let Some(last_str) = split_str.last() {
                            col_nr += last_str.len()
                        }
                    }
                    _ => {}
                }
            }
        }

        // Make sure return makes sense
        if let Some((line, column)) = caret_opt {
            let caret_pos = TextPos { line, column };
            if sel_start_opt.is_none() && sel_end_opt.is_none() {
                Ok(CaretWSelect::new(caret_pos, None))
            } else if let Some((start_line, start_column)) = sel_start_opt {
                if let Some((end_line, end_column)) = sel_end_opt {
                    Ok(CaretWSelect::new(
                        caret_pos,
                        Some(
                            validate_selection(
                                TextPos {
                                    line: start_line,
                                    column: start_column,
                                },
                                TextPos {
                                    line: end_line,
                                    column: end_column,
                                },
                            )
                            .unwrap(),
                        ),
                    ))
                } else {
                    Err("Selection end '❯' was not found, but selection start '❮' was. Bad input string.".to_owned())
                }
            } else {
                Err("Selection start '❮' was not found, but selection end '❯' was. Bad input string.".to_owned())
            }
        } else {
            Err("No caret was found in lines.".to_owned())
        }
    }

    // show selection and caret position as symbols in lines for easy testing
    pub fn convert_selection_to_dsl(
        caret_w_select: CaretWSelect,
        lines: Vec<String>,
    ) -> UIResult<Vec<String>> {
        let selection_opt = caret_w_select.selection_opt;
        let caret_pos = caret_w_select.caret_pos;
        let mut mut_lines = lines;

        if let Some(sel) = selection_opt {
            let mut to_insert = vec![(sel.start_pos, '❮'), (sel.end_pos, '❯'), (caret_pos, '┃')];
            let symbol_map: HashMap<char, usize> =
                [('❮', 2), ('❯', 0), ('┃', 1)].iter().cloned().collect();

            // sort for nice printing
            to_insert.sort_by(|a, b| {
                let pos_cmp = a.0.cmp(&b.0);
                if pos_cmp == Ordering::Equal {
                    symbol_map.get(&a.1).cmp(&symbol_map.get(&b.1))
                } else {
                    pos_cmp
                }
            });

            // insert symbols into text lines
            for i in 0..to_insert.len() {
                let (pos, insert_char) = *slice_get(i, &to_insert)?;

                insert_at_pos(&mut mut_lines, pos, insert_char)?;

                // shift position of following symbols now that symbol is inserted
                for j in i..to_insert.len() {
                    let (old_pos, _) = get_mut_res(j, &mut to_insert)?;

                    if old_pos.line == pos.line {
                        old_pos.column += 1;
                    }
                }
            }
        } else {
            insert_at_pos(&mut mut_lines, caret_pos, '┃')?;
        }

        Ok(mut_lines)
    }

    fn insert_at_pos(lines: &mut [String], pos: TextPos, insert_char: char) -> UIResult<()> {
        let line = get_mut_res(pos.line, lines)?;

        let mut chars: Vec<char> = line.chars().collect();
        chars.insert(pos.column, insert_char);

        *line = chars.into_iter().collect::<String>();

        Ok(())
    }

    // It's much nicer to have get_mut return a Result with clear error than an Option
    fn get_mut_res<T>(
        index: usize,
        vec: &mut [T],
    ) -> UIResult<&mut <usize as SliceIndex<[T]>>::Output> {
        let vec_len = vec.len();

        let elt_ref = vec.get_mut(index).context(OutOfBounds {
            index,
            collection_name: "Slice",
            len: vec_len,
        })?;

        Ok(elt_ref)
    }
}
