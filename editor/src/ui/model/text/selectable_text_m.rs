
use crate::ui::error::UIResult;
use super::txt_pos::TxtPos;
use std::fmt;

impl std::fmt::Display for RawSelection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "RawSelection: start_pos: line:{} col:{}, end_pos: line:{} col:{}",
            self.start_pos.line, self.start_pos.column, self.end_pos.line, self.end_pos.column
        )
    }
}

trait SelectableLines {
    fn get_line(&self, line_nr: usize) -> UIResult<&str>;

    fn line_len(&self, line_nr: usize) -> UIResult<usize>;

    fn nr_of_lines(&self) -> usize;
    
    fn nr_of_chars(&self) -> usize;

    // TODO use pool allocation here
    fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a>;

    fn last_TxtPos(&self) -> TxtPos;

    fn get_selection(&self, raw_sel: RawSelection) -> UIResult<&str>;
}

trait MutSelectableLines {
    fn insert_char(&mut self, caret_pos: TxtPos, new_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, caret_pos: TxtPos, new_str: &str) -> UIResult<()>;

    fn pop_char(&mut self, caret_pos: TxtPos);

    fn del_selection(&mut self, raw_sel: RawSelection) -> UIResult<()>;
}

#[derive(Debug)]
pub struct SelectableText {
    pub lines: SelectableLines,
    pub caret_pos: TxtPos,
    pub selection_opt: Option<RawSelection>,
}