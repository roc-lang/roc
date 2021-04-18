use crate::ui::text::lines::Lines;
use crate::ui::text::selection::Selection;
use crate::ui::ui_error::UIResult;
use crate::ui::util::slice_get;
use crate::ui::util::slice_get_mut;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use std::fmt;

#[derive(Debug)]
pub struct CodeLines {
    pub lines: Vec<String>,
    pub nr_of_chars: usize,
}

impl CodeLines {
    pub fn from_str(code_str: &str) -> CodeLines {
        CodeLines {
            lines: code_str
                .split_inclusive('\n')
                .map(|s| s.to_owned())
                .collect(),
            nr_of_chars: code_str.len(),
        }
    }

    pub fn insert_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        new_str: &str,
    ) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.insert_str(index, new_str);

        self.nr_of_chars += new_str.len();

        Ok(())
    }

    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.remove(index);

        self.nr_of_chars -= 1;

        Ok(())
    }

    pub fn del_selection(&mut self, selection: Selection) -> UIResult<()> {
        if selection.is_on_same_line() {
            let line_ref = slice_get_mut(selection.start_pos.line, &mut self.lines)?;

            line_ref.drain(selection.start_pos.column..selection.end_pos.column);
        } else {
            // TODO support multiline selections
        }

        Ok(())
    }
}

impl Lines for CodeLines {
    fn get_line(&self, line_nr: usize) -> UIResult<&str> {
        let line_string = slice_get(line_nr, &self.lines)?;

        Ok(&line_string)
    }

    fn line_len(&self, line_nr: usize) -> UIResult<usize> {
        self.get_line(line_nr).map(|line| line.len())
    }

    fn nr_of_lines(&self) -> usize {
        self.lines.len()
    }

    fn nr_of_chars(&self) -> usize {
        self.nr_of_chars
    }

    fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a> {
        let mut lines = BumpString::with_capacity_in(self.nr_of_chars(), arena);

        for line in &self.lines {
            lines.push_str(&line);
        }

        lines
    }

    fn is_last_line(&self, line_nr: usize) -> bool {
        line_nr == self.nr_of_lines() - 1
    }

    fn last_char(&self, line_nr: usize) -> UIResult<Option<char>> {
        Ok(self.get_line(line_nr)?.chars().last())
    }
}

impl fmt::Display for CodeLines {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.lines {
            let row_str = row
                .chars()
                .map(|code_char| format!("'{}'", code_char))
                .collect::<Vec<String>>()
                .join(", ");

            write!(f, "\n{}", row_str)?;
        }

        write!(f, "      (code_lines)")?;

        Ok(())
    }
}
