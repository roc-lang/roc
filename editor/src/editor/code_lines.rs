use crate::ui::text::lines::Lines;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::{LineInsertionFailed, OutOfBounds, UIResult};
use crate::ui::util::slice_get;
use crate::ui::util::slice_get_mut;
use std::cmp::Ordering;
use std::fmt;

#[derive(Debug, Default)]
pub struct CodeLines {
    pub lines: Vec<String>,
    pub nr_of_chars: usize,
}

impl CodeLines {
    pub fn insert_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        new_str: &str,
    ) -> UIResult<()> {
        let nr_of_lines = self.lines.len();

        if line_nr < nr_of_lines {
            let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

            line_ref.insert_str(index, new_str);
        } else if line_nr >= self.lines.len() {
            for _ in 0..((line_nr - nr_of_lines) + 1) {
                self.push_empty_line();
            }

            self.insert_between_line(line_nr, index, new_str)?;
        } else {
            LineInsertionFailed {
                line_nr,
                nr_of_lines,
            }
            .fail()?;
        }

        self.nr_of_chars += new_str.len();

        Ok(())
    }

    pub fn insert_empty_line(&mut self, line_nr: usize) -> UIResult<()> {
        if line_nr <= self.lines.len() {
            self.lines.insert(line_nr, String::new());

            Ok(())
        } else {
            OutOfBounds {
                index: line_nr,
                collection_name: "code_lines.lines".to_owned(),
                len: self.lines.len(),
            }
            .fail()
        }
    }

    pub fn push_empty_line(&mut self) {
        self.lines.push(String::new())
    }

    pub fn break_line(&mut self, line_nr: usize, col_nr: usize) -> UIResult<()> {
        // clippy prefers this over if-else
        match line_nr.cmp(&self.lines.len()) {
            Ordering::Less => {
                self.insert_empty_line(line_nr + 1)?;

                let line_ref = self.lines.get_mut(line_nr).unwrap(); // safe because we checked line_nr

                if col_nr < line_ref.len() {
                    let next_line_str: String = line_ref.drain(col_nr..).collect();

                    let next_line_ref = self.lines.get_mut(line_nr + 1).unwrap(); // safe because we just added the line

                    *next_line_ref = next_line_str;
                }

                Ok(())
            }
            Ordering::Equal => self.insert_empty_line(line_nr + 1),
            Ordering::Greater => OutOfBounds {
                index: line_nr,
                collection_name: "code_lines.lines".to_owned(),
                len: self.lines.len(),
            }
            .fail(),
        }
    }

    pub fn clear_line(&mut self, line_nr: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        *line_ref = String::new();

        Ok(())
    }

    pub fn del_line(&mut self, line_nr: usize) -> UIResult<()> {
        let line_len = self.line_len(line_nr)?;

        self.lines.remove(line_nr);

        self.nr_of_chars -= line_len;

        Ok(())
    }

    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.remove(index);

        self.nr_of_chars -= 1;

        Ok(())
    }

    pub fn del_range_at_line(
        &mut self,
        line_nr: usize,
        col_range: std::ops::Range<usize>,
    ) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.drain(col_range);

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

    // last column of last line
    pub fn end_txt_pos(&self) -> TextPos {
        let last_line_nr = self.nr_of_lines() - 1;

        TextPos {
            line: last_line_nr,
            column: self.line_len(last_line_nr).unwrap(), // safe because we just calculated last_line
        }
    }
}

impl Lines for CodeLines {
    fn get_line_ref(&self, line_nr: usize) -> UIResult<&str> {
        let line_string = slice_get(line_nr, &self.lines)?;

        Ok(line_string)
    }

    fn line_len(&self, line_nr: usize) -> UIResult<usize> {
        self.get_line_ref(line_nr).map(|line| line.len())
    }

    fn nr_of_lines(&self) -> usize {
        self.lines.len()
    }

    fn nr_of_chars(&self) -> usize {
        self.nr_of_chars
    }

    fn all_lines_as_string(&self) -> String {
        self.lines.join("\n")
    }

    fn is_last_line(&self, line_nr: usize) -> bool {
        line_nr == self.nr_of_lines() - 1
    }

    fn last_char(&self, line_nr: usize) -> UIResult<Option<char>> {
        Ok(self.get_line_ref(line_nr)?.chars().last())
    }
}

impl fmt::Display for CodeLines {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.lines {
            let row_str = row
                .chars()
                .map(|code_char| format!("{}", code_char))
                .collect::<Vec<String>>()
                .join(" ");

            let escaped_row_str = row_str.replace("\n", "\\n");

            write!(f, "\n{}", escaped_row_str)?;
        }

        writeln!(f, "      (code_lines, {:?} lines)", self.lines.len())?;

        Ok(())
    }
}
