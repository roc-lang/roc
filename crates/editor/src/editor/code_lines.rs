use crate::ui::text::lines::Lines;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::ui::util::slice_get;
use std::fmt;

#[derive(Debug, Default)]
pub struct CodeLines {
    pub lines: Vec<String>,
    pub nr_of_chars: usize,
}

impl CodeLines {
    pub fn from_str(code_str: &str) -> CodeLines {
        CodeLines {
            lines: code_str.split('\n').map(|s| s.to_owned()).collect(),
            nr_of_chars: code_str.len(),
        }
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
                .map(|code_char| format!("{code_char}"))
                .collect::<Vec<String>>()
                .join(" ");

            let escaped_row_str = row_str.replace('\n', "\\n");

            write!(f, "\n{escaped_row_str}")?;
        }

        writeln!(f, "      (code_lines, {:?} lines)", self.lines.len())?;

        Ok(())
    }
}
