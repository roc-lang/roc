#![allow(dead_code)]
use snafu::ensure;

use crate::markup_error::{CaretNotFound, MarkResult};

#[derive(Debug, Copy, Clone)]
pub struct Caret {
    pub offset_col: usize,
}

impl Caret {
    pub fn new_attr(offset_col: usize) -> Attribute {
        Attribute::Caret {
            caret: Caret { offset_col },
        }
    }
}
#[derive(Debug)]
pub struct SelectionStart {
    offset_col: usize,
}
#[derive(Debug)]
pub struct SelectionEnd {
    offset_col: usize,
}

// Highlight is used for example when searching for a specific string to highlight all search results in the module
#[derive(Debug)]
pub struct HighlightStart {
    offset_col: usize,
}
#[derive(Debug)]
pub struct HighlightEnd {
    offset_col: usize,
}

// Underline is used for warnings and errors
#[derive(Debug)]
pub struct UnderlineStart {
    offset_col: usize,
}
#[derive(Debug)]
pub struct UnderlineEnd {
    offset_col: usize,
}

#[derive(Debug)]
pub enum Attribute {
    // Rust does not yet support types for enum variants so we have to do it like this
    Caret { caret: Caret },

    SelectionStart { selection_start: SelectionStart },
    SelectionEnd { selection_end: SelectionEnd },

    HighlightStart { highlight_start: HighlightStart },
    HighlightEnd { highlight_end: HighlightEnd },

    UnderlineStart { underline_start: UnderlineStart },
    UnderlineEnd { underline_end: UnderlineEnd },
}

#[derive(Debug, Default)]
pub struct Attributes {
    pub all: Vec<Attribute>,
}

impl Attributes {
    pub fn add(&mut self, attr: Attribute) {
        self.all.push(attr);
    }

    pub fn add_caret(&mut self, offset_col: usize) {
        self.all.push(Attribute::Caret {
            caret: Caret { offset_col },
        });
    }

    pub fn get_mut_carets(&mut self) -> Vec<&mut Caret> {
        let mut carets = Vec::new();

        for attr in self.all.iter_mut() {
            if let Attribute::Caret { caret } = attr {
                carets.push(caret)
            }
        }

        carets
    }

    pub fn get_carets(&self) -> Vec<Caret> {
        let mut carets = Vec::new();

        for attr in self.all.iter() {
            if let Attribute::Caret { caret } = attr {
                carets.push(*caret)
            }
        }

        carets
    }

    pub fn delete_caret(&mut self, offset_col: usize, node_id: usize) -> MarkResult<()> {
        let old_len = self.all.len();

        self.all.retain(|attr| {
            if let Attribute::Caret { caret } = attr {
                caret.offset_col != offset_col
            } else {
                true
            }
        });

        let new_len = self.all.len();

        ensure!(old_len != new_len, CaretNotFound { node_id });

        Ok(())
    }
}
