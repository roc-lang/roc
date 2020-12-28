#[derive(Debug)]
pub struct Model {
    pub file_text: String,
    pub caret_pos: Position,
    pub selection_opt: Option<RawSelection>
}

pub fn init_model() -> Model {
    Model {
        file_text:
            String::new(),
        caret_pos:
            Position {
                line: 0, column: 0
            },
        selection_opt:
            None
    }
}

//Is model.rs the right place for these structs?
#[derive(Debug, Copy, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize
}

#[derive(Debug, Copy, Clone)]
pub struct RawSelection {
    pub start_pos: Position,
    pub end_pos: Position
}