
pub struct Model {
    caret_pos: Position,
    selection: RawSelection
}

//Is model.rs the right place for these structs?
pub struct Position {
    pub line: usize,
    pub column: usize
}

pub struct RawSelection {
    pub start_pos: Position,
    pub end_pos: Position
}