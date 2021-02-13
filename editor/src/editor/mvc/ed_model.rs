use crate::error::EdResult;
use crate::graphics::primitives::rect::Rect;
use crate::text_buffer;
use crate::text_buffer::TextBuffer;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel {
    pub text_buf: TextBuffer,
    pub caret_pos: Position,
    pub selection_opt: Option<RawSelection>,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
}

pub fn init_model(file_path: &Path) -> EdResult<EdModel> {
    Ok(EdModel {
        text_buf: text_buffer::from_path(file_path)?,
        caret_pos: Position { line: 0, column: 0 },
        selection_opt: None,
        glyph_dim_rect_opt: None,
        has_focus: true,
    })
}

impl EdModel {
    pub fn get_selected_str(&self) -> EdResult<Option<&str>> {
        if let Some(curr_selection) = self.selection_opt {
            let selected_str = self.text_buf.get_selection(curr_selection)?;

            Ok(Some(selected_str))
        } else {
            Ok(None)
        }
    }

    pub fn del_selection(&mut self) -> EdResult<()> {
        if let Some(selection) = self.selection_opt {
            self.text_buf.del_selection(selection)?;

            self.caret_pos = selection.start_pos;
        }
        self.selection_opt = None;

        Ok(())
    }
}
