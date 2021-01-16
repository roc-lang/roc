
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use roc_editor::mvc::update::handle_new_char;
use roc_editor::mvc::ed_model::{EdModel, Position, RawSelection};
use roc_editor::mvc::app_model::AppModel;
use roc_editor::text_buffer::TextBuffer;
use ropey::Rope;

// duplicate inside mvc::update
fn mock_app_model(
    text_buf: TextBuffer,
    caret_pos: Position,
    selection_opt: Option<RawSelection>,
) -> AppModel {
    AppModel {
        ed_model_opt: Some(EdModel {
            text_buf,
            caret_pos,
            selection_opt,
            glyph_dim_rect_opt: None,
            has_focus: true,
        }),
    }
}

fn text_buffer_from_str(lines_str: &str) -> TextBuffer {
    TextBuffer {
        text_rope: Rope::from_str(lines_str),
        path_str: "".to_owned(),
    }
}

pub fn char_insert_benchmark(c: &mut Criterion) {
    let text_buf = text_buffer_from_str("");
    
    let caret_pos = Position {
        line: 0,
        column: 0
    };
    
    let selection_opt: Option<RawSelection> = None;
    let mut app_model = mock_app_model(text_buf, caret_pos, selection_opt);
    c.bench_function("single char insert, small buffer", |b| b.iter(|| handle_new_char(&mut app_model, &'a')));
}

pub fn file_open_benchmark(c: &mut Criterion) {
    ed_model::init_model(path)
    //TODO continue here
}

criterion_group!(benches, char_insert_benchmark);
criterion_main!(benches);
