use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use roc_editor::mvc::app_model::AppModel;
use roc_editor::mvc::ed_model::{EdModel, Position, RawSelection};
use roc_editor::mvc::update::handle_new_char;
use roc_editor::text_buffer;
use roc_editor::text_buffer::TextBuffer;
use ropey::Rope;
use std::cmp::min;
use std::path::Path;

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

pub fn char_insert_bench(c: &mut Criterion) {
    let text_buf = text_buffer_from_str("");

    let caret_pos = Position { line: 0, column: 0 };

    let selection_opt: Option<RawSelection> = None;
    let mut app_model = mock_app_model(text_buf, caret_pos, selection_opt);
    c.bench_function("single char insert, small buffer", |b| {
        b.iter(|| handle_new_char(&mut app_model, &'a'))
    });
}

pub fn char_pop_bench(c: &mut Criterion) {
    let nr_lines = 50000;
    let mut text_buf = buf_from_dummy_file(nr_lines);

    let mut rand_gen_pos = StdRng::seed_from_u64(44);

    c.bench_function(
        &format!("single char pop, {} lines", text_buf.nr_of_lines()),
        |b| {
            b.iter(|| {
                let max_line_nr = text_buf.nr_of_lines();
                let rand_line_nr = rand_gen_pos.gen_range(0..max_line_nr);
                let max_col = text_buf
                    .line_len(rand_line_nr)
                    .expect("Failed to retrieve line length.");

                let caret_pos = Position {
                    line: rand_line_nr,
                    column: rand_gen_pos.gen_range(0..max_col),
                };

                text_buf.pop_char(caret_pos);
            })
        },
    );
}

fn get_all_lines_helper(nr_lines: usize, c: &mut Criterion) {
    let text_buf = buf_from_dummy_file(nr_lines);

    let arena = Bump::new();

    c.bench_function(
        &format!("get all {:?} lines from textbuffer", nr_lines),
        |b| b.iter(|| text_buf.all_lines(&arena)),
    );
}

fn get_all_lines_bench(c: &mut Criterion) {
    get_all_lines_helper(10000, c)
}

fn get_line_len_helper(nr_lines: usize, c: &mut Criterion) {
    let text_buf = buf_from_dummy_file(nr_lines);

    let mut rand_gen = StdRng::seed_from_u64(45);

    c.bench_function(
        &format!("get random line len from {:?}-line textbuffer", nr_lines),
        |b| b.iter(|| text_buf.line_len(rand_gen.gen_range(0..nr_lines)).unwrap()),
    );
}

fn get_line_len_bench(c: &mut Criterion) {
    get_line_len_helper(10000, c)
}

fn get_line_helper(nr_lines: usize, c: &mut Criterion) {
    let text_buf = buf_from_dummy_file(nr_lines);

    let mut rand_gen = StdRng::seed_from_u64(46);

    c.bench_function(
        &format!("get random line from {:?}-line textbuffer", nr_lines),
        |b| b.iter(|| text_buf.line(rand_gen.gen_range(0..nr_lines)).unwrap()),
    );
}

fn get_line_bench(c: &mut Criterion) {
    get_line_helper(10000, c)
}

pub fn del_select_bench(c: &mut Criterion) {
    let nr_lines = 25000000;
    let mut text_buf = buf_from_dummy_file(nr_lines);

    let mut rand_gen = StdRng::seed_from_u64(47);

    c.bench_function(
        &format!(
            "delete rand selection, {}-line file",
            text_buf.nr_of_lines()
        ),
        |b| {
            b.iter(|| {
                let rand_sel = gen_rand_selection(&mut rand_gen, &text_buf);

                text_buf.del_selection(rand_sel).unwrap();
            })
        },
    );
}

fn gen_rand_selection(rand_gen: &mut StdRng, text_buf: &TextBuffer) -> RawSelection {
    let max_line_nr = text_buf.nr_of_lines();
    let rand_line_nr_a = rand_gen.gen_range(0..max_line_nr - 3);
    let max_col_a = text_buf.line_len(rand_line_nr_a).expect(&format!(
        "Failed to retrieve line length. For line {}, with {} lines in buffer",
        rand_line_nr_a,
        text_buf.nr_of_lines()
    ));
    let rand_col_a = if max_col_a > 0 {
        rand_gen.gen_range(0..max_col_a)
    } else {
        0
    };

    let max_sel_end = min(rand_line_nr_a + 5, max_line_nr);
    let rand_line_nr_b = rand_gen.gen_range((rand_line_nr_a + 1)..max_sel_end);
    let max_col_b = text_buf.line_len(rand_line_nr_b).expect(&format!(
        "Failed to retrieve line length. For line {}, with {} lines in buffer",
        rand_line_nr_b,
        text_buf.nr_of_lines()
    ));
    let rand_col_b = if max_col_b > 0 {
        rand_gen.gen_range(0..max_col_b)
    } else {
        0
    };

    RawSelection {
        start_pos: Position {
            line: rand_line_nr_a,
            column: rand_col_a,
        },
        end_pos: Position {
            line: rand_line_nr_b,
            column: rand_col_b,
        },
    }
}

fn buf_from_dummy_file(nr_lines: usize) -> TextBuffer {
    let path_str = format!("benches/resources/{}_lines.roc", nr_lines);

    text_buffer::from_path(Path::new(&path_str)).expect("Failed to read file at given path.")
}

//TODO remove all random generation from inside measured execution block
//criterion_group!(benches, del_select_bench);
criterion_group!(
    benches,
    char_pop_bench,
    char_insert_bench,
    get_all_lines_bench,
    get_line_len_bench,
    get_line_bench,
    del_select_bench
);
criterion_main!(benches);
