use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand::distributions::Alphanumeric;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use roc_editor::mvc::app_model::AppModel;
use roc_editor::mvc::ed_model::{EdModel, Position, RawSelection};
use roc_editor::mvc::update::handle_new_char;
use roc_editor::text_buffer;
use roc_editor::text_buffer::TextBuffer;
use ropey::Rope;
use std::fs::File;
use std::io::Write;
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
        mem_arena: bumpalo::Bump::new(),
    }
}

pub fn char_insert_benchmark(c: &mut Criterion) {
    let text_buf = text_buffer_from_str("");

    let caret_pos = Position { line: 0, column: 0 };

    let selection_opt: Option<RawSelection> = None;
    let mut app_model = mock_app_model(text_buf, caret_pos, selection_opt);
    c.bench_function("single char insert, small buffer", |b| {
        b.iter(|| handle_new_char(&mut app_model, &'a'))
    });
}

static ROC_SOURCE_START: &str = "interface LongStrProvider
    exposes [ longStr ]
    imports []

longStr : Str
longStr =
    \"\"\"";

static ROC_SOURCE_END: &str = "\"\"\"";

fn line_count(lines: &str) -> usize {
    lines.matches("\n").count()
}

pub fn gen_file(nr_lines: usize) {
    let nr_of_str_lines = nr_lines - line_count(ROC_SOURCE_START);
    let path_str = bench_resource_path(nr_lines);
    let path = Path::new(&path_str);
    let display = path.display();

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why),
        Ok(file) => file,
    };

    file.write(ROC_SOURCE_START.as_bytes())
        .expect("Failed to write String to file.");

    let mut rand_gen_line = StdRng::seed_from_u64(42);

    for _ in 0..nr_of_str_lines {
        let line_len = rand_gen_line.gen_range(1..90);

        let char_seed = rand_gen_line.gen_range(0..1000);

        let mut rand_string: String = StdRng::seed_from_u64(char_seed)
            .sample_iter(&Alphanumeric)
            .take(line_len)
            .map(char::from)
            .collect();
        rand_string.push('\n');

        file.write(rand_string.as_bytes())
            .expect("Failed to write String to file.");
    }

    file.write(ROC_SOURCE_END.as_bytes())
        .expect("Failed to write String to file.");
}

fn bench_resource_path(nr_lines: usize) -> String {
    let resource_path_res = std::env::var("BENCH_RESOURCE_PATH");
    let resource_path_str = 
        if let Ok(resource_path) = resource_path_res {
            resource_path
        } else {
            "benches/resources/".to_owned()
        };

    format!("{}{}_lines.roc", resource_path_str, nr_lines)
}

fn file_read_bench_helper(nr_lines: usize, c: &mut Criterion) {
    let path_str = bench_resource_path(nr_lines);
    text_buffer::from_path(Path::new(&path_str)).expect("Failed to read file at given path.");
    c.bench_function(
        &format!("read {:?} line file into textbuffer", nr_lines),
        |b| b.iter(|| text_buffer::from_path(black_box(Path::new(&path_str)))),
    );
}

fn file_read_bench_10(c: &mut Criterion) {
    // generate dummy files
    /*let lines_vec = vec![100, 500, 1000, 10000, 50000, 100000, 25000000];

    for nr_lines in lines_vec.iter(){
        gen_file(*nr_lines);
    }*/

    file_read_bench_helper(10, c)
}

fn file_read_bench_100(c: &mut Criterion) {
    file_read_bench_helper(100, c)
}
fn file_read_bench_500(c: &mut Criterion) {
    file_read_bench_helper(500, c)
}
fn file_read_bench_1k(c: &mut Criterion) {
    file_read_bench_helper(1000, c)
}
fn file_read_bench_10k(c: &mut Criterion) {
    file_read_bench_helper(10000, c)
}
fn file_read_bench_100k(c: &mut Criterion) {
    file_read_bench_helper(100000, c)
}
fn file_read_bench_25m(c: &mut Criterion) {
    file_read_bench_helper(25000000, c)
}

criterion_group!(
    benches,
    file_read_bench_10,
    file_read_bench_100,
    file_read_bench_500,
    file_read_bench_1k,
    file_read_bench_10k,
    file_read_bench_100k,
    file_read_bench_25m
);
criterion_main!(benches);
