use std::path::PathBuf;

use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use roc_parse::{module, module::module_defs, parser::Parser, state::State};

pub fn parse_benchmark(c: &mut Criterion) {
    c.bench_function("parse false-interpreter", |b| {
        let mut path = PathBuf::from(std::env!("ROC_WORKSPACE_DIR"));
        path.push("examples");
        path.push("cli");
        path.push("false-interpreter");
        path.push("False.roc");
        let src = std::fs::read_to_string(&path).unwrap();

        b.iter(|| {
            let arena = Bump::new();

            let (_actual, state) =
                module::parse_header(&arena, State::new(src.as_bytes())).unwrap();

            let min_indent = 0;
            let res = module_defs()
                .parse(&arena, state, min_indent)
                .map(|tuple| tuple.1)
                .unwrap();

            black_box(res.len());
        })
    });

    c.bench_function("parse Num builtin", |b| {
        let mut path = PathBuf::from(std::env!("ROC_WORKSPACE_DIR"));
        path.push("crates");
        path.push("compiler");
        path.push("builtins");
        path.push("roc");
        path.push("Num.roc");
        let src = std::fs::read_to_string(&path).unwrap();

        b.iter(|| {
            let arena = Bump::new();

            let (_actual, state) =
                module::parse_header(&arena, State::new(src.as_bytes())).unwrap();

            let min_indent = 0;
            let res = module_defs()
                .parse(&arena, state, min_indent)
                .map(|tuple| tuple.1)
                .unwrap();

            black_box(res.len());
        })
    });
}

criterion_group!(benches, parse_benchmark);
criterion_main!(benches);
