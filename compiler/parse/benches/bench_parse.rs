use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use roc_parse::{module, module::module_defs, parser::Parser, state::State};

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_owned();
    path.push("examples");
    path.push("false-interpreter");
    path.push("False.roc");

    let src = std::fs::read_to_string(&path).unwrap();

    c.bench_function("parse false-interpreter", |b| {
        b.iter(|| {
            let arena = Bump::new();

            let (_actual, state) =
                module::parse_header(&arena, State::new(src.as_bytes())).unwrap();

            let res = module_defs()
                .parse(&arena, state)
                .map(|tuple| tuple.1)
                .unwrap();

            black_box(res.len());
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
