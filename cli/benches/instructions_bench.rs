use cli_utils::bench_utils::{bench_astar, bench_base64, bench_cfold, bench_closure, bench_deriv, bench_nqueens, bench_rbtree, bench_rbtree_delete};

fn bench_nqueens_iai() {
    bench_nqueens(None);
}

fn bench_cfold_iai() {
    bench_cfold(None);
}

fn bench_deriv_iai() {
    bench_deriv(None);
}

fn bench_rbtree_iai() {
    bench_rbtree(None);
}

fn bench_rbtree_delete_iai() {
    bench_rbtree_delete(None);
}

fn bench_astar_iai() {
    bench_astar(None);
}

fn bench_base64_iai() {
    bench_base64(None);
}

fn bench_closure_iai() {
    bench_closure(None);
}


iai::main!(
    bench_nqueens_iai,
    bench_cfold_iai,
    bench_deriv_iai,
    bench_rbtree_iai,
    bench_rbtree_delete_iai,
    bench_astar_iai,
    bench_base64_iai,
    bench_closure_iai,
);
