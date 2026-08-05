app [make_boxed_callable, drop_boxed_callable] { pf: platform "./platform/main.roc" }

make_boxed_callable : U64 -> Box(U64 -> U64)
make_boxed_callable = |offset| Box.box(|value| value + offset)

drop_boxed_callable : Box(U64 -> U64) -> {}
drop_boxed_callable = |_callable| {}
