app [make_handler] { pf: platform "./platform/main.roc" }

make_handler : U64 -> Box(U64 -> U64)
make_handler = |_| Box.box(|n| n + 1)
