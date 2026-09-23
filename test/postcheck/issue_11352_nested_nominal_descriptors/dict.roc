# Run with: roc test dict.roc --no-cache --specialize=no
insert_inner : Dict(U64, Dict(Str, U64)) -> U64
insert_inner = |d| d.insert(1, Dict.empty()).len()

expect insert_inner(Dict.empty()) == 1
