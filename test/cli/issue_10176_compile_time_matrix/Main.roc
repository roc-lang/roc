import "input.txt" as input : Str

# Repro for https://github.com/roc-lang/roc/issues/10176: CTFE must evaluate a
# large imported string mapped through Str.to_utf8 without crashing.
f = |_matrix| {
    "M"
}

main! = |_| {
    matrix = List.map(Str.split_on(input, "\n"), Str.to_utf8)
    val =
        f(matrix) == "M"
        and f(matrix) == "A"
        and f(matrix) == "S"
        and f(matrix) == "S"

    Ok(val)
}
