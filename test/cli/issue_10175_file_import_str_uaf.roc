import "issue_10175_input.txt" as raw_input : Str

# The issue's input has no trailing newline; keep the text fixture ordinary and
# trim it here so the imported Str has the same runtime shape.
input = Str.drop_suffix(raw_input, "\n")

f = |_matrix, _x, _y| {
    'M'
}

main! = |_| {
    matrix = List.map(Str.split_on(input, "\n"), Str.to_utf8)
    val =
        if
            f(matrix, 1, 1) == 'M'
            and f(matrix, 2, 2) == 'A'
            and f(matrix, 3, 3) == 'S'
        {
            1
        } else {
            0
        }
    Ok(val)
}
