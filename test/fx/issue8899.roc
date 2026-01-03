module []

# Reproduction of index out of bounds panic in compress function
# https://github.com/roc-lang/roc/issues/8899
# The bug was a panic when decref tried to read the captures_layout_idx
# from raw memory instead of using the layout parameter.

compress : List(a) -> List(a) where [a.is_eq : a, a -> Bool]
compress = |l| {
    var $acc = []
    for e in l {
        match List.last($acc) {
            Ok(last_elem) => {
                if e != last_elem {
                    $acc = List.append($acc, e)
                }
            }
            Err(_) => {
                $acc = [e]
            }
        }
    }
    $acc
}

# Test that compress can be called without the interpreter panicking.
# The original bug was a panic: "index out of bounds: index 131, len 73"
# in decrefLayoutPtr when cleaning up closures.
# This test verifies the function runs without crashing the compiler.
expect compress(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == ["a", "b", "c", "a", "d", "e"]
