# Forward-reference matrix (plan.md tests): a top-level def referencing
# later functions (annotated and unannotated), and a function referencing a
# later function — all backward references in graph order, so no re-entrant
# checking is ever needed.
combined = |extra| quadruple(11) + halve(44) + extra

quadruple = |n| double(n) + double(n)

halve : U64 -> U64
halve = |n| n // 2

double = |n| n * 2

main! = |args| {
    if combined(List.len(args)) != 66 {
        crash "forward-referenced defs computed the wrong value"
    }
    Ok({})
}
