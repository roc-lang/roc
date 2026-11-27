# design.md Value Rows: Uses Share One Row. The model implements this
# section's reading for annotated local values (see README Limits).
H := [H].{}

# === an unannotated local value is widened by its use
f = |b| {
    x = if b A else B
    match x { A => 1.U64, B => 2.U64, C => 3.U64 }
}

# === an annotated local value is not widened by its use
f = |b| {
    x : [A, B]
    x = if b A else B
    match x { A => 1.U64, B => 2.U64, C => 3.U64 }
}
