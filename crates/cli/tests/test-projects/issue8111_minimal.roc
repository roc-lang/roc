module []

TypeAB x : [A, B]x

TypeCD x : TypeAB [C]x

Combined : TypeCD (TypeAB [])

combined_to_int : Combined -> U8
combined_to_int = |test1|
    when test1 is
        A -> 1
        B -> 2
        C -> 3

expect
  value = A
  int_value = combined_to_int(value)
  int_value == 1u8