module []

TypeAB x : [A, B]x

TypeCD x : TypeAB [C, D]x

TypeEF x : TypeCD [E, F]x

Combined : TypeCD (TypeAB (TypeEF []))

combined_to_int : Combined -> U8
combined_to_int = |test1|
    when test1 is
        A -> 1
        B -> 2
        C -> 3
        D -> 4
        E -> 5
        F -> 6
        _ -> crash("combined_to_int() failed")

expect
  value = E
  int_value = combined_to_int(value)
  int_value == 5