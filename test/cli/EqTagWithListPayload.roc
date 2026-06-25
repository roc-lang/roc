EqTagWithListPayload := {}

# Regression test for: panic "non-structural equality reached direct LIR
# structural equality lowering" when == is used on a tag union whose payload
# contains a List.
# See: src/postcheck/solved_lir_lower.zig lowerEqLocalsInto

MyType : [HasList(List(U64)), NoList]

expect {
    x = HasList([1, 2, 3])
    y = HasList([1, 2, 3])
    x == y
}

expect {
    a : MyType
    a = NoList
    b = HasList([1, 2])
    a != b
}
