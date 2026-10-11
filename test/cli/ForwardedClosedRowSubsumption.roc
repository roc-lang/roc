# A function whose body forwards its closed input into an annotated open
# result row, and one whose body constructs it, are called at a wider union
# (design.md "Row Subsumption").
ForwardedClosedRowSubsumption := {}

# Identical signatures; one body forwards its closed input, the other builds
# its result. Callers may widen either.
forward : [A, B] -> [A, B]
forward = |x| x

construct : [A, B] -> [A, B]
construct = |x| match x {
    A => A
    B => B
}

describe : [A, B, C] -> Str
describe = |tag| match tag {
    A => "a"
    B => "b"
    C => "c"
}

expect describe(forward(B)) == "b"
expect describe(construct(B)) == "b"
expect describe(forward(A)) == "a"
expect forward(A) == A
