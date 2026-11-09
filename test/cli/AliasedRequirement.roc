# A where requirement written through a function type alias is that function
# type: dispatch, forwarding to and from plain signatures, and nested aliases
# all resolve its method.
ToStr(x) : x -> Str
ToStr2(x) : ToStr(x)
Combine(x) : x, x -> x

a.Showable : where [a.to_str : ToStr(a)]

AliasedRequirement := [I(U64)].{
    to_str : AliasedRequirement -> Str
    to_str = |I(n)| "I${n.to_str()}"

    plus : AliasedRequirement, AliasedRequirement -> AliasedRequirement
    plus = |I(a), I(b)| I(a + b)

    is_eq : AliasedRequirement, AliasedRequirement -> Bool
    is_eq = |I(a), I(b)| a == b

    show : a -> Str where [a.to_str : ToStr(a)]
    show = |x| x.to_str()

    show_nested : a -> Str where [a.to_str : ToStr2(a)]
    show_nested = |x| x.to_str()

    show_alias : a -> Str where [a.Showable]
    show_alias = |x| x.to_str()

    forward : a -> Str where [a.to_str : a -> Str]
    forward = |x| show(x)

    forward_back : a -> Str where [a.to_str : ToStr(a)]
    forward_back = |x| forward(x)

    sum_pair : a, a -> a where [a.plus : Combine(a)]
    sum_pair = |x, y| x + y
}

expect AliasedRequirement.show(12.U8) == "12"
expect AliasedRequirement.show(AliasedRequirement.I(3)) == "I3"
expect AliasedRequirement.show_nested(AliasedRequirement.I(4)) == "I4"
expect AliasedRequirement.show_alias(AliasedRequirement.I(5)) == "I5"
expect AliasedRequirement.forward(AliasedRequirement.I(6)) == "I6"
expect AliasedRequirement.forward_back(7.U64) == "7"
expect AliasedRequirement.sum_pair(AliasedRequirement.I(1), AliasedRequirement.I(2)) == AliasedRequirement.I(3)
