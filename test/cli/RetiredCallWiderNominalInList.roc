RetiredCallWiderNominalInList :: [].{}

# The callback's record pattern has a lift-only tail, so a nominal with an extra
# optional field does not lift into it, and the call is retired with its
# operands. Its callee is still evaluated before the crash, and that use
# instantiates the numeral the fold starts from.

Wide := { count : U64, note ?: Str }.{}

wide : Wide
wide = { count: 1 }

sum_all = |list| List.fold(list, 0, |acc, { count }| acc + count)

n8 = sum_all([wide])
