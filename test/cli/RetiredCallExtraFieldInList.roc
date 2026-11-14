RetiredCallExtraFieldInList :: [].{}

# The list argument holds a record with a field the callback's pattern does not
# take, so the call is retired with its operands. Its callee is still evaluated
# before the crash, and that use instantiates the numeral the fold starts from.

sum_all = |list| List.fold(list, 0, |acc, { count }| acc + count)

n8 = sum_all([{ count: 1.U64, other: 2.U64 }])
