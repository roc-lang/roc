import Factory

# Control: constructing and calling the same closure before storing its
# result must agree with restoring the completed function value.
direct = Factory.direct(7.U64)
closed_direct = Factory.closed({})({})

expect direct == "Local(7)"
expect closed_direct == "Local(7)"
