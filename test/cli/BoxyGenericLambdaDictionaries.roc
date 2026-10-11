BoxyGenericLambdaDictionaries := {}

# A lambda inside an unannotated numeric function calls that function back.
# Without specialization the lambda's worker takes one dictionary for each
# numeric operation it reaches, and its use site supplies all of them.
count_down = |n| if n == 0 0 else 0 |> (|_| count_down(n - 1))

through_argument = |n| if n == 0 0 else n |> (|m| through_argument(m - 1))

expect count_down(20) == 0
expect through_argument(20) == 0
