# Aliases an imported generalized recursive callable value at a concrete type.
RecursiveCounterAlias := [].{}

import RecursiveCounter

my_count : U64 -> [Done]
my_count = RecursiveCounter.count

expect my_count(3) == Done
expect RecursiveCounter.count(0) == Done
