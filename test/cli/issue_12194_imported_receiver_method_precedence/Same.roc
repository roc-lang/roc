# A namespace function with the same name and type as a method the imported
# receiver declares. Method-call syntax reaches the receiver's own method rather
# than recursing into the namespace function.

import Crate

Same :: [].{
	get : Crate, U64 -> U64
	get = |c, k| c.get(k)

	get_plus_zero : Crate, U64 -> U64
	get_plus_zero = |c, k| c.get(k) + 0
}

expect Same.get(Crate.new(1), 1) == 2

expect Same.get_plus_zero(Crate.new(1), 1) == 2
