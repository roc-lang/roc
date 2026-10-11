# A namespace function with the same name as a method the imported receiver
# declares, but a different arity. Method-call syntax reaches the receiver's own
# method.

import Crate

Narrower :: [].{
	get : Crate -> U64
	get = |c| c.get(1)
}

expect Narrower.get(Crate.new(1)) == 2
