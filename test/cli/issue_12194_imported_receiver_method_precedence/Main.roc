# A method an imported receiver declares wins over a same-named receiver
# extension from a namespace, while extensions naming methods the receiver does
# not declare still apply.

import Crate
import Narrower
import Same

Main :: [].{
	fetch : Crate -> U64
	fetch = |c| c.get(1) + 10
}

expect {
	crate = Crate.new(1)
	crate.fetch() == 12
}

expect Narrower.get(Crate.new(2)) == 3

expect Same.get(Crate.new(2), 2) == 4
