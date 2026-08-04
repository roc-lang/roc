Wrap :: { a : U8, b : U8 }

use : Wrap -> U8
use = |wrap| wrap.a

inc_and_use : Wrap -> U8
inc_and_use = |wrap| use({ ..wrap, a: wrap.a + 1 })

main! = |args| {
	input = if List.len(args) == 0 Wrap.{ a: 1, b: 2 } else Wrap.{ a: 2, b: 3 }
	if inc_and_use(input) > input.a Ok({}) else Err(Exit(1))
}
