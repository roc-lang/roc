# A tag constructed with a ZST payload inside a generic function arrives at a
# concrete-union consumer as a dynamic tag value; materializing it to the
# concrete union layout must place the zero-sized payload and discriminant
# correctly for both the payload-carrying and payload-free variants.
wrap : a -> [Some(a), None]
wrap = |x| Some(x)

describe : [Some({}), None] -> Str
describe = |maybe| match maybe {
	Some(_) => "some unit"
	None => "none"
}

main! = |_args| {
	echo!("${describe(wrap({}))}\n")
	echo!("${describe(None)}\n")
	Ok({})
}
