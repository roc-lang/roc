Main := {}

import SharedVal

fwd_a : {} -> [Shared, Conflict({ a : U64 })]
fwd_a = |{}| SharedVal.shared_value

fwd_b : {} -> [Shared, Conflict({ b : Str })]
fwd_b = |{}| SharedVal.shared_value

describe_a : [Shared, Conflict({ a : U64 })] -> Str
describe_a = |v| match v {
	Shared => "shared"
	Conflict({ a }) => a.to_str()
}

describe_b : [Shared, Conflict({ b : Str })] -> Str
describe_b = |v| match v {
	Shared => "shared"
	Conflict({ b }) => b
}

expect describe_a(fwd_a({})) == "shared"
expect describe_b(fwd_b({})) == "shared"
