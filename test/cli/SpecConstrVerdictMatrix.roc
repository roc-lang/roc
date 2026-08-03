SpecConstrVerdictMatrix :: [].{}

# Statically undecidable pattern forms (list, string, and numeric-literal
# patterns) nested inside tuple, record, and tag patterns over
# partially-symbolic scrutinees. Call-pattern specialization must leave
# these matches residual: folding a branch away because its verdict is
# undecidable would either panic or select the wrong branch.

# List patterns inside a tuple pattern.
classify_pair : List(U8), U64 -> Str
classify_pair = |l, n| {
	match (l, n) {
		([], _) => "empty"
		([_, ..], 0) => "zero"
		_ => "other"
	}
}

# Numeric literal patterns inside list patterns inside a tuple pattern.
count_zeros : List(U64), U64 -> U64
count_zeros = |l, acc| {
	match (l, acc) {
		([], _) => acc
		([0, .. as rest], _) => count_zeros(rest, acc + 1)
		([_, .. as rest], _) => count_zeros(rest, acc)
	}
}

# A string literal pattern and a numeric literal pattern inside record
# patterns.
describe_user : { name : Str, score : U64 } -> Str
describe_user = |user| {
	match user {
		{ name: "admin", score: _ } => "admin"
		{ name: _, score: 0 } => "zero"
		_ => "user"
	}
}

# A numeric literal pattern inside a tag pattern.
describe_count : [Count(U64), None] -> Str
describe_count = |t| {
	match t {
		Count(0) => "zero"
		Count(_) => "some"
		None => "none"
	}
}

expect classify_pair([], 5) == "empty"
expect classify_pair([1], 0) == "zero"
expect classify_pair([1], 9) == "other"
expect count_zeros([0, 3, 0], 0) == 2
expect describe_user({ name: "admin", score: 3 }) == "admin"
expect describe_user({ name: "guest", score: 0 }) == "zero"
expect describe_user({ name: "guest", score: 7 }) == "user"
expect describe_count(Count(0)) == "zero"
expect describe_count(Count(4)) == "some"
expect describe_count(None) == "none"
