# https://github.com/roc-lang/roc/issues/11963
# Run with: roc test main.roc --no-cache
#
# A nominal type's custom `to_hash` hashes a structural tag union with the
# derived structural hash. Using that type as a Dict key must work.
import Rule
import Sub
import Schedule

Key :: { n : I64 }.{
	is_eq : Key, Key -> Bool
	is_eq = |a, b| a.n == b.n

	to_hash : Key, Hasher -> Hasher
	to_hash = |value, hasher| {
		tag : [Small, Large]
		tag = if value.n < 10 Small else Large
		tag.to_hash(hasher)
	}
}

expect {
	d = Dict.empty().insert(Key.{ n: 1 }, 11).insert(Key.{ n: 20 }, 22)
	d.get(Key.{ n: 20 }) == Ok(22)
}

expect {
	calendar = Schedule.new(Rule.calendar(1))
	subdaily = Schedule.new(Rule.subdaily(Sub.new(Minutely, 15)))
	d = Dict.empty().insert(calendar, 1.U8).insert(subdaily, 2.U8)
	d.len() == 2 and d.get(Schedule.new(Rule.subdaily(Sub.new(Minutely, 15)))) == Ok(2.U8)
}
