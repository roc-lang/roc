# A constant only an uncalled function reads is left out of a build, a
# constant two modules read is in it once, and a constant only a method
# reached through a dispatch reads is in it (design.md "Constant Pool").
import PoolTables
import PoolOther

value_of : a -> U64
	where [a.value : a -> U64]
value_of = |x| x.value()

main! = |args| {
	n = args.len()
	echo!("${PoolTables.shared(n).to_str()} ${PoolOther.other(n).to_str()} ${value_of(PoolTables.probe(n)).to_str()}\n")
	Ok({})
}
