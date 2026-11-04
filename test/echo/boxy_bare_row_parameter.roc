# A parameter whose type is a record made only of a row variable, called with a
# concrete record.
size : { ..r } -> U64
size = |_rec| 1

main! = |_args| {
	echo!(size({ id: 7.U64 }).to_str())
	Ok({})
}
