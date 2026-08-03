# repro for https://github.com/roc-lang/roc/issues/9815
# The discarded Iter.collect result leaves output polymorphic, so check should
# report a normal missing-method diagnostic for output.from_iter.
main! = |_| {
	f = |_| Try.Err(NoMore)

	_ = Iter.collect(Iter.custom(0.U64, Unknown, f))

	Ok({})
}
