app [probe] { pf: platform "./platform/main.roc" }

# https://github.com/roc-lang/roc/issues/13009
# The derived `to_hash` of a record of tag unions feeds the hasher the writes
# its specification orders: fields in record order, and for each tag union
# its variant index, in tag order, then its payloads. `manual` makes those
# writes by hand. `reordered` makes the same writes with two fields swapped,
# so it differs whenever the order matters.
Status : [Loading, Ready(Str), Failed(Str)]

Big : { a : Status, b : Status, c : Status, d : U64 }

probe : Hasher -> { derived : Hasher, manual : Hasher, reordered : Hasher }
probe = |hasher| {
	big : Big
	big = { a: Ready("x"), b: Failed("yz"), c: Loading, d: 7 }

	# Variant indices follow tag order: Failed 0, Loading 1, Ready 2.
	write_a = |h| Str.to_hash("x", h.write_u64(2))
	write_b = |h| Str.to_hash("yz", h.write_u64(0))
	write_c = |h| h.write_u64(1)
	write_d = |h| U64.to_hash(7, h)

	{
		derived: big.to_hash(hasher),
		manual: write_d(write_c(write_b(write_a(hasher)))),
		reordered: write_d(write_c(write_a(write_b(hasher)))),
	}
}
