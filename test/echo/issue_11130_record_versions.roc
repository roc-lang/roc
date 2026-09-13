consume = |record| "${record.left}:${record.right}"

main! = |args| {
	original = {
		left: {
			echo!("first\n")
			"old"
		},
		right: args.len().to_str(),
	}
	updated = {
		..original,
		left: {
			echo!("second\n")
			"new"
		},
	}
	alias = original
	echo!("${consume(original)}\n")
	echo!("${consume(updated)}\n")
	echo!("${alias.left}\n")
	chosen = if args.is_empty() {
		original
	} else {
		updated
	}
	echo!("${chosen.left}\n")
	unused = {
		..updated,
		left: {
			echo!("unused\n")
			"discarded"
		},
	}
	_ = unused.right
	Ok({})
}
