main! = |args| {
	trim_input = List.len(args) < 100
	config = { format: if trim_input |s| s.trim() else |s| s }
	echo!("${(config.format)("  hi  ")}\n")
	chosen = {
		format: match trim_input {
			True => |s| s.trim()
			False => |s| s
		},
	}
	echo!("${(chosen.format)("  match  ")}\n")
	block = {
		format: {
			|s| s.trim()
		},
	}
	echo!("${(block.format)("  block  ")}\n")
	listed = [if trim_input |s| s.trim() else |s| s]
	match List.first(listed) {
		Ok(f) => echo!("${f("  list  ")}\n")
		Err(_) => {}
	}
	Ok({})
}
