import Shared

Matching := [].{
	simple_match : [Red, Green, Blue, BabyBlue] -> Str
	simple_match = |color| {
		match color {
			Red => "The color is red."
			Green => "The color is green."
			Blue | BabyBlue => "The color is blue."
		}
	}

	match_list_patterns : List(U64) -> U64
	match_list_patterns = |lst| {
		match lst {
			[] => 0
			[x] => x
			[1, 2, 3] => 6
			[1, 2, ..] => 66
			[2, .., 1] => 88
			[1, .. as tail] => 77 + tail.len()
			[_head, 5] => 55
			[99, x] if Shared.is_small(x) => 99 + x
			_ => 100
		}
	}

	match_tag_union_advanced : Try({}, [StdoutErr(Str), Other]) -> Str
	match_tag_union_advanced = |try|
		match try {
			Ok(_) =>
				"Success"

			Err(StdoutErr(err)) =>
				"StdoutErr: ${Str.inspect(err)}"

			Err(_) =>
				"Unknown error"
		}

	multi_payload_tag : [Foo(I64, Str), Bar] -> Str
	multi_payload_tag = |tag| match tag {
		Foo(num, name) => Shared.label("Foo", "${num.to_str()} and ${name}")
		Bar => "Just Bar"
	}

	color_to_str : [Red, Green, ..] -> Str
	color_to_str = |color| match color {
		Red => "red"
		Green => "green"
		_ => "other color"
	}
}
