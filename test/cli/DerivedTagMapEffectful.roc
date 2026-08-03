app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

NamedMaybe(a) := [NamedJust(a), NamedNothing].{
	map! : _
}

main! = || {
	value : [Just(U64), Nothing]
	value = Just(41)
	mapped = value.map!(|number| {
		Stdout.line!("transformed")
		number + 1
	})

	match mapped {
		Just(42) => {}
		_ => {
			crash "map! returned the wrong value"
		}
	}

	named_value : NamedMaybe(U64)
	named_value = NamedJust(41)
	named_mapped = named_value.map!(|number| {
		Stdout.line!("named transformed")
		number + 1
	})

	match named_mapped {
		NamedJust(42) => {}
		_ => {
			crash "nominal map! returned the wrong value"
		}
	}

	{}
}
