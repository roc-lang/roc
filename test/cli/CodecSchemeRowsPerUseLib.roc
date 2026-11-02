CodecSchemeRowsPerUseLib :: [].{
	any_parser = |_| U64.parser_for(Format.Default)

	Format := [Default].{
		parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, [Negative])
		parse_u64 = |_, state| if state == 7 {
			Err(Negative)
		} else {
			Ok({ value: state, rest: state + 1 })
		}
	}
}
