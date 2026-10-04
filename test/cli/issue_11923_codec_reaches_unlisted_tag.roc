# Each expect reaches, through a generated parser, a format method whose body
# produces a tag its annotation does not list. Checking reports that once, and
# each expect counts as a compiler error instead of running.
Format := [Default].{
	parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
	parse_u8 = |_, _| Err(OtherErr)
}

expect (U8.parser_for(Format.Default))({}) == Err(Bad)

p = |fmt| U8.parser_for(fmt)

expect (p(Format.Default))({}) == Err(Bad)

expect 1 + 1 == 2
