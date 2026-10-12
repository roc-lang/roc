JsonCustomParserOverDerived :: [].{}

# https://github.com/roc-lang/roc/issues/11838
Flag := [On, Off].{
	parser_for : _
}

Wrap := [W(Flag)].{
	parser_for = |encoding| {
		parse_flag = Flag.parser_for(encoding)
		|state| {
			parsed = parse_flag(state)?
			Ok({ value: W(parsed.value), rest: parsed.rest })
		}
	}
}

is_on : Wrap -> Bool
is_on = |wrap|
	match wrap {
		W(On) => True
		W(Off) => False
	}

Setting := { enabled : Bool }.{
	parser_for : _
}

Toggle := [T(Setting)].{
	parser_for = |encoding| {
		parse_setting = Setting.parser_for(encoding)
		|state| {
			parsed = parse_setting(state)?
			Ok({ value: T(parsed.value), rest: parsed.rest })
		}
	}
}

is_enabled : Toggle -> Bool
is_enabled = |toggle|
	match toggle {
		T(setting) => setting.enabled
	}

expect {
	parsed : Try(Wrap, _)
	parsed = Json.parse("\"On\"")
	parsed.map_ok(is_on) == Ok(True)
}

expect {
	parsed : Try(List(Wrap), _)
	parsed = Json.parse("[\"On\",\"Off\"]")
	parsed.map_ok(|wraps| wraps.map(is_on)) == Ok([True, False])
}

expect {
	parsed : Try({ w : Wrap }, _)
	parsed = Json.parse("{\"w\":\"Off\"}")
	parsed.map_ok(|record| is_on(record.w)) == Ok(False)
}

expect {
	parsed : Try(List(Wrap), _)
	parsed = Json.parse("[\"Maybe\"]")
	match parsed {
		Err(InvalidJson(_)) => True
		_ => False
	}
}

expect {
	parsed : Try(List(Toggle), _)
	parsed = Json.parse("[{\"enabled\":true},{\"enabled\":false}]")
	parsed.map_ok(|toggles| toggles.map(is_enabled)) == Ok([True, False])
}
