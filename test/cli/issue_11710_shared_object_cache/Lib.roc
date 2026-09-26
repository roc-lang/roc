# Reduced from basic-cli's Url module. Its helpers are stubs, but its function
# values must keep this shape: Lib's module pack and Repro.roc's program join
# them into different callable sets.
Lib :: {
	scheme : [Http, Https],
	host : Str,
	query : [None, Some(Str)],
}.{
	parse = |input| parse_absolute(input)
	query_pairs : Lib -> List((Str, Str))
	query_pairs = |url|
		match url.query {
			None => []
			Some(query_str) =>
				Str.split_on(query_str, "&").map(
					|pair|
						match split_first(pair, "=") {
							NotFound => (form_decode(pair), "")
						},
				)
			}
}

parse_absolute = |input| {
	scheme_parts =
		match split_first(input, "://") {
			NotFound =>
				if Str.contains(input, ":") {
					Err(MissingAuthority)
				} else {
					Err(MissingScheme)
				}
			}?
	scheme =
		match ascii_lower(scheme_parts.before) {
			"https" => Ok(Https)
			other => Err(UnsupportedScheme(other))
		}?
	{ authority, suffix } = split_authority(scheme_parts.after)
	parsed_authority = parse_authority(authority, scheme)?
	components = parse_suffix(suffix)?
	Ok(
		Lib.{
			scheme: Https,
			host: parsed_authority.host,
			query: components.query,
		},
	)
}

parse_authority = |authority, scheme| {
	if starts_with(authority, "[") {
		match split_first(authority, "]") {
			Found({ before, after }) => {
				raw_ipv6 = drop_prefix(before, "[")
				host = validate_ipv6(raw_ipv6)?
				port =
					if Str.is_empty(after) {
						Ok(None)
					} else if starts_with(after, ":") {
						parse_port(drop_prefix(after, ":"), scheme)
					} else {
						Err(InvalidIpv6(authority))
					}
				Ok({ host: Str.concat(Str.concat("[", host), "]"), port: port? })
			}
		}
	} else {
		{ raw_host, raw_port } =
			match split_last(authority, ":") {
				Found({ before, after }) => { raw_host: before, raw_port: Some(after) }
				NotFound => { raw_host: authority, raw_port: None }
			}
		host = validate_host(raw_host)?
		port =
			match raw_port {
				None => Ok(None)
				Some(raw) => parse_port(raw, scheme)
			}
		Ok({ host, port: port? })
	}
}

validate_host = |raw_host| {
	if Str.is_empty(raw_host) {
		Err(InternationalHostUnsupported)
	} else if List.all(Str.to_utf8(raw_host), |byte| is_digit(byte) or byte == 46) {
		validate_ipv4(raw_host)
	} else {
		validate_dns_name(raw_host)
	}
}

validate_dns_name = |_raw_host| crash "stub"

validate_ipv4 = |_raw_host| crash "stub"

parse_port = |_raw, _scheme| crash "stub"

validate_ipv6 = |raw| {
	pieces = Str.split_on(raw, "::")
	left = parse_ipv6_side(get_or_empty(pieces, 0))?
	count = List.len(left)
	if count >= 8 {
		Err(InvalidIpv6(raw))
	} else {
		groups = List.concat(List.concat(left, List.repeat(0, 8 - count)), left)
		Ok(serialize_ipv6(groups))
	}
}

parse_ipv6_side = |raw|
	if Str.is_empty(raw) {
		Ok([])
	} else {
		parse_hex_groups(Str.split_on(raw, ":"), [])
	}

parse_hex_groups = |parts, out|
	match parts {
		[] => Ok(out)
		[first, .. as rest] => {
			bytes = Str.to_utf8(first)
			if List.is_empty(bytes) or List.len(bytes) > 4 or Bool.not(List.all(bytes, is_hex)) {
				Err(InvalidIpv6(first))
			} else {
				value = List.fold(bytes, 0, |acc, byte| acc * 16 + U8.to_u16(hex_value(byte)))
				parse_hex_groups(rest, out.append(value))
			}
		}
	}

serialize_ipv6 = |_groups| crash "stub"

parse_suffix = |_suffix| crash "stub"

form_decode = |input| Str.from_utf8_lossy(form_decode_help(Str.to_utf8(input), 0, []))

form_decode_help = |bytes, index, out| {
	if index >= List.len(bytes) {
		out
	} else {
		byte = get_or_zero(bytes, index)
		if is_hex(byte) {
			decoded = hex_value(byte)
			form_decode_help(bytes, index + 3, out.append(decoded))
		} else {
			form_decode_help(bytes, index + 1, out.append(byte))
		}
	}
}

split_authority = |_after_scheme| crash "stub"

ascii_lower = |_input| crash "stub"

is_digit = |_byte| crash "stub"

is_hex = |_byte| crash "stub"

hex_value = |_byte| crash "stub"

get_or_zero = |_list, _index| crash "stub"

get_or_empty = |_list, _index| crash "stub"

starts_with = |_str, _prefix| crash "stub"

drop_prefix = |_str, _prefix| crash "stub"

split_first = |_str, _separator| crash "stub"

split_last = |_str, _separator| crash "stub"
