# A parser constant whose output row is open is polymorphic: `parse_left` and
# `parse_right` are each read at the closed union `parse_line` names. A
# polymorphic constant read at an open type inside a generic function
# (`wrap`) reaches each caller's closed type through the call chain
# (`with_first`), in this module and through an imported module
# (`PolyConstDep`).
import PolyConstDep

Parser(input, a, err) := [P(input -> Try({ value : a, rest : input }, err))].{
	run : Parser(input, a, err), input -> Try({ value : a, rest : input }, err)
	run = |Parser.P(f), input| f(input)

	const : a -> Parser(input, a, err)
	const = |value| Parser.P(|input| Ok({ value, rest: input }))

	keep : Parser(input, (b -> c), err), Parser(input, b, err) -> Parser(input, c, err)
	keep = |pf, pb| Parser.P(|input|
		match pf.run(input) {
			Ok({ value: f, rest }) =>
				match pb.run(rest) {
					Ok({ value: b, rest: after }) => Ok({ value: f(b), rest: after })
					Err(e) => Err(e)
				}
			Err(e) => Err(e)
		})

	skip : Parser(input, a, err), Parser(input, _b, err) -> Parser(input, a, err)
	skip = |pa, pb| Parser.P(|input|
		match pa.run(input) {
			Ok({ value, rest }) =>
				match pb.run(rest) {
					Ok({ value: _, rest: after }) => Ok({ value, rest: after })
					Err(e) => Err(e)
				}
			Err(e) => Err(e)
		})

	one_of : List(Parser(input, a, [NoMatch])) -> Parser(input, a, [NoMatch])
	one_of = |parsers| Parser.P(|input| {
		var $result = Err(NoMatch)
		for p in parsers {
			if $result.is_ok() {
				break
			}
			$result = p.run(input)
		}
		$result
	})

	Utf8 := [].{
		codeunit : U8 -> Parser(List(U8), U8, [NoMatch])
		codeunit = |unit| Parser.P(|input|
			match List.first(input) {
				Ok(first) if first == unit => Ok({ value: first, rest: List.drop_first(input, 1) })
				_ => Err(NoMatch)
			})

		digits : {} -> Parser(List(U8), U64, [NoMatch])
		digits = |{}| Parser.P(|input| {
			var $n = 0.U64
			var $count = 0.U64
			for unit in input {
				if $count == List.len(input) or unit < '0' or unit > '9' {
					break
				}
				$n = $n * 10 + (unit - '0').to_u64()
				$count = $count + 1
			}
			if $count == 0 {
				Err(NoMatch)
			} else {
				Ok({ value: $n, rest: List.drop_first(input, $count) })
			}
		})
	}
}

parse_left : Parser(List(U8), [Left(U64), ..], _)
parse_left = Parser.const(|n| Left(n)).skip(Parser.Utf8.codeunit('L')).keep(Parser.Utf8.digits({}))

parse_right : Parser(List(U8), [Right(U64), ..], _)
parse_right = Parser.const(|n| Right(n)).skip(Parser.Utf8.codeunit('R')).keep(Parser.Utf8.digits({}))

parse_line : Parser(List(U8), [Left(U64), Right(U64)], _)
parse_line = Parser.one_of([parse_left, parse_right])

empty_items : Parser(List(U8), List(_b), [NoMatch])
empty_items = Parser.const([])

wrap : () -> Parser(List(U8), List(_b), [NoMatch])
wrap = || empty_items

with_first : a, List(U8) -> List(a)
with_first = |x, bytes|
	match wrap().run(bytes) {
		Ok({ value, rest: _ }) => List.append(value, x)
		Err(_) => []
	}

with_imported : a -> List(a)
with_imported = |x| List.append(PolyConstDep.wrap(), x)

describe : List(U8) -> Str
describe = |bytes|
	match parse_line.run(bytes) {
		Ok({ value: Left(n), rest: _ }) => "left ${n.to_str()}"
		Ok({ value: Right(n), rest: _ }) => "right ${n.to_str()}"
		Err(_) => "no match"
	}

main! = |_args| {
	echo!("${describe(Str.to_utf8("L12"))}\n")
	echo!("${describe(Str.to_utf8("R7"))}\n")
	echo!("${describe(Str.to_utf8("X"))}\n")
	echo!("${Str.inspect(with_first(1.U8, []))} ${Str.inspect(with_first("s", []))}\n")
	echo!("${Str.inspect(with_imported(2.U16))} ${Str.inspect(with_imported("t"))}\n")
	Ok({})
}
