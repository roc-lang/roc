# A recursive parser built with `Parser.lazy` and `many`, run from a test.
# `Parser.parse_partial` takes no descriptor for its `input` variable, which
# appears only inside function types, so the adapter that converts the
# restored parser's function describes it by the caller's type at the same
# position.
import Parser

main! = |_args| Ok({})

T := { props : Dict(Str, List(Str)), kids : List(T) }.{
	is_eq : _
}

open_paren : Parser(List(U8), U8)
open_paren = Parser.build_primitive_parser(
	|input| {
		match input {
			['(', .. as rest] => Ok({ val: '(', input: rest })
			_ => Err(ParsingFailure("no paren"))
		}
	},
)

tree : Parser(List(U8), T)
tree =
	Parser.const(|k| { props: Dict.empty(), kids: k })
		.skip(open_paren)
		.keep(sub.many())

sub : Parser(List(U8), T)
sub = Parser.const(|t| t).keep(Parser.lazy(|_| tree))

expect Parser.parse_partial(tree, ['(', '(']).map_ok(|r| r.val) == Ok({ props: Dict.empty(), kids: [{ props: Dict.empty(), kids: [] }] })
