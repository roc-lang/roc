# Repro for https://github.com/roc-lang/roc/issues/10303.
# A delayed recursive Parser value must check and store cleanly.
import Parser

game_tree : Parser(List(U8), {})
game_tree =
	Parser.const(|_| {})
		.skip(Parser.fail("missing game tree"))
		.keep(sub_tree.many())

sub_tree : Parser(List(U8), {})
sub_tree = Parser.lazy(|_| game_tree)

expect {
	result = Parser.parse(game_tree, [], |remaining| remaining.is_empty())
	result.is_err()
}

main! = |_args| Ok({})
