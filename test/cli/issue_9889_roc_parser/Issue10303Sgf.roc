# Repro for https://github.com/roc-lang/roc/issues/10303.
# A delayed recursive Parser value must check and store cleanly.
import Parser

game_tree : Parser(List(U8), {})
game_tree =
	Parser.const(|_| {})
		.keep(sub_tree.many())

sub_tree : Parser(List(U8), {})
sub_tree = Parser.lazy(|_| game_tree)

main! = |_args| Ok({})
