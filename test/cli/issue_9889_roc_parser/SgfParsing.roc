# Repro module for https://github.com/roc-lang/roc/issues/10912.
# The SGF grammar lives in its own module so the parser combinators that build
# `GameTree` are imported rather than defined next to the test that runs them.
import Parser
import String

SgfParsing :: {}.{
	NodeProperties : Dict(Str, List(Str))
	GameTree := { properties : NodeProperties, children : List(GameTree) }.{
		is_eq : _
	}

	parse : Str -> Try(GameTree, [ParsingFailure(Str), ParsingIncomplete(Str)])
	parse = |sgf| {
		String.parse_str(game_tree, sgf)
	}
}

build_game_node : List(SgfParsing.NodeProperties), List(SgfParsing.GameTree) -> SgfParsing.GameTree
build_game_node = |node_props, alternatives| {
	help : List(SgfParsing.NodeProperties), List(SgfParsing.GameTree) -> SgfParsing.GameTree
	help = |remaining_node_props, sub_trees| {
		match remaining_node_props {
			[root_node] =>
				{ properties: root_node, children: sub_trees }

			[.. as rest, last] =>
				help(rest, [{ properties: last, children: sub_trees }])

			[] => {
				crash "Unreachable: remaining_node_props list cannot be empty"
			}
		}
	}
	help(node_props, alternatives)
}

game_tree : Parser(List(U8), SgfParsing.GameTree)
game_tree =
	Parser.const(|node_props| |alternatives| build_game_node(node_props, alternatives))
		.skip(String.codeunit('('))
		.keep(node.one_or_more())
		.keep(sub_tree.many())
		.skip(String.codeunit(')'))

sub_tree : Parser(List(U8), SgfParsing.GameTree)
sub_tree =
	Parser.const(|t| t)
		.keep(
			Parser.const(|t| t).keep(Parser.lazy(|_| game_tree)),
		)

node : Parser(List(U8), SgfParsing.NodeProperties)
node =
	Parser.const(|s| s)
		.skip(String.codeunit(';'))
		.keep(Parser.many(property))
		.map(|properties| Dict.from_list(properties))

property : Parser(List(U8), (Str, List(Str)))
property =
	Parser.map2(
		prop_ident,
		Parser.one_or_more(prop_value),
		|id, values|
			(
				(Str.from_utf8(id) ?? "<BadUTF8>"),
				values.map(|value| (Str.from_utf8(value) ?? "<BadUTF8>")),
			),
	)

prop_ident : Parser(List(U8), List(U8))
prop_ident =
	Parser.one_or_more(uc_letter)

prop_value : Parser(List(U8), List(U8))
prop_value =
	Parser.const(|value| value)
		.skip(String.codeunit('['))
		.keep(value_type)
		.skip(String.codeunit(']'))

value_type : Parser(List(U8), List(U8))
value_type =
	Parser.build_primitive_parser(
		|input| {
			help = |result, chars| {
				match chars {
					[] => Err(ParsingFailure("No closing bracket"))
					[']', ..] => Ok({ val: result, input: chars })
					['\\', '\t', .. as rest] => help(result.append(' '), rest)
					['\\', '\n', .. as rest] => help(result, rest)
					['\\', c, .. as rest] => help(result.append(c), rest)
					['\t', .. as rest] => help(result.append(' '), rest)
					[c, .. as rest] => help(result.append(c), rest)
				}
			}
			help([], input)
		},
	)

uc_letter : Parser(List(U8), U8)
uc_letter =
	String.codeunit_satisfies(|b| b >= 'A' and b <= 'Z')
