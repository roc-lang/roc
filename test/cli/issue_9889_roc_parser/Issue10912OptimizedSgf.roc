# Repro for https://github.com/roc-lang/roc/issues/10912.
# Parsing the minimal SGF document "(;)" through an imported grammar module must
# yield a game tree with no properties and no children under `--opt=speed`.
import SgfParsing exposing [parse]

expect {
	sgf = "(;)"
	result = parse(sgf)
	expected = {
		properties: Dict.empty(),
		children: [],
	}
	result == Ok(expected)
}

main! = |_args| Ok({})
