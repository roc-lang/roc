# Values of an imported recursive nominal, one restored from a stored
# combinator constant, compared with literals through `==`.
import Tree exposing [empty]

main! = |_args| Ok({})

expect Tree.make([]) == { props: Dict.empty(), kids: [] }

expect {
	expected = { props: Dict.empty(), kids: [] }
	empty("") == Ok(expected)
}
