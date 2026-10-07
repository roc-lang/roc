# `check_one` relates `Blub.parse`'s result to `[Friendly]`, so the `==`
# rejects the instantiated requirement `a.parser_for` (a tag union's parser
# needs `Parser.parse_tag_union`), and the call through that instantiation
# cannot run. A hoisted binding calls an imported function whose body
# dispatches to a method that calls `check_one`. The call is never evaluated
# at compile time: in every lowering mode it runs in source order, after
# "before" and the `dbg`, and crashes at the dispatch that cannot run, before
# "middle".
import RejectedRequirementHelper

main! = |_args| {
	echo!("before\n")
	same = RejectedRequirementHelper.run("Friendly")
	echo!("middle\n")
	echo!("${Str.inspect(same)}\n")
	Ok({})
}

