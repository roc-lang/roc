# test/echo/all_syntax_test.roc split into modules, for ci/perf/bench.roc.
# Shared is imported by every module and Leaf by Strings only, so the
# benchmarks can time an edit of a widely used module, a leaf and this entry
# file against a warm cache.
import Animal
import Builder
import Control
import Matching
import Operators
import Records
import Secret
import ServerConfig
import Shared
import Strings

# ci/perf/bench.roc rewrites this number before each timed run.
perf_edit : U64
perf_edit = 0

NominalTypeRecord := { x : U64 }

destructure_nominal_type : NominalTypeRecord -> U64
destructure_nominal_type = |NominalTypeRecord.{ x }| x

Letters(others) : [A, B, ..others]

letter_to_str : Letters([C]) -> Str
letter_to_str = |letter| match letter {
	A => "A"
	B => "B"
	_ => "other letter"
}

effect_demo! : Str => {}
effect_demo! = |msg|
	echo!("${msg}\n")

print! = |something| {
	echo!("${Str.inspect(something)}\n")
}

dbg_keyword = || {
	foo : Dec
	foo = 42.0

	dbg foo

	foo
}

main! = |_args| {
	echo!("${Str.inspect(Operators.number_operators(10, 5))}\n")
	print!(Operators.boolean_operators(Bool.True, Bool.False))
	print!("Three" |> Strings.my_concat(" Four"))

	echo!("${Matching.simple_match(Red)}\n")
	print!(Matching.match_list_patterns([1, 10]))
	echo!("${Matching.match_tag_union_advanced(Ok({}))}\n")

	echo!("${Strings.multiline_str(3)}\n")
	effect_demo!(Strings.greeting("Roc"))

	print!(Control.question_postfix(["1", "not a number", "100"]))
	print!(Control.question_with_err_map([]))
	print!(Control.question_with_err_lambda([]))

	sum = Control.for_loop([1, 2, 3, 4, 5])
	print!(sum)
	expect sum == 15

	print!(Control.break_in_for_loop([True, True, False, True, True]))
	print!(Control.while_loop(5))
	print!(dbg_keyword())
	echo!("${Control.if_demo(2)}\n")

	print!(Records.tuple_demo)
	print!(Records.type_var(["a", "b"]))
	print!(Records.destructuring())
	print!(Records.record_builder())
	print!(Builder.size_hint)
	print!(destructure_nominal_type(NominalTypeRecord.{ x: 42 }))
	print!(Records.record_update({ name: "Alice", age: 30 }))
	print!(Records.remove_record_field({ name: "Alice", age: 30, email: "alice@example.com" }))
	print!(Records.number_literals)

	minimal_config : ServerConfig
	minimal_config = ServerConfig.{ host: "localhost" }
	print!(minimal_config.describe())
	expect minimal_config.port == 8080

	full_config : ServerConfig
	full_config = ServerConfig.{ host: "example.com", port: 80, timeout_ms: 5000 }
	print!(full_config.describe())

	secret = Secret.new("my_secret_key")
	print!(secret.unlock("open sesame"))

	dog : Animal
	dog = Dog("Fido")
	cat : Animal
	cat = Cat("Whiskers")
	print!(dog == cat)
	print!(dog.describe())

	print!(Operators.early_return(Bool.False))
	print!(Operators.stringify(12345))
	print!(Matching.multi_payload_tag(Foo(42, "hello")))
	print!(Matching.color_to_str(Blue))
	print!(Strings.format_names(["  Alice ", "Bob  ", " Charlie"]))
	print!(letter_to_str(A))
	print!(letter_to_str(C))
	print!(perf_edit + Shared.perf_edit)

	Ok({})
}
