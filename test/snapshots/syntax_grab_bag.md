# META
~~~ini
description=A grab bag of all v0.1 syntax, heavily commented to show multiline formatting
type=file
~~~
# SOURCE
~~~roc
# This is a module comment!
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout exposing [line!, write!]

import # Comment after import keyword
	pf # Comment after qualifier
		.StdoutMultiline # Comment after ident
		exposing [ # Comment after exposing open
			line!, # Comment after exposed item
			write!, # Another after exposed item
		] # Comment after exposing close

import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]

import BadName as GoodName
import
	BadNameMultiline
		as
		GoodNameMultiline

Map(a, b) : List(a), (a -> b) -> List(b)
MapML( # Comment here
	a, # And here
	b,
) # And after the last arg
	: # And after the colon
		List( # Inside Tag args
			a, # After tag arg
		),
		(a -> b) -> # After arrow
			List( # Inside tag args
				b,
			) # And after the type decl

Foo : (Bar, Baz)

FooMultiline : ( # Comment after pattern tuple open
	Bar, # Comment after pattern tuple item
	Baz, # Another after pattern tuple item
) # Comment after pattern tuple close

Some(a) : { foo : Ok(a), bar : Something }
SomeMl(a) : { # After record open
	foo : Ok(a), # After field
	bar : Something, # After last field
}

SomeMultiline(a) : { # Comment after pattern record open
	foo # After field name
		: # Before field anno
			Ok(a), # Comment after pattern record field
	bar : Something, # Another after pattern record field
} # Comment after pattern record close

Maybe(a) : [Some(a), None]

MaybeMultiline(a) : [ # Comment after tag union open
	Some(a), # Comment after tag union member
	None, # Another after tag union member
] # Comment after tag union close

SomeFunc(a) : Maybe(a), a -> Maybe(a)

add_one_oneline = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num {
		dbg # After debug
			some_func() # After debug expr
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, # After arg
	b,
| # After args
	match a {
		Blue | Green | Red => {
			x = 12
			x
		}
		Blue # After pattern in alt
		| # Before pattern in alt
			Green
		| Red # After alt pattern
			=> {
				x = 12
				x
			}
		lower # After pattern comment
			=> 1
		"foo" => # After arrow comment
			100
		"foo" | "bar" => 200
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
				123 # After branch comment

		# Just a random comment

		[1, 2 | 5, 3, .. as rest] => 123
		[
			1,
			2 | 5,
			3,
			.. # After DoubleDot
				as # Before alias
					rest, # After last pattern in list
		] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # After pattern record open
			foo # After pattern record field name
				: # Before pattern record field value
					1, # After pattern record field
			bar: 2,
			.. # After spread operator
				rest, # After last field
		} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			foo: 1,
			bar: 2 | 7, # After last record field
		} => 12
		Ok(123) => 123
		Ok(Some(dude)) => dude
		TwoArgs("hello", Some("world")) => 1000
	}

expect # Comment after expect keyword
	blah == 1 # Comment after expect statement

main! : List(String) -> Result({}, _)
main! = |_| { # Yeah I can leave a comment here
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comment after return keyword
		tag # Comment after return statement

	# Just a random comment!

	...
	match_time(
		..., # Single args with comment
	)
	some_func(
		dbg # After debug
			42, # After debug expr
	)
	crash # Comment after crash keyword
		"Unreachable!" # Comment after crash statement
	tag_with_payload = Ok(number)
	interpolated = "Hello, ${world}"
	list = [
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
		), # Comment one
		456, # Comment two
		789, # Comment three
	]
	for n in list {
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
	multiline_tuple = (
		123,
		"World",
		tag1,
		Ok(world), # This one has a comment
		(nested, tuple),
		[1, 2, 3],
	)
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
	Stdout.line!(interpolated)?
	Stdout.line!(
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
		} as a string?",
	)
} # Comment after top-level decl

empty : {}
empty = {}

tuple : Value((a, b, c))

expect {
	foo = 1 # This should work too
	blah = 1
	blah == foo
}
~~~
# TOKENS
~~~text
LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent KwAs UpperIdent Comma UpperIdent Dot OpStar CloseSquare KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent KwAs UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound LowerIdent Comma CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma CloseRound UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent Comma CloseRound UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent Comma CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg LowerIdent OpenRound CloseRound Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma OpBar KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly LowerIdent OpFatArrow Int String OpFatArrow Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent Comma CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent Comma CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int Comma CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound String Comma UpperIdent OpenRound String CloseRound CloseRound OpFatArrow Int CloseCurly KwExpect LowerIdent OpEquals Int LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LowerIdent TripleDot LowerIdent OpenRound TripleDot Comma CloseRound LowerIdent OpenRound KwDbg Int Comma CloseRound KwCrash String LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LowerIdent Comma CloseRound Comma Int Comma Int Comma CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly KwAs LowerIdent LowerIdent OpQuestion MalformedString CloseRound CloseCurly LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound KwExpect OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (block
    (binop_colon
      (lc "pf")
      (str_literal_big "../basic-cli/platform.roc")
    )
    (malformed malformed:expr_unexpected_token)
    (list_literal
      (not_lc "main")
    )
  )
  (import
    (binop_exposing
      (binop_pipe
        (lc "pf")
        (uc "Stdout")
      )
      (list_literal
        (lc "line")
        (lc "write")
      )
    )
  )
  (import
    (binop_exposing
      (binop_pipe
        (lc "pf")
        (uc "StdoutMultiline")
      )
      (list_literal
        (lc "line")
        (lc "write")
      )
    )
  )
  (import
    (binop_exposing
      (binop_pipe
        (lc "pkg")
        (uc "Something")
      )
      (list_literal
        (lc "func")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (lc "function")
  (malformed malformed:expr_unexpected_token)
  (uc "Type")
  (malformed malformed:expr_unexpected_token)
  (uc "ValueCategory")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_dot_suffix_not_allowed)
  (malformed malformed:expr_unexpected_token)
  (import
    (binop_as
      (uc "BadName")
      (uc "GoodName")
    )
  )
  (import
    (binop_as
      (uc "BadNameMultiline")
      (uc "GoodNameMultiline")
    )
  )
  (binop_colon
    (apply_uc
      (uc "Map")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "List")
          (lc "b")
        )
      )
    )
  )
  (apply_uc
    (uc "MapML")
    (binop_colon
      (tuple_literal
        (lc "a")
        (lc "b")
        (malformed malformed:expr_unexpected_token)
      )
      (apply_uc
        (uc "List")
        (binop_thin_arrow
          (lc "a")
          (binop_thin_arrow
            (malformed malformed:expr_unexpected_token)
            (binop_thin_arrow
              (binop_thin_arrow
                (lc "a")
                (lc "b")
              )
              (apply_uc
                (uc "List")
                (tuple_literal
                  (lc "b")
                  (malformed malformed:expr_unexpected_token)
                )
              )
            )
          )
        )
      )
    )
  )
  (binop_colon
    (uc "Foo")
    (tuple_literal
      (uc "Bar")
      (uc "Baz")
    )
  )
  (binop_colon
    (uc "FooMultiline")
    (tuple_literal
      (uc "Bar")
      (uc "Baz")
      (malformed malformed:expr_unexpected_token)
    )
  )
  (binop_colon
    (apply_uc
      (uc "Some")
      (lc "a")
    )
    (record_literal
      (binop_colon
        (lc "foo")
        (apply_uc
          (uc "Ok")
          (lc "a")
        )
      )
      (binop_colon
        (lc "bar")
        (uc "Something")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "SomeMl")
      (lc "a")
    )
    (record_literal
      (binop_colon
        (lc "foo")
        (apply_uc
          (uc "Ok")
          (lc "a")
        )
      )
      (binop_colon
        (lc "bar")
        (uc "Something")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "SomeMultiline")
      (lc "a")
    )
    (record_literal
      (binop_colon
        (lc "foo")
        (apply_uc
          (uc "Ok")
          (lc "a")
        )
      )
      (binop_colon
        (lc "bar")
        (uc "Something")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
    )
  )
  (binop_colon
    (apply_uc
      (uc "MaybeMultiline")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
    )
  )
  (binop_colon
    (apply_uc
      (uc "SomeFunc")
      (lc "a")
    )
    (binop_thin_arrow
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
      (binop_thin_arrow
        (lc "a")
        (apply_uc
          (uc "Maybe")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "add_one_oneline")
    (lambda
      (body
        (if_else
          (condition             (lc "num")
)
          (then             (num_literal_i32 2)
)
          (else             (num_literal_i32 5)
))
      )
      (args
        (lc "num")
      )
    )
  )
  (binop_colon
    (lc "add_one")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "add_one")
    (lambda
      (body
        (block
          (binop_equals
            (lc "other")
            (num_literal_i32 1)
          )
          (if_else
            (condition               (lc "num")
)
            (then               (block
                (malformed malformed:expr_unexpected_token)
                (apply_lc
                  (lc "some_func")
                )
                (num_literal_i32 0)
              )
)
            (else               (block
                (malformed malformed:expr_unexpected_token)
                (num_literal_i32 123)
                (binop_colon
                  (lc "other")
                  (lc "other")
                )
              )
))
        )
      )
      (args
        (lc "num")
      )
    )
  )
  (binop_equals
    (lc "match_time")
    (lambda
      (body
        (match
          (scrutinee             (lc "a")
))
      )
      (args
        (lc "a")
        (lc "b")
      )
    )
  )
  (uc "Green")
  (malformed malformed:expr_unexpected_token)
  (block
    (binop_equals
      (lc "x")
      (num_literal_i32 12)
    )
    (lc "x")
  )
  (uc "Blue")
  (lambda
    (body
      (binop_thick_arrow
        (uc "Red")
        (block
          (binop_equals
            (lc "x")
            (num_literal_i32 12)
          )
          (binop_colon
            (lc "x")
            (lc "x")
          )
        )
      )
    )
    (args
      (uc "Green")
    )
  )
  (lc "lower")
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 1)
  (str_literal_small "foo")
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 100)
  (str_literal_small "foo")
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 200)
  (list_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
    (num_literal_i32 3)
    (unary_double_dot <unary>)
  )
  (lc "rest")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (list_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (list_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (frac_literal_small 3.14)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 314)
  (frac_literal_small 3.14)
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (num_literal_i32 314)
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (num_literal_i32 123)
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (record_literal
    (binop_colon
      (lc "foo")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "bar")
      (num_literal_i32 2)
    )
    (double_dot_lc "rest")
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (malformed malformed:expr_unexpected_token)
  (apply_lc
    (lc "add")
    (num_literal_i32 34)
  )
  (record_literal
    (binop_colon
      (lc "foo")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "bar")
      (num_literal_i32 2)
    )
    (double_dot_lc "rest")
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (record_literal
    (binop_colon
      (lc "foo")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "bar")
      (num_literal_i32 2)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (record_literal
    (binop_colon
      (lc "foo")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "bar")
      (num_literal_i32 2)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (apply_uc
    (uc "Ok")
    (num_literal_i32 123)
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (apply_uc
    (uc "Ok")
    (apply_uc
      (uc "Some")
      (lc "dude")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (lc "dude")
  (apply_uc
    (uc "TwoArgs")
    (tuple_literal
      (str_literal_big "hello")
      (apply_uc
        (uc "Some")
        (str_literal_big "world")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 1000)
  (malformed malformed:expr_unexpected_token)
  (expect
    (binop_double_equals
      (lc "blah")
      (num_literal_i32 1)
    )
  )
  (binop_colon
    (not_lc "main")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (uc "String")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (record_literal)
          (underscore)
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal
          (binop_equals
            (lc "world")
            (str_literal_big "World")
          )
          (binop_equals
            (var_lc "number")
            (num_literal_i32 123)
          )
          (expect
            (binop_double_equals
              (lc "blah")
              (num_literal_i32 1)
            )
          )
          (binop_equals
            (lc "tag")
            (uc "Blue")
          )
          (ret <statement>)
          (ellipsis)
          (apply_lc
            (lc "match_time")
            (tuple_literal
              (ellipsis)
              (malformed malformed:expr_unexpected_token)
            )
          )
          (apply_lc
            (lc "some_func")
            (malformed malformed:expr_unexpected_token)
          )
          (num_literal_i32 42)
          (malformed malformed:expr_unexpected_token)
        )
      )
      (args
        (underscore)
      )
    )
  )
  (crash <statement>)
  (binop_equals
    (lc "tag_with_payload")
    (apply_uc
      (uc "Ok")
      (lc "number")
    )
  )
  (binop_equals
    (lc "interpolated")
    (str_literal_big "Hello, ${world}")
  )
  (binop_equals
    (lc "list")
    (list_literal
      (apply_lc
        (lc "add_one")
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
  (lc "number")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 456)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 789)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (for_loop
    (lc "n")
    (lc "list")
    (block
      (apply_anon
        (binop_pipe
          (uc "Stdout")
          (not_lc "line")
        )
        (str_literal_big "Adding ${n} to ${number}")
      )
      (binop_equals
        (lc "number")
        (binop_plus
          (lc "number")
          (lc "n")
        )
      )
    )
  )
  (binop_equals
    (lc "record")
    (record_literal
      (binop_colon
        (lc "foo")
        (num_literal_i32 123)
      )
      (binop_colon
        (lc "bar")
        (str_literal_big "Hello")
      )
      (binop_colon
        (lc "baz")
        (lc "tag")
      )
      (binop_colon
        (lc "qux")
        (apply_uc
          (uc "Ok")
          (lc "world")
        )
      )
      (lc "punned")
    )
  )
  (binop_equals
    (lc "tuple")
    (tuple_literal
      (num_literal_i32 123)
      (str_literal_big "World")
      (lc "tag")
      (apply_uc
        (uc "Ok")
        (lc "world")
      )
      (tuple_literal
        (lc "nested")
        (lc "tuple")
      )
      (list_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
    )
  )
  (binop_equals
    (lc "multiline_tuple")
    (tuple_literal
      (num_literal_i32 123)
      (str_literal_big "World")
      (lc "tag1")
      (apply_uc
        (uc "Ok")
        (lc "world")
      )
      (tuple_literal
        (lc "nested")
        (lc "tuple")
      )
      (list_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
  (binop_equals
    (lc "bin_op_result")
    (binop_or
      (binop_or
        (binop_gt
          (binop_double_question
            (apply_uc
              (uc "Err")
              (lc "foo")
            )
            (num_literal_i32 12)
          )
          (binop_star
            (num_literal_i32 5)
            (num_literal_i32 5)
          )
        )
        (binop_and
          (binop_lt
            (binop_plus
              (num_literal_i32 13)
              (num_literal_i32 2)
            )
            (num_literal_i32 5)
          )
          (binop_gte
            (binop_minus
              (num_literal_i32 10)
              (num_literal_i32 1)
            )
            (num_literal_i32 16)
          )
        )
      )
      (binop_lte
        (num_literal_i32 12)
        (binop_slash
          (num_literal_i32 3)
          (num_literal_i32 5)
        )
      )
    )
  )
  (binop_equals
    (lc "static_dispatch_style")
    (apply_lc
      (lc "some_fn")
      (lc "arg1")
    )
  )
  (apply_anon
    (binop_pipe
      (malformed malformed:expr_unexpected_token)
      (dot_lc "static_dispatch_method")
    )
  )
  (apply_anon
    (binop_pipe
      (malformed malformed:expr_unexpected_token)
      (dot_lc "next_static_dispatch_method")
    )
  )
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (dot_lc "record_field")
  )
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (binop_pipe
      (uc "Stdout")
      (not_lc "line")
    )
    (lc "interpolated")
  )
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (binop_pipe
      (uc "Stdout")
      (not_lc "line")
    )
    (malformed malformed:expr_unexpected_token)
  )
  (apply_anon
    (binop_pipe
      (uc "Num")
      (dot_lc "toStr")
    )
    (lc "number")
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (lc "a")
  (lc "string")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "empty")
    (record_literal)
  )
  (binop_equals
    (lc "empty")
    (record_literal)
  )
  (binop_colon
    (lc "tuple")
    (apply_uc
      (uc "Value")
      (tuple_literal
        (lc "a")
        (lc "b")
        (lc "c")
      )
    )
  )
  (expect
    (block
      (binop_equals
        (lc "foo")
        (num_literal_i32 1)
      )
      (binop_equals
        (lc "blah")
        (num_literal_i32 1)
      )
      (binop_double_equals
        (lc "blah")
        (lc "foo")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
# This is a module comment!

app {
	pf : "../basic-cli/platform.roc"
	platform 
	[main!]
}
import pf.Stdout exposing [line, write]
import pf.StdoutMultiline exposing [
	line,
	write,
]
import pkg.Something exposing [func]
as function, Type
as ValueCategory, 
.*]

import BadName as GoodName
import BadNameMultiline as GoodNameMultiline
Map((a, b)) : List a -> (a -> b) -> List b
MapML(
	(
		a,
		b,
	) :
		List (a -> ) -> (a -> b) -> List(b, ) # And after the type decl

)),
)
Foo : (Bar, Baz)
FooMultiline : (
	Bar,
	Baz,
)
Some(a) : {foo : Ok a, bar : Something}
SomeMl(a) :
	{
		foo : Ok a,
		bar : Something
	}
SomeMultiline(a) :
	{
		foo : Ok a,
		bar : Something
	}
Maybe(a) : [Some(a), None]
MaybeMultiline(a) : [
	Some(a),
	None,
]
SomeFunc(a) : Maybe a -> a -> Maybe a
add_one_oneline = |num| if num 2 else 5
add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num
		{
			dbg # After debug
			
			some_func()
			0
		}
	else {
		dbg 
		123
		other : other
	}
}
match_time = |a, b| match a
Green
=> {
	x = 12
	x
}
Blue
|Green| Red => {
	x = 12
	x : x
}
lower
=> 1
"foo"
=> # After arrow comment
			100
"foo"
=> 200
[1, 2, 3, ..as ]rest] # After pattern comment
			=> # After arrow comment
				123
[1, 2]
rest] => 123
[1, 2]
rest, # After last pattern in list
		] => 123
3.14
=> 314
3.14
=> 314((1, 2, 3))
=> 123((1, 2))
) => 123
{ foo : 1, bar : 2, ..rest }
=> 12-->add(34)
{
	foo : 1,
	bar : 2,
	..rest,
}
=> 12
{ foo : 1, bar : 2 }
} => 12
{ foo : 1, bar : 2 }
=> 12
Ok(123)
=> 123
Ok(Some(dude))
=> dude
TwoArgs(("hello", Some("world")))
=> 1000
}

expect blah == 1
main! : List String -> Result({}, _)
main! = |
	_,
| {
	world = "World",
	var number = 123,
	expect blah == 1,
	tag = Blue,
	return tag,
	...,
	match_time(
		(
			...,
		),
	),
	some_func(dbg # After debug
			),
	42,
}
crash "Unreachable!"
tag_with_payload = Ok(number)
interpolated = "Hello, ${world}"
list = [
	add_one(dbg # After dbg in list
				),
]
number, # after dbg expr as arg
		), # Comment one
		456, # Comment two
		789, # Comment three
	]
	for n in list {
	{
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo : 123, bar : "Hello", baz : tag, qux : Ok world, punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
multiline_tuple = (
	123,
	"World",
	tag1,
	Ok(world),
	(nested, tuple),
	[1, 2, 3],
)
bin_op_result = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
static_dispatch_style = some_fn(arg1)
? | .static_dispatch_method()? | .next_static_dispatch_method()? | .record_field?
	Stdout.line!(interpolated)?
	Stdout.line!("How about ${ # Comment after string interpolation open
			)Num.toStr(number)
} as a
string?",
	)
} # Comment after top-level decl

empty : {}
empty = {  }
tuple : Value(a, b, c)
expect {
	foo = 1
	blah = 1
	blah == foo
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:28 to 2:1

**Parse Error**
at 2:1 to 2:5

**Parse Error**
at 2:39 to 2:48

**Parse Error**
at 14:37 to 14:40

**Parse Error**
at 14:48 to 14:50

**Parse Error**
at 14:55 to 14:58

**Parse Error**
at 14:71 to 14:73

**Parse Error**
at 14:79 to 14:81

**Parse Error**
at 14:81 to 16:1

**Parse Error**
at 26:1 to 27:2

**Parse Error**
at 30:3 to 30:4

**Parse Error**
at 34:4 to 36:1

**Parse Error**
at 32:4 to 36:1

**Parse Error**
at 28:3 to 36:1

**Parse Error**
at 23:1 to 36:1

**Parse Error**
at 41:1 to 43:1

**Parse Error**
at 43:1 to 43:1

**Parse Error**
at 71:3 to 72:4

**Parse Error**
at 75:3 to 75:7

**Parse Error**
at 85:3 to 85:10

**Parse Error**
at 85:22 to 85:25

**Parse Error**
at 98:4 to 98:7

**Parse Error**
at 99:9 to 100:4

**Parse Error**
at 101:17 to 101:20

**Parse Error**
at 102:16 to 102:19

**Parse Error**
at 102:3 to 102:19

**Parse Error**
at 102:23 to 103:4

**Parse Error**
at 103:4 to 104:5

**Parse Error**
at 108:3 to 108:9

**Parse Error**
at 108:20 to 108:23

**Parse Error**
at 108:23 to 108:27

**Parse Error**
at 108:27 to 108:29

**Parse Error**
at 108:29 to 108:32

**Parse Error**
at 109:3 to 111:6

**Parse Error**
at 114:5 to 115:6

**Parse Error**
at 115:6 to 115:10

**Parse Error**
at 115:10 to 116:3

**Parse Error**
at 116:3 to 116:5

**Parse Error**
at 116:5 to 116:8

**Parse Error**
at 117:8 to 117:11

**Parse Error**
at 118:15 to 118:18

**Parse Error**
at 119:13 to 119:16

**Parse Error**
at 119:16 to 120:9

**Parse Error**
at 120:15 to 120:17

**Parse Error**
at 120:17 to 120:20

**Parse Error**
at 121:30 to 121:33

**Parse Error**
at 121:35 to 121:37

**Parse Error**
at 129:5 to 129:8

**Parse Error**
at 130:3 to 130:20

**Parse Error**
at 130:24 to 130:26

**Parse Error**
at 130:26 to 130:29

**Parse Error**
at 131:3 to 133:11

**Parse Error**
at 134:3 to 134:5

**Parse Error**
at 134:5 to 134:8

**Parse Error**
at 135:11 to 135:14

**Parse Error**
at 136:18 to 136:21

**Parse Error**
at 137:35 to 137:38

**Parse Error**
at 138:2 to 140:1

**Parse Error**
at 157:2 to 158:2

**Parse Error**
at 155:2 to 158:2

**Parse Error**
at 159:3 to 160:4

**Parse Error**
at 158:2 to 160:4

**Parse Error**
at 161:2 to 162:2

**Parse Error**
at 144:13 to 162:2

**Parse Error**
at 168:4 to 169:5

**Parse Error**
at 167:3 to 169:5

**Parse Error**
at 166:9 to 169:5

**Parse Error**
at 169:11 to 170:3

**Parse Error**
at 170:3 to 170:4

**Parse Error**
at 170:4 to 171:3

**Parse Error**
at 171:6 to 172:3

**Parse Error**
at 172:6 to 173:2

**Parse Error**
at 173:2 to 174:2

**Parse Error**
at 187:2 to 188:2

**Parse Error**
at 188:2 to 188:2

**Parse Error**
at 189:39 to 189:40

**Parse Error**
at 189:65 to 189:66

**Parse Error**
at 189:96 to 189:97

**Parse Error**
at 189:110 to 190:2

**Parse Error**
at 190:28 to 191:2

**Parse Error**
at 192:3 to 193:4

**Parse Error**
at 191:2 to 193:4

**Parse Error**
at 194:3 to 194:5

**Parse Error**
at 194:5 to 194:8

**Parse Error**
at 194:16 to 194:17

**Parse Error**
at 194:17 to 195:2

**Parse Error**
at 195:2 to 196:1

**Parse Error**
at 196:1 to 198:1

**Unsupported Node**
at 4:1 to 4:42

**Unsupported Node**
at 6:1 to 12:4

**Unsupported Node**
at 14:1 to 14:36

**Unsupported Node**
at 16:1 to 16:27

**Unsupported Node**
at 17:1 to 20:20

**Unsupported Node**
at 92:5 to 92:8

**Unsupported Node**
at 91:4 to 91:9

**Pattern in Expression Context**
at 143:36 to 143:37

**Unsupported Node**
at 175:3 to 175:9

**Unsupported Node**
at 189:39 to 189:40

**Unsupported Node**
at 189:65 to 189:66

**Unsupported Node**
at 189:96 to 189:97

**Unsupported Node**
at 190:2 to 190:8

**Unsupported Node**
at 191:2 to 191:8

**Unsupported Node**
at 193:4 to 193:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.str_literal_big)
    )
    (Expr.malformed)
    (Expr.list_literal)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "function")
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.lookup "b")
        )
        (Expr.apply_tag)
      )
    )
  )
  (Expr.apply_tag)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.apply_tag)
      (Expr.apply_tag)
      (Expr.malformed)
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.lookup "a")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "add_one_oneline")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "add_one")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "add_one")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "match_time")
    (Expr.lambda)
  )
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.block
    (Expr.binop_equals
      (Expr.lookup "x")
      (Expr.num_literal_i32 12)
    )
    (Expr.lookup "x")
  )
  (Expr.apply_tag)
  (Expr.lambda)
  (Expr.lookup "lower")
  (Expr.malformed)
  (Expr.num_literal_i32 1)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.num_literal_i32 100)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.num_literal_i32 200)
  (Expr.list_literal)
  (Expr.lookup "rest")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 123)
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 123)
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 123)
  (Expr.frac_literal_small 3.14)
  (Expr.malformed)
  (Expr.num_literal_i32 314)
  (Expr.frac_literal_small 3.14)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 123)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "foo")
      (Expr.num_literal_i32 1)
    )
    (Expr.binop_colon
      (Expr.lookup "bar")
      (Expr.num_literal_i32 2)
    )
    (Expr.unary_double_dot)
  )
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "foo")
      (Expr.num_literal_i32 1)
    )
    (Expr.binop_colon
      (Expr.lookup "bar")
      (Expr.num_literal_i32 2)
    )
    (Expr.unary_double_dot)
  )
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "foo")
      (Expr.num_literal_i32 1)
    )
    (Expr.binop_colon
      (Expr.lookup "bar")
      (Expr.num_literal_i32 2)
    )
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "foo")
      (Expr.num_literal_i32 1)
    )
    (Expr.binop_colon
      (Expr.lookup "bar")
      (Expr.num_literal_i32 2)
    )
  )
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.num_literal_i32 123)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.lookup "dude")
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.num_literal_i32 1000)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
  (Expr.crash
    (Expr.str_literal_big)
  )
  (Expr.binop_equals
    (Expr.lookup "tag_with_payload")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "interpolated")
    (Expr.str_literal_big)
  )
  (Expr.binop_equals
    (Expr.lookup "list")
    (Expr.list_literal)
  )
  (Expr.lookup "number")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 456)
  (Expr.malformed)
  (Expr.num_literal_i32 789)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.block
    (Expr.malformed)
    (Expr.lookup "list")
    (Expr.block
      (Expr.apply_ident)
      (Expr.binop_equals
        (Expr.lookup "number")
        (Expr.binop_plus
          (Expr.lookup "number")
          (Expr.lookup "n")
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "record")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.num_literal_i32 123)
      )
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "baz")
        (Expr.lookup "tag")
      )
      (Expr.binop_colon
        (Expr.lookup "qux")
        (Expr.apply_tag)
      )
      (Expr.lookup "punned")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "tuple")
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag")
      (Expr.apply_tag)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "multiline_tuple")
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag1")
      (Expr.apply_tag)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "bin_op_result")
    (Expr.binop_or
      (Expr.binop_or
        (Expr.binop_gt
          (Expr.binop_double_question
            (Expr.apply_tag)
            (Expr.num_literal_i32 12)
          )
          (Expr.binop_star
            (Expr.num_literal_i32 5)
            (Expr.num_literal_i32 5)
          )
        )
        (Expr.binop_and
          (Expr.binop_lt
            (Expr.binop_plus
              (Expr.num_literal_i32 13)
              (Expr.num_literal_i32 2)
            )
            (Expr.num_literal_i32 5)
          )
          (Expr.binop_gte
            (Expr.binop_minus
              (Expr.num_literal_i32 10)
              (Expr.num_literal_i32 1)
            )
            (Expr.num_literal_i32 16)
          )
        )
      )
      (Expr.binop_lte
        (Expr.num_literal_i32 12)
        (Expr.binop_slash
          (Expr.num_literal_i32 3)
          (Expr.num_literal_i32 5)
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "static_dispatch_style")
    (Expr.apply_ident)
  )
  (Expr.apply_ident)
  (Expr.apply_ident)
  (Expr.lambda)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "empty")
    (Expr.record_literal
    )
  )
  (Expr.binop_equals
    (Expr.lookup "empty")
    (Expr.record_literal
    )
  )
  (Expr.binop_colon
    (Expr.lookup "tuple")
    (Expr.apply_tag)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
add_one_oneline : _d
add_one : _d
match_time : _d
tag_with_payload : []_others
interpolated : Str
list : List(_elem)
record : {}
tuple : _d
multiline_tuple : _d
bin_op_result : [True, False]_others
static_dispatch_style : _d
empty : {}
~~~
