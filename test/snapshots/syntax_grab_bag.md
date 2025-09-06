# META
~~~ini
description=A grab bag of all v0.1 syntax, heavily commented to show multiline formatting
type=file
~~~
# SOURCE
~~~roc
# This is a module comment!
app [main!] { pf: platform "../basic-cli/platform.roc" }

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
LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare BlankLine KwImport LineComment LowerIdent LineComment Dot UpperIdent LineComment KwExposing OpenSquare LineComment LowerIdent OpBang Comma LineComment LowerIdent OpBang Comma LineComment CloseSquare LineComment BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent KwAs UpperIdent Comma UpperIdent Dot OpStar CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent KwAs UpperIdent BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment LowerIdent Comma LineComment LowerIdent Comma CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment LowerIdent Comma LineComment CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LineComment LowerIdent Comma CloseRound LineComment BlankLine UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine UpperIdent OpColon OpenRound LineComment UpperIdent Comma LineComment UpperIdent Comma LineComment CloseRound LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LineComment LowerIdent OpColon UpperIdent Comma LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent LineComment OpColon LineComment UpperIdent OpenRound LowerIdent CloseRound Comma LineComment LowerIdent OpColon UpperIdent Comma LineComment CloseCurly LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare LineComment UpperIdent OpenRound LowerIdent CloseRound Comma LineComment UpperIdent Comma LineComment CloseSquare LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg LineComment LowerIdent OpenRound CloseRound LineComment Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment LowerIdent Comma OpBar LineComment KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly UpperIdent LineComment OpBar LineComment UpperIdent OpBar UpperIdent LineComment OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly LowerIdent LineComment OpFatArrow Int String OpFatArrow LineComment Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare LineComment OpFatArrow LineComment Int LineComment BlankLine LineComment BlankLine OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot LineComment KwAs LineComment LowerIdent Comma LineComment CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LineComment LowerIdent LineComment OpColon LineComment Int Comma LineComment LowerIdent OpColon Int Comma DoubleDot LineComment LowerIdent Comma LineComment CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int Comma LineComment CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound String Comma UpperIdent OpenRound String CloseRound CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent OpEquals Int LineComment BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LineComment LowerIdent LineComment BlankLine LineComment BlankLine TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound LowerIdent OpenRound KwDbg LineComment Int Comma LineComment CloseRound KwCrash LineComment String LineComment LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LineComment LowerIdent Comma LineComment CloseRound Comma LineComment Int Comma LineComment Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly KwAs LowerIdent LowerIdent OpQuestion MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (import
    (binop_exposing
      (binop_dot
        (lc "pf")
        (uc "Stdout")
      )
      (list_literal
        (not_lc "line")
        (not_lc "write")
      )
    )
  )
  (import
    (binop_exposing
      (binop_dot
        (lc "pf")
        (uc "StdoutMultiline")
      )
      (list_literal
        (not_lc "line")
        (not_lc "write")
      )
    )
  )
  (import
    (binop_exposing
      (binop_dot
        (lc "pkg")
        (uc "Something")
      )
      (list_literal
        (lc "func")
      )
    )
  )
  (malformed)
  (lc "function")
  (malformed)
  (uc "Type")
  (malformed)
  (uc "ValueCategory")
  (malformed)
  (malformed)
  (malformed)
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
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_arrow_call
        (binop_arrow_call
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
  (binop_colon
    (apply_uc
      (uc "MapML")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_arrow_call
        (binop_arrow_call
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
    (binop_arrow_call
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
      (binop_arrow_call
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
    (binop_arrow_call
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
                (malformed)
                (apply_lc
                  (lc "some_func")
                )
                (num_literal_i32 0)
              )
)
            (else               (block
                (malformed)
                (num_literal_i32 123)
                (lc "other")
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
)
          (branch1             (binop_thick_arrow
              (binop_or
                (binop_or
                  (uc "Blue")
                  (uc "Green")
                )
                (uc "Red")
              )
              (block
                (binop_equals
                  (lc "x")
                  (num_literal_i32 12)
                )
                (lc "x")
              )
            )
)
          (branch2             (binop_thick_arrow
              (binop_or
                (binop_or
                  (uc "Blue")
                  (uc "Green")
                )
                (uc "Red")
              )
              (block
                (block
                  (binop_equals
                    (lc "x")
                    (num_literal_i32 12)
                  )
                  (lc "x")
                )
                (binop_thick_arrow
                  (lc "lower")
                  (num_literal_i32 1)
                )
                (binop_thick_arrow
                  (str_literal_small "foo")
                  (num_literal_i32 100)
                )
                (str_literal_small "foo")
                (malformed)
                (num_literal_i32 200)
                (list_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                  (num_literal_i32 3)
                  (unary_double_dot <unary_op>)
                )
                (lc "rest")
                (binop_thick_arrow
                  (malformed)
                  (num_literal_i32 123)
                )
                (list_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                )
                (malformed)
                (binop_thick_arrow
                  (malformed)
                  (num_literal_i32 123)
                )
                (list_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                )
                (malformed)
                (binop_thick_arrow
                  (malformed)
                  (num_literal_i32 123)
                )
                (binop_thick_arrow
                  (frac_literal_small 3.14)
                  (num_literal_i32 314)
                )
                (frac_literal_small 3.14)
                (malformed)
                (apply_anon
                  (num_literal_i32 123)
                  (tuple_literal
                    (num_literal_i32 1)
                    (num_literal_i32 2)
                  )
                )
                (binop_thick_arrow
                  (malformed)
                  (num_literal_i32 123)
                )
                (binop_arrow_call
                  (binop_thick_arrow
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
                    (num_literal_i32 12)
                  )
                  (apply_lc
                    (lc "add")
                    (num_literal_i32 34)
                  )
                )
                (binop_thick_arrow
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
                  (num_literal_i32 12)
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
                )
                (binop_thick_arrow
                  (malformed)
                  (num_literal_i32 12)
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
                )
                (malformed)
                (num_literal_i32 12)
              )
            )
)
          (branch3             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (num_literal_i32 123)
              )
              (num_literal_i32 123)
            )
)
          (branch4             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (apply_uc
                  (uc "Some")
                  (lc "dude")
                )
              )
              (lc "dude")
            )
)
          (branch5             (binop_thick_arrow
              (apply_uc
                (uc "TwoArgs")
                (str_literal_big "hello")
                (apply_uc
                  (uc "Some")
                  (str_literal_big "world")
                )
              )
              (num_literal_i32 1000)
            )
))
      )
      (args
        (lc "a")
        (lc "b")
      )
    )
  )
  (expect
    (binop_double_equals
      (lc "blah")
      (num_literal_i32 1)
    )
  )
  (binop_colon
    (not_lc "main")
    (binop_arrow_call
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
          (ret
            (lc "tag")
          )
          (ellipsis)
          (apply_lc
            (lc "match_time")
            (ellipsis)
          )
          (apply_lc
            (lc "some_func")
            (malformed)
          )
          (num_literal_i32 42)
          (malformed)
        )
      )
      (args
        (underscore)
      )
    )
  )
  (crash
    (str_literal_big "Unreachable!")
  )
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
        (malformed)
      )
    )
  )
  (lc "number")
  (malformed)
  (malformed)
  (malformed)
  (num_literal_i32 456)
  (malformed)
  (num_literal_i32 789)
  (malformed)
  (malformed)
  (for_loop
    (lc "n")
    (lc "list")
    (block
      (apply_anon
        (binop_dot
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
      (binop_colon
        (lc "punned")
        (lc "punned")
      )
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
    (binop_dot
      (malformed)
      (dot_lc "static_dispatch_method")
    )
  )
  (apply_anon
    (binop_dot
      (malformed)
      (dot_lc "next_static_dispatch_method")
    )
  )
  (binop_dot
    (malformed)
    (dot_lc "record_field")
  )
  (malformed)
  (apply_anon
    (binop_dot
      (uc "Stdout")
      (not_lc "line")
    )
    (lc "interpolated")
  )
  (malformed)
  (apply_anon
    (binop_dot
      (uc "Stdout")
      (not_lc "line")
    )
    (malformed)
  )
  (apply_anon
    (binop_dot
      (uc "Num")
      (dot_lc "toStr")
    )
    (lc "number")
  )
  (malformed)
  (malformed)
  (lc "a")
  (lc "string")
  (malformed)
  (malformed)
  (malformed)
  (malformed)
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
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout exposing [line!, write!]
import # Comment after import keyword
pf. # Comment after qualifier
StdoutMultiline exposing [ # Comment after ident
	# Comment after exposing open
	line!, # Comment after exposed item
	write!, # Another after exposed item
]
# Comment after exposing close
import pkg.Something exposing [func]
as 
function
, 
Type
as 
ValueCategory
, 
.*
]

import BadName as GoodName
import BadNameMultiline as GoodNameMultiline
Map((a, b)) : List a -> (a -> b) -> List b
# Comment here
# And here
# And after the last arg
MapML(
	(
		a,
		b,
	),
) : # And after the colon
List # Inside Tag args
a -> ( # After tag arg
a -> b) -> # After arrow
List # Inside tag args
b
# And after the type decl
Foo : (Bar, Baz)
FooMultiline : # Comment after pattern tuple open
(
	Bar, # Comment after pattern tuple item
	Baz,
)
# Another after pattern tuple item
# Comment after pattern tuple close
Some(a) : {foo: Ok(a), bar: Something}
SomeMl(a) :
	{
		 # After record open
foo: Ok(a),
		 # After field
bar: Something
	}
# After last field
SomeMultiline(a) :
	{
		 # Comment after pattern record open
foo: # After field name
		# Before field anno
Ok(a),
		 # Comment after pattern record field
bar: Something
	}
# Another after pattern record field
# Comment after pattern record close
Maybe(a) : [Some(a), None]
MaybeMultiline(a) : [ # Comment after tag union open
	Some(a), # Comment after tag union member
	None, # Another after tag union member
]
# Comment after tag union close
SomeFunc(a) : Maybe a -> a -> Maybe a
add_one_oneline = |num| if num 2 else 5
add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num
		{
			dbg # After debug
			
			some_func()
			# After debug expr
			0
		}
	else {
		dbg 
		123
		other
	}
}

match_time = |a, # After arg
b| # After args
match a
	(Blue || Green) || Red => 
		x = 12
		x
	(Blue || # After pattern in alt
	# Before pattern in alt
Green) || Red => 
		 # After alt pattern
{
			x = 12
			x
		}
		lower => # After pattern comment
1
		"foo" => # After arrow comment
100
		"foo"
		=> 
		200
		[1, 2, 3, ..as ]
		rest
		] # After pattern comment
			 => # After arrow comment
123
		 # After branch comment
		# Just a random comment
[1, 2]
		rest
		] => 123
		[1, 2]
		 # After DoubleDot
		# Before alias
rest
		 # After last pattern in list
] => 123
		3.14 => 314
		3.14
		=> 
		123((1, 2))
		) => 123
		({ foo: 1, bar: 2, ..rest } => 12-) -> add(34)
		{ # After pattern record open
			foo: # After pattern record field name
			# Before pattern record field value
1, # After pattern record field
			bar: 2,
			..rest, # After spread operator
		# After last field
		} => 12
		{ foo: 1, bar: 2 }
		} => 12
		{ foo: 1, bar: 2 }
		 # After last record field
=> 
		12
	Ok(123) => 123
	Ok(Some(dude)) => dude
	TwoArgs("hello", Some("world")) => 1000

expect # Comment after expect keyword
blah == 1
# Comment after expect statement
main! : List String -> Result({}, _)
main! = |
	_,
| { # Yeah I can leave a comment here
	world = "World",
	var number = 123,
	expect blah == 1,
	tag = Blue,
	return # Comment after return keyword
tag, # Comment after return statement
	# Just a random comment!
	...,
	match_time(
		...,
	), # Single args with comment
	some_func(dbg # After debug
			),
	42, # After debug expr
}

crash # Comment after crash keyword
"Unreachable!"
# Comment after crash statement
tag_with_payload = Ok(number)
interpolated = "Hello, ${world}"
list = [
	add_one(dbg # After dbg in list
				),
]

number
, # after dbg expr as arg
		
)
, # Comment one
		
456
, # Comment two
		
789
, # Comment three
	
]
	
for n in list {
	{
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned: punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
multiline_tuple = (
	123,
	"World",
	tag1,
	Ok(world), # This one has a comment
	(nested, tuple),
	[1, 2, 3],
)

bin_op_result = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
static_dispatch_style = some_fn(arg1)
?..static_dispatch_method()
?..next_static_dispatch_method()
?..record_field
?
	
Stdout.line!(interpolated)
?
	
Stdout.line!("How about ${ # Comment after string interpolation open
			)
Num..toStr(number)
# Comment after string interpolation expr
} 
as 
a
string
?
",
	
)
} # Comment after top-level decl

empty : {}
empty = {}
tuple : Value(a, b, c)
expect {
	foo = 1
	# This should work too
	blah = 1
	blah == foo
}
~~~
# EXPECTED
UNDECLARED TYPE - syntax_grab_bag.md:36:8:36:11
UNDECLARED TYPE - syntax_grab_bag.md:36:13:36:16
UNDECLARED TYPE - syntax_grab_bag.md:39:2:39:5
UNDECLARED TYPE - syntax_grab_bag.md:40:2:40:5
UNDECLARED TYPE - syntax_grab_bag.md:43:19:43:21
UNDECLARED TYPE - syntax_grab_bag.md:43:32:43:41
UNDECLARED TYPE - syntax_grab_bag.md:45:8:45:10
UNDECLARED TYPE - syntax_grab_bag.md:46:8:46:17
UNDECLARED TYPE - syntax_grab_bag.md:52:4:52:6
UNDECLARED TYPE - syntax_grab_bag.md:53:8:53:17
MODULE NOT FOUND - syntax_grab_bag.md:4:1:4:42
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
MODULE NOT FOUND - syntax_grab_bag.md:6:1:12:4
MODULE NOT FOUND - syntax_grab_bag.md:14:1:14:82
MODULE NOT FOUND - syntax_grab_bag.md:16:1:16:27
MODULE NOT FOUND - syntax_grab_bag.md:17:1:20:20
UNDEFINED VARIABLE - syntax_grab_bag.md:72:4:72:13
UNUSED VARIABLE - syntax_grab_bag.md:97:3:97:8
UNUSED VARIABLE - syntax_grab_bag.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - syntax_grab_bag.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - syntax_grab_bag.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - syntax_grab_bag.md:121:21:121:27
UNUSED VARIABLE - syntax_grab_bag.md:127:4:128:9
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - syntax_grab_bag.md:82:2:82:3
UNDEFINED VARIABLE - syntax_grab_bag.md:141:2:141:6
UNDECLARED TYPE - syntax_grab_bag.md:143:14:143:20
UNDEFINED VARIABLE - syntax_grab_bag.md:147:9:147:13
UNDEFINED VARIABLE - syntax_grab_bag.md:158:2:158:11
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - syntax_grab_bag.md:178:63:178:69
UNDEFINED VARIABLE - syntax_grab_bag.md:179:42:179:48
UNDEFINED VARIABLE - syntax_grab_bag.md:183:3:183:7
UNDEFINED VARIABLE - syntax_grab_bag.md:185:4:185:10
UNDEFINED VARIABLE - syntax_grab_bag.md:188:22:188:25
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - syntax_grab_bag.md:193:4:193:13
UNUSED VARIABLE - syntax_grab_bag.md:164:2:164:18
UNUSED VARIABLE - syntax_grab_bag.md:165:2:165:14
UNUSED VARIABLE - syntax_grab_bag.md:166:2:166:6
UNUSED VARIABLE - syntax_grab_bag.md:178:2:178:8
UNUSED VARIABLE - syntax_grab_bag.md:180:2:180:17
UNUSED VARIABLE - syntax_grab_bag.md:188:2:188:15
UNUSED VARIABLE - syntax_grab_bag.md:189:2:189:23
UNDECLARED TYPE - syntax_grab_bag.md:201:9:201:14
INVALID IF CONDITION - syntax_grab_bag.md:70:5:70:5
INCOMPATIBLE MATCH PATTERNS - syntax_grab_bag.md:84:2:84:2
TYPE MISMATCH - syntax_grab_bag.md:155:2:157:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:37:14:40:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:48:14:50:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:55:14:58:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:71:14:73:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                      ^^


**PARSE ERROR**
A parsing error occurred: **expr_dot_suffix_not_allowed**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:14:79:14:81:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:81:16:1:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]

import BadName as GoodName
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After debug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:71:3:72:4:**
```roc
		dbg # After debug
			some_func() # After debug expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:75:3:75:7:**
```roc
		dbg 123
```
		^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:101:17:101:20:**
```roc
		"foo" | "bar" => 200
```
		              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:102:16:102:19:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		             ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:102:3:102:19:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] # After pattern comment
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:102:23:103:4:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:108:3:108:9:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:20:108:23:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:23:108:27:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:27:108:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:109:3:111:6:**
```roc
		[
			1,
			2 | 5,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as # Before alias
					** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:114:5:115:6:**
```roc
				as # Before alias
					rest, # After last pattern in list
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:115:6:115:10:**
```roc
					rest, # After last pattern in list
```
					^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:116:3:116:5:**
```roc
		] => 123
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:118:15:118:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:118:21:119:3:**
```roc
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:119:19:120:3:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:119:16:120:9:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:120:15:120:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:130:3:130:20:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:130:24:130:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:131:3:133:11:**
```roc
		{
			foo: 1,
			bar: 2 | 7, # After last record field
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:134:3:134:5:**
```roc
		} => 12
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:134:5:134:8:**
```roc
		} => 12
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After debug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:159:3:160:4:**
```roc
		dbg # After debug
			42, # After debug expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:158:2:160:4:**
```roc
	some_func(
		dbg # After debug
			42, # After debug expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:161:2:162:2:**
```roc
	)
	crash # Comment after crash keyword
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:144:13:162:2:**
```roc
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
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After dbg in list
				** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:168:4:169:5:**
```roc
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:167:3:169:5:**
```roc
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:166:9:169:5:**
```roc
	list = [
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # after dbg expr as arg
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:169:11:170:3:**
```roc
				number, # after dbg expr as arg
		), # Comment one
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:170:3:170:4:**
```roc
		), # Comment one
```
		^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment one
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:170:4:171:3:**
```roc
		), # Comment one
		456, # Comment two
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment two
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:171:6:172:3:**
```roc
		456, # Comment two
		789, # Comment three
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment three
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:172:6:173:2:**
```roc
		789, # Comment three
	]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:173:2:174:2:**
```roc
	]
	for n in list {
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:39:189:40:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:65:189:66:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:96:189:97:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                                                              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:110:190:2:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
	Stdout.line!(interpolated)?
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:190:28:191:2:**
```roc
	Stdout.line!(interpolated)?
	Stdout.line!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"How about ${ # Comment after string interpolation open
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:192:3:193:4:**
```roc
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:191:2:193:4:**
```roc
	Stdout.line!(
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:3:194:5:**
```roc
		} as a string?",
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:5:194:8:**
```roc
		} as a string?",
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:16:194:17:**
```roc
		} as a string?",
```
		             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:17:195:2:**
```roc
		} as a string?",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:195:2:196:1:**
```roc
	)
} # Comment after top-level decl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Comment after top-level decl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:196:1:198:1:**
```roc
} # Comment after top-level decl

empty : {}
```


**UNDEFINED VARIABLE**
Nothing is named **function** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:14:40:14:48:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                       ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:72:4:72:13:**
```roc
			some_func() # After debug expr
```
			^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:85:3:85:21:**
```roc
		Blue | Green | Red => {
```
		^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:89:3:92:8:**
```roc
		Blue # After pattern in alt
		| # Before pattern in alt
			Green
		| Red # After alt pattern
```


**UNDEFINED VARIABLE**
Nothing is named **rest** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:102:19:102:23:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		                ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:102:23:103:4:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:108:27:108:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:116:3:116:5:**
```roc
		] => 123
```
		^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:120:15:120:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**UNDEFINED VARIABLE**
Nothing is named **add** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:121:37:121:40:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                                  ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:130:24:130:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:141:2:141:6:**
```roc
	blah == 1 # Comment after expect statement
```
	^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:145:2:145:7:**
```roc
	world = "World"
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **var number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:146:2:146:12:**
```roc
	var number = 123
```
	^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:147:9:147:13:**
```roc
	expect blah == 1
```
	       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:148:2:148:5:**
```roc
	tag = Blue
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:150:3:150:6:**
```roc
		tag # Comment after return statement
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:158:2:158:11:**
```roc
	some_func(
```
	^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:164:24:164:30:**
```roc
	tag_with_payload = Ok(number)
```
	                      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:169:5:169:11:**
```roc
				number, # after dbg expr as arg
```
				^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:175:9:175:15:**
```roc
		Stdout.line!("Adding ${n} to ${number}")
```
		      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:42:178:45:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                        ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:55:178:60:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **punned** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:63:178:69:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:25:179:28:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                       ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:33:179:38:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:42:179:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                                        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:183:3:183:7:**
```roc
		tag1,
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:184:6:184:11:**
```roc
		Ok(world), # This one has a comment
```
		   ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:185:4:185:10:**
```roc
		(nested, tuple),
```
		 ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:188:22:188:25:**
```roc
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	                    ^^^


**UNDEFINED VARIABLE**
Nothing is named **some_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:189:26:189:33:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:189:34:189:38:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                ^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:190:8:190:14:**
```roc
	Stdout.line!(interpolated)?
```
	      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:191:8:191:14:**
```roc
	Stdout.line!(
```
	      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:193:14:193:20:**
```roc
			Num.toStr(number) # Comment after string interpolation expr
```
			          ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **string** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:194:10:194:16:**
```roc
		} as a string?",
```
		       ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Expr.malformed)
  (Expr.lookup "function")
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.assign
    (pattern (Patt.ident "add_one_oneline"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add_one"))
    (type type_173)
  )
  (Stmt.assign
    (pattern (Patt.ident "add_one"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "match_time"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.expr)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_367)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
  (Expr.crash
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "tag_with_payload"))
    (Expr.tag_applied)
  )
  (Stmt.assign
    (pattern (Patt.ident "interpolated"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "list"))
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
  (Expr.for_loop)
  (Stmt.assign
    (pattern (Patt.ident "record"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 123)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.lookup "tag")
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.tag_applied)
      )
      (Expr.record_field
        (Expr.lookup "punned")
        (Expr.lookup "punned")
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "tuple"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag")
      (Expr.tag_applied)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "multiline_tuple"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag1")
      (Expr.tag_applied)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "bin_op_result"))
    (Expr.binop_or
      (Expr.binop_or
        (Expr.binop_gt
          (Expr.binop_double_question
            (Expr.tag_applied)
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
  (Stmt.assign
    (pattern (Patt.ident "static_dispatch_style"))
    (Expr.fn_call)
  )
  (Expr.fn_call)
  (Expr.fn_call)
  (Expr.record_access)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "a")
  (Expr.lookup "string")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "empty"))
    (type type_559)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.record_literal
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "tuple"))
    (type type_570)
  )
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 636
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 _)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 _)
(var #129 _)
(var #130 _)
(var #131 _)
(var #132 _)
(var #133 _)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 _)
(var #141 _)
(var #142 _)
(var #143 _)
(var #144 _)
(var #145 _)
(var #146 _)
(var #147 _)
(var #148 _)
(var #149 _)
(var #150 _)
(var #151 _)
(var #152 _)
(var #153 _)
(var #154 _)
(var #155 _)
(var #156 _)
(var #157 _)
(var #158 _)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 -> #591)
(var #163 _)
(var #164 _)
(var #165 Num *)
(var #166 Num *)
(var #167 _)
(var #168 -> #591)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 -> #593)
(var #176 _)
(var #177 -> #178)
(var #178 Num *)
(var #179 _)
(var #180 _)
(var #181 _)
(var #182 _)
(var #183 _)
(var #184 Num *)
(var #185 _)
(var #186 _)
(var #187 Num *)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 -> #593)
(var #193 _)
(var #194 -> #596)
(var #195 _)
(var #196 _)
(var #197 _)
(var #198 _)
(var #199 _)
(var #200 _)
(var #201 _)
(var #202 _)
(var #203 _)
(var #204 Num *)
(var #205 _)
(var #206 _)
(var #207 _)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 _)
(var #214 _)
(var #215 Num *)
(var #216 _)
(var #217 _)
(var #218 _)
(var #219 _)
(var #220 Num *)
(var #221 _)
(var #222 _)
(var #223 Num *)
(var #224 _)
(var #225 Str)
(var #226 _)
(var #227 _)
(var #228 Num *)
(var #229 Num *)
(var #230 Num *)
(var #231 Num *)
(var #232 _)
(var #233 _)
(var #234 _)
(var #235 _)
(var #236 _)
(var #237 Num *)
(var #238 _)
(var #239 Num *)
(var #240 Num *)
(var #241 _)
(var #242 _)
(var #243 _)
(var #244 _)
(var #245 _)
(var #246 _)
(var #247 _)
(var #248 Num *)
(var #249 _)
(var #250 Num *)
(var #251 Num *)
(var #252 _)
(var #253 _)
(var #254 _)
(var #255 _)
(var #256 _)
(var #257 _)
(var #258 _)
(var #259 Num *)
(var #260 _)
(var #261 _)
(var #262 Num *)
(var #263 _)
(var #264 F64)
(var #265 _)
(var #266 _)
(var #267 _)
(var #268 _)
(var #269 _)
(var #270 _)
(var #271 _)
(var #272 _)
(var #273 Num *)
(var #274 Num *)
(var #275 Num *)
(var #276 _)
(var #277 _)
(var #278 _)
(var #279 _)
(var #280 _)
(var #281 Num *)
(var #282 _)
(var #283 _)
(var #284 _)
(var #285 _)
(var #286 _)
(var #287 _)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 Num *)
(var #292 _)
(var #293 _)
(var #294 Num *)
(var #295 _)
(var #296 _)
(var #297 _)
(var #298 _)
(var #299 _)
(var #300 _)
(var #301 _)
(var #302 _)
(var #303 _)
(var #304 _)
(var #305 Num *)
(var #306 _)
(var #307 _)
(var #308 Num *)
(var #309 _)
(var #310 _)
(var #311 Num *)
(var #312 _)
(var #313 _)
(var #314 _)
(var #315 _)
(var #316 Num *)
(var #317 _)
(var #318 _)
(var #319 Num *)
(var #320 _)
(var #321 _)
(var #322 Num *)
(var #323 _)
(var #324 _)
(var #325 _)
(var #326 _)
(var #327 _)
(var #328 Num *)
(var #329 _)
(var #330 _)
(var #331 _)
(var #332 _)
(var #333 _)
(var #334 Num *)
(var #335 _)
(var #336 _)
(var #337 _)
(var #338 _)
(var #339 _)
(var #340 _)
(var #341 _)
(var #342 _)
(var #343 _)
(var #344 _)
(var #345 _)
(var #346 _)
(var #347 _)
(var #348 _)
(var #349 Num *)
(var #350 _)
(var #351 _)
(var #352 -> #596)
(var #353 _)
(var #354 _)
(var #355 Num *)
(var #356 _)
(var #357 _)
(var #358 _)
(var #359 _)
(var #360 _)
(var #361 _)
(var #362 _)
(var #363 _)
(var #364 _)
(var #365 _)
(var #366 _)
(var #367 _)
(var #368 _)
(var #369 -> #600)
(var #370 _)
(var #371 _)
(var #372 Str)
(var #373 _)
(var #374 _)
(var #375 Num *)
(var #376 _)
(var #377 _)
(var #378 Num *)
(var #379 _)
(var #380 _)
(var #381 _)
(var #382 _)
(var #383 _)
(var #384 _)
(var #385 _)
(var #386 _)
(var #387 _)
(var #388 _)
(var #389 _)
(var #390 _)
(var #391 _)
(var #392 _)
(var #393 Num *)
(var #394 _)
(var #395 -> #599)
(var #396 -> #600)
(var #397 _)
(var #398 Str)
(var #399 _)
(var #400 -> #403)
(var #401 -> #601)
(var #402 _)
(var #403 _)
(var #404 _)
(var #405 -> #406)
(var #406 Str)
(var #407 _)
(var #408 -> #412)
(var #409 _)
(var #410 _)
(var #411 _)
(var #412 _)
(var #413 _)
(var #414 _)
(var #415 _)
(var #416 _)
(var #417 _)
(var #418 Num *)
(var #419 _)
(var #420 Num *)
(var #421 _)
(var #422 _)
(var #423 _)
(var #424 _)
(var #425 _)
(var #426 _)
(var #427 _)
(var #428 Str)
(var #429 _)
(var #430 _)
(var #431 _)
(var #432 _)
(var #433 _)
(var #434 _)
(var #435 _)
(var #436 {})
(var #437 -> #609)
(var #438 _)
(var #439 Num *)
(var #440 _)
(var #441 _)
(var #442 Str)
(var #443 _)
(var #444 _)
(var #445 _)
(var #446 _)
(var #447 _)
(var #448 _)
(var #449 _)
(var #450 _)
(var #451 _)
(var #452 _)
(var #453 _)
(var #454 -> #609)
(var #455 _)
(var #456 -> #612)
(var #457 Num *)
(var #458 Str)
(var #459 _)
(var #460 -> #610)
(var #461 _)
(var #462 _)
(var #463 _)
(var #464 _)
(var #465 -> #611)
(var #466 Num *)
(var #467 Num *)
(var #468 Num *)
(var #469 _)
(var #470 -> #612)
(var #471 _)
(var #472 -> #615)
(var #473 Num *)
(var #474 Str)
(var #475 _)
(var #476 -> #613)
(var #477 _)
(var #478 _)
(var #479 _)
(var #480 _)
(var #481 -> #614)
(var #482 Num *)
(var #483 Num *)
(var #484 Num *)
(var #485 _)
(var #486 -> #615)
(var #487 _)
(var #488 -> #515)
(var #489 _)
(var #490 _)
(var #491 _)
(var #492 Num *)
(var #493 -> #496)
(var #494 -> #495)
(var #495 -> #496)
(var #496 -> #497)
(var #497 -> #508)
(var #498 -> #499)
(var #499 -> #500)
(var #500 -> #501)
(var #501 -> #502)
(var #502 -> #507)
(var #503 -> #504)
(var #504 -> #505)
(var #505 -> #506)
(var #506 -> #507)
(var #507 -> #508)
(var #508 -> #509)
(var #509 -> #514)
(var #510 -> #513)
(var #511 -> #512)
(var #512 -> #513)
(var #513 -> #514)
(var #514 -> #515)
(var #515 Num *)
(var #516 _)
(var #517 -> #520)
(var #518 -> #616)
(var #519 _)
(var #520 _)
(var #521 _)
(var #522 _)
(var #523 _)
(var #524 -> #618)
(var #525 _)
(var #526 _)
(var #527 _)
(var #528 -> #620)
(var #529 _)
(var #530 _)
(var #531 _)
(var #532 _)
(var #533 _)
(var #534 _)
(var #535 _)
(var #536 -> #623)
(var #537 _)
(var #538 _)
(var #539 _)
(var #540 _)
(var #541 _)
(var #542 -> #626)
(var #543 _)
(var #544 _)
(var #545 _)
(var #546 _)
(var #547 -> #627)
(var #548 _)
(var #549 _)
(var #550 _)
(var #551 _)
(var #552 _)
(var #553 _)
(var #554 _)
(var #555 _)
(var #556 _)
(var #557 _)
(var #558 _)
(var #559 _)
(var #560 _)
(var #561 -> #634)
(var #562 -> #634)
(var #563 _)
(var #564 _)
(var #565 _)
(var #566 _)
(var #567 _)
(var #568 _)
(var #569 _)
(var #570 _)
(var #571 _)
(var #572 _)
(var #573 Num *)
(var #574 _)
(var #575 _)
(var #576 Num *)
(var #577 _)
(var #578 _)
(var #579 _)
(var #580 _)
(var #581 _)
(var #582 _)
(var #583 _)
(var #584 _)
(var #585 _)
(var #586 _)
(var #587 _)
(var #588 _)
(var #589 _)
(var #590 _)
(var #591 fn_pure)
(var #592 _)
(var #593 fn_pure)
(var #594 _)
(var #595 _)
(var #596 fn_pure)
(var #597 _)
(var #598 _)
(var #599 {})
(var #600 fn_pure)
(var #601 fn_pure)
(var #602 _)
(var #603 _)
(var #604 _)
(var #605 _)
(var #606 _)
(var #607 _)
(var #608 {})
(var #609 record)
(var #610 fn_pure)
(var #611 tuple)
(var #612 tuple)
(var #613 fn_pure)
(var #614 tuple)
(var #615 tuple)
(var #616 fn_pure)
(var #617 _)
(var #618 fn_pure)
(var #619 _)
(var #620 fn_pure)
(var #621 _)
(var #622 _)
(var #623 fn_pure)
(var #624 _)
(var #625 _)
(var #626 fn_pure)
(var #627 fn_pure)
(var #628 _)
(var #629 _)
(var #630 _)
(var #631 _)
(var #632 _)
(var #633 _)
(var #634 {})
(var #635 _)
~~~
# TYPES
~~~roc
x : _d
number : _d
other : Num(_size)
blah : _d
empty : {}
rest : _d
match_time : _arg, _arg2 -> _ret
dude : _d
bin_op_result : Num(_size)
multiline_tuple : (Num(_size), Str, _field, _field2, (_field3, _field4), _field5)
record : { punned: _field }
tag_with_payload : _d
tuple : _d
list : _d
n : _d
b : _d
a : _d
lower : _d
foo : _d
add_one : _arg -> _ret
interpolated : Str
add_one_oneline : _arg -> _ret
num : _d
static_dispatch_style : _d
main : _arg -> {}
~~~
