# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot
		exposing [ #tem
		] # Cose

import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
import
	Ba
Map(a, b) : List(a), (a -> b) -> List(b)
MapML( # Cere
	a, # Anre
	b,
) # Ag
	: # Aon
		List( #rg
		),
		(a -> b) -> # row
			List(			b	) #

Foo : (Bar, Baz)

line : ( # Cpen
	Bar, #
	Baz, #m
) # Co
Some(a) : { foo : Ok(a), bar : g }
Ml(a) : { # d
	bar : Som# Afld
}

Soine(a) : { #d
	bar : Som
} #
Maya) : [ #
] #se

Func(a) : Maybe(a), a -> Maybe(a)

ane = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num {
		dbg # bug
() #r
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, #rg
	b,
| # As
	match a {lue | Red => {
			x x
		}
		Blue		=> 1
		"foo" => # ent
00
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
			=> ment


		[1, 2 | 5, 3, .. as rest] => 123
		[
ist
		] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # Afrd open
			foo #
				: #ue
					1, # Aftd field
			bar: 2,
			..} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			foo: 1,
			} => 12
		Ok(123) => 121000
	}

expect # Commeneyword
	blah == 1 # Commnt

main! : List(String) -> Result({}, _)
main! = |_| { # Yeah Ie
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comd
		tag

	# Jusnt!

	...
	match_time(
		..., #
	)
	some_func(
		dbg # bug
			42, # Aft expr
	)
	crash "Unreachtement
	tag_with = Ok(number)
	ited = "Hello, ${world}"
	list = [
		add_one(
			dbg # Afin list
e[, # afarg
		),	456, # ee
	]
	for n in list {
	line!("Adding ${n} to ${number}")
		number = number + n
	}
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
	m_tuple = (
		123,
		"World",
		tag1,
		Ok(world), # Thisnt
		(nested, tuple),
		[1, 2, 3],
	)
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
	Stdoline!(
		"How about ${ #
			Num.toStr(number) # on expr
		} as a",
	)
} # Commenl decl

empty : {}
empty = {}

tuple : Value((a, b, c))

expect {
	foo = 1 # Thio
	blah = 1
	blah == foo
}
~~~
# TOKENS
~~~text
LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare LineComment CloseSquare LineComment BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent Dot OpStar CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment LowerIdent Comma LineComment LowerIdent Comma CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LowerIdent CloseRound LineComment BlankLine UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine LowerIdent OpColon OpenRound LineComment UpperIdent Comma LineComment UpperIdent Comma LineComment CloseRound LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent CloseCurly LineComment UpperIdent CloseRound OpColon OpenSquare LineComment CloseSquare LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg LineComment OpenRound CloseRound LineComment Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment LowerIdent Comma OpBar LineComment KwMatch LowerIdent OpenCurly LowerIdent OpBar UpperIdent OpThinArrow OpenCurly LowerIdent LowerIdent CloseCurly UpperIdent OpThinArrow Int String OpThinArrow LineComment Int String OpBar String OpThinArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare LineComment OpThinArrow LowerIdent BlankLine OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpThinArrow Int OpenSquare LowerIdent CloseSquare OpThinArrow Int Float OpThinArrow Int Float OpBar Float OpThinArrow Int OpenRound Int Comma Int Comma Int CloseRound OpThinArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpThinArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpThinArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LineComment LowerIdent LineComment OpColon LineComment Int Comma LineComment LowerIdent OpColon Int Comma DoubleDot CloseCurly OpThinArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpThinArrow Int OpenCurly LowerIdent OpColon Int Comma CloseCurly OpThinArrow Int UpperIdent OpenRound Int CloseRound OpThinArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent OpEquals Int LineComment BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LineComment LowerIdent BlankLine LineComment BlankLine TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound LowerIdent OpenRound KwDbg LineComment Int Comma LineComment CloseRound KwCrash MalformedString LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LineComment LowerIdent OpenSquare Comma LineComment CloseRound Comma Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly KwAs LowerIdent MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
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
        (str_literal_small "c")
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
        (not_lc "e")
      )
    )
  )
  (import
    (binop_exposing
      (uc "Stdot")
      (list_literal)
    )
  )
  (import
    (binop_exposing
      (binop_dot
        (lc "pkg")
        (uc "S")
      )
      (list_literal
        (lc "func")
      )
    )
  )
  (malformed)
  (lc "fry")
  (malformed)
  (malformed)
  (malformed)
  (import
    (binop_as
      (uc "Bae")
      (uc "Gooe")
    )
  )
  (import
    (uc "Ba")
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
    (lc "line")
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
        (lc "g")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Ml")
      (lc "a")
    )
    (block
      (binop_colon
        (lc "bar")
        (uc "Som")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Soine")
      (lc "a")
    )
    (block
      (binop_colon
        (lc "bar")
        (uc "Som")
      )
    )
  )
  (uc "Maya")
  (binop_colon
    (malformed)
    (list_literal)
  )
  (binop_colon
    (apply_uc
      (uc "Func")
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
    (lc "ane")
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
                (apply_anon
                  (malformed)
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
                (lc "lue")
                (uc "Red")
              )
              (block
                (lc "x")
                (lc "x")
              )
            )
)
          (branch2             (binop_thick_arrow
              (uc "Blue")
              (block
                (num_literal_i32 1)
                (binop_thick_arrow
                  (str_literal_small "foo")
                  (num_literal_i32 0)
                )
                (str_literal_small "foo")
                (malformed)
                (num_literal_i32 20)
                (list_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                  (num_literal_i32 3)
                  (unary_double_dot <unary_op>)
                )
                (lc "rest")
                (binop_thick_arrow
                  (malformed)
                  (lc "ment")
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
                  (list_literal
                    (lc "ist")
                  )
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
                    (underscore)
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
                (binop_thick_arrow
                  (record_literal
                    (binop_colon
                      (lc "foo")
                      (num_literal_i32 1)
                    )
                  )
                  (num_literal_i32 12)
                )
              )
            )
)
          (branch3             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (num_literal_i32 123)
              )
              (num_literal_i32 121000)
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
    (malformed)
  )
  (binop_equals
    (lc "tag_with")
    (apply_uc
      (uc "Ok")
      (lc "number")
    )
  )
  (binop_equals
    (lc "ited")
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
  (lc "e")
  (list_literal
    (malformed)
  )
  (malformed)
  (malformed)
  (num_literal_i32 456)
  (malformed)
  (malformed)
  (for_loop
    (lc "n")
    (lc "list")
    (block
      (apply_anon
        (not_lc "line")
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
    (lc "m_tuple")
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
    (lc "bsult")
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
    (lc "stale")
    (apply_lc
      (lc "some_fn")
      (lc "arg1")
    )
  )
  (apply_anon
    (binop_dot
      (malformed)
      (dot_lc "statod")
    )
  )
  (apply_anon
    (binop_dot
      (malformed)
      (dot_lc "ned")
    )
  )
  (binop_dot
    (malformed)
    (dot_lc "recd")
  )
  (malformed)
  (uc "Stdoline")
  (unary_not <unary_op>)
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
# Thnt!
app [main!] { pf: "c" platform [] }

import pf.Stdout exposing [line!, e!]
import Stdot exposing [, #tem
]
# Cose
import pkg.S exposing [func]
as
fry
,
.*
]

import Bae as Gooe
import Ba
Map((a, b)) : List a -> (a -> b) -> List b
# Cere
# Anre
# Ag
MapML(
	(
		a,
		b,
	),
) : # Aon
List -> ( #rg
a -> b) -> # row
List b
#
Foo : (Bar, Baz)
line : # Cpen
(
	Bar, #
	Baz,
)
#m
# Co
Some(a) : {foo: Ok(a), bar: g}
Ml(a) : {
	# d
	bar : Som
}
# Afld
Soine(a) : {
	#d
	bar : Som
}
#
Maya
) : [, #
]
#se
Func(a) : Maybe a -> a -> Maybe a
ane = |num| if num 2 else 5
add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num
		{
			dbg # bug
()
			#r
			0
		}
	else {
		dbg
		123
		other
	}
}

match_time = |a, #rg
b| # As
match a
	lue || Red =>
		x
		x
	Blue =>
		1
		"foo" => # ent
00
		"foo"
		=>
		20
		[1, 2, 3, ..as ]
		rest
		] # Aftet
			 => ment
		[1, 2]
		rest
		] => 123
		[ist] => 123
		3.14 => 314
		3.14
		=>
		123((1, 2))
		) => 123
		({ foo: 1, bar: 2, ..rest } => 12-) -> add(34)
		{ # Afrd open
			foo: #
			#ue
1, # Aftd field
			bar: 2,
			_,
		} => 12
		{ foo: 1, bar: 2 }
		} => 12
		{
			foo: 1,
		} => 12
	Ok(123) => 121000

expect # Commeneyword
blah == 1
# Commnt
main! : List String -> Result({}, _)
main! = |
	_,
| { # Yeah Ie
	world = "World",
	var number = 123,
	expect blah == 1,
	tag = Blue,
	return # Comd
tag, # Jusnt!
	...,
	match_time(
		...,
	), #
	some_func(dbg # bug
			),
	42, # Aft expr
}

crash "Unreachtement

tag_with = Ok(number)
ited = "Hello, ${world}"
list = [
	add_one(dbg # Afin list
),
]

e
[
	, # afarg
		,
]
)
,
456
, # ee

]

for n in list {
	{
		line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned: punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
m_tuple = (
	123,
	"World",
	tag1,
	Ok(world), # Thisnt
	(nested, tuple),
	[1, 2, 3],
)

bsult = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
stale = some_fn(arg1)
?..statod()
?..ned()
?..recd
?

Stdoline
!"How about ${ #

Num..toStr(number)
# on expr
}
as
a
",

)
} # Commenl decl

empty : {}
empty = {}
tuple : Value(a, b, c)
expect {
	foo = 1
	# Thio
	blah = 1
	blah == foo
}
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_027.md:40:5:40:6
PARSE ERROR - fuzz_crash_027.md:40:7:40:8
PARSE ERROR - fuzz_crash_027.md:40:9:40:10
PARSE ERROR - fuzz_crash_027.md:41:1:41:2
PARSE ERROR - fuzz_crash_027.md:122:3:122:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:125:3:125:4
UNDECLARED TYPE - fuzz_crash_027.md:26:8:26:11
UNDECLARED TYPE - fuzz_crash_027.md:26:13:26:16
UNDECLARED TYPE - fuzz_crash_027.md:32:19:32:21
UNDECLARED TYPE VARIABLE - fuzz_crash_027.md:32:32:32:33
UNDECLARED TYPE - fuzz_crash_027.md:34:8:34:11
UNDECLARED TYPE - fuzz_crash_027.md:38:8:38:11
UNDECLARED TYPE - fuzz_crash_027.md:43:11:43:16
UNDECLARED TYPE - fuzz_crash_027.md:43:26:43:31
MODULE NOT FOUND - fuzz_crash_027.md:4:1:4:38
MODULE NOT FOUND - fuzz_crash_027.md:6:1:8:4
MODULE NOT FOUND - fuzz_crash_027.md:10:1:10:46
MODULE NOT FOUND - fuzz_crash_027.md:12:1:12:19
MODULE NOT FOUND - fuzz_crash_027.md:13:1:14:4
UNDECLARED TYPE - fuzz_crash_027.md:29:2:29:5
UNDECLARED TYPE - fuzz_crash_027.md:30:2:30:5
EMPTY TUPLE NOT ALLOWED - fuzz_crash_027.md:52:1:52:3
UNDEFINED VARIABLE - fuzz_crash_027.md:65:4:65:5
UNDEFINED VARIABLE - fuzz_crash_027.md:65:6:65:7
UNUSED VARIABLE - fuzz_crash_027.md:64:11:64:14
UNDEFINED VARIABLE - fuzz_crash_027.md:71:7:71:11
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
UNUSED VARIABLE - fuzz_crash_027.md:76:1:76:4
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:82:21:82:27
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:62:2:62:3
UNDEFINED VARIABLE - fuzz_crash_027.md:97:2:97:6
UNDECLARED TYPE - fuzz_crash_027.md:99:14:99:20
UNDEFINED VARIABLE - fuzz_crash_027.md:103:9:103:13
UNDEFINED VARIABLE - fuzz_crash_027.md:114:2:114:11
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_027.md:131:63:131:69
UNDEFINED VARIABLE - fuzz_crash_027.md:132:42:132:48
UNDEFINED VARIABLE - fuzz_crash_027.md:136:3:136:7
UNDEFINED VARIABLE - fuzz_crash_027.md:138:4:138:10
UNDEFINED VARIABLE - fuzz_crash_027.md:141:14:141:17
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_027.md:145:4:145:13
UNUSED VARIABLE - fuzz_crash_027.md:119:2:119:10
UNUSED VARIABLE - fuzz_crash_027.md:120:2:120:6
UNUSED VARIABLE - fuzz_crash_027.md:121:2:121:6
UNUSED VARIABLE - fuzz_crash_027.md:131:2:131:8
UNUSED VARIABLE - fuzz_crash_027.md:133:2:133:9
UNUSED VARIABLE - fuzz_crash_027.md:141:2:141:7
UNUSED VARIABLE - fuzz_crash_027.md:142:2:142:7
UNDECLARED TYPE - fuzz_crash_027.md:153:9:153:14
INVALID IF CONDITION - fuzz_crash_027.md:50:5:50:5
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_027.md:64:2:64:2
TYPE MISMATCH - fuzz_crash_027.md:111:2:113:3
TYPE MISMATCH - fuzz_crash_027.md:142:10:142:47
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:10:29:10:32:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                            ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:10:35:10:37:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                  ^^


**PARSE ERROR**
A parsing error occurred: **expr_dot_suffix_not_allowed**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:10:43:10:45:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:10:45:12:1:**
```roc
import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:40:5:40:7:**
```roc
Maya) : [ #
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:51:3:52:1:**
```roc
		dbg # bug
() #r
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:55:3:55:7:**
```roc
		dbg 123
```
		^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:17:70:20:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:35:70:38:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                                ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:70:22:70:38:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                   ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] # Aftet
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:42:71:4:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
			=> ment
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:74:3:74:9:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:20:74:23:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:23:74:27:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:27:74:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:79:15:79:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:79:21:80:3:**
```roc
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:80:19:81:3:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:80:16:81:9:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:81:15:81:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:89:3:89:20:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:89:24:89:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:115:3:116:4:**
```roc
		dbg # bug
			42, # Aft expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:114:2:116:4:**
```roc
	some_func(
		dbg # bug
			42, # Aft expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:117:2:118:2:**
```roc
	)
	crash "Unreachtement
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:100:13:118:2:**
```roc
main! = |_| { # Yeah Ie
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comd
		tag

	# Jusnt!

	...
	match_time(
		..., #
	)
	some_func(
		dbg # bug
			42, # Aft expr
	)
	crash "Unreachtement
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"Unreachtement
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:118:8:119:2:**
```roc
	crash "Unreachtement
	tag_with = Ok(number)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # Afin list
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:123:4:124:1:**
```roc
			dbg # Afin list
e[, # afarg
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:122:3:124:1:**
```roc
		add_one(
			dbg # Afin list
e[, # afarg
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:121:9:124:1:**
```roc
	list = [
		add_one(
			dbg # Afin list
e[, # afarg
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # afarg
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:124:3:125:3:**
```roc
e[, # afarg
		),	456, # ee
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:124:2:125:3:**
```roc
e[, # afarg
		),	456, # ee
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:3:125:4:**
```roc
		),	456, # ee
```
		^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:4:125:6:**
```roc
		),	456, # ee
```
		 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ee
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:9:126:2:**
```roc
		),	456, # ee
	]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:126:2:127:2:**
```roc
	]
	for n in list {
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:23:142:24:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:33:142:34:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:40:142:41:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:46:143:2:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
	Stdoline!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"How about ${ #
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:144:3:145:4:**
```roc
		"How about ${ #
			Num.toStr(number) # on expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:3:146:5:**
```roc
		} as a",
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:5:146:8:**
```roc
		} as a",
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:9:147:2:**
```roc
		} as a",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:147:2:148:1:**
```roc
	)
} # Commenl decl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Commenl decl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:148:1:150:1:**
```roc
} # Commenl decl

empty : {}
```


**UNDEFINED VARIABLE**
Nothing is named **fry** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:10:32:10:35:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                               ^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:28:1:28:5:**
```roc
line : ( # Cpen
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:45:1:45:4:**
```roc
ane = |num| if num 2 else 5
```
^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:47:1:47:8:**
```roc
add_one : U64 -> U64
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:48:12:48:15:**
```roc
add_one = |num| {
```
           ^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:48:1:48:8:**
```roc
add_one = |num| {
```
^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:64:11:64:20:**
```roc
	match a {lue | Red => {
```
	         ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:65:4:65:5:**
```roc
			x x
```
			^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:65:6:65:7:**
```roc
			x x
```
			  ^


**UNDEFINED VARIABLE**
Nothing is named **rest** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:70:38:70:42:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                                   ^^^^


**UNDEFINED VARIABLE**
Nothing is named **ment** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:71:7:71:11:**
```roc
			=> ment
```
			   ^^^^


**UNDEFINED VARIABLE**
Nothing is named **add** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:82:37:82:40:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                                  ^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:60:1:60:11:**
```roc
match_time = |
```
^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:97:2:97:6:**
```roc
	blah == 1 # Commnt
```
	^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:99:1:99:6:**
```roc
main! : List(String) -> Result({}, _)
```
^^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:101:2:101:7:**
```roc
	world = "World"
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **var number** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:102:2:102:12:**
```roc
	var number = 123
```
	^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:103:9:103:13:**
```roc
	expect blah == 1
```
	       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:104:2:104:5:**
```roc
	tag = Blue
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:106:3:106:6:**
```roc
		tag
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:114:2:114:11:**
```roc
	some_func(
```
	^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:100:1:100:6:**
```roc
main! = |_| { # Yeah Ie
```
^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:119:16:119:22:**
```roc
	tag_with = Ok(number)
```
	              ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:119:2:119:10:**
```roc
	tag_with = Ok(number)
```
	^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:120:2:120:6:**
```roc
	ited = "Hello, ${world}"
```
	^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:121:2:121:6:**
```roc
	list = [
```
	^^^^


**UNDEFINED VARIABLE**
Nothing is named **e** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:124:1:124:2:**
```roc
e[, # afarg
```
^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:42:131:45:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                        ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:55:131:60:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **punned** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:63:131:69:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                             ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:131:2:131:8:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:25:132:28:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                       ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:33:132:38:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:42:132:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                                        ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:132:2:132:7:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:136:3:136:7:**
```roc
		tag1,
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:137:6:137:11:**
```roc
		Ok(world), # Thisnt
```
		   ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:138:4:138:10:**
```roc
		(nested, tuple),
```
		 ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:133:2:133:9:**
```roc
	m_tuple = (
```
	^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:141:14:141:17:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	            ^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:141:2:141:7:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **some_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:142:10:142:17:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:142:18:142:22:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                ^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:142:2:142:7:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:145:14:145:20:**
```roc
			Num.toStr(number) # on expr
```
			          ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:150:1:150:6:**
```roc
empty : {}
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:151:1:151:6:**
```roc
empty = {}
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_027.md:153:1:153:6:**
```roc
tuple : Value((a, b, c))
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Expr.malformed)
  (Expr.lookup "fry")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "line"))
    (type type_79)
  )
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Expr.tag_no_args)
  (Stmt.standalone_type_anno
    (pattern (Patt.malformed))
    (type type_112)
  )
  (Stmt.type_alias)
  (Stmt.assign
    (pattern (Patt.ident "ane"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add_one"))
    (type type_138)
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
    (type type_289)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
  (Expr.crash
    (Expr.malformed)
  )
  (Stmt.assign
    (pattern (Patt.ident "tag_with"))
    (Expr.tag_applied)
  )
  (Stmt.assign
    (pattern (Patt.ident "ited"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "list"))
    (Expr.list_literal)
  )
  (Expr.lookup "e")
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.num_literal_i32 456)
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
    (pattern (Patt.ident "m_tuple"))
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
    (pattern (Patt.ident "bsult"))
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
    (pattern (Patt.ident "stale"))
    (Expr.fn_call)
  )
  (Expr.fn_call)
  (Expr.fn_call)
  (Expr.record_access)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.unary_not)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "a")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "empty"))
    (type type_468)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.record_literal
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "tuple"))
    (type type_479)
  )
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 552
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
(var #127 -> #499)
(var #128 _)
(var #129 -> #498)
(var #130 -> #131)
(var #131 Num *)
(var #132 -> #131)
(var #133 -> #499)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 -> #505)
(var #141 _)
(var #142 -> #143)
(var #143 Num *)
(var #144 _)
(var #145 -> #501)
(var #146 _)
(var #147 _)
(var #148 Num *)
(var #149 -> #153)
(var #150 _)
(var #151 Num *)
(var #152 _)
(var #153 _)
(var #154 -> #153)
(var #155 _)
(var #156 -> #505)
(var #157 _)
(var #158 -> #509)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 _)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 _)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 Num *)
(var #171 _)
(var #172 Num *)
(var #173 _)
(var #174 Str)
(var #175 _)
(var #176 _)
(var #177 Num *)
(var #178 Num *)
(var #179 Num *)
(var #180 Num *)
(var #181 _)
(var #182 _)
(var #183 _)
(var #184 _)
(var #185 _)
(var #186 _)
(var #187 _)
(var #188 Num *)
(var #189 Num *)
(var #190 _)
(var #191 _)
(var #192 _)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 _)
(var #197 Num *)
(var #198 _)
(var #199 _)
(var #200 _)
(var #201 Num *)
(var #202 _)
(var #203 _)
(var #204 Num *)
(var #205 _)
(var #206 F64)
(var #207 _)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 _)
(var #214 _)
(var #215 Num *)
(var #216 Num *)
(var #217 Num *)
(var #218 _)
(var #219 _)
(var #220 _)
(var #221 _)
(var #222 _)
(var #223 Num *)
(var #224 _)
(var #225 _)
(var #226 _)
(var #227 _)
(var #228 _)
(var #229 _)
(var #230 _)
(var #231 _)
(var #232 _)
(var #233 Num *)
(var #234 _)
(var #235 _)
(var #236 Num *)
(var #237 _)
(var #238 _)
(var #239 _)
(var #240 _)
(var #241 _)
(var #242 _)
(var #243 _)
(var #244 _)
(var #245 _)
(var #246 _)
(var #247 Num *)
(var #248 _)
(var #249 _)
(var #250 Num *)
(var #251 _)
(var #252 _)
(var #253 Num *)
(var #254 _)
(var #255 _)
(var #256 _)
(var #257 _)
(var #258 Num *)
(var #259 _)
(var #260 _)
(var #261 _)
(var #262 _)
(var #263 _)
(var #264 Num *)
(var #265 _)
(var #266 _)
(var #267 _)
(var #268 _)
(var #269 _)
(var #270 _)
(var #271 Num *)
(var #272 _)
(var #273 _)
(var #274 -> #509)
(var #275 _)
(var #276 _)
(var #277 Num *)
(var #278 _)
(var #279 _)
(var #280 _)
(var #281 _)
(var #282 _)
(var #283 _)
(var #284 _)
(var #285 _)
(var #286 _)
(var #287 _)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 -> #513)
(var #292 _)
(var #293 _)
(var #294 Str)
(var #295 _)
(var #296 _)
(var #297 Num *)
(var #298 _)
(var #299 _)
(var #300 Num *)
(var #301 _)
(var #302 _)
(var #303 _)
(var #304 _)
(var #305 _)
(var #306 _)
(var #307 _)
(var #308 _)
(var #309 _)
(var #310 _)
(var #311 _)
(var #312 _)
(var #313 _)
(var #314 _)
(var #315 Num *)
(var #316 _)
(var #317 -> #512)
(var #318 -> #513)
(var #319 _)
(var #320 _)
(var #321 -> #514)
(var #322 -> #325)
(var #323 -> #515)
(var #324 _)
(var #325 _)
(var #326 _)
(var #327 -> #328)
(var #328 Str)
(var #329 _)
(var #330 -> #518)
(var #331 -> #517)
(var #332 _)
(var #333 _)
(var #334 -> #518)
(var #335 _)
(var #336 _)
(var #337 _)
(var #338 -> #520)
(var #339 _)
(var #340 _)
(var #341 Num *)
(var #342 _)
(var #343 _)
(var #344 _)
(var #345 _)
(var #346 _)
(var #347 Str)
(var #348 _)
(var #349 _)
(var #350 _)
(var #351 _)
(var #352 _)
(var #353 _)
(var #354 _)
(var #355 {})
(var #356 -> #527)
(var #357 _)
(var #358 Num *)
(var #359 _)
(var #360 _)
(var #361 Str)
(var #362 _)
(var #363 _)
(var #364 _)
(var #365 _)
(var #366 _)
(var #367 -> #525)
(var #368 _)
(var #369 _)
(var #370 _)
(var #371 _)
(var #372 _)
(var #373 -> #527)
(var #374 _)
(var #375 -> #531)
(var #376 Num *)
(var #377 Str)
(var #378 _)
(var #379 -> #528)
(var #380 _)
(var #381 _)
(var #382 _)
(var #383 _)
(var #384 -> #529)
(var #385 Num *)
(var #386 -> #385)
(var #387 -> #385)
(var #388 -> #530)
(var #389 -> #531)
(var #390 _)
(var #391 -> #535)
(var #392 Num *)
(var #393 Str)
(var #394 _)
(var #395 -> #532)
(var #396 _)
(var #397 _)
(var #398 _)
(var #399 _)
(var #400 -> #533)
(var #401 Num *)
(var #402 -> #401)
(var #403 -> #401)
(var #404 -> #534)
(var #405 -> #535)
(var #406 _)
(var #407 -> #434)
(var #408 _)
(var #409 _)
(var #410 _)
(var #411 Num *)
(var #412 -> #415)
(var #413 -> #414)
(var #414 -> #415)
(var #415 -> #416)
(var #416 -> #427)
(var #417 -> #418)
(var #418 -> #419)
(var #419 -> #420)
(var #420 -> #421)
(var #421 -> #426)
(var #422 -> #423)
(var #423 -> #424)
(var #424 -> #425)
(var #425 -> #426)
(var #426 -> #427)
(var #427 -> #428)
(var #428 -> #433)
(var #429 -> #432)
(var #430 -> #431)
(var #431 -> #432)
(var #432 -> #433)
(var #433 -> #434)
(var #434 Num *)
(var #435 _)
(var #436 -> #439)
(var #437 -> #536)
(var #438 _)
(var #439 _)
(var #440 _)
(var #441 _)
(var #442 _)
(var #443 -> #538)
(var #444 _)
(var #445 _)
(var #446 _)
(var #447 -> #540)
(var #448 _)
(var #449 _)
(var #450 _)
(var #451 _)
(var #452 _)
(var #453 _)
(var #454 _)
(var #455 -> #543)
(var #456 _)
(var #457 _)
(var #458 -> #544)
(var #459 _)
(var #460 _)
(var #461 _)
(var #462 _)
(var #463 _)
(var #464 _)
(var #465 _)
(var #466 _)
(var #467 _)
(var #468 _)
(var #469 _)
(var #470 -> #550)
(var #471 -> #550)
(var #472 _)
(var #473 _)
(var #474 _)
(var #475 _)
(var #476 _)
(var #477 _)
(var #478 _)
(var #479 _)
(var #480 _)
(var #481 _)
(var #482 Num *)
(var #483 _)
(var #484 _)
(var #485 Num *)
(var #486 _)
(var #487 _)
(var #488 _)
(var #489 _)
(var #490 _)
(var #491 _)
(var #492 _)
(var #493 _)
(var #494 _)
(var #495 _)
(var #496 _)
(var #497 _)
(var #498 _)
(var #499 fn_pure)
(var #500 _)
(var #501 _)
(var #502 -> #503)
(var #503 fn_pure)
(var #504 _)
(var #505 fn_pure)
(var #506 _)
(var #507 _)
(var #508 fn_pure)
(var #509 fn_pure)
(var #510 _)
(var #511 _)
(var #512 {})
(var #513 fn_pure)
(var #514 <error>)
(var #515 fn_pure)
(var #516 _)
(var #517 fn_pure)
(var #518 List #333)
(var #519 _)
(var #520 List #519)
(var #521 _)
(var #522 _)
(var #523 _)
(var #524 _)
(var #525 fn_pure)
(var #526 {})
(var #527 record)
(var #528 fn_pure)
(var #529 tuple)
(var #530 List #385)
(var #531 tuple)
(var #532 fn_pure)
(var #533 tuple)
(var #534 List #401)
(var #535 tuple)
(var #536 fn_pure)
(var #537 _)
(var #538 fn_pure)
(var #539 _)
(var #540 fn_pure)
(var #541 _)
(var #542 _)
(var #543 _)
(var #544 fn_pure)
(var #545 _)
(var #546 _)
(var #547 _)
(var #548 _)
(var #549 _)
(var #550 {})
(var #551 _)
~~~
# TYPES
~~~roc
b : _d
add_one : _arg -> _ret
tag_with : _d
stale : _d
rest : _d
line : _d
empty : {}
a : _d
list : List(_elem)
m_tuple : (Num(_size), Str, _field, _field2, (_field3, _field4), List(Num(_size2)))
n : _d
ited : Str
tuple : _d
num : _d
ist : _d
main : _arg -> {}
ane : _arg -> Num(_size)
match_time : _arg -> _arg2 -> _ret
record : { foo: Num(_size), bar: Str, baz: _field, qux: _field2, punned: _field3 }
bsult : Num(_size)
~~~
