# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app { pf: "c" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare KwImport UpperIdent KwExposing OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent Dot OpStar CloseSquare KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent Comma CloseRound UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly UpperIdent CloseRound OpColon OpenSquare CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg OpenRound CloseRound Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma OpBar KwMatch LowerIdent OpenCurly LowerIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent LowerIdent CloseCurly UpperIdent OpFatArrow Int String OpFatArrow Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly KwExpect LowerIdent OpEquals Int LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LowerIdent TripleDot LowerIdent OpenRound TripleDot Comma CloseRound LowerIdent OpenRound KwDbg Int Comma CloseRound KwCrash MalformedString LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LowerIdent OpenSquare Comma CloseRound Comma Int Comma CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly KwAs LowerIdent MalformedString CloseRound CloseCurly LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound KwExpect OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
    (lc "line")
    (lc "e")
  )
  (import
    (uc "Stdot")
  )
  (import
    (lc "pkg")
    (uc "S")
    (lc "func")
  )
  (malformed malformed:expr_unexpected_token)
  (lc "fry")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_dot_suffix_not_allowed)
  (malformed malformed:expr_unexpected_token)
  (import
    (uc "Bae")
    (uc "Gooe")
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
    (binop_thin_arrow
      (binop_colon
        (tuple_literal
          (lc "a")
          (lc "b")
          (malformed malformed:expr_unexpected_token)
        )
        (apply_uc
          (uc "List")
        )
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
    (malformed malformed:expr_unexpected_token)
    (list_literal)
  )
  (binop_colon
    (apply_uc
      (uc "Func")
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
    (lc "ane")
    (lambda
      (body
        (if_else <91 branches>)
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
          (if_else <111 branches>)
        )
      )
      (args
        (lc "num")
      )
    )
  )
  (binop_equals
    (lc "match_time")
    (malformed malformed:expr_unexpected_token)
  )
  (lc "ist")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 123)
  (frac_literal_small 3.14)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 314)
  (binop_pipe
    (frac_literal_small 3.14)
    (frac_literal_small 6.28)
  )
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
      (binop_pipe
        (tuple_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
        )
        (num_literal_i32 5)
      )
      (num_literal_i32 3)
    )
  )
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
    (underscore)
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (binop_pipe
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
    (num_literal_i32 7)
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (record_literal
    (binop_colon
      (lc "foo")
      (num_literal_i32 1)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 12)
  (apply_uc
    (uc "Ok")
    (num_literal_i32 123)
  )
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 121000)
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
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
  (lc "e")
  (list_literal
    (malformed malformed:expr_unexpected_token)
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (num_literal_i32 456)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
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
      (malformed malformed:expr_unexpected_token)
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
    (binop_pipe
      (malformed malformed:expr_unexpected_token)
      (dot_lc "statod")
    )
  )
  (apply_anon
    (binop_pipe
      (malformed malformed:expr_unexpected_token)
      (dot_lc "ned")
    )
  )
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (dot_lc "recd")
  )
  (malformed malformed:expr_unexpected_token)
  (uc "Stdoline")
  (unary_not <unary>)
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
# Thnt!
app
{
	# Thnt!
	# Thnt!
pf: "c" platform [
		main,
	],
}

import pf.Stdout exposing [line, e]
import Stdot
import pkg.S exposing [func]
as
fry
.*
]

import Bae as Gooe
import Ba
Map((a, b)) : List a -> (a -> b) -> List b
MapML((a, b) : List # Aon -> ((a -> b) -> List(b) #))
Foo : (Bar, Baz)
line : (Bar, Baz)
Some(a) : {foo : Ok a, bar : g}
Ml(a) : {
	bar : Som # Afld
}

Soine(a) : {
	bar : Som
} #
Maya
) : [] #se

Func(a) : Maybe a -> a -> Maybe a
ane = \num -> if num 2 else 5

add_one : U64 -> U64
add_one = \num -> {
	other = 1
	if num
		{
			dbg # bug
() #r()
			0
		}
	else {
		dbg
		123
		other : other
	}
}

match_time = [
ist
]
=>
123
3.14
=>
314
3.14 | 6.28
=>
314((1, 2, 3))
=>
123(((1, 2) | 5, 3))
=>
123
{ foo : 1, bar : 2, ..rest }
=>
12-->
add(34)
{ foo : 1, bar : 2, _ }
=>
12
{ foo : 1, bar : 2 } | 7
}
=>
12
{
	foo : 1,
}
=>
12
Ok(123)
=>
121000
}

expect blah == 1 # Commnt

main! : List String -> Result ({  }, _)
main! = \_ -> { world = "World", var number = 123, expect blah == 1, tag = Blue, return tag, 

# Jusnt!
..., match_time((...)), some_func(dbg # bug), 42 }
crash "Unreachtement
tag_with = Ok(number)
ited = "Hello, ${world}"
list = [add_one(dbg # Afin list)]
e[, # afarg])
456, # ee
]
for n in list {
	{
		line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo : 123, bar : "Hello", baz : tag, qux : Ok world, punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
m_tuple = (123, "World", tag1, Ok(world), (nested, tuple), [1, 2, 3])
bsult = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
stale = some_fn(arg1)
? | .statod()? | .ned()? | .recd?
Stdoline!"How about ${ #
Num.toStr(number) # on expr
}
as
a"
)
} # Commenl decl

empty : {}
empty = {  }

tuple : Value (a, b, c)
expect {
	foo = 1 # Thio
	blah = 1
	blah == foo
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 10:29 to 10:29

**Parse Error**
at 10:35 to 10:35

**Parse Error**
at 10:43 to 10:44

**Parse Error**
at 10:45 to 10:45

**Parse Error**
at 19:1 to 19:1

**Parse Error**
at 16:1 to 26:1

**Parse Error**
at 31:1 to 31:1

**Parse Error**
at 32:1 to 32:1

**Parse Error**
at 40:5 to 40:5

**Parse Error**
at 45:13 to 45:20

**Parse Error**
at 50:2 to 50:9

**Parse Error**
at 51:3 to 51:3

**Parse Error**
at 55:3 to 55:3

**Parse Error**
at 67:3 to 67:3

**Parse Error**
at 67:9 to 67:9

**Parse Error**
at 68:9 to 68:9

**Parse Error**
at 70:17 to 70:17

**Parse Error**
at 70:35 to 70:35

**Parse Error**
at 70:22 to 70:38

**Parse Error**
at 70:42 to 70:42

**Parse Error**
at 71:4 to 71:4

**Parse Error**
at 74:3 to 74:9

**Parse Error**
at 64:10 to 74:20

**Parse Error**
at 74:27 to 74:27

**Parse Error**
at 75:3 to 75:3

**Parse Error**
at 77:3 to 77:3

**Parse Error**
at 77:5 to 77:5

**Parse Error**
at 78:8 to 78:8

**Parse Error**
at 79:15 to 79:15

**Parse Error**
at 80:13 to 80:13

**Parse Error**
at 81:17 to 81:17

**Parse Error**
at 82:30 to 82:30

**Parse Error**
at 82:35 to 82:35

**Parse Error**
at 88:8 to 88:8

**Parse Error**
at 89:3 to 89:20

**Parse Error**
at 89:24 to 89:24

**Parse Error**
at 89:26 to 89:26

**Parse Error**
at 92:6 to 92:6

**Parse Error**
at 93:11 to 93:11

**Parse Error**
at 94:2 to 94:2

**Parse Error**
at 113:2 to 113:2

**Parse Error**
at 111:2 to 114:2

**Parse Error**
at 115:3 to 115:3

**Parse Error**
at 114:2 to 116:4

**Parse Error**
at 117:2 to 117:2

**Parse Error**
at 100:13 to 118:2

**Parse Error**
at 118:8 to 118:8

**Parse Error**
at 123:4 to 123:4

**Parse Error**
at 122:3 to 124:1

**Parse Error**
at 121:9 to 124:1

**Parse Error**
at 124:3 to 124:3

**Parse Error**
at 124:2 to 125:3

**Parse Error**
at 125:3 to 125:3

**Parse Error**
at 125:4 to 125:4

**Parse Error**
at 125:9 to 125:9

**Parse Error**
at 126:2 to 126:2

**Parse Error**
at 140:2 to 140:2

**Parse Error**
at 141:2 to 141:2

**Parse Error**
at 142:23 to 142:23

**Parse Error**
at 142:33 to 142:33

**Parse Error**
at 142:40 to 142:40

**Parse Error**
at 142:46 to 142:46

**Parse Error**
at 144:3 to 144:3

**Parse Error**
at 145:4 to 145:4

**Parse Error**
at 146:3 to 146:3

**Parse Error**
at 146:5 to 146:5

**Parse Error**
at 146:9 to 146:9

**Parse Error**
at 147:2 to 147:2

**Parse Error**
at 148:1 to 148:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.binop_thin_arrow)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.binop_double_equals)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.frac_literal_big)
  (Expr.malformed)
  (Expr.binop_colon)
  (Expr.malformed)
  (Expr.binop_colon)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.binop_double_slash)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.malformed)
  (Expr.binop_thick_arrow)
  (Expr.binop_double_slash)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.frac_literal_big)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.binop_double_slash)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.binop_thin_arrow)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.malformed)
  (Expr.binop_minus)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.binop_or)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_star)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon)
  (Expr.binop_colon)
  (Expr.frac_literal_big)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.unary_not)
  (Expr.binop_colon)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_minus)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
