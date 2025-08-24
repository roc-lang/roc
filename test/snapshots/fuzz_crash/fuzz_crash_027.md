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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare KwImport UpperIdent KwExposing OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent Dot OpStar CloseSquare KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent Comma CloseRound UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly UpperIdent CloseRound OpColon OpenSquare CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg OpenRound CloseRound Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma OpBar KwMatch LowerIdent OpenCurly LowerIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent LowerIdent CloseCurly UpperIdent OpFatArrow Int String OpFatArrow Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly KwExpect LowerIdent OpEquals Int LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LowerIdent TripleDot LowerIdent OpenRound TripleDot Comma CloseRound LowerIdent OpenRound KwDbg Int Comma CloseRound KwCrash MalformedString LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LowerIdent OpenSquare Comma CloseRound Comma Int Comma CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly KwAs LowerIdent MalformedString CloseRound CloseCurly LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound KwExpect OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
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
    (block
      (binop_colon
        (lc "foo")
        (binop_colon
          (tuple_literal
            (apply_uc
              (uc "Ok")
              (lc "a")
            )
            (lc "bar")
          )
          (lc "g")
        )
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
        (if_else <89 branches>)
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
          (if_else <109 branches>)
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
NO CHANGE
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
TYPE MISMATCH - fuzz_crash_027.md:142:10:142:41
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
at 64:2 to 64:10

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
at 74:20 to 74:20

**Parse Error**
at 74:3 to 74:23

**Parse Error**
at 74:27 to 74:27

**Parse Error**
at 74:29 to 74:29

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
at 88:6 to 88:6

**Parse Error**
at 89:26 to 89:26

**Parse Error**
at 92:4 to 92:4

**Parse Error**
at 93:11 to 93:11

**Parse Error**
at 99:7 to 99:7

**Parse Error**
at 99:22 to 99:22

**Parse Error**
at 100:7 to 100:7

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
at 126:2 to 126:2

**Parse Error**
at 83:3 to 127:2

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
at 64:2 to 146:9

**Parse Error**
at 146:9 to 146:9

**Parse Error**
at 147:2 to 147:2

**Parse Error**
at 148:1 to 148:1

**Unsupported Node**
at 4:1 to 4:36

**Unsupported Node**
at 6:1 to 6:13

**Unsupported Node**
at 10:1 to 10:28

**Unsupported Node**
at 10:29 to 10:29

**Unsupported Node**
at 10:35 to 10:35

**Unsupported Node**
at 10:43 to 10:43

**Unsupported Node**
at 10:45 to 10:45

**Unsupported Node**
at 12:1 to 12:19

**Unsupported Node**
at 13:1 to 14:4

**Unsupported Node**
at 15:13 to 15:41

**Pattern in Expression Context**
at 26:1 to 26:4

**Unsupported Node**
at 26:16 to 26:17

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 32:30 to 32:30

**Pattern in Expression Context**
at 34:8 to 34:11

**Pattern in Expression Context**
at 38:8 to 38:11

**Pattern in Expression Context**
at 40:1 to 40:5

**Unsupported Node**
at 40:5 to 40:5

**Unsupported Node**
at 40:9 to 40:10

**Unsupported Node**
at 43:11 to 43:34

**Unsupported Node**
at 45:7 to 45:13

**Unsupported Node**
at 47:11 to 47:21

**Unsupported Node**
at 48:11 to 48:17

**Unsupported Node**
at 148:1 to 148:1

**Unsupported Node**
at 155:1 to 159:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "fry")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.apply_tag)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "line")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.lookup "g")
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.malformed)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "bar")
        (Expr.malformed)
      )
    )
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "add_one")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "empty")
    (Expr.record_literal
    )
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "tuple")
    (Expr.apply_tag)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
