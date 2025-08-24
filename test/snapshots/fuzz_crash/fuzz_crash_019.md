# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
		exposing [ #tem
Cust]

import Bae as Gooe
import
	Ba
Map(a, b) : Lis, (ab) -> List(b)
MapML( # Cb,
) # Ag
	: # Aon
		List( #rg
		),
		(ab) -> # row
			List(			b	) #z)

line : ( # Cm
) # Co
Som : { foo : O, bar : g }
Ml(a) : { # ld
}

Soine(a) : { #
} #
Maybe(a) : [Somne]

Mayine(a) : [ #
] #)

ane = |num| if num 2 else 5

one : U6
add = |num| {
	1
	if num {
		dbg # bug
			s exp0
	} else {
		dbg 123
		r
	}
}

me = |
	a, Tb,
| # As
	match a {lue  {
	x
		}
		Blue=> {x
			}
	er #ent
			1	"for" => 20[1, ] # t
		ment
		[1, 2, 3,est]123
		[
		] 23
		3.1 314
		3.14 | 6.28 => 314
		(1, ) => 123
		(1, 2, 3)123
		{ 	} => 12
		Ok(123) => 12
	}

expect # Cord
	nt

main! : Listlt({}, _)
ma= |_| { e
	w = "d"
	var er = 123
	expect blaue
	return #d
		tag

	#
	...
	me(
		..., # r
	)crash ke"Unr!" #)
	i= "H, ${d}"
t = [
		one(er, 		),	456, # two
9, #ee
	]
	for n in list {
	line!("Ag ${n} to ${er}")
		ber + n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m (
		123,
		"World",ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
		"Ho${ #
			r(nu) # xpr
		} ",
	)
} # Cocl

y : {}
e = {}

t : V((a,c))

expect {
	foo == 1
h == foo
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent Comma OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound CloseRound OpColon UpperIdent OpenRound CloseRound Comma OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpColon OpenRound CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly Int KwIf LowerIdent OpenCurly KwDbg LowerIdent LowerIdent CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma UpperIdent Comma OpBar KwMatch LowerIdent OpenCurly LowerIdent OpenCurly LowerIdent CloseCurly UpperIdent OpFatArrow OpenCurly LowerIdent CloseCurly LowerIdent Int String OpFatArrow Int OpenSquare Int Comma CloseSquare LowerIdent OpenSquare Int Comma Int Comma Int Comma LowerIdent CloseSquare Int OpenSquare CloseSquare Int Float Int Float OpBar Float OpFatArrow Int OpenRound Int Comma CloseRound OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound Int OpenCurly CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly KwExpect LowerIdent LowerIdent OpBang OpColon UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent KwReturn LowerIdent TripleDot LowerIdent OpenRound TripleDot Comma CloseRound KwCrash LowerIdent String LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound LowerIdent Comma CloseRound Comma Int Comma Int Comma CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpDoubleQuestion Int OpGreaterThan Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString LowerIdent OpenRound LowerIdent CloseRound CloseCurly MalformedString CloseRound CloseCurly LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound KwExpect OpenCurly LowerIdent OpEquals Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
    (lc "line")
  )
  (import
    (uc "Stdot")
    (uc "Cust")
  )
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
      (uc "Lis")
      (binop_thin_arrow
        (lc "ab")
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
    )
    (binop_thin_arrow
      (apply_uc
        (uc "List")
      )
      (binop_thin_arrow
        (lc "ab")
        (apply_uc
          (uc "List")
          (lc "b")
        )
      )
    )
  )
  (binop_colon
    (lc "line")
    (tuple_literal)
  )
  (binop_colon
    (uc "Som")
    (block
      (binop_colon
        (lc "foo")
        (binop_colon
          (tuple_literal
            (uc "O")
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
    (record_literal)
  )
  (binop_colon
    (apply_uc
      (uc "Soine")
      (lc "a")
    )
    (record_literal)
  )
  (binop_colon
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (uc "Somne")
    )
  )
  (binop_colon
    (apply_uc
      (uc "Mayine")
      (lc "a")
    )
    (list_literal)
  )
  (binop_equals
    (lc "ane")
    (lambda
      (body
        (if_else <55 branches>)
      )
      (args
        (lc "num")
      )
    )
  )
  (binop_colon
    (lc "one")
    (uc "U6")
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (block
          (num_literal_i32 1)
          (if_else <76 branches>)
        )
      )
      (args
        (lc "num")
      )
    )
  )
  (binop_equals
    (lc "me")
    (malformed malformed:expr_unexpected_token)
  )
  (unary_not <unary>)
  (apply_uc
    (uc "Listlt")
    (tuple_literal
      (record_literal)
      (underscore)
    )
  )
  (binop_equals
    (lc "ma")
    (lambda
      (body
        (block
          (binop_colon
            (lc "e")
            (lc "e")
          )
          (binop_equals
            (lc "w")
            (str_literal_small "d")
          )
          (binop_equals
            (var_lc "er")
            (num_literal_i32 123)
          )
          (expect
            (lc "blaue")
          )
          (ret <statement>)
          (ellipsis)
          (apply_lc
            (lc "me")
            (tuple_literal
              (ellipsis)
              (malformed malformed:expr_unexpected_token)
            )
          )
          (crash <statement>)
          (str_literal_small "Unr!")
          (binop_equals
            (lc "i")
            (str_literal_big "H, ${d}")
          )
          (binop_equals
            (lc "t")
            (list_literal
              (apply_lc
                (lc "one")
                (tuple_literal
                  (lc "er")
                  (malformed malformed:expr_unexpected_token)
                  (num_literal_i32 456)
                  (num_literal_i32 9)
                  (malformed malformed:expr_unexpected_token)
                )
              )
            )
          )
          (for_loop
            (lc "n")
            (lc "list")
            (block
              (binop_colon
                (lc "line")
                (lc "line")
              )
              (unary_not <unary>)
              (binop_plus
                (lc "ber")
                (lc "n")
              )
            )
          )
          (binop_equals
            (lc "rd")
            (block
              (binop_colon
                (lc "foo")
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (binop_colon
                        (tuple_literal
                          (binop_colon
                            (tuple_literal
                              (num_literal_i32 123)
                              (lc "bar")
                            )
                            (str_literal_small "H")
                          )
                          (lc "baz")
                        )
                        (lc "tag")
                      )
                      (lc "qux")
                    )
                    (apply_uc
                      (uc "Ok")
                      (lc "world")
                    )
                  )
                  (lc "ned")
                )
              )
            )
          )
          (binop_equals
            (lc "t")
            (tuple_literal
              (num_literal_i32 123)
              (str_literal_big "World")
              (lc "tag")
              (uc "O")
              (tuple_literal
                (lc "nd")
                (lc "t")
              )
              (list_literal
                (tuple_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                  (num_literal_i32 3)
                )
              )
            )
          )
          (apply_lc
            (lc "m")
            (tuple_literal
              (num_literal_i32 123)
              (str_literal_big "World")
              (lc "ag1")
              (uc "O")
              (tuple_literal
                (lc "ne")
                (lc "tuple")
              )
              (list_literal
                (tuple_literal
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                  (num_literal_i32 3)
                )
              )
              (malformed malformed:expr_unexpected_token)
            )
          )
          (binop_or
            (binop_or
              (binop_gt
                (binop_double_question
                  (lc "b")
                  (num_literal_i32 12)
                )
                (num_literal_i32 5)
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
              (num_literal_i32 3)
            )
          )
          (apply_lc
            (lc "e_fn")
            (lc "arg1")
          )
          (apply_anon
            (binop_pipe
              (malformed malformed:expr_unexpected_token)
              (dot_lc "od")
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
          (uc "Stdo")
          (unary_not <unary>)
          (apply_lc
            (lc "r")
            (lc "nu")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "y")
    (record_literal)
  )
  (binop_equals
    (lc "e")
    (record_literal)
  )
  (binop_colon
    (lc "t")
    (apply_uc
      (uc "V")
      (tuple_literal
        (lc "a")
        (lc "c")
      )
    )
  )
  (expect
    (block
      (binop_double_equals
        (lc "foo")
        (num_literal_i32 1)
      )
      (binop_double_equals
        (lc "h")
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
PARSE ERROR - fuzz_crash_019.md:52:16:52:16
PARSE ERROR - fuzz_crash_019.md:58:4:58:4
PARSE ERROR - fuzz_crash_019.md:59:3:59:3
PARSE ERROR - fuzz_crash_019.md:60:16:60:16
PARSE ERROR - fuzz_crash_019.md:62:5:62:5
PARSE ERROR - fuzz_crash_019.md:63:7:63:7
PARSE ERROR - fuzz_crash_019.md:66:12:66:12
UNDECLARED TYPE - fuzz_crash_019.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_019.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:24:24:24:25
MODULE NOT FOUND - fuzz_crash_019.md:4:1:4:34
MODULE NOT FOUND - fuzz_crash_019.md:6:1:8:6
MODULE NOT FOUND - fuzz_crash_019.md:10:1:10:19
MODULE NOT FOUND - fuzz_crash_019.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_019.md:37:7:37:9
UNDEFINED VARIABLE - fuzz_crash_019.md:42:4:42:5
UNDEFINED VARIABLE - fuzz_crash_019.md:42:6:42:10
UNDEFINED VARIABLE - fuzz_crash_019.md:45:3:45:4
UNDEFINED VARIABLE - fuzz_crash_019.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_019.md:52:11:52:14
UNDEFINED VARIABLE - fuzz_crash_019.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_019.md:57:2:57:4
UNDEFINED VARIABLE - fuzz_crash_019.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_019.md:60:12:60:15
UNDEFINED VARIABLE - fuzz_crash_019.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_019.md:74:9:74:15
UNDEFINED VARIABLE - fuzz_crash_019.md:75:11:75:12
UNDEFINED VARIABLE - fuzz_crash_019.md:78:9:78:14
UNDEFINED VARIABLE - fuzz_crash_019.md:80:3:80:6
CRASH EXPECTS STRING - fuzz_crash_019.md:86:3:86:11
UNDEFINED VARIABLE - fuzz_crash_019.md:87:11:87:12
UNDEFINED VARIABLE - fuzz_crash_019.md:89:3:89:6
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_019.md:96:34:96:37
UNDEFINED VARIABLE - fuzz_crash_019.md:96:47:96:52
UNDEFINED VARIABLE - fuzz_crash_019.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_019.md:97:2:97:3
UNDEFINED VARIABLE - fuzz_crash_019.md:97:21:97:24
UNDEFINED VARIABLE - fuzz_crash_019.md:97:30:97:32
UNDEFINED VARIABLE - fuzz_crash_019.md:98:2:98:3
UNDEFINED VARIABLE - fuzz_crash_019.md:100:11:100:14
UNDEFINED VARIABLE - fuzz_crash_019.md:102:4:102:6
UNDEFINED VARIABLE - fuzz_crash_019.md:102:8:102:13
UNDEFINED VARIABLE - fuzz_crash_019.md:105:2:105:3
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_019.md:108:4:108:5
UNDEFINED VARIABLE - fuzz_crash_019.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_019.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_019.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_019.md:96:2:96:4
UNDECLARED TYPE - fuzz_crash_019.md:116:5:116:6
UNDEFINED VARIABLE - fuzz_crash_019.md:119:2:119:5
UNDEFINED VARIABLE - fuzz_crash_019.md:120:1:120:2
UNDEFINED VARIABLE - fuzz_crash_019.md:120:6:120:9
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_019.md:52:2:52:2
TYPE MISMATCH - fuzz_crash_019.md:84:2:86:3
# PROBLEMS
**Parse Error**
at 35:13 to 35:20

**Parse Error**
at 40:2 to 40:9

**Parse Error**
at 41:3 to 41:3

**Parse Error**
at 44:3 to 44:3

**Parse Error**
at 52:2 to 52:10

**Parse Error**
at 55:7 to 55:7

**Parse Error**
at 58:12 to 58:12

**Parse Error**
at 58:21 to 58:21

**Parse Error**
at 58:17 to 59:3

**Parse Error**
at 65:7 to 65:7

**Parse Error**
at 64:18 to 66:12

**Parse Error**
at 66:12 to 66:12

**Parse Error**
at 67:8 to 67:8

**Parse Error**
at 68:11 to 68:11

**Parse Error**
at 52:2 to 71:1

**Parse Error**
at 71:1 to 71:1

**Parse Error**
at 72:2 to 72:2

**Parse Error**
at 74:1 to 74:1

**Parse Error**
at 74:7 to 74:7

**Parse Error**
at 86:2 to 86:2

**Parse Error**
at 84:2 to 86:3

**Parse Error**
at 89:13 to 89:13

**Parse Error**
at 91:2 to 91:2

**Parse Error**
at 89:3 to 92:2

**Parse Error**
at 88:5 to 92:2

**Parse Error**
at 104:2 to 104:2

**Parse Error**
at 98:2 to 105:2

**Parse Error**
at 105:65 to 105:65

**Parse Error**
at 105:71 to 105:71

**Parse Error**
at 105:78 to 105:78

**Parse Error**
at 105:84 to 105:84

**Parse Error**
at 107:3 to 107:3

**Parse Error**
at 108:4 to 108:4

**Parse Error**
at 109:5 to 109:5

**Parse Error**
at 110:2 to 110:2

**Parse Error**
at 111:1 to 111:1

**Unsupported Node**
at 4:1 to 4:32

**Unsupported Node**
at 6:1 to 8:5

**Unsupported Node**
at 10:1 to 10:19

**Unsupported Node**
at 11:1 to 12:4

**Unsupported Node**
at 13:13 to 13:33

**Unsupported Node**
at 17:3 to 20:14

**Unsupported Node**
at 22:8 to 22:9

**Pattern in Expression Context**
at 24:1 to 24:4

**Unsupported Node**
at 24:22 to 24:22

**Unsupported Node**
at 30:12 to 30:19

**Unsupported Node**
at 32:13 to 32:14

**Unsupported Node**
at 35:7 to 35:13

**Pattern in Expression Context**
at 37:7 to 37:9

**Unsupported Node**
at 38:7 to 38:13

**Unsupported Node**
at 74:1 to 74:1

**Unsupported Node**
at 74:7 to 74:7

**Unsupported Node**
at 75:5 to 75:9

**Unsupported Node**
at 109:5 to 109:5

**Unsupported Node**
at 110:2 to 110:2

**Unsupported Node**
at 111:1 to 111:1

**Unsupported Node**
at 118:1 to 121:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "line")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
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
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "one")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "y")
    (Expr.record_literal
    )
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "t")
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
