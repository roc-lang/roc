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
add = |Rum| {
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
	a, #b,
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
LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare LineComment UpperIdent CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent Comma OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment CloseRound Comma OpenRound LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LowerIdent CloseRound LineComment BlankLine LowerIdent OpColon OpenRound LineComment CloseRound LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare LineComment CloseSquare LineComment BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar UpperIdent OpBar OpenCurly Int KwIf LowerIdent OpenCurly KwDbg LineComment LowerIdent LowerIdent CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment OpBar LineComment KwMatch LowerIdent OpenCurly LowerIdent OpenCurly LowerIdent CloseCurly UpperIdent OpFatArrow OpenCurly LowerIdent CloseCurly LowerIdent LineComment Int String OpFatArrow Int OpenSquare Int Comma CloseSquare LineComment LowerIdent OpenSquare Int Comma Int Comma Int Comma LowerIdent CloseSquare Int OpenSquare CloseSquare Int Float Int Float OpBar Float OpFatArrow Int OpenRound Int Comma CloseRound OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound Int OpenCurly CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent KwReturn LineComment LowerIdent BlankLine LineComment TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound KwCrash LowerIdent String LineComment LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound LowerIdent Comma CloseRound Comma Int Comma LineComment Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpDoubleQuestion Int OpGreaterThan Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpEquals Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
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
      )
    )
  )
  (import
    (binop_exposing
      (uc "Stdot")
      (list_literal
        (uc "Cust")
      )
    )
  )
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
      (uc "Lis")
      (binop_arrow_call
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
    (binop_arrow_call
      (apply_uc
        (uc "List")
      )
      (binop_arrow_call
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
    (record_literal
      (binop_colon
        (lc "foo")
        (uc "O")
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
    (lc "one")
    (uc "U6")
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (block
          (num_literal_i32 1)
          (if_else
            (condition               (lc "num")
)
            (then               (block
                (malformed)
                (lc "s")
                (lc "exp0")
              )
)
            (else               (block
                (malformed)
                (num_literal_i32 123)
                (lc "r")
              )
))
        )
      )
      (args
        (uc "Rum")
      )
    )
  )
  (binop_equals
    (lc "me")
    (lambda
      (body
        (match
          (scrutinee             (lc "a")
))
      )
      (args
        (lc "a")
      )
    )
  )
  (lc "x")
  (malformed)
  (uc "Blue")
  (malformed)
  (block
    (lc "x")
  )
  (lc "er")
  (num_literal_i32 1)
  (str_literal_small "for")
  (malformed)
  (num_literal_i32 20)
  (list_literal
    (num_literal_i32 1)
  )
  (lc "ment")
  (list_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
    (num_literal_i32 3)
    (lc "est")
  )
  (num_literal_i32 123)
  (list_literal)
  (num_literal_i32 23)
  (frac_literal_small 3.1)
  (num_literal_i32 314)
  (frac_literal_small 3.14)
  (malformed)
  (apply_anon
    (num_literal_i32 314)
    (num_literal_i32 1)
  )
  (malformed)
  (apply_anon
    (num_literal_i32 123)
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (num_literal_i32 123)
  (record_literal)
  (malformed)
  (num_literal_i32 12)
  (apply_uc
    (uc "Ok")
    (num_literal_i32 123)
  )
  (malformed)
  (num_literal_i32 12)
  (malformed)
  (expect
    (lc "nt")
  )
  (binop_colon
    (not_lc "main")
    (apply_uc
      (uc "Listlt")
      (tuple_literal
        (record_literal)
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "ma")
    (lambda
      (body
        (block
          (lc "e")
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
          (ret
            (lc "tag")
          )
          (ellipsis)
          (apply_lc
            (lc "me")
            (ellipsis)
          )
          (crash
            (lc "ke")
          )
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
                (lc "er")
              )
              (num_literal_i32 456)
              (num_literal_i32 9)
            )
          )
          (for_loop
            (lc "n")
            (lc "list")
            (block
              (apply_anon
                (not_lc "line")
                (str_literal_big "Ag ${n} to ${er}")
              )
              (binop_plus
                (lc "ber")
                (lc "n")
              )
            )
          )
          (binop_equals
            (lc "rd")
            (record_literal
              (binop_colon
                (lc "foo")
                (num_literal_i32 123)
              )
              (binop_colon
                (lc "bar")
                (str_literal_small "H")
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
                (lc "ned")
                (lc "ned")
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
                (num_literal_i32 1)
                (num_literal_i32 2)
                (num_literal_i32 3)
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
                (num_literal_i32 1)
                (num_literal_i32 2)
                (num_literal_i32 3)
              )
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
            (binop_dot
              (malformed)
              (dot_lc "od")
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
          (uc "Stdo")
          (unary_not <unary_op>)
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
  (malformed)
  (malformed)
  (malformed)
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
# Thnt!
app [main!] { pf: "c" platform [] }

import pf.Stdout exposing [line!]
import Stdot exposing [ #tem
	Cust,
]
import Bae as Gooe
import Ba
Map((a, b)) : Lis -> ab -> List b
# Cb,
# Ag
MapML() : # Aon
List -> #rg
ab -> # row
List b
#z)
line : (
,
)
# Cm
# Co
Som : {foo: O, bar: g}
Ml(a) :
	{
	}
# ld
Soine(a) :
	{
	}
#
#
Maybe(a) : [Somne]
Mayine(a) : [, #
]
#)
ane = |num| if num 2 else 5
one : U6
add = |Rum| {
	1
	if num
		{
			dbg # bug
			
			s
			exp0
		}
	else {
		dbg 
		123
		r
	}
}

me = |a| #b,
# As
match a

x
}
		
Blue
=> 
{
	x
}
er
#ent
1
"for"
=> 
20
[
	1,
]
# t
ment
[1, 2, 3, est]
123
[]
23
3.1
314
3.14
=> 
314(
	1,
)
=> 
123((1, 2, 3))
123
{}
=> 
12
Ok(123)
=> 
12
}

expect # Cord
nt
main! : Listlt({}, _)
ma = |
	_,
| {
	e
	w = "d"
	var er = 123
	expect blaue
	return #d
tag
	#
	...
	me(
		...,
	)
	crash ke
	"Unr!"
	#)
	i = "H, ${d}"
	t = [
		one(
			er,
		),
		456, # two
		9, #ee
	]

	for n in list {
		{
			line!("Ag ${n} to ${er}")
			ber + n
		}
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world), ned: ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m((
		123,
		"World",
		ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	))
	(b ?? 12 > 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3
	e_fn(arg1)
	?..od()
	?..ned()
	?..recd
	?
	
	Stdo
	!"Ho${ #
			
	r(nu)
}

",
	
)
} # Cocl

y : {}
e = {}
t : V(a, c)
expect {
	foo == 1
	h == foo
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_020.md:52:16:52:16
PARSE ERROR - fuzz_crash_020.md:58:4:58:4
PARSE ERROR - fuzz_crash_020.md:59:3:59:3
PARSE ERROR - fuzz_crash_020.md:60:16:60:16
PARSE ERROR - fuzz_crash_020.md:62:5:62:5
PARSE ERROR - fuzz_crash_020.md:63:7:63:7
PARSE ERROR - fuzz_crash_020.md:66:12:66:12
UNDECLARED TYPE - fuzz_crash_020.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_020.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:24:24:24:25
MODULE NOT FOUND - fuzz_crash_020.md:4:1:4:34
MODULE NOT FOUND - fuzz_crash_020.md:6:1:8:6
MODULE NOT FOUND - fuzz_crash_020.md:10:1:10:19
MODULE NOT FOUND - fuzz_crash_020.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_020.md:37:7:37:9
UNDEFINED VARIABLE - fuzz_crash_020.md:40:5:40:8
UNDEFINED VARIABLE - fuzz_crash_020.md:42:4:42:5
UNDEFINED VARIABLE - fuzz_crash_020.md:42:6:42:10
UNDEFINED VARIABLE - fuzz_crash_020.md:45:3:45:4
UNDEFINED VARIABLE - fuzz_crash_020.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_020.md:52:11:52:14
UNDEFINED VARIABLE - fuzz_crash_020.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_020.md:57:2:57:4
UNDEFINED VARIABLE - fuzz_crash_020.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_020.md:60:12:60:15
UNDEFINED VARIABLE - fuzz_crash_020.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_020.md:74:9:74:15
UNDEFINED VARIABLE - fuzz_crash_020.md:75:11:75:12
UNDEFINED VARIABLE - fuzz_crash_020.md:78:9:78:14
UNDEFINED VARIABLE - fuzz_crash_020.md:80:3:80:6
CRASH EXPECTS STRING - fuzz_crash_020.md:86:3:86:11
UNDEFINED VARIABLE - fuzz_crash_020.md:87:11:87:12
UNDEFINED VARIABLE - fuzz_crash_020.md:89:3:89:6
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_020.md:96:34:96:37
UNDEFINED VARIABLE - fuzz_crash_020.md:96:47:96:52
UNDEFINED VARIABLE - fuzz_crash_020.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_020.md:97:2:97:3
UNDEFINED VARIABLE - fuzz_crash_020.md:97:21:97:24
UNDEFINED VARIABLE - fuzz_crash_020.md:97:30:97:32
UNDEFINED VARIABLE - fuzz_crash_020.md:98:2:98:3
UNDEFINED VARIABLE - fuzz_crash_020.md:100:11:100:14
UNDEFINED VARIABLE - fuzz_crash_020.md:102:4:102:6
UNDEFINED VARIABLE - fuzz_crash_020.md:102:8:102:13
UNDEFINED VARIABLE - fuzz_crash_020.md:105:2:105:3
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_020.md:108:4:108:5
UNDEFINED VARIABLE - fuzz_crash_020.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_020.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_020.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_020.md:96:2:96:4
UNDECLARED TYPE - fuzz_crash_020.md:116:5:116:6
UNDEFINED VARIABLE - fuzz_crash_020.md:119:2:119:5
UNDEFINED VARIABLE - fuzz_crash_020.md:120:1:120:2
UNDEFINED VARIABLE - fuzz_crash_020.md:120:6:120:9
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_020.md:52:2:52:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:41:3:42:4:**
```roc
		dbg # bug
			s exp0
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:44:3:44:7:**
```roc
		dbg 123
```
		^^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:52:16:53:2:**
```roc
	match a {lue  {
	x
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:54:3:55:3:**
```roc
		}
		Blue=> {x
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:55:7:55:10:**
```roc
		Blue=> {x
```
		    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:58:12:58:15:**
```roc
			1	"for" => 20[1, ] # t
```
			 	      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:64:15:64:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:64:21:65:3:**
```roc
		3.14 | 6.28 => 314
		(1, ) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:65:9:65:12:**
```roc
		(1, ) => 123
```
		      ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:65:15:66:3:**
```roc
		(1, ) => 123
		(1, 2, 3)123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:67:8:67:11:**
```roc
		{ 	} => 12
```
		  	  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:68:11:68:14:**
```roc
		Ok(123) => 12
```
		        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:69:2:71:1:**
```roc
	}

expect # Cord
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:98:3:98:4:**
```roc
	m (
```
	 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:105:65:105:66:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:105:71:105:72:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:105:78:105:79:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:105:84:106:2:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"Ho${ #
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:107:3:108:4:**
```roc
		"Ho${ #
			r(nu) # xpr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **",
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:109:5:110:2:**
```roc
		} ",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:110:2:111:1:**
```roc
	)
} # Cocl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Cocl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:111:1:113:1:**
```roc
} # Cocl

y : {}
```


**UNDEFINED VARIABLE**
Nothing is named **s** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:42:4:42:5:**
```roc
			s exp0
```
			^


**UNDEFINED VARIABLE**
Nothing is named **exp0** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:42:6:42:10:**
```roc
			s exp0
```
			  ^^^^


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:45:3:45:4:**
```roc
		r
```
		^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:53:2:53:3:**
```roc
	x
```
	^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:55:11:55:12:**
```roc
		Blue=> {x
```
		        ^


**UNDEFINED VARIABLE**
Nothing is named **er** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:57:2:57:4:**
```roc
	er #ent
```
	^^


**UNDEFINED VARIABLE**
Nothing is named **ment** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:59:3:59:7:**
```roc
		ment
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **est** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:60:12:60:15:**
```roc
		[1, 2, 3,est]123
```
		         ^^^


**UNDEFINED VARIABLE**
Nothing is named **nt** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:72:2:72:4:**
```roc
	nt
```
	^^


**UNDEFINED VARIABLE**
Nothing is named **blaue** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:78:9:78:14:**
```roc
	expect blaue
```
	       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:80:3:80:6:**
```roc
		tag
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **ke** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:86:9:86:11:**
```roc
	)crash ke"Unr!" #)
```
	       ^^


**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:92:11:92:15:**
```roc
	for n in list {
```
	         ^^^^


**UNDEFINED VARIABLE**
Nothing is named **ber** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:94:3:94:6:**
```roc
		ber + n
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:96:34:96:37:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:96:47:96:52:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **ned** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:96:54:96:57:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                                    ^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:97:21:97:24:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                   ^^^


**UNDEFINED VARIABLE**
Nothing is named **nd** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:97:30:97:32:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                            ^^


**UNDEFINED VARIABLE**
Nothing is named **m** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:98:2:98:3:**
```roc
	m (
```
	^


**UNDEFINED VARIABLE**
Nothing is named **ag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:100:11:100:14:**
```roc
		"World",ag1,
```
		        ^^^


**UNDEFINED VARIABLE**
Nothing is named **ne** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:102:4:102:6:**
```roc
		(ne, tuple),
```
		 ^^


**UNDEFINED VARIABLE**
Nothing is named **tuple** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:102:8:102:13:**
```roc
		(ne, tuple),
```
		     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:105:2:105:3:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	^


**UNDEFINED VARIABLE**
Nothing is named **e_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:105:55:105:59:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                     ^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:105:60:105:64:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                          ^^^^


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:108:4:108:5:**
```roc
			r(nu) # xpr
```
			^


**UNDEFINED VARIABLE**
Nothing is named **nu** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:108:6:108:8:**
```roc
			r(nu) # xpr
```
			  ^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:119:2:119:5:**
```roc
	foo == 1
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **h** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:120:1:120:2:**
```roc
h == foo
```
^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_020.md:120:6:120:9:**
```roc
h == foo
```
     ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "line"))
    (type type_50)
  )
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.assign
    (pattern (Patt.ident "ane"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "one"))
    (type type_91)
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "me"))
    (Expr.lambda (canonicalized))
  )
  (Expr.lookup "x")
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.block
    (Expr.lookup "x")
  )
  (Expr.lookup "er")
  (Expr.num_literal_i32 1)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.num_literal_i32 20)
  (Expr.list_literal)
  (Expr.lookup "ment")
  (Expr.list_literal)
  (Expr.num_literal_i32 123)
  (Expr.list_literal)
  (Expr.num_literal_i32 23)
  (Expr.frac_literal_small 3.1)
  (Expr.num_literal_i32 314)
  (Expr.frac_literal_small 3.14)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.num_literal_i32 123)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.malformed)
  (Stmt.expr)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_171)
  )
  (Stmt.assign
    (pattern (Patt.ident "ma"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "y"))
    (type type_310)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.record_literal
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "t"))
    (type type_320)
  )
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 379
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
(var #82 -> #332)
(var #83 _)
(var #84 _)
(var #85 Num *)
(var #86 Num *)
(var #87 _)
(var #88 -> #332)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 -> #334)
(var #94 _)
(var #95 Num *)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 Num *)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 -> #334)
(var #108 _)
(var #109 -> #336)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 -> #336)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 _)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 Num *)
(var #125 Str)
(var #126 _)
(var #127 Num *)
(var #128 Num *)
(var #129 _)
(var #130 _)
(var #131 Num *)
(var #132 Num *)
(var #133 Num *)
(var #134 _)
(var #135 _)
(var #136 Num *)
(var #137 _)
(var #138 Num *)
(var #139 F64)
(var #140 Num *)
(var #141 F64)
(var #142 _)
(var #143 _)
(var #144 -> #341)
(var #145 Num *)
(var #146 _)
(var #147 _)
(var #148 -> #344)
(var #149 Num *)
(var #150 Num *)
(var #151 Num *)
(var #152 -> #343)
(var #153 _)
(var #154 Num *)
(var #155 -> #345)
(var #156 _)
(var #157 Num *)
(var #158 -> #347)
(var #159 Num *)
(var #160 _)
(var #161 _)
(var #162 Num *)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 _)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 -> #373)
(var #174 _)
(var #175 _)
(var #176 -> #177)
(var #177 Str)
(var #178 _)
(var #179 _)
(var #180 Num *)
(var #181 _)
(var #182 _)
(var #183 _)
(var #184 _)
(var #185 _)
(var #186 _)
(var #187 -> #356)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 Str)
(var #193 -> #194)
(var #194 Str)
(var #195 _)
(var #196 -> #202)
(var #197 _)
(var #198 _)
(var #199 _)
(var #200 Num *)
(var #201 Num *)
(var #202 _)
(var #203 _)
(var #204 _)
(var #205 _)
(var #206 _)
(var #207 Str)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 {})
(var #214 -> #358)
(var #215 _)
(var #216 Num *)
(var #217 _)
(var #218 _)
(var #219 Str)
(var #220 _)
(var #221 _)
(var #222 _)
(var #223 _)
(var #224 _)
(var #225 _)
(var #226 _)
(var #227 _)
(var #228 _)
(var #229 _)
(var #230 _)
(var #231 -> #358)
(var #232 _)
(var #233 -> #360)
(var #234 Num *)
(var #235 Str)
(var #236 _)
(var #237 _)
(var #238 _)
(var #239 _)
(var #240 -> #359)
(var #241 Num *)
(var #242 Num *)
(var #243 Num *)
(var #244 _)
(var #245 -> #360)
(var #246 _)
(var #247 -> #363)
(var #248 Num *)
(var #249 Str)
(var #250 _)
(var #251 _)
(var #252 _)
(var #253 _)
(var #254 -> #361)
(var #255 Num *)
(var #256 Num *)
(var #257 Num *)
(var #258 _)
(var #259 -> #362)
(var #260 _)
(var #261 _)
(var #262 Num *)
(var #263 -> #264)
(var #264 -> #265)
(var #265 -> #276)
(var #266 -> #267)
(var #267 -> #268)
(var #268 -> #269)
(var #269 -> #270)
(var #270 -> #275)
(var #271 -> #272)
(var #272 -> #273)
(var #273 -> #274)
(var #274 -> #275)
(var #275 -> #276)
(var #276 -> #277)
(var #277 -> #280)
(var #278 -> #279)
(var #279 -> #280)
(var #280 -> #281)
(var #281 Num *)
(var #282 -> #364)
(var #283 _)
(var #284 _)
(var #285 _)
(var #286 _)
(var #287 -> #366)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 -> #368)
(var #292 _)
(var #293 _)
(var #294 _)
(var #295 _)
(var #296 _)
(var #297 _)
(var #298 _)
(var #299 -> #371)
(var #300 -> #372)
(var #301 _)
(var #302 _)
(var #303 _)
(var #304 -> #373)
(var #305 _)
(var #306 _)
(var #307 _)
(var #308 _)
(var #309 _)
(var #310 _)
(var #311 _)
(var #312 -> #377)
(var #313 -> #377)
(var #314 _)
(var #315 _)
(var #316 _)
(var #317 _)
(var #318 _)
(var #319 _)
(var #320 _)
(var #321 _)
(var #322 _)
(var #323 Num *)
(var #324 _)
(var #325 _)
(var #326 _)
(var #327 _)
(var #328 _)
(var #329 _)
(var #330 _)
(var #331 _)
(var #332 fn_pure)
(var #333 _)
(var #334 fn_pure)
(var #335 _)
(var #336 fn_pure)
(var #337 _)
(var #338 _)
(var #339 _)
(var #340 _)
(var #341 <error>)
(var #342 _)
(var #343 tuple)
(var #344 <error>)
(var #345 {})
(var #346 _)
(var #347 fn_pure)
(var #348 _)
(var #349 _)
(var #350 _)
(var #351 _)
(var #352 _)
(var #353 _)
(var #354 _)
(var #355 _)
(var #356 fn_pure)
(var #357 {})
(var #358 record)
(var #359 tuple)
(var #360 tuple)
(var #361 tuple)
(var #362 tuple)
(var #363 fn_pure)
(var #364 fn_pure)
(var #365 _)
(var #366 fn_pure)
(var #367 _)
(var #368 fn_pure)
(var #369 _)
(var #370 _)
(var #371 _)
(var #372 fn_pure)
(var #373 fn_pure)
(var #374 _)
(var #375 _)
(var #376 _)
(var #377 {})
(var #378 _)
~~~
# TYPES
~~~roc
n : _d
er : _d
e : {}
y : _d
me : _arg -> _ret
a : _d
line : _d
w : Str
rd : { ned: _field }
ane : _arg -> _ret
add : _arg -> _ret
one : _d
main : _d
i : Str
num : _d
t : _d
ma : _arg -> _ret
~~~
