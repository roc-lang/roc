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
LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare LineComment UpperIdent CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent Comma OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment CloseRound Comma OpenRound LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LowerIdent CloseRound LineComment BlankLine LowerIdent OpColon OpenRound LineComment CloseRound LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare LineComment CloseSquare LineComment BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly Int KwIf LowerIdent OpenCurly KwDbg LineComment LowerIdent LowerIdent CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma UpperIdent Comma OpBar LineComment KwMatch LowerIdent OpenCurly LowerIdent OpenCurly LowerIdent CloseCurly UpperIdent OpFatArrow OpenCurly LowerIdent CloseCurly LowerIdent LineComment Int String OpFatArrow Int OpenSquare Int Comma CloseSquare LineComment LowerIdent OpenSquare Int Comma Int Comma Int Comma LowerIdent CloseSquare Int OpenSquare CloseSquare Int Float Int Float OpBar Float OpFatArrow Int OpenRound Int Comma CloseRound OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound Int OpenCurly CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent KwReturn LineComment LowerIdent BlankLine LineComment TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound KwCrash LowerIdent String LineComment LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound LowerIdent Comma CloseRound Comma Int Comma LineComment Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpDoubleQuestion Int OpGreaterThan Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpEquals Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
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
      (binop_pipe
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
                (binop_colon
                  (lc "s")
                  (lc "s")
                )
                (binop_colon
                  (lc "exp0")
                  (lc "exp0")
                )
              )
)
            (else               (block
                (malformed)
                (num_literal_i32 123)
                (binop_colon
                  (lc "r")
                  (lc "r")
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
    (lc "me")
    (lambda
      (body
        (match
          (scrutinee             (lc "a")
))
      )
      (args
        (lc "a")
        (uc "Tb")
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
            (binop_pipe
              (malformed)
              (dot_lc "od")
            )
          )
          (apply_anon
            (binop_pipe
              (malformed)
              (dot_lc "ned")
            )
          )
          (binop_pipe
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
add = |num| {
	1
	if num
		{
			dbg # bug
			
			s : s
			exp0 : exp0
		}
	else {
		dbg 
		123
		r : r
	}
}

me = |a, Tb| # As
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
	e : e
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
	? | .od()
	? | .ned()
	? | .recd
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
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:41:3:42:4:**
```roc
		dbg # bug
			s exp0
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:44:3:44:7:**
```roc
		dbg 123
```
		^^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_019.md:52:16:53:2:**
```roc
	match a {lue  {
	x
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:54:3:55:3:**
```roc
		}
		Blue=> {x
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:55:7:55:10:**
```roc
		Blue=> {x
```
		    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:58:12:58:15:**
```roc
			1	"for" => 20[1, ] # t
```
			 	      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:64:15:64:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_019.md:64:21:65:3:**
```roc
		3.14 | 6.28 => 314
		(1, ) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:65:9:65:12:**
```roc
		(1, ) => 123
```
		      ^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_019.md:65:15:66:3:**
```roc
		(1, ) => 123
		(1, 2, 3)123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:67:8:67:11:**
```roc
		{ 	} => 12
```
		  	  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:68:11:68:14:**
```roc
		Ok(123) => 12
```
		        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:69:2:71:1:**
```roc
	}

expect # Cord
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_019.md:98:3:98:4:**
```roc
	m (
```
	 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:105:65:105:66:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:105:71:105:72:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:105:78:105:79:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:105:84:106:2:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"Ho${ #
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:107:3:108:4:**
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

**fuzz_crash_019.md:109:5:110:2:**
```roc
		} ",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:110:2:111:1:**
```roc
	)
} # Cocl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Cocl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_019.md:111:1:113:1:**
```roc
} # Cocl

y : {}
```


**UNDEFINED VARIABLE**
Nothing is named **pf** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:4:8:4:10:**
```roc
import pf.Stdout exposing [line!]
```
       ^^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:53:2:53:3:**
```roc
	x
```
	^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:55:11:55:12:**
```roc
		Blue=> {x
```
		        ^


**UNDEFINED VARIABLE**
Nothing is named **er** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:57:2:57:4:**
```roc
	er #ent
```
	^^


**UNDEFINED VARIABLE**
Nothing is named **ment** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:59:3:59:7:**
```roc
		ment
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **est** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:60:12:60:15:**
```roc
		[1, 2, 3,est]123
```
		         ^^^


**UNDEFINED VARIABLE**
Nothing is named **nt** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:72:2:72:4:**
```roc
	nt
```
	^^


**UNDEFINED VARIABLE**
Nothing is named **blaue** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:78:9:78:14:**
```roc
	expect blaue
```
	       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:80:3:80:6:**
```roc
		tag
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **ke** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:86:9:86:11:**
```roc
	)crash ke"Unr!" #)
```
	       ^^


**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:92:11:92:15:**
```roc
	for n in list {
```
	         ^^^^


**UNDEFINED VARIABLE**
Nothing is named **ber** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:94:3:94:6:**
```roc
		ber + n
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:96:34:96:37:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:96:47:96:52:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:97:21:97:24:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                   ^^^


**UNDEFINED VARIABLE**
Nothing is named **nd** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:97:30:97:32:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                            ^^


**UNDEFINED VARIABLE**
Nothing is named **m** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:98:2:98:3:**
```roc
	m (
```
	^


**UNDEFINED VARIABLE**
Nothing is named **ag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:100:11:100:14:**
```roc
		"World",ag1,
```
		        ^^^


**UNDEFINED VARIABLE**
Nothing is named **ne** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:102:4:102:6:**
```roc
		(ne, tuple),
```
		 ^^


**UNDEFINED VARIABLE**
Nothing is named **tuple** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:102:8:102:13:**
```roc
		(ne, tuple),
```
		     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:105:2:105:3:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	^


**UNDEFINED VARIABLE**
Nothing is named **e_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:105:55:105:59:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                     ^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:105:60:105:64:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                          ^^^^


**UNDEFINED VARIABLE**
Nothing is named **nu** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:108:6:108:8:**
```roc
			r(nu) # xpr
```
			  ^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:119:2:119:5:**
```roc
	foo == 1
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **h** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:120:1:120:2:**
```roc
h == foo
```
^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_019.md:120:6:120:9:**
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
    (type type_175)
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
    (type type_315)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.record_literal
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "t"))
    (type type_325)
  )
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 381
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
(var #82 -> #337)
(var #83 _)
(var #84 _)
(var #85 Num *)
(var #86 Num *)
(var #87 _)
(var #88 -> #337)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 -> #339)
(var #94 _)
(var #95 Num *)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 Num *)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 -> #339)
(var #111 _)
(var #112 -> #342)
(var #113 _)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 -> #342)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 Num *)
(var #129 Str)
(var #130 _)
(var #131 Num *)
(var #132 Num *)
(var #133 _)
(var #134 _)
(var #135 Num *)
(var #136 Num *)
(var #137 Num *)
(var #138 _)
(var #139 _)
(var #140 Num *)
(var #141 _)
(var #142 Num *)
(var #143 F64)
(var #144 Num *)
(var #145 F64)
(var #146 _)
(var #147 _)
(var #148 -> #347)
(var #149 Num *)
(var #150 _)
(var #151 _)
(var #152 -> #350)
(var #153 Num *)
(var #154 Num *)
(var #155 Num *)
(var #156 -> #349)
(var #157 _)
(var #158 Num *)
(var #159 -> #351)
(var #160 _)
(var #161 Num *)
(var #162 -> #353)
(var #163 Num *)
(var #164 _)
(var #165 _)
(var #166 Num *)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 -> #375)
(var #178 _)
(var #179 _)
(var #180 _)
(var #181 -> #182)
(var #182 Str)
(var #183 _)
(var #184 _)
(var #185 Num *)
(var #186 _)
(var #187 _)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 -> #362)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 _)
(var #197 Str)
(var #198 -> #199)
(var #199 Str)
(var #200 _)
(var #201 -> #207)
(var #202 _)
(var #203 _)
(var #204 _)
(var #205 Num *)
(var #206 Num *)
(var #207 _)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 Str)
(var #213 _)
(var #214 _)
(var #215 _)
(var #216 _)
(var #217 _)
(var #218 {})
(var #219 -> #363)
(var #220 _)
(var #221 Num *)
(var #222 _)
(var #223 _)
(var #224 Str)
(var #225 _)
(var #226 _)
(var #227 _)
(var #228 _)
(var #229 _)
(var #230 _)
(var #231 _)
(var #232 _)
(var #233 _)
(var #234 _)
(var #235 _)
(var #236 -> #363)
(var #237 _)
(var #238 -> #365)
(var #239 Num *)
(var #240 Str)
(var #241 _)
(var #242 _)
(var #243 _)
(var #244 _)
(var #245 -> #364)
(var #246 Num *)
(var #247 Num *)
(var #248 Num *)
(var #249 _)
(var #250 -> #365)
(var #251 _)
(var #252 -> #368)
(var #253 Num *)
(var #254 Str)
(var #255 _)
(var #256 _)
(var #257 _)
(var #258 _)
(var #259 -> #366)
(var #260 Num *)
(var #261 Num *)
(var #262 Num *)
(var #263 _)
(var #264 -> #367)
(var #265 _)
(var #266 _)
(var #267 Num *)
(var #268 -> #269)
(var #269 -> #270)
(var #270 -> #281)
(var #271 -> #272)
(var #272 -> #273)
(var #273 -> #274)
(var #274 -> #275)
(var #275 -> #280)
(var #276 -> #277)
(var #277 -> #278)
(var #278 -> #279)
(var #279 -> #280)
(var #280 -> #281)
(var #281 -> #282)
(var #282 -> #285)
(var #283 -> #284)
(var #284 -> #285)
(var #285 -> #286)
(var #286 Num *)
(var #287 -> #369)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 _)
(var #292 -> #370)
(var #293 _)
(var #294 _)
(var #295 _)
(var #296 -> #371)
(var #297 _)
(var #298 _)
(var #299 _)
(var #300 _)
(var #301 _)
(var #302 _)
(var #303 _)
(var #304 -> #373)
(var #305 -> #374)
(var #306 _)
(var #307 _)
(var #308 _)
(var #309 -> #375)
(var #310 _)
(var #311 _)
(var #312 _)
(var #313 _)
(var #314 _)
(var #315 _)
(var #316 _)
(var #317 -> #379)
(var #318 -> #379)
(var #319 _)
(var #320 _)
(var #321 _)
(var #322 _)
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
(var #334 _)
(var #335 _)
(var #336 _)
(var #337 fn_pure)
(var #338 _)
(var #339 fn_pure)
(var #340 _)
(var #341 _)
(var #342 fn_pure)
(var #343 _)
(var #344 _)
(var #345 _)
(var #346 _)
(var #347 <error>)
(var #348 _)
(var #349 tuple)
(var #350 <error>)
(var #351 {})
(var #352 _)
(var #353 fn_pure)
(var #354 _)
(var #355 _)
(var #356 _)
(var #357 _)
(var #358 _)
(var #359 _)
(var #360 _)
(var #361 _)
(var #362 fn_pure)
(var #363 {})
(var #364 tuple)
(var #365 tuple)
(var #366 tuple)
(var #367 tuple)
(var #368 fn_pure)
(var #369 fn_pure)
(var #370 fn_pure)
(var #371 fn_pure)
(var #372 _)
(var #373 _)
(var #374 fn_pure)
(var #375 fn_pure)
(var #376 _)
(var #377 _)
(var #378 _)
(var #379 {})
(var #380 _)
~~~
# TYPES
~~~roc
s : _d
ma : _arg -> _ret
n : _d
rd : {}
er : _d
t : _d
line : _d
y : _d
a : _d
ane : _arg -> _ret
i : Str
exp0 : _d
add : _arg -> _ret
w : Str
e : {}
one : _d
main : _d
num : _d
me : _arg, _arg2 -> _ret
r : _d
~~~
