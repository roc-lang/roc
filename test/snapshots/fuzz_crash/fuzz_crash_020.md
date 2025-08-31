# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app { pf: "c" platform [main!] }

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
LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare LineComment UpperIdent CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent Comma OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment CloseRound Comma OpenRound LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LowerIdent CloseRound LineComment BlankLine LowerIdent OpColon OpenRound LineComment CloseRound LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment CloseCurly LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare LineComment CloseSquare LineComment BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar UpperIdent OpBar OpenCurly Int KwIf LowerIdent OpenCurly KwDbg LineComment LowerIdent LowerIdent CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment OpBar LineComment KwMatch LowerIdent OpenCurly LowerIdent OpenCurly LowerIdent CloseCurly UpperIdent OpFatArrow OpenCurly LowerIdent CloseCurly LowerIdent LineComment Int String OpFatArrow Int OpenSquare Int Comma CloseSquare LineComment LowerIdent OpenSquare Int Comma Int Comma Int Comma LowerIdent CloseSquare Int OpenSquare CloseSquare Int Float Int Float OpBar Float OpFatArrow Int OpenRound Int Comma CloseRound OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound Int OpenCurly CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent KwReturn LineComment LowerIdent BlankLine LineComment TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound KwCrash LowerIdent String LineComment LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound LowerIdent Comma CloseRound Comma Int Comma LineComment Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpenRound Int Comma String Comma LowerIdent Comma UpperIdent Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpDoubleQuestion Int OpGreaterThan Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpEquals Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_small "c")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
# Thnt!
app { pf: "c" platform [main!] }

import pf.Stdout exposing [line!]
import Stdot exposing [
	Cust,
]
import Bae as Gooe
import Ba
Map((a, b)) : Lis -> ab -> List b
MapML() : List -> ab -> List b
line : (,
)
Som : {foo : O, bar : g}
Ml(a) :
	{
	}
Soine(a) :
	{
	}
Maybe(a) : [Somne]
Mayine(a) : [,
]
ane = |num| if num 2 else 5
one : U6
add = |Rum| {
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

me = |a| match a

x
}
		
Blue
=> 
{
	x
}
er
1
"for"
=> 
20
[
	1,
]
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

expect nt
main! : Listlt({}, _)
ma = |
	_,
| {
	e : e
	w = "d"
	var er = 123
	expect blaue
	return tag
	...
	me(
		...,
	)
	crash ke
	"Unr!"
	i = "H, ${d}"
	t = [
		one(
			er,
		),
		456,
		9,
	]

	for n in list {
		{
			line!("Ag ${n} to ${er}")
			ber + n
		}
	}
	rd = { foo : 123, bar : "H", baz : tag, qux : Ok world, ned : ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m((
		123,
		"World",
		ag1,
		O,
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
NIL
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


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_020.md:65:9:65:12:**
```roc
		(1, ) => 123
```
		      ^^^


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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:4:1:4:34:**
```roc
import pf.Stdout exposing [line!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:6:1:8:6:**
```roc
import Stdot
		exposing [ #tem
Cust]
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:10:1:10:19:**
```roc
import Bae as Gooe
```
^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:11:1:12:4:**
```roc
import
	Ba
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:38:8:38:11:**
```roc
add = |Rum| {
```
       ^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**fuzz_crash_020.md:74:20:74:21:**
```roc
main! : Listlt({}, _)
```
                   ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:105:65:105:66:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                               ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:105:71:105:72:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                     ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_020.md:105:78:105:79:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	                                                                            ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.lookup "ab")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.lookup "ab")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "line")
    (Expr.tuple_literal
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
        (Expr.lookup "g")
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
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "ane")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "one")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "add")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "me")
    (Expr.lambda)
  )
  (Expr.lookup "x")
  (Expr.malformed)
  (Expr.apply_tag)
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
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.num_literal_i32 123)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.num_literal_i32 12)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "ma")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "y")
    (Expr.record_literal
    )
  )
  (Expr.binop_equals
    (Expr.lookup "e")
    (Expr.record_literal
    )
  )
  (Expr.binop_colon
    (Expr.lookup "t")
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
ane : _d
add : _d
me : _d
ma : _d
e : {}
~~~
