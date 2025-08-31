# META
~~~ini
description=Singleline with comma formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12,]
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : (a, a,) -> Str, module(a).a2 : (a, a,) -> Str,
B(b) : b where module(b).b1 : (b, b,) -> Str, module(b).b2 : (b, b,) -> Str,

C(a, b,) : (a, b,)
D(a, b,) : C(a, b,)
E : { a : Str, b : Str, }
F : [A, B,]

g : e -> e where module(e).A, module(e).B,

h = |x, y,| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
	h2 = h(x, y,)
	h3 = A(x, y,)
	h4 = [x, y,]
	h5 = (x, y,)

	match x {
		Z1((a, b,)) => a
		Z2(a, b,) => a
		Z3({ a, b, }) => a
		Z4([a, b,]) => a
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent Comma CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpArrow UpperIdent Comma BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma CloseRound BlankLine KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent Comma CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent Comma CloseCurly CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [I21]
as 
Ias1
, 
I22
as 
Ias2
,
]

# Where constraint
A(a) : ((((a where module(a).a1 : (a, a)) -> Str, module(a).a2 : (a, a) -> Str, B(b)) : b where module(b).b1 : (b, b)) -> Str, module(b).b2 : (b, b) -> Str, 

C(
	(a, b),
)) : (a, b)
D(
	(a, b),
) :
	C(a, b)
E :
	{
		a : Str,
		b : Str,
	}
F : [
	A,
	B,
]
g : e -> e where module(e) | A, module(e) | B, 

h = |
	x,
	y,
| {
	h1 = {
		h11 : x,
		h12 : x,
		h13 :
			{
				h131 : x,
				h132 : y,
			},
	}

	h2 = h(
		(x, y),
	)

	h3 = A(
		(x, y),
	)

	h4 = [
		x,
		y,
	]

	h5 = (x, y)

	match x
		Z1(a, b) => a
	Z3(
		{
			a : a,
			b : b,
		},
	)
	=> 
	a : a
	Z4(
		[
			a,
			b,
		],
	)
	=> 
	a : a
}

}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:25:5:28:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:32:5:34:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:38:5:41:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                     ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:45:5:46:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

# Where constraint
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:46:8:1:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : (a, a,) -> Str, module(a).a2 : (a, a,) -> Str,
```


**UNEXPECTED TOKEN IN PATTERN**
The token **) ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**everything.md:27:11:27:13:**
```roc
		Z2(a, b,) => a
```
		        ^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**everything.md:27:13:27:16:**
```roc
		Z2(a, b,) => a
```
		          ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**everything.md:27:16:28:3:**
```roc
		Z2(a, b,) => a
		Z3({ a, b, }) => a
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:28:17:28:20:**
```roc
		Z3({ a, b, }) => a
```
		              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:29:15:29:18:**
```roc
		Z4([a, b,]) => a
```
		            ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:31:1:31:2:**
```roc
}
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:25:5:28:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                        ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:28:5:32:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                           ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:32:5:34:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                               ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:34:5:37:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:38:5:41:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                     ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:41:5:45:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                        ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:45:5:46:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
                                            ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:46:8:1:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : (a, a,) -> Str, module(a).a2 : (a, a,) -> Str,
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**everything.md:16:5:30:3:**
```roc
g : e -> e where module(e).A, module(e).B,

h = |x, y,| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
	h2 = h(x, y,)
	h3 = A(x, y,)
	h4 = [x, y,]
	h5 = (x, y,)

	match x {
		Z1((a, b,)) => a
		Z2(a, b,) => a
		Z3({ a, b, }) => a
		Z4([a, b,]) => a
	}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:31:1:31:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_colon)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type apply_uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name "g")
    (type binop_equals)
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
