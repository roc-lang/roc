# META
~~~ini
description=Multiline without comma formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [
	I11,
	I12
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]

# Where constraint
A(a) : a
	where
		module(a).a1 : (
			a,
			a
		) -> Str,
		module(a).a2 : (
			a,
			a
		) -> Str
B(b) : b
	where
		module(b).b1 : (
			b,
			b
		) -> Str,
		module(b).b2 : (
			b,
			b
		) -> Str

C(
	a,
	b
) : (
	a,
	b
)
D(
	a,
	b
) : C(
	a,
	b
)
E : {
	a : Str,
	b : Str
}
F : [
	A,
	B
]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y
		}
	}
	h2 = h(
		x,
		y
	)
	h3 = A(
		x,
		y
	)
	h4 = [
		x,
		y
	]
	h5 = (
		x,
		y
	)

	match x {
		Z1(
			(
				a,
				b
			)
		) => a
		Z2(
			a,
			b
		) => a
		Z3(
			{
				a,
				b
			}
		) => a
		Z4(
			[
				a,
				b
			]
		) => a
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment KwImport UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent Comma UpperIdent KwAs UpperIdent CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseRound OpFatArrow LowerIdent UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12]
import I2 exposing [I21]
as 
Ias1
,
	
I22
as 
Ias2
]

# Where constraint
A(a) : (a where module(a).a1 : (a, a)) -> Str, module(a).a2 : (a, a) -> Str
B(b) : (b where module(b).b1 : (b, b)) -> Str, module(b).b2 : (b, b) -> Str

C((a, b)) : (a, b)
D((a, b)) : C(a, b)
E : {a : Str, b : Str}
F : [A, B]

g : e -> e where module(e) | A, module(e) | B

h = |x, y| {
	h1 = { h11 : x, h12 : x, h13 : {h131 : x, h132 : y} }
	h2 = h((x, y))
	h3 = A((x, y))
	h4 = [x, y]
	h5 = (x, y)

	match x
		Z1(a, b) => a
		Z2(a, b) => a
		Z3({a, b}) => a
		Z4([a, b]) => a
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:9:6:9:9:**
```roc
	I21 as Ias1,
```
	    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:9:13:10:2:**
```roc
	I21 as Ias1,
	I22 as Ias2
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:10:6:10:9:**
```roc
	I22 as Ias2
```
	    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

# Where constraint
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:11:1:14:1:**
```roc
]

# Where constraint
A(a) : a
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:9:6:9:9:**
```roc
	I21 as Ias1,
```
	    ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:9:9:9:13:**
```roc
	I21 as Ias1,
```
	       ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:9:13:10:2:**
```roc
	I21 as Ias1,
	I22 as Ias2
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:10:2:10:5:**
```roc
	I22 as Ias2
```
	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:10:6:10:9:**
```roc
	I22 as Ias2
```
	    ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:10:9:10:13:**
```roc
	I22 as Ias2
```
	       ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:11:1:14:1:**
```roc
]

# Where constraint
A(a) : a
```


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**everything.md:58:24:58:27:**
```roc
g : e -> e where module(e).A, module(e).B
```
                       ^^^


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**everything.md:58:27:58:29:**
```roc
g : e -> e where module(e).A, module(e).B
```
                          ^^


**UNDEFINED VARIABLE**
Nothing is named **h11** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:62:3:62:6:**
```roc
		h11: x,
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **h12** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:63:3:63:6:**
```roc
		h12: x,
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **h13** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:64:3:64:6:**
```roc
		h13: {
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **h131** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:65:4:65:8:**
```roc
			h131: x,
```
			^^^^


**UNDEFINED VARIABLE**
Nothing is named **h132** in this scope.
Is there an **import** or **exposing** missing up-top?

**everything.md:66:4:66:8:**
```roc
			h132: y
```
			^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:87:3:92:9:**
```roc
		Z1(
			(
				a,
				b
			)
		) => a
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:97:3:102:9:**
```roc
		Z3(
			{
				a,
				b
			}
		) => a
```


**UNUSED VARIABLE**
Variable **h1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:

**everything.md:61:2:61:4:**
```roc
	h1 = {
```
	^^


**UNUSED VARIABLE**
Variable **h2** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:

**everything.md:69:2:69:4:**
```roc
	h2 = h(
```
	^^


**UNUSED VARIABLE**
Variable **h3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:

**everything.md:73:2:73:4:**
```roc
	h3 = A(
```
	^^


**UNUSED VARIABLE**
Variable **h5** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:

**everything.md:81:2:81:4:**
```roc
	h5 = (
```
	^^


**UNUSED VARIABLE**
Variable **h4** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:

**everything.md:77:2:77:4:**
```roc
	h4 = [
```
	^^


**UNUSED VARIABLE**
Variable **b** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:

**everything.md:106:5:106:6:**
```roc
				b
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
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type tuple_literal)
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
    (type tuple_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
