# META
~~~ini
description=Singleline formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12]
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
B(b) : b where module(b).b1 : (b, b) -> Str, module(b).b2 : (b, b) -> Str

C(a, b) : (a, b)
D(a, b) : C(a, b)
E : { a : Str, b : Str }
F : [A, B]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
	h2 = h(x, y)
	h3 = A(x, y)
	h4 = [x, y]
	h5 = (x, y)

	match x {
		Z1((a, b)) => a
		Z2(a, b) => a
		Z3({ a, b }) => a
		Z4([a, b]) => a
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

**everything.md:5:25:5:28:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:32:5:34:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:38:5:41:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                                     ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

# Where constraint
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**everything.md:5:45:8:1:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:25:5:28:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                        ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:28:5:32:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                           ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:32:5:34:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                               ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:34:5:37:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                                 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:38:5:41:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                                     ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:41:5:45:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
                                        ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:5:45:8:1:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**everything.md:19:9:19:15:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
	       ^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**everything.md:19:17:19:23:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
	               ^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**everything.md:19:25:19:50:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
	                       ^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:26:3:26:18:**
```roc
		Z1((a, b)) => a
```
		^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**everything.md:28:3:28:20:**
```roc
		Z3({ a, b }) => a
```
		^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **h1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:

**everything.md:19:2:19:4:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
	^^


**UNUSED VARIABLE**
Variable **h2** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:

**everything.md:20:2:20:4:**
```roc
	h2 = h(x, y)
```
	^^


**UNUSED VARIABLE**
Variable **h3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:

**everything.md:21:2:21:4:**
```roc
	h3 = A(x, y)
```
	^^


**UNUSED VARIABLE**
Variable **h5** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:

**everything.md:23:2:23:4:**
```roc
	h5 = (x, y)
```
	^^


**UNUSED VARIABLE**
Variable **h4** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:

**everything.md:22:2:22:4:**
```roc
	h4 = [x, y]
```
	^^


**UNUSED VARIABLE**
Variable **b** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:

**everything.md:29:10:29:11:**
```roc
		Z4([a, b]) => a
```
		       ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
