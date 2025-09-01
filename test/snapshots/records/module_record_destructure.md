# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module [extract_age]

extract_age : { age : U64 } -> U64
extract_age = |person| {
    { age } = person

	{ a: 0 }.a + age - { a: 0 }.a
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenCurly LowerIdent CloseCurly OpAssign LowerIdent BlankLine OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent OpPlus LowerIdent OpBinaryMinus OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "extract_age")
))
~~~
# FORMATTED
~~~roc
module [extract_age]

extract_age : {
	age : U64
} -> U64
extract_age = |person| {
	{
		age : age
	} = person

	(({
		a : 0
	} | .a) + age) - ({
		a : 0
	} | .a)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**module_record_destructure.md:3:15:3:28:**
```roc
extract_age : { age : U64 } -> U64
```
              ^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**module_record_destructure.md:5:5:5:12:**
```roc
    { age } = person
```
    ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**module_record_destructure.md:7:2:7:12:**
```roc
	{ a: 0 }.a + age - { a: 0 }.a
```
	^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**module_record_destructure.md:7:15:7:18:**
```roc
	{ a: 0 }.a + age - { a: 0 }.a
```
	             ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**module_record_destructure.md:7:21:7:31:**
```roc
	{ a: 0 }.a + age - { a: 0 }.a
```
	                   ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "extract_age")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "extract_age"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
