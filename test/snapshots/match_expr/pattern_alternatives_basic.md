# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=file
~~~
# SOURCE
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "kind")
))
~~~
# FORMATTED
~~~roc
module [kind]


Color : [Red, Green, Blue, Yellow, Orange, Purple]
kind : Color -> Str
kind = |color| match color
	(Red || Green) || Blue => "primary"
	(Yellow || Orange) || Purple => "secondary"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_basic.md:7:5:7:36:**
```roc
    Red | Green | Blue => "primary"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_basic.md:8:5:8:29:**
```roc
    Yellow | Orange | Purple => "secondary"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
