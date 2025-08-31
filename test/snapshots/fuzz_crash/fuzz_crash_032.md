# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
module [tus,r]

LocalStatus :lue => Loc= [Pending, Complete]

olor : _ -> tus
olor = |color| { import Color.RGB

    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
  B.Blue => LocalStatus.Pending
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColon LowerIdent OpFatArrow UpperIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon Underscore OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwImport UpperIdent Dot UpperIdent BlankLine KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent OpFatArrow UpperIdent OpUnaryMinus UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "tus")

    (lc "r")
))
~~~
# FORMATTED
~~~roc
module [tus, r]

LocalStatus : lue => Loc = [Pending, Complete]
olor : _ -> tus
olor = |color| {
	import Color.RGB
	match color
		RGB => LocalStatus
	Green
	=> 
	LocalStatus - Complete
	B.Blue
	=> 
	LocalStatus.Pending
}

}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **LocalStatus** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_032.md:8:26:8:37:**
```roc
    match color { RGB => LocalStatus.Pending
```
                         ^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**fuzz_crash_032.md:8:37:8:38:**
```roc
    match color { RGB => LocalStatus.Pending
```
                                    ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_032.md:8:38:9:1:**
```roc
    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_032.md:9:7:9:10:**
```roc
Green => LocalStatus-Complete
```
      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_032.md:10:10:10:13:**
```roc
  B.Blue => LocalStatus.Pending
```
         ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_032.md:12:1:12:2:**
```roc
}
```
^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**fuzz_crash_032.md:5:8:5:9:**
```roc
olor : _ -> tus
```
       ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_032.md:6:18:6:34:**
```roc
olor = |color| { import Color.RGB
```
                 ^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_032.md:8:19:8:37:**
```roc
    match color { RGB => LocalStatus.Pending
```
                  ^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.binop_equals
        (Expr.apply_tag)
        (Expr.list_literal)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "olor")
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.lookup "tus")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "olor")
    (Expr.lambda)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
olor : _a
~~~
