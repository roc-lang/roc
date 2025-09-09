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
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColon LowerIdent OpThinArrow UpperIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon Underscore OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwImport UpperIdent Dot UpperIdent BlankLine KwMatch LowerIdent OpenCurly UpperIdent OpThinArrow UpperIdent Dot UpperIdent UpperIdent OpThinArrow UpperIdent OpUnaryMinus UpperIdent UpperIdent Dot UpperIdent OpThinArrow UpperIdent Dot UpperIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "tus")

    (lc "r")
))
(block
  (binop_colon
    (uc "LocalStatus")
    (binop_thick_arrow
      (lc "lue")
      (binop_equals
        (uc "Loc")
        (list_literal
          (uc "Pending")
          (uc "Complete")
        )
      )
    )
  )
  (binop_colon
    (lc "olor")
    (binop_arrow_call
      (underscore)
      (lc "tus")
    )
  )
  (binop_equals
    (lc "olor")
    (lambda
      (body
        (block
          (import
            (binop_dot
              (uc "Color")
              (uc "RGB")
            )
          )
          (match
            (scrutinee               (lc "color")
)
            (branch1               (binop_thick_arrow
                (uc "RGB")
                (malformed)
              )
))
          (uc "Green")
          (malformed)
          (binop_minus
            (uc "LocalStatus")
            (uc "Complete")
          )
          (binop_dot
            (uc "B")
            (uc "Blue")
          )
          (malformed)
          (binop_dot
            (uc "LocalStatus")
            (uc "Pending")
          )
        )
      )
      (args
        (lc "color")
      )
    )
  )
  (malformed)
)
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
PARSE ERROR - fuzz_crash_032.md:3:24:3:25
PARSE ERROR - fuzz_crash_032.md:3:26:3:27
PARSE ERROR - fuzz_crash_032.md:3:34:3:35
PARSE ERROR - fuzz_crash_032.md:3:44:3:45
IMPORT MUST BE TOP LEVEL - fuzz_crash_032.md:6:18:6:24
UNEXPECTED TOKEN IN PATTERN - fuzz_crash_032.md:9:21:9:22
PARSE ERROR - fuzz_crash_032.md:9:22:9:22
UNDECLARED TYPE VARIABLE - fuzz_crash_032.md:3:14:3:17
UNDECLARED TYPE - fuzz_crash_032.md:3:21:3:24
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:6:25:6:30
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:8:26:8:37
INVALID PATTERN - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:10:3:10:4
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:10:13:10:24
EXPOSED BUT NOT DEFINED - fuzz_crash_032.md:1:13:1:14
EXPOSED BUT NOT DEFINED - fuzz_crash_032.md:1:9:1:12
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


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_032.md:3:21:3:45:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                    ^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_032.md:5:1:5:5:**
```roc
olor : _ -> tus
```
^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_032.md:6:18:6:34:**
```roc
olor = |color| { import Color.RGB
```
                 ^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_032.md:6:1:6:5:**
```roc
olor = |color| { import Color.RGB
```
^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "olor"))
    (type type_15)
  )
  (Stmt.assign
    (pattern (Patt.ident "olor"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 55
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
(var #17 -> #53)
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
(var #32 -> #33)
(var #33 -> #34)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #50)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 -> #52)
(var #42 _)
(var #43 -> #53)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 fn_pure)
(var #54 _)
~~~
# TYPES
~~~roc
olor : _arg -> _ret
color : _a
~~~
