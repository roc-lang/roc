# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=file
~~~
# SOURCE
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon Underscore OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly BlankLine LineComment KwImport UpperIdent Dot UpperIdent BlankLine KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "LocalStatus")

    (lc "processColor")
))
(block
  (binop_colon_equals
    (uc "LocalStatus")
    (list_literal
      (uc "Pending")
      (uc "Complete")
    )
  )
  (binop_colon
    (lc "processColor")
    (binop_arrow_call
      (underscore)
      (uc "LocalStatus")
    )
  )
  (binop_equals
    (lc "processColor")
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
))
          (uc "Red")
          (malformed)
          (binop_dot
            (uc "LocalStatus")
            (uc "Pending")
          )
          (binop_dot
            (uc "RGB")
            (uc "Green")
          )
          (malformed)
          (binop_dot
            (uc "LocalStatus")
            (uc "Complete")
          )
          (binop_dot
            (uc "RGB")
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
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]
processColor : _ -> LocalStatus
processColor = |color| {
	# bring RGB into scope
	import Color.RGB
	match color
	Red
	=> 
	LocalStatus.Pending
	RGB.Green
	=> 
	LocalStatus.Complete
	RGB.Blue
	=> 
	LocalStatus.Pending
}

}
~~~
# EXPECTED
IMPORT MUST BE TOP LEVEL - nominal_mixed_scope.md:9:5:9:11
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - nominal_mixed_scope.md:9:12:9:17
UNDECLARED TYPE - nominal_mixed_scope.md:12:9:12:12
UNDECLARED TYPE - nominal_mixed_scope.md:13:9:13:12
UNDECLARED TYPE - nominal_mixed_scope.md:14:9:14:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nominal_mixed_scope.md:12:12:12:13:**
```roc
        RGB.Red => LocalStatus.Pending
```
           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_mixed_scope.md:12:17:12:20:**
```roc
        RGB.Red => LocalStatus.Pending
```
                ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_mixed_scope.md:13:19:13:22:**
```roc
        RGB.Green => LocalStatus.Complete
```
                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_mixed_scope.md:14:18:14:21:**
```roc
        RGB.Blue => LocalStatus.Pending
```
                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_mixed_scope.md:16:1:16:2:**
```roc
}
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_mixed_scope.md:9:5:9:21:**
```roc
    import Color.RGB
```
    ^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processColor"))
    (type type_11)
  )
  (Stmt.assign
    (pattern (Patt.ident "processColor"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 54
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
(var #13 -> #52)
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
(var #43 -> #52)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 fn_pure)
(var #53 _)
~~~
# TYPES
~~~roc
color : _a
processColor : _arg -> _ret
~~~
