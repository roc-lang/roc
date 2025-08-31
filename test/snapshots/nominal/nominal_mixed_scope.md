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
NIL
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

**nominal_mixed_scope.md:3:13:3:15:**
```roc
LocalStatus := [Pending, Complete]
```
            ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_mixed_scope.md:9:5:9:21:**
```roc
    import Color.RGB
```
    ^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **LocalStatus.Pending** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_mixed_scope.md:12:20:12:39:**
```roc
        RGB.Red => LocalStatus.Pending
```
                   ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **RGB.Green** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_mixed_scope.md:13:9:13:18:**
```roc
        RGB.Green => LocalStatus.Complete
```
        ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **LocalStatus.Complete** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_mixed_scope.md:13:22:13:42:**
```roc
        RGB.Green => LocalStatus.Complete
```
                     ^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **RGB.Blue** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_mixed_scope.md:14:9:14:17:**
```roc
        RGB.Blue => LocalStatus.Pending
```
        ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **LocalStatus.Pending** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_mixed_scope.md:14:21:14:40:**
```roc
        RGB.Blue => LocalStatus.Pending
```
                    ^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_mixed_scope.md:16:1:16:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.type_anno
    (name "processColor")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processColor"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
