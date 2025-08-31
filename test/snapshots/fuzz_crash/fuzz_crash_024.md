# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# TOKENS
~~~text
KwModule OpenSquare KwModule CloseSquare OpenCurly LowerIdent OpColon KwPlatform MalformedString BlankLine LineComment KwVar LowerIdent OpAssign CloseSquare BlankLine LineComment KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [module ]

{
	pf : platform 
	".-/main._]where # A

#el
	var t = ]

#el

	var t = 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:9:1:16:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
        ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **platform ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_024.md:1:24:1:33:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                       ^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **".-/main._]where # A

#el
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_024.md:1:33:4:1:**
```roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

#el
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_024.md:4:8:7:1:**
```roc
var t= ]

#el
var t= 0
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:18:7:9:**
```roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_024.md:1:18:7:9:**
```roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
```


# CANONICALIZE
~~~clojure
(Expr.block
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
