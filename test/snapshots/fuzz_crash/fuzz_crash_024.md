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
    (malformed)
))
(block
  (block
    (binop_colon
      (lc "pf")
      (malformed)
    )
    (malformed)
    (binop_equals
      (var_lc "t")
      (malformed)
    )
    (binop_equals
      (var_lc "t")
      (num_literal_i32 0)
    )
  )
)
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
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_024.md:1:9:1:15
PARSE ERROR - fuzz_crash_024.md:1:18:1:19
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_024.md:1:24:1:32
PARSE ERROR - fuzz_crash_024.md:1:33:1:34
PARSE ERROR - fuzz_crash_024.md:1:34:1:53
PARSE ERROR - fuzz_crash_024.md:1:53:1:53
PARSE ERROR - fuzz_crash_024.md:4:1:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:4:8:4:9
PARSE ERROR - fuzz_crash_024.md:7:1:7:4
MALFORMED TYPE - fuzz_crash_024.md:1:24:1:32
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:4:8:4:9
DUPLICATE DEFINITION - fuzz_crash_024.md:7:5:7:6
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Stmt.standalone_type_anno
      (pattern (Patt.ident "pf"))
      (type type_3)
    )
    (Expr.malformed)
    (Stmt.init_var
      (pattern (Patt.var_ident "t"))
      (Expr.malformed)
    )
    (Stmt.init_var
      (pattern (Patt.var_ident "t"))
      (Expr.num_literal_i32 0)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
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
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
~~~
# TYPES
~~~roc
pf : _a
t : _a
~~~
