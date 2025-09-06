# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma Underscore CloseRound LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon OpenRound MalformedUnknownToken UpperIdent Comma UpperIdent OpArrow UpperIdent CloseRound LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "foo")
    (uc "U64")
  )
  (binop_colon
    (lc "bar")
    (apply_uc
      (uc "Thing")
      (tuple_literal
        (lc "a")
        (lc "b")
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "biz")
    (tuple_literal
      (lc "a")
      (lc "b")
      (lc "c")
    )
  )
  (binop_colon
    (lc "add_one")
    (malformed)
  )
  (uc "U8")
  (malformed)
  (uc "U16")
  (malformed)
  (uc "U32")
  (malformed)
  (binop_colon
    (not_lc "main")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (uc "String")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (record_literal)
          (underscore)
        )
      )
    )
  )
  (binop_colon
    (lc "tag_tuple")
    (apply_uc
      (uc "Value")
      (tuple_literal
        (lc "a")
        (lc "b")
        (lc "c")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : 
U8
, 
U16
-> 
U32
)
main! : List String -> Result({}, _)
tag_tuple : Value(a, b, c)
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_048.md:4:7:4:12
UNDECLARED TYPE - fuzz_crash_048.md:8:14:8:20
UNDECLARED TYPE - fuzz_crash_048.md:9:13:9:18
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:6:12:7:1:**
```roc
add_one : (
U8, U16 -> U32)
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:3:7:5:**
```roc
U8, U16 -> U32)
```
  ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **-> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:9:7:12:**
```roc
U8, U16 -> U32)
```
        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:15:8:1:**
```roc
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_048.md:6:12:7:1:**
```roc
add_one : (
U8, U16 -> U32)
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_2)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "bar"))
    (type type_10)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "biz"))
    (type type_16)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add_one"))
    (type type_19)
  )
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_36)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "tag_tuple"))
    (type type_44)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 50
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
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
~~~
# TYPES
~~~roc
add_one : _d
biz : _d
tag_tuple : _d
foo : _d
bar : _d
main : _d
~~~
