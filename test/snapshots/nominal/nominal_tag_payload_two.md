# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [MyResult, ok, is_ok]

MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
    MyResult.Ok(_) => Bool.True
    MyResult.Err(_) => Bool.False
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent Comma Underscore CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyResult")

    (lc "ok")

    (lc "is_ok")
))
~~~
# FORMATTED
~~~roc
module [MyResult, ok, is_ok]

MyResult((ok, err)) := [Ok(ok), Err(err)]
ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)
is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result

Ok(_)
=> 
Bool.True
MyResult.Err(_)
=> 
Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nominal_tag_payload_two.md:10:13:10:14:**
```roc
    MyResult.Ok(_) => Bool.True
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:10:20:10:23:**
```roc
    MyResult.Ok(_) => Bool.True
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:11:21:11:24:**
```roc
    MyResult.Err(_) => Bool.False
```
                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:12:1:12:2:**
```roc
}
```
^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**nominal_tag_payload_two.md:5:25:5:26:**
```roc
ok : ok -> MyResult(ok, _)
```
                        ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**nominal_tag_payload_two.md:10:17:10:18:**
```roc
    MyResult.Ok(_) => Bool.True
```
                ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**nominal_tag_payload_two.md:11:18:11:19:**
```roc
    MyResult.Err(_) => Bool.False
```
                 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "ok")
    (Expr.binop_thin_arrow
      (Expr.lookup "ok")
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "ok")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "is_ok")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok")
    (Expr.lambda)
  )
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
ok : _b
is_ok : _b
~~~
