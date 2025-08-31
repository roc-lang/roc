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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:3:19:3:21:**
```roc
MyResult(ok, err) := [Ok(ok), Err(err)]
```
                  ^^


**UNDEFINED VARIABLE**
Nothing is named **MyResult.Ok** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_payload_two.md:6:10:6:21:**
```roc
ok = |a| MyResult.Ok(a)
```
         ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:10:14:10:19:**
```roc
    MyResult.Ok(_) => Bool.True
```
             ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:10:20:10:23:**
```roc
    MyResult.Ok(_) => Bool.True
```
                   ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:10:23:10:32:**
```roc
    MyResult.Ok(_) => Bool.True
```
                      ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:11:5:11:20:**
```roc
    MyResult.Err(_) => Bool.False
```
    ^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:11:21:11:24:**
```roc
    MyResult.Err(_) => Bool.False
```
                    ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:11:24:11:34:**
```roc
    MyResult.Err(_) => Bool.False
```
                       ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload_two.md:12:1:12:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
