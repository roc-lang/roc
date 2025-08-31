# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person,
 age: 31 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "person")

    (lc "updated")
))
~~~
# FORMATTED
~~~roc
module [person, updated]


person = { name : "Alice", age : 30 }
updated = { ..person }
age : 31
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_047.md:4:11:5:2:**
```roc
updated = { ..person,
 age: 31 }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_047.md:5:10:5:11:**
```roc
 age: 31 }
```
         ^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**fuzz_crash_047.md:3:12:3:25:**
```roc
person = { name: "Alice", age: 30 }
```
           ^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**fuzz_crash_047.md:3:27:3:34:**
```roc
person = { name: "Alice", age: 30 }
```
                          ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_047.md:5:10:5:11:**
```roc
 age: 31 }
```
         ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
