# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field: _, other: U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine UpperIdent OpColonEqual UpperIdent OpenRound Underscore CloseRound BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare BlankLine UpperIdent OpColonEqual OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly BlankLine UpperIdent OpColonEqual Underscore OpArrow Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine UpperIdent OpColonEqual OpenRound Underscore Comma UpperIdent CloseRound BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenRound String Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


BadType := _
foo : BadType
foo = 42
BadList := List _
bar : BadList
bar = [1, 2, 3]
BadRecord := {field : _, other : U32}
baz : BadRecord
baz = { field : "hi", other : 5 }
BadFunction := _ -> _
qux : BadFunction
qux = |x| x
BadTuple := (_, U32)
quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_type.md:3:9:3:11:**
```roc
BadType := _
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_type.md:8:9:8:11:**
```roc
BadList := List(_)
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_type.md:13:11:13:13:**
```roc
BadRecord := { field: _, other: U32 }
```
          ^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**underscore_error_type.md:16:9:16:20:**
```roc
baz = { field: "hi", other: 5 }
```
        ^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**underscore_error_type.md:16:22:16:30:**
```roc
baz = { field: "hi", other: 5 }
```
                     ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_type.md:18:13:18:15:**
```roc
BadFunction := _ -> _
```
            ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_type.md:23:10:23:12:**
```roc
BadTuple := (_, U32)
```
         ^^


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
