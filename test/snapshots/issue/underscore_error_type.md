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


**UNDEFINED VARIABLE**
Nothing is named **field** in this scope.
Is there an **import** or **exposing** missing up-top?

**underscore_error_type.md:16:9:16:14:**
```roc
baz = { field: "hi", other: 5 }
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **other** in this scope.
Is there an **import** or **exposing** missing up-top?

**underscore_error_type.md:16:22:16:27:**
```roc
baz = { field: "hi", other: 5 }
```
                     ^^^^^


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
  (Stmt.malformed)
  (Stmt.type_anno
    (name "foo")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "bar")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "bar"))
    (Expr.list_literal)
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "baz")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "baz"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "field")
        (Expr.str_literal_small)
      )
      (Expr.binop_colon
        (Expr.lookup "other")
        (Expr.num_literal_i32 5)
      )
    )
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "qux")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "qux"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "quux")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "quux"))
    (Expr.tuple_literal
      (Expr.str_literal_big)
      (Expr.num_literal_i32 42)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
