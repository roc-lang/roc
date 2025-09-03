# META
~~~ini
description=Undeclared type usage should produce error
type=file
~~~
# SOURCE
~~~roc
module [MyType, processValue]

MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
    "processed"
}

AnotherType : SomeModule.MissingType
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly String CloseCurly BlankLine UpperIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyType")

    (lc "processValue")
))
~~~
# FORMATTED
~~~roc
module [MyType, processValue]

MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
	"processed"
}

AnotherType : SomeModule.MissingType
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:3:10:3:21
MODULE NOT IMPORTED - type_undeclared_usage.md:10:15:10:37
UNDECLARED TYPE - type_undeclared_usage.md:5:16:5:32
UNUSED VARIABLE - type_undeclared_usage.md:6:17:6:22
# PROBLEMS
**UNUSED VARIABLE**
Variable **value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:

**type_undeclared_usage.md:6:17:6:22:**
```roc
processValue = |value| {
```
                ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name "processValue")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processValue"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name node:uc)
    (type binop_pipe)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
