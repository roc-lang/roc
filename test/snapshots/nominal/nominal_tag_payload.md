# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Maybe")

    (lc "some1")

    (lc "none1")

    (lc "some2")

    (lc "none2")
))
~~~
# FORMATTED
~~~roc
module [Maybe, some1, none1, some2, none2]


Maybe(a) := [Some(a), None]
some1 : a -> Maybe a
some1 = |a| Maybe.Some(a)
none1 : Maybe _a
none1 = Maybe.None
some2 = |a| Maybe.Some(a)
none2 = Maybe.None
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_payload.md:3:10:3:12:**
```roc
Maybe(a) := [Some(a), None]
```
         ^^


**UNDEFINED VARIABLE**
Nothing is named **Maybe.Some** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_payload.md:6:13:6:23:**
```roc
some1 = |a| Maybe.Some(a)
```
            ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Maybe.None** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_payload.md:9:9:9:19:**
```roc
none1 = Maybe.None
```
        ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Maybe.Some** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_payload.md:11:13:11:23:**
```roc
some2 = |a| Maybe.Some(a)
```
            ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Maybe.None** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_payload.md:13:9:13:19:**
```roc
none2 = Maybe.None
```
        ^^^^^^^^^^


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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
