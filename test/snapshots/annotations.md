# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

Pair(a) := [Pair(a, a)]

pairU64 : Pair(U64)
pairU64 = Pair.Pair(1, 2)

pairStr : Pair(Str)
pairStr = Pair.Pair("hello", "world")

mkPair : a, a -> Pair(a)
mkPair = |x, y| Pair.Pair(x, y)

succeedPairSameType : Pair(U8)
succeedPairSameType = mkPair(1, 2)

failPairDiffTypes : Pair(U8)
failPairDiffTypes = mkPair("1", 2)

failPairDiffTypes2 : Pair(U64)
failPairDiffTypes2 = Pair.Pair(1, "str")

mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)

mkPairInferred = |x, y| Pair.Pair(x, y)

failWithImplicit = mkPairInferred("str", 2)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound String Comma String CloseRound BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound String Comma Int CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int Comma String CloseRound BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpAssign LowerIdent OpenRound String Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

Pair(a) := [Pair((a, a))]

pairU64 : Pair U64
pairU64 = Pair.Pair((1, 2))

pairStr : Pair Str
pairStr = Pair.Pair(("hello", "world"))

mkPair : a -> a -> Pair a
mkPair = |x, y| Pair.Pair((x, y))

succeedPairSameType : Pair U8
succeedPairSameType = mkPair((1, 2))

failPairDiffTypes : Pair U8
failPairDiffTypes = mkPair(("1", 2))

failPairDiffTypes2 : Pair U64
failPairDiffTypes2 = Pair.Pair((1, "str"))

mkPairInvalid : a -> b -> Pair a
mkPairInvalid = |x, y| Pair.Pair((x, y))

mkPairInferred = |x, y| Pair.Pair((x, y))

failWithImplicit = mkPairInferred(("str", 2))
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**annotations.md:3:9:3:11:**
```roc
Pair(a) := [Pair(a, a)]
```
        ^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:6:11:6:20:**
```roc
pairU64 = Pair.Pair(1, 2)
```
          ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:9:11:9:20:**
```roc
pairStr = Pair.Pair("hello", "world")
```
          ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:12:17:12:26:**
```roc
mkPair = |x, y| Pair.Pair(x, y)
```
                ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:21:22:21:31:**
```roc
failPairDiffTypes2 = Pair.Pair(1, "str")
```
                     ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:24:24:24:33:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
                       ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Pair.Pair** in this scope.
Is there an **import** or **exposing** missing up-top?

**annotations.md:26:25:26:34:**
```roc
mkPairInferred = |x, y| Pair.Pair(x, y)
```
                        ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.type_anno
    (name "pairU64")
    (type <mutated_tag:165>)
  )
  (Stmt.assign
    (pattern (Patt.ident "pairU64"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "pairStr")
    (type <mutated_tag:165>)
  )
  (Stmt.assign
    (pattern (Patt.ident "pairStr"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "mkPair")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "succeedPairSameType")
    (type <mutated_tag:165>)
  )
  (Stmt.assign
    (pattern (Patt.ident "succeedPairSameType"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "failPairDiffTypes")
    (type <mutated_tag:165>)
  )
  (Stmt.assign
    (pattern (Patt.ident "failPairDiffTypes"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "failPairDiffTypes2")
    (type <mutated_tag:165>)
  )
  (Stmt.assign
    (pattern (Patt.ident "failPairDiffTypes2"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "mkPairInvalid")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPairInvalid"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "mkPairInferred"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "failWithImplicit"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
