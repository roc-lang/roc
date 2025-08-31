# META
~~~ini
description=Mixed destructue patterns
type=expr
~~~
# SOURCE
~~~roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent Comma LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseCurly OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon OpenRound Int Comma Int CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_plus
        (binop_plus
          (binop_plus
            (binop_plus
              (lc "a")
              (lc "b")
            )
            (lc "c")
          )
          (lc "d")
        )
        (lc "e")
      )
    )
    (args
      (record_literal
        (binop_colon
          (lc "a")
          (lc "a")
        )
        (binop_colon
          (lc "x")
          (tuple_literal
            (lc "b")
            (lc "c")
          )
        )
        (binop_colon
          (lc "y")
          (record_literal
            (binop_colon
              (lc "d")
              (lc "d")
            )
            (binop_colon
              (lc "e")
              (lc "e")
            )
          )
        )
      )
    )
  )
  (record_literal
    (binop_colon
      (lc "a")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "x")
      (tuple_literal
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
    )
    (binop_colon
      (lc "y")
      (record_literal
        (binop_colon
          (lc "d")
          (num_literal_i32 4)
        )
        (binop_colon
          (lc "e")
          (num_literal_i32 5)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
(|{ a : a, x : (b, c), y : {d : d, e : e} }| (((a + b) + c) + d) + e)({ a : 1, x : (2, 3), y : {d : 4, e : 5} })
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**mixed_destructure_closure.md:1:55:1:59:**
```roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
```
                                                      ^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**mixed_destructure_closure.md:1:61:1:69:**
```roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
```
                                                            ^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**mixed_destructure_closure.md:1:72:1:87:**
```roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
```
                                                                       ^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_f")
~~~
# TYPES
~~~roc
_f
~~~
