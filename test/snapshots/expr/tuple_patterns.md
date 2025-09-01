# META
~~~ini
description=Tuple pattern matching tests
type=expr
~~~
# SOURCE
~~~roc
{

    # Simple tuple destructuring
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")

    {}
}
~~~
# TOKENS
~~~text
OpenCurly BlankLine LineComment OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound Int Comma Int CloseRound BlankLine LineComment OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound OpAssign OpenRound OpenRound Int Comma Int CloseRound Comma OpenRound Int Comma Int CloseRound CloseRound BlankLine LineComment OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound Int Comma Int Comma Int CloseRound BlankLine LineComment OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound String Comma String Comma UpperIdent CloseRound BlankLine LineComment OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound OpenSquare Int Comma Int Comma Int CloseSquare Comma String CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (binop_equals
      (binop_equals
        (binop_equals
          (binop_equals
            (tuple_literal
              (lc "x")
              (lc "y")
            )
            (apply_anon
              (tuple_literal
                (num_literal_i32 1)
                (num_literal_i32 2)
              )
              (tuple_literal
                (tuple_literal
                  (lc "a")
                  (lc "b")
                )
                (tuple_literal
                  (lc "c")
                  (lc "d")
                )
              )
            )
          )
          (apply_anon
            (tuple_literal
              (tuple_literal
                (num_literal_i32 10)
                (num_literal_i32 20)
              )
              (tuple_literal
                (num_literal_i32 30)
                (num_literal_i32 40)
              )
            )
            (tuple_literal
              (lc "first")
              (lc "second")
              (lc "third")
            )
          )
        )
        (apply_anon
          (tuple_literal
            (num_literal_i32 100)
            (num_literal_i32 42)
            (num_literal_i32 200)
          )
          (tuple_literal
            (lc "name")
            (lc "string")
            (lc "boolean")
          )
        )
      )
      (apply_anon
        (tuple_literal
          (str_literal_big "Alice")
          (str_literal_big "fixed")
          (uc "True")
        )
        (tuple_literal
          (lc "list")
          (lc "hello")
        )
      )
    )
    (tuple_literal
      (list_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
      (str_literal_big "hello")
    )
  )
  (record_literal)
)
~~~
# FORMATTED
~~~roc


# Simple tuple destructuring
(((((x, y) = (1, 2)(

# Nested tuple patterns
((a, b), (c, d)))

) = ((10, 20), (30, 40))(

# Mixed patterns with literals
(first, second, third))

) = (100, 42, 200)(

# Tuple with string and tag patterns
(name, string, boolean))

) = ("Alice", "fixed", True)(

# Tuple with list pattern
(list, hello))

) = ([1, 2, 3], "hello")

{}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tuple_patterns.md:4:6:16:18:**
```roc
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.malformed))
    (Expr.tuple_literal
      (Expr.list_literal)
      (Expr.str_literal_big)
    )
  )
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
