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
OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound Int Comma Int CloseRound OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound OpAssign OpenRound OpenRound Int Comma Int CloseRound Comma OpenRound Int Comma Int CloseRound CloseRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound Int Comma Int Comma Int CloseRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound String Comma String Comma UpperIdent CloseRound OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign OpenRound OpenSquare Int Comma Int Comma Int CloseSquare Comma String CloseRound OpenCurly CloseCurly CloseCurly ~~~
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
        (tuple_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
          (num_literal_i32 3)
        )
      )
      (str_literal_big "hello")
    )
  )
  (record_literal)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - tuple_patterns.md:4:6:4:7
UNUSED VARIABLE - tuple_patterns.md:4:9:4:10
UNUSED VARIABLE - tuple_patterns.md:7:7:7:8
UNUSED VARIABLE - tuple_patterns.md:7:10:7:11
UNUSED VARIABLE - tuple_patterns.md:7:15:7:16
UNUSED VARIABLE - tuple_patterns.md:7:18:7:19
UNUSED VARIABLE - tuple_patterns.md:10:6:10:11
UNUSED VARIABLE - tuple_patterns.md:10:13:10:19
UNUSED VARIABLE - tuple_patterns.md:10:21:10:26
UNUSED VARIABLE - tuple_patterns.md:13:6:13:10
UNUSED VARIABLE - tuple_patterns.md:13:12:13:18
UNUSED VARIABLE - tuple_patterns.md:13:20:13:27
UNUSED VARIABLE - tuple_patterns.md:16:6:16:10
UNUSED VARIABLE - tuple_patterns.md:16:12:16:17
# PROBLEMS
**Unsupported Node**
at 4:10 to 16:19

**Unsupported Node**
at 16:40 to 16:41

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "{}")
~~~
# TYPES
~~~roc
~~~
