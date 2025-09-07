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
(((((x, y) = (1, 2)( # Nested tuple patterns
((a, b), (c, d)))

) = ((10, 20), (30, 40))( # Mixed patterns with literals
(first, second, third))

) = (100, 42, 200)( # Tuple with string and tag patterns
(name, string, boolean))

) = ("Alice", "fixed", True)( # Tuple with list pattern
(list, hello))

) = ([1, 2, 3], "hello")
{}
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
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:4:19:7:5:**
```roc
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:7:42:10:5:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:10:43:13:5:**
```roc
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:13:54:16:5:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")
```


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
; Total type variables: 60
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 -> #58)
(var #48 Num *)
(var #49 -> #48)
(var #50 -> #48)
(var #51 -> #57)
(var #52 Str)
(var #53 -> #58)
(var #54 _)
(var #55 -> #59)
(var #56 _)
(var #57 List #48)
(var #58 tuple)
(var #59 {})
~~~
# TYPES
~~~roc
~~~
