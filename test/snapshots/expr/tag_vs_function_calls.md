# META
~~~ini
description=Tag applications vs function calls
type=expr
~~~
# SOURCE
~~~roc
{
    someTag: Some(42),
    noneTag: None,
    okTag: Ok("hello"),
    errTag: Err("oops"),
    addOne: |x| x + 1,
    result: addOne(5),
    nested: Some(Ok(Just(42))),
    tagList: [Some(1), Some(2), None, Some(3)],
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon UpperIdent OpenRound Int CloseRound Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent OpenRound String CloseRound Comma LowerIdent OpColon UpperIdent OpenRound String CloseRound Comma LowerIdent OpColon OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma LowerIdent OpColon LowerIdent OpenRound Int CloseRound Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound Int CloseRound CloseRound CloseRound Comma LowerIdent OpColon OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent OpenRound Int CloseRound Comma UpperIdent Comma UpperIdent OpenRound Int CloseRound CloseSquare Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "someTag")
    (apply_uc
      (uc "Some")
      (num_literal_i32 42)
    )
  )
  (binop_colon
    (lc "noneTag")
    (uc "None")
  )
  (binop_colon
    (lc "okTag")
    (apply_uc
      (uc "Ok")
      (str_literal_big "hello")
    )
  )
  (binop_colon
    (lc "errTag")
    (apply_uc
      (uc "Err")
      (str_literal_small "oops")
    )
  )
  (binop_colon
    (lc "addOne")
    (lambda
      (body
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (binop_colon
                  (tuple_literal
                    (binop_plus
                      (lc "x")
                      (num_literal_i32 1)
                    )
                    (lc "result")
                  )
                  (apply_lc
                    (lc "addOne")
                    (num_literal_i32 5)
                  )
                )
                (lc "nested")
              )
              (apply_uc
                (uc "Some")
                (apply_uc
                  (uc "Ok")
                  (apply_uc
                    (uc "Just")
                    (num_literal_i32 42)
                  )
                )
              )
            )
            (lc "tagList")
          )
          (list_literal
            (apply_uc
              (uc "Some")
              (num_literal_i32 1)
            )
            (apply_uc
              (uc "Some")
              (num_literal_i32 2)
            )
            (uc "None")
            (apply_uc
              (uc "Some")
              (num_literal_i32 3)
            )
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
{ someTag: Some(42), noneTag: None, okTag: Ok("hello"), errTag: Err("oops"), addOne: |x| (((x + 1, result) : addOne(5), nested) : Some Ok Just 42, tagList) : [Some(1), Some(2), None, Some(3)] }
~~~
# EXPECTED
UNDEFINED VARIABLE - tag_vs_function_calls.md:7:13:7:19
# PROBLEMS
**STATEMENT IN EXPRESSION CONTEXT**
Found a statement where an expression was expected.
Statements like **return**, **dbg**, or **expect** cannot be used in expression contexts.

**tag_vs_function_calls.md:6:17:9:47:**
```roc
    addOne: |x| x + 1,
    result: addOne(5),
    nested: Some(Ok(Just(42))),
    tagList: [Some(1), Some(2), None, Some(3)],
```


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.tag_applied)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.tag_no_args)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.tag_applied)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.tag_applied)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 58
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 Str)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 Str)
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
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 -> #57)
(var #57 {})
~~~
# TYPES
~~~roc
x : _a
~~~
