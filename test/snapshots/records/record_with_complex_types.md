# META
~~~ini
description=Record construction with complex field types including lists and tag unions
type=expr
~~~
# SOURCE
~~~roc
{
    name: "Alice",
    scores: [95, 87, 92, 78],
    status: Active({ since: "2023-01-15" }),
    preferences: { theme: Dark, notifications: Email("alice@example.com") },
    metadata: Ok({
        tags: ["developer", "senior", "fullstack"],
        permissions: [Read, Write, Admin],
    }),
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
    },
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon OpenSquare Int Comma Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon UpperIdent OpenRound OpenCurly LowerIdent OpColon String CloseCurly CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent OpenRound String CloseRound CloseCurly Comma LowerIdent OpColon UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare Comma CloseCurly CloseRound Comma LowerIdent OpColon OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma LowerIdent OpColon OpenCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound String CloseRound Comma UpperIdent Comma UpperIdent OpenRound String CloseRound CloseSquare Comma LowerIdent OpColon UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly CloseRound Comma CloseCurly Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "name")
    (binop_colon
      (tuple_literal
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (binop_colon
                  (tuple_literal
                    (str_literal_big "Alice")
                    (lc "scores")
                  )
                  (list_literal
                    (tuple_literal
                      (num_literal_i32 95)
                      (num_literal_i32 87)
                      (num_literal_i32 92)
                      (num_literal_i32 78)
                    )
                  )
                )
                (lc "status")
              )
              (apply_uc
                (uc "Active")
                (block
                  (binop_colon
                    (lc "since")
                    (str_literal_big "2023-01-15")
                  )
                )
              )
            )
            (lc "preferences")
          )
          (block
            (binop_colon
              (lc "theme")
              (binop_colon
                (tuple_literal
                  (uc "Dark")
                  (lc "notifications")
                )
                (apply_uc
                  (uc "Email")
                  (str_literal_big "alice@example.com")
                )
              )
            )
          )
        )
        (lc "metadata")
      )
      (apply_uc
        (uc "Ok")
        (record_literal
          (binop_colon
            (lc "tags")
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (list_literal
                    (tuple_literal
                      (str_literal_big "developer")
                      (str_literal_big "senior")
                      (str_literal_big "fullstack")
                    )
                  )
                  (lc "permissions")
                )
                (list_literal
                  (tuple_literal
                    (uc "Read")
                    (uc "Write")
                    (uc "Admin")
                  )
                )
              )
              (malformed malformed:expr_unexpected_token)
            )
          )
          (malformed malformed:expr_unexpected_token)
          (binop_colon
            (lc "callback")
            (lambda
              (body
                (binop_colon
                  (tuple_literal
                    (binop_plus
                      (lc "x")
                      (num_literal_i32 1)
                    )
                    (lc "nested")
                  )
                  (block
                    (binop_colon
                      (lc "items")
                      (tuple_literal
                        (binop_colon
                          (tuple_literal
                            (list_literal
                              (tuple_literal
                                (apply_uc
                                  (uc "Some")
                                  (str_literal_big "first")
                                )
                                (uc "None")
                                (apply_uc
                                  (uc "Some")
                                  (str_literal_big "third")
                                )
                              )
                            )
                            (lc "result")
                          )
                          (apply_uc
                            (uc "Success")
                            (block
                              (binop_colon
                                (lc "data")
                                (binop_colon
                                  (tuple_literal
                                    (list_literal
                                      (tuple_literal
                                        (num_literal_i32 1)
                                        (num_literal_i32 2)
                                        (num_literal_i32 3)
                                      )
                                    )
                                    (lc "timestamp")
                                  )
                                  (str_literal_big "2024-01-01")
                                )
                              )
                            )
                          )
                        )
                        (malformed malformed:expr_unexpected_token)
                        (malformed malformed:expr_unexpected_token)
                      )
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
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:5 to 9:5

**Parse Error**
at 9:6 to 9:6

**Parse Error**
at 14:5 to 14:5

**Parse Error**
at 15:1 to 15:1

**Parse Error**
at 11:13 to 15:2

**Parse Error**
at 6:18 to 15:2

**Parse Error**
at 6:15 to 15:2

**Parse Error**
at 1:1 to 15:2

**Unsupported Node**
at 6:13 to 6:14

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.binop_colon
      (Expr.malformed)
      (Expr.apply_tag)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
