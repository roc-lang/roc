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
(record_literal
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
  (binop_colon
    (lc "scores")
    (list_literal
      (num_literal_i32 95)
      (num_literal_i32 87)
      (num_literal_i32 92)
      (num_literal_i32 78)
    )
  )
  (binop_colon
    (lc "status")
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
  (binop_colon
    (lc "preferences")
    (record_literal
      (binop_colon
        (lc "theme")
        (uc "Dark")
      )
      (binop_colon
        (lc "notifications")
        (apply_uc
          (uc "Email")
          (str_literal_big "alice@example.com")
        )
      )
    )
  )
  (binop_colon
    (lc "metadata")
    (apply_uc
      (uc "Ok")
      (record_literal
        (binop_colon
          (lc "tags")
          (list_literal
            (str_literal_big "developer")
            (str_literal_big "senior")
            (str_literal_big "fullstack")
          )
        )
        (binop_colon
          (lc "permissions")
          (list_literal
            (uc "Read")
            (uc "Write")
            (uc "Admin")
          )
        )
      )
    )
  )
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
          (record_literal
            (binop_colon
              (lc "items")
              (list_literal
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
            (binop_colon
              (lc "result")
              (apply_uc
                (uc "Success")
                (record_literal
                  (binop_colon
                    (lc "data")
                    (list_literal
                      (num_literal_i32 1)
                      (num_literal_i32 2)
                      (num_literal_i32 3)
                    )
                  )
                  (binop_colon
                    (lc "timestamp")
                    (str_literal_big "2024-01-01")
                  )
                )
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
~~~
# FORMATTED
~~~roc
{ name: "Alice", scores: [95, 87, 92, 78], status: Active({
	since : "2023-01-15"
}), preferences: { theme: Dark, notifications: Email("alice@example.com") }, metadata: Ok({ tags: ["developer", "senior", "fullstack"], permissions: [Read, Write, Admin] }), callback: |x| (x + 1, nested) : {items: [Some("first"), None, Some("third")], result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" })} }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.list_literal)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.tag_applied)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.tag_no_args)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.tag_applied)
      )
    )
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.tag_applied)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 99
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 -> #5)
(var #7 -> #5)
(var #8 -> #5)
(var #9 -> #83)
(var #10 _)
(var #11 _)
(var #12 -> #86)
(var #13 _)
(var #14 Str)
(var #15 _)
(var #16 -> #85)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 -> #87)
(var #25 Str)
(var #26 _)
(var #27 _)
(var #28 -> #89)
(var #29 _)
(var #30 _)
(var #31 -> #94)
(var #32 _)
(var #33 Str)
(var #34 -> #33)
(var #35 -> #33)
(var #36 -> #90)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 -> #39)
(var #41 -> #39)
(var #42 -> #91)
(var #43 _)
(var #44 -> #93)
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
(var #56 Str)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 Str)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 Num *)
(var #68 Num *)
(var #69 Num *)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 Str)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 -> #96)
(var #81 _)
(var #82 -> #98)
(var #83 List #5)
(var #84 {})
(var #85 record)
(var #86 fn_pure)
(var #87 fn_pure)
(var #88 {})
(var #89 record)
(var #90 List #33)
(var #91 List #39)
(var #92 {})
(var #93 record)
(var #94 fn_pure)
(var #95 _)
(var #96 fn_pure)
(var #97 {})
(var #98 record)
~~~
# TYPES
~~~roc
x : _a
~~~
