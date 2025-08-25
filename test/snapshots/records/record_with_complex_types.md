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
        (tuple_literal
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
          (malformed malformed:expr_unexpected_token)
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
{
	name: "Alice",
	scores: [95, 87, 92, 78],
	status: Active({
		since: "2023-01-15"
	}),
	preferences: { theme: Dark, notifications: Email("alice@example.com") },
	metadata: Ok({
		tags: ["developer", "senior", "fullstack"],
		permissions: [Read, Write, Admin],
	}),
	callback: \
		x,
	 -> ((
		x + 1,
		nested,
	): {
		items: [Some("first"), None, Some("third")],
		result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
	}),
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 15:1 to 15:1

**Parse Error**
at 1:1 to 15:2

**Unsupported Node**
at 3:13 to 3:29

**Unsupported Node**
at 7:15 to 7:51

**Unsupported Node**
at 8:22 to 8:42

**Unsupported Node**
at 10:15 to 10:19

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "scores")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "status")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "preferences")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "theme")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "notifications")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "metadata")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "callback")
    (Expr.malformed)
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
