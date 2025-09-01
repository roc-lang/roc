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
{ name : "Alice", scores : [95, 87, 92, 78], status : Active {
	since : "2023-01-15"
}, preferences : {theme : Dark, notifications : Email "alice@example.com"}, metadata : Ok {tags : ["developer", "senior", "fullstack"], permissions : [Read, Write, Admin]}, callback : |x| (x + 1, nested) : {items : [Some("first"), None, Some("third")], result : Success {data : [1, 2, 3], timestamp : "2024-01-01"}} }
~~~
# EXPECTED
NIL
# PROBLEMS
**STATEMENT IN EXPRESSION CONTEXT**
Found a statement where an expression was expected.
Statements like **return**, **dbg**, or **expect** cannot be used in expression contexts.

**record_with_complex_types.md:4:22:4:41:**
```roc
    status: Active({ since: "2023-01-15" }),
```
                     ^^^^^^^^^^^^^^^^^^^


**STATEMENT IN EXPRESSION CONTEXT**
Found a statement where an expression was expected.
Statements like **return**, **dbg**, or **expect** cannot be used in expression contexts.

**record_with_complex_types.md:10:19:14:6:**
```roc
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
    },
```


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**record_with_complex_types.md:10:16:10:17:**
```roc
    callback: |x| x + 1,
```
               ^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (lc "name")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (lc "scores")
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (lc "status")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (lc "preferences")
    (Expr.record_literal
      (Expr.binop_colon
        (lc "theme")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (lc "notifications")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (lc "metadata")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (lc "callback")
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{ name: Str, scores: List(Num(_size)), status: _field, preferences: { theme: _field2, notifications: _field3 }, metadata: _field4, callback: _field5 }")
~~~
# TYPES
~~~roc
{ name: Str, scores: List(Num(_size)), status: _field, preferences: { theme: _field2, notifications: _field3 }, metadata: _field4, callback: _field5 }
~~~
