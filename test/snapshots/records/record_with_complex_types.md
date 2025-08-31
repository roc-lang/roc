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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:2:5:2:18:**
```roc
    name: "Alice",
```
    ^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:3:5:3:29:**
```roc
    scores: [95, 87, 92, 78],
```
    ^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:4:5:4:44:**
```roc
    status: Active({ since: "2023-01-15" }),
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:5:5:5:76:**
```roc
    preferences: { theme: Dark, notifications: Email("alice@example.com") },
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:6:5:9:7:**
```roc
    metadata: Ok({
        tags: ["developer", "senior", "fullstack"],
        permissions: [Read, Write, Admin],
    }),
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_with_complex_types.md:10:5:14:6:**
```roc
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
    },
```


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
