# META
~~~ini
description=Multiple type aliases with cross-references
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

UserId : U64
UserName : Str
UserAge : U8
User : { id : UserId, name : UserName, age : UserAge }

create_user : UserId, UserName, UserAge -> User
create_user = |id, name, age| { id, name, age }

get_user_name : User -> UserName
get_user_name = |user| user.name

main! = |_| {
	user = create_user(123, "Alice", 25)
	get_user_name(user)
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int Comma String Comma Int CloseRound LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (uc "UserName")
    (uc "Str")
  )
  (binop_colon
    (uc "UserAge")
    (uc "U8")
  )
  (binop_colon
    (uc "User")
    (block
      (binop_colon
        (lc "id")
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (uc "UserId")
                (lc "name")
              )
              (uc "UserName")
            )
            (lc "age")
          )
          (uc "UserAge")
        )
      )
    )
  )
  (binop_colon
    (lc "create_user")
    (binop_thin_arrow
      (uc "UserId")
      (binop_thin_arrow
        (uc "UserName")
        (binop_thin_arrow
          (uc "UserAge")
          (uc "User")
        )
      )
    )
  )
  (binop_equals
    (lc "create_user")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "id")
            (lc "id")
          )
          (tuple_literal
            (lc "name")
            (lc "age")
          )
        )
      )
      (args
        (tuple_literal
          (lc "id")
          (lc "name")
          (lc "age")
        )
      )
    )
  )
  (binop_colon
    (lc "get_user_name")
    (binop_thin_arrow
      (uc "User")
      (uc "UserName")
    )
  )
  (binop_equals
    (lc "get_user_name")
    (lambda
      (body
        (binop_pipe
          (lc "user")
          (dot_lc "name")
        )
      )
      (args
        (lc "user")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (block
      (binop_equals
        (lc "user")
        (apply_lc
          (lc "create_user")
          (tuple_literal
            (num_literal_i32 123)
            (str_literal_big "Alice")
            (num_literal_i32 25)
          )
        )
      )
      (apply_lc
        (lc "get_user_name")
        (lc "user")
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
at 14:7 to 14:7

**Pattern in Expression Context**
at 3:1 to 3:7

**Pattern in Expression Context**
at 3:10 to 3:13

**Pattern in Expression Context**
at 4:1 to 4:9

**Pattern in Expression Context**
at 4:12 to 4:15

**Pattern in Expression Context**
at 5:1 to 5:8

**Pattern in Expression Context**
at 5:11 to 5:13

**Pattern in Expression Context**
at 6:1 to 6:5

**Unsupported Node**
at 6:44 to 6:44

**Pattern in Expression Context**
at 6:46 to 6:53

**Unsupported Node**
at 8:15 to 8:48

**Unsupported Node**
at 9:15 to 9:31

**Unsupported Node**
at 11:17 to 11:33

**Unsupported Node**
at 12:17 to 12:24

**Unsupported Node**
at 14:5 to 14:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "id")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.malformed)
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "create_user")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "get_user_name")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
