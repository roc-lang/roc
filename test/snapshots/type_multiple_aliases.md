# META
~~~ini
description=Multiple type aliases with cross-references
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int Comma String Comma Int CloseRound LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
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
    (record_literal
      (binop_colon
        (lc "id")
        (uc "UserId")
      )
      (binop_colon
        (lc "name")
        (uc "UserName")
      )
      (binop_colon
        (lc "age")
        (uc "UserAge")
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
          (binop_colon
            (lc "name")
            (lc "name")
          )
          (binop_colon
            (lc "age")
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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
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
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

UserId : U64
UserName : Str
UserAge : U8
User : {id : UserId, name : UserName, age : UserAge}
create_user : UserId -> UserName -> UserAge -> User
create_user = |id, name, age| { id : id, name : name, age : age }
get_user_name : User -> UserName
get_user_name = |user| user.name
main! = |_| {
	user = create_user((123, "Alice", 25))
	get_user_name(user)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 9:16 to 9:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "id")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "create_user")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.binop_thin_arrow
          (Expr.apply_tag)
          (Expr.apply_tag)
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "create_user")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "get_user_name")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "get_user_name")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
create_user : _a
get_user_name : _a
~~~
