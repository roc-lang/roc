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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int Comma String Comma Int CloseRound LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
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
    (binop_arrow_call
      (uc "UserId")
      (binop_arrow_call
        (uc "UserName")
        (binop_arrow_call
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
          (lc "id")
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
        (lc "id")
        (lc "name")
        (lc "age")
      )
    )
  )
  (binop_colon
    (lc "get_user_name")
    (binop_arrow_call
      (uc "User")
      (uc "UserName")
    )
  )
  (binop_equals
    (lc "get_user_name")
    (lambda
      (body
        (binop_dot
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
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

UserId : U64
UserName : Str
UserAge : U8
User : {id: UserId, name: UserName, age: UserAge}
create_user : UserId -> UserName -> UserAge -> User
create_user = |id, name, age| { id, name: name, age: age }
get_user_name : User -> UserName
get_user_name = |user| user..name
main! = |_| {
	user = create_user((123, "Alice", 25))
	get_user_name(user)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "create_user"))
    (type type_35)
  )
  (Stmt.assign
    (pattern (Patt.ident "create_user"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "get_user_name"))
    (type type_52)
  )
  (Stmt.assign
    (pattern (Patt.ident "get_user_name"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 91
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
(var #37 -> #83)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 -> #82)
(var #47 -> #83)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 -> #85)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 -> #85)
(var #60 _)
(var #61 -> #90)
(var #62 _)
(var #63 -> #69)
(var #64 -> #88)
(var #65 Num *)
(var #66 Str)
(var #67 Num *)
(var #68 -> #87)
(var #69 _)
(var #70 _)
(var #71 -> #89)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 -> #90)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 {})
(var #82 record)
(var #83 fn_pure)
(var #84 _)
(var #85 fn_pure)
(var #86 _)
(var #87 tuple)
(var #88 fn_pure)
(var #89 fn_pure)
(var #90 fn_pure)
~~~
# TYPES
~~~roc
user : _a
age : _a
create_user : _arg, _arg2, _arg3 -> { id: _field, name: _field2, age: _field3 }
main : _arg -> _ret
get_user_name : _arg -> _ret
id : _a
name : _a
~~~
