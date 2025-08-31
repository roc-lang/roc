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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int Comma String Comma Int CloseRound LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }


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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**type_multiple_aliases.md:9:33:9:35:**
```roc
create_user = |id, name, age| { id, name, age }
```
                                ^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**type_multiple_aliases.md:9:37:9:41:**
```roc
create_user = |id, name, age| { id, name, age }
```
                                    ^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**type_multiple_aliases.md:9:43:9:46:**
```roc
create_user = |id, name, age| { id, name, age }
```
                                          ^^^


**UNUSED VARIABLE**
Variable **id** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_id` to suppress this warning.
The unused variable is declared here:

**type_multiple_aliases.md:9:16:9:18:**
```roc
create_user = |id, name, age| { id, name, age }
```
               ^^


**UNUSED VARIABLE**
Variable **name** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:

**type_multiple_aliases.md:9:20:9:24:**
```roc
create_user = |id, name, age| { id, name, age }
```
                   ^^^^


**UNUSED VARIABLE**
Variable **age** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.
The unused variable is declared here:

**type_multiple_aliases.md:9:26:9:29:**
```roc
create_user = |id, name, age| { id, name, age }
```
                         ^^^


**UNUSED VARIABLE**
Variable **user** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_user` to suppress this warning.
The unused variable is declared here:

**type_multiple_aliases.md:12:24:12:28:**
```roc
get_user_name = |user| user.name
```
                       ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
