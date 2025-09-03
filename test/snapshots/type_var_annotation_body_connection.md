# META
~~~ini
description=Type variable connection between function annotation and body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| {
    thing : a  # refers to the type var introduced in function type annotation
    thing = x  # refers to the value from the function parameter
    thing
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent LineComment LowerIdent OpAssign LowerIdent LineComment LowerIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

identity : a -> a
identity = |x| {
	thing : a
	# refers to the type var introduced in function type annotation
	thing = x
	# refers to the value from the function parameter
	thing : thing
}

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - type_var_annotation_body_connection.md:6:13:6:14
# PROBLEMS
**UNUSED VARIABLE**
Variable **thing** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_thing` to suppress this warning.
The unused variable is declared here:

**type_var_annotation_body_connection.md:7:5:7:10:**
```roc
    thing
```
    ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "identity")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
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
~~~
# TYPES
~~~roc
~~~
