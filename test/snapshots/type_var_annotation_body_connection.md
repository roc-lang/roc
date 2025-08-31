# META
~~~ini
description=Type variable connection between function annotation and body
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent LineComment LowerIdent OpAssign LowerIdent LineComment LowerIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }


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
NIL
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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
