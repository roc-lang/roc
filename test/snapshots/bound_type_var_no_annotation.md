# META
~~~ini
description=A bound type variable (for identity function) with no type annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
    # Test identity with different types
    num = identity(42)
    text = identity("hello")

    # Test combine function
    pair = combine(num, text)

    # Test concrete function
    result = addOne(5)

    result
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LowerIdent CloseCurly ~~~
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

identity = |x| x

# Test function with multiple type parameters
combine : a -> b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
	# Test identity with different types
	num = identity(42)
	text = identity("hello")

	# Test combine function
	pair = combine((num, text))

	# Test concrete function
	result = addOne(5)

	result : result
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **result** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result` to suppress this warning.
The unused variable is declared here:

**bound_type_var_no_annotation.md:24:5:24:11:**
```roc
    result
```
    ^^^^^^


**UNUSED VARIABLE**
Variable **pair** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:

**bound_type_var_no_annotation.md:19:5:19:9:**
```roc
    pair = combine(num, text)
```
    ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "combine")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "combine"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "addOne")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "addOne"))
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
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
