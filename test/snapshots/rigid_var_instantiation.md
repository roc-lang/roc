# META
~~~ini
description=Polymorphic function with rigid type variable used at multiple call sites
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
    # First call with number
    num = identity(42)
    
    # Second call with string
    str = identity("hello")
    
    # Third call with list
    lst = identity([1, 2, 3])
    
    {}
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
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
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
	# First call with number
	num = identity(42)

	# Second call with string
	str = identity("hello")

	# Third call with list
	lst = identity([1, 2, 3])

	{}
}
~~~
# EXPECTED
UNUSED VARIABLE - rigid_var_instantiation.md:10:5:10:8
UNUSED VARIABLE - rigid_var_instantiation.md:13:5:13:8
UNUSED VARIABLE - rigid_var_instantiation.md:16:5:16:8
# PROBLEMS
**UNUSED VARIABLE**
Variable **num** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:10:5:10:8:**
```roc
    num = identity(42)
```
    ^^^


**UNUSED VARIABLE**
Variable **lst** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lst` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:16:5:16:8:**
```roc
    lst = identity([1, 2, 3])
```
    ^^^


**UNUSED VARIABLE**
Variable **str** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:13:5:13:8:**
```roc
    str = identity("hello")
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "identity")
    (type <mutated_tag:161>)
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
