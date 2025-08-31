# META
~~~ini
description=Test that rigid type variables are properly instantiated for polymorphic functions
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Identity function with rigid type variable
id : a -> a
id = |x| x

main! = |_| {
    # Should instantiate 'a' as Int
    num = id(42)
    
    # Should instantiate 'a' as Str (fresh instance)
    text = id("hello")
    
    # Should instantiate 'a' as List Int (fresh instance)
    list = id([1, 2, 3])
    
    {}
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
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


# Identity function with rigid type variable
id : a -> a
id = |x| x
main! = |_| {
	# Should instantiate 'a' as Int
	num = id(42)
	# Should instantiate 'a' as Str (fresh instance)
	text = id("hello")
	# Should instantiate 'a' as List Int (fresh instance)
	list = id([1, 2, 3])
	{}
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:15:5:15:9:**
```roc
    list = id([1, 2, 3])
```
    ^^^^


**UNUSED VARIABLE**
Variable **text** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_text` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:12:5:12:9:**
```roc
    text = id("hello")
```
    ^^^^


**UNUSED VARIABLE**
Variable **num** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:

**rigid_var_instantiation.md:9:5:9:8:**
```roc
    num = id(42)
```
    ^^^


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
