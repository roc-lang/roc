# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

main! = |_| {
    # Regular unused variable - should warn
    unused_var = 42

    # Regular used variable - should be fine
    used_var = 100

    # Another unused variable - should warn
    another_unused = "hello"

    # Underscore variable that is unused - should be fine
    _ignored # Comment 1
     = # Comment 2
      999 # Comment 3

    # Use only the used_var
    result = used_var + 10
    result
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign String BlankLine LineComment LowerIdent LineComment OpAssign LineComment Int LineComment BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
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


main! = |_| {
	# Regular unused variable - should warn
	unused_var = 42
	# Regular used variable - should be fine
	used_var = 100
	# Another unused variable - should warn
	another_unused = "hello"
	# Underscore variable that is unused - should be fine
	_ignored = # Comment 1
	# Comment 2
999

	# Comment 3

	# Use only the used_var
	result = used_var + 10
	result : result
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **another_unused** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_another_unused` to suppress this warning.
The unused variable is declared here:

**unused_vars_block.md:11:5:11:19:**
```roc
    another_unused = "hello"
```
    ^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **unused_var** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused_var` to suppress this warning.
The unused variable is declared here:

**unused_vars_block.md:5:5:5:15:**
```roc
    unused_var = 42
```
    ^^^^^^^^^^


**UNUSED VARIABLE**
Variable **_ignored** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__ignored` to suppress this warning.
The unused variable is declared here:

**unused_vars_block.md:14:5:14:13:**
```roc
    _ignored # Comment 1
```
    ^^^^^^^^


**UNUSED VARIABLE**
Variable **result** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result` to suppress this warning.
The unused variable is declared here:

**unused_vars_block.md:20:5:20:11:**
```roc
    result
```
    ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
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
