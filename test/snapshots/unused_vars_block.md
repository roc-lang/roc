# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign String BlankLine LineComment LowerIdent LineComment OpAssign LineComment Int LineComment BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
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
(block
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "unused_var")
            (num_literal_i32 42)
          )
          (binop_equals
            (lc "used_var")
            (num_literal_i32 100)
          )
          (binop_equals
            (lc "another_unused")
            (str_literal_big "hello")
          )
          (binop_equals
            (lc "_ignored")
            (num_literal_i32 999)
          )
          (binop_equals
            (lc "result")
            (binop_plus
              (lc "used_var")
              (num_literal_i32 10)
            )
          )
          (lc "result")
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

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
	result
}
~~~
# EXPECTED
UNUSED VARIABLE - unused_vars_block.md:5:5:5:15
UNUSED VARIABLE - unused_vars_block.md:11:5:11:19
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**unused_vars_block.md:3:1:3:6:**
```roc
main! = |_| {
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 33
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #32)
(var #8 _)
(var #9 -> #10)
(var #10 Num *)
(var #11 _)
(var #12 -> #13)
(var #13 Num *)
(var #14 _)
(var #15 -> #16)
(var #16 Str)
(var #17 _)
(var #18 -> #19)
(var #19 Num *)
(var #20 _)
(var #21 -> #24)
(var #22 -> #23)
(var #23 -> #24)
(var #24 Num *)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #32)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 fn_pure)
~~~
# TYPES
~~~roc
main : _arg -> _ret
~~~
