# META
~~~ini
description="The test case from the original request, involving multiple levels of nesting and local assignments. This will be the ultimate validation."
type=expr
~~~
# SOURCE
~~~roc
(((|a| {
    a_loc = a * 2
    |b| {
        b_loc = a_loc + b
        |c| b_loc + c
    }
})(100))(20))(3)
~~~
# TOKENS
~~~text
OpenRound OpenRound OpenRound OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent OpStar Int OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent OpPlus LowerIdent OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseCurly CloseCurly CloseRound OpenRound Int CloseRound CloseRound OpenRound Int CloseRound CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (apply_anon
      (lambda
        (body
          (block
            (num_literal_i32 2)
            (lambda
              (body
                (block
                  (binop_colon
                    (lc "b")
                    (lc "b")
                  )
                  (lambda
                    (body
                      (binop_plus
                        (lc "b_loc")
                        (lc "c")
                      )
                    )
                    (args
                      (lc "c")
                    )
                  )
                )
              )
              (args
                (lc "b")
              )
            )
          )
        )
        (args
          (lc "a")
        )
      )
      (num_literal_i32 100)
    )
    (num_literal_i32 20)
  )
  (num_literal_i32 3)
)
~~~
# FORMATTED
~~~roc
(|a| {
	2
	|b| {
		b : b
		|c| b_loc + c
	}
})(100)(20)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**deeply_nested_capture.md:7:2:7:3:**
```roc
})(100))(20))(3)
```
 ^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**deeply_nested_capture.md:7:8:7:9:**
```roc
})(100))(20))(3)
```
       ^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**deeply_nested_capture.md:7:13:7:14:**
```roc
})(100))(20))(3)
```
            ^


**UNDEFINED VARIABLE**
Nothing is named **b_loc** in this scope.
Is there an **import** or **exposing** missing up-top?

**deeply_nested_capture.md:5:13:5:18:**
```roc
        |c| b_loc + c
```
            ^^^^^


**UNUSED VARIABLE**
Variable **b** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:

**deeply_nested_capture.md:4:25:4:26:**
```roc
        b_loc = a_loc + b
```
                        ^


**UNUSED VARIABLE**
Variable **a** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:

**deeply_nested_capture.md:1:5:1:6:**
```roc
(((|a| {
```
    ^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
