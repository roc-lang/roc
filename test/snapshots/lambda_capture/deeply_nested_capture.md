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
                  (lc "b")
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
		b
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


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 33
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #11)
(var #11 -> #12)
(var #12 _)
(var #13 -> #27)
(var #14 _)
(var #15 -> #28)
(var #16 -> #19)
(var #17 -> #29)
(var #18 Num *)
(var #19 -> #31)
(var #20 Num *)
(var #21 -> #32)
(var #22 Num *)
(var #23 _)
(var #24 -> #18)
(var #25 _)
(var #26 _)
(var #27 fn_pure)
(var #28 fn_pure)
(var #29 -> #30)
(var #30 fn_pure)
(var #31 fn_pure)
(var #32 fn_pure)
~~~
# TYPES
~~~roc
a : _d
b : _d
c : _d
~~~
