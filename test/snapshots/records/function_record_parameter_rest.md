# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# TOKENS
~~~text
OpBar OpenCurly LowerIdent Comma DoubleDot LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(lambda
  (body
    (str_literal_big "Hello ${first_name} ${rest.last_name}")
  )
  (args
    (record_literal
      (binop_colon
        (lc "first_name")
        (lc "first_name")
      )
      (double_dot_lc "rest")
    )
  )
)
~~~
# FORMATTED
~~~roc
|{ first_name : first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **first_name** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first_name` to suppress this warning.
The unused variable is declared here:

**function_record_parameter_rest.md:1:4:1:14:**
```roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
```
   ^^^^^^^^^^


**UNUSED VARIABLE**
Variable **..rest** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_..rest` to suppress this warning.
The unused variable is declared here:

**function_record_parameter_rest.md:1:16:1:22:**
```roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
```
               ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
