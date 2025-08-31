# META
~~~ini
description=Function with record parameter destructuring and string interpolation
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# TOKENS
~~~text
OpBar OpenCurly LowerIdent Comma LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(lambda
  (body
    (str_literal_big "Hello ${name}, you are ${age.to_str()} years old")
  )
  (args
    (record_literal
      (binop_colon
        (lc "name")
        (lc "name")
      )
      (binop_colon
        (lc "age")
        (lc "age")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
|{ name : name, age : age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **age** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.
The unused variable is declared here:

**function_record_parameter.md:1:10:1:13:**
```roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
```
         ^^^


**UNUSED VARIABLE**
Variable **name** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:

**function_record_parameter.md:1:4:1:8:**
```roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
```
   ^^^^


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
