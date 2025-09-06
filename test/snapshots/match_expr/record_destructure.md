# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, age } => "${name} is ${age.to_str()} years old"
    { name, address: { city } } => "${city} is the city of ${name}"
    {} => "empty"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
)
  (branch1     (binop_thick_arrow
      (record_literal
        (lc "name")
        (lc "age")
      )
      (block
        (str_literal_big "${name} is ${age.to_str()} years old")
        (binop_thick_arrow
          (record_literal
            (lc "name")
            (binop_colon
              (lc "address")
              (block
                (lc "city")
              )
            )
          )
          (str_literal_big "${city} is the city of ${name}")
        )
        (binop_thick_arrow
          (record_literal)
          (str_literal_big "empty")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{name, age} => 
		"${name} is ${age.to_str()} years old"
		{ name, address: {
			city
		} } => "${city} is the city of ${name}"
		{} => "empty"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_destructure.md:2:5:4:18:**
```roc
    { name, age } => "${name} is ${age.to_str()} years old"
    { name, address: { city } } => "${city} is the city of ${name}"
    {} => "empty"
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 20
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
~~~
# TYPES
~~~roc
~~~
