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
		{ name, address : {
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
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
