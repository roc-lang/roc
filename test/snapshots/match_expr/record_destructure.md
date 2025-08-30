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
      (str_literal_big "${name} is ${age.to_str()} years old")
    )
)
  (branch2     (binop_thick_arrow
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
)
  (branch3     (binop_thick_arrow
      (record_literal)
      (str_literal_big "empty")
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{name, age} => "${name} is ${age.to_str()} years old"
	{name, address: {
		city
	}} => "${city} is the city of ${name}"
	{} => "empty"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:19 to 2:21

**Unsupported Node**
at 3:5 to 3:32

**Unsupported Node**
at 4:8 to 4:10

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
