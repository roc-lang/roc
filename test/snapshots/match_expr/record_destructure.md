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
(match <15 branches>)
~~~
# FORMATTED
~~~roc
when ... is {
	{ name, age }
	=>
	"${name} is ${age.to_str()} years old"
	{ name, address : {
		city
	} }
	=>
	"${city} is the city of ${name}"
	{  }
	=>
	"empty"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:11

**Parse Error**
at 2:19 to 2:19

**Parse Error**
at 3:33 to 3:33

**Parse Error**
at 4:8 to 4:8

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
