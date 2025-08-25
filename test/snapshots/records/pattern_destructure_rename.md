# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <9 branches>)
~~~
# FORMATTED
~~~roc
when person is {
	{
		name: ((userName, age): userAge)
	}
	<malformed>
	"User ${userName} is ${userAge.to_str()} years old"
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:38 to 2:38

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:14 to 3:1

**Unsupported Node**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
