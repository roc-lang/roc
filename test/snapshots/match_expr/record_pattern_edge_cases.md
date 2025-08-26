# META
~~~ini
description=Edge cases for nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { a: { b: { c } } } => "deeply nested: ${c}"
    { x, y: {} } => "mixed with empty: ${x}"
    { outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
    { a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
    { name: x } => "renamed: ${x}"
    { person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
    {} => "empty record"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly Comma LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <49 branches>)
~~~
# FORMATTED
~~~roc
when ... is {
	{
		a : {
			b : {
				c
			}
		}
	}
	=>
	"deeply nested: ${c}"
	{ x, y : {} }
	=>
	"mixed with empty: ${x}"
	{ outer : {
		inner
	}, simple }
	=>
	"mixed: ${inner} and ${simple}"
	{ a : {
		b
	}, c : {
		d
	} }
	=>
	"multiple nested: ${b}, ${d}"
	{
		name : x
	}
	=>
	"renamed: ${x}"
	{
		person : {name : firstName, age : userAge}
	}
	=>
	"renamed nested: ${firstName} (${userAge.to_str()})"
	{  }
	=>
	"empty record"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:11

**Parse Error**
at 2:25 to 2:25

**Parse Error**
at 3:18 to 3:18

**Parse Error**
at 4:34 to 4:34

**Parse Error**
at 5:28 to 5:28

**Parse Error**
at 6:17 to 6:17

**Parse Error**
at 7:51 to 7:51

**Parse Error**
at 8:8 to 8:8

**Parse Error**
at 1:1 to 9:2

**Parse Error**
at 9:2 to 9:2

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_e")
~~~
# TYPES
~~~roc
_e
~~~
