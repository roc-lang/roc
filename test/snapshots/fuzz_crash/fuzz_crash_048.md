# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_048.md:2:7:2:12
UNDECLARED TYPE - fuzz_crash_048.md:6:14:6:20
UNDECLARED TYPE - fuzz_crash_048.md:7:13:7:18
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:2:7:2:12:**
```roc
bar : Thing(a, b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:6:14:6:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:7:13:7:18:**
```roc
tag_tuple : Value((a, b, c))
```
            ^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,Underscore,CloseRound,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty (name "U64")))
		(s-type-anno (name "bar")
			(ty-apply
				(ty (name "Thing"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(_)))
		(s-type-anno (name "biz")
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "c"))))
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U16"))
				(ty (name "U32"))))
		(s-type-anno (name "main!")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "String")))
				(ty-apply
					(ty (name "Result"))
					(ty-record)
					(_))))
		(s-type-anno (name "tag_tuple")
			(ty-apply
				(ty (name "Value"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b"))
					(ty-var (raw "c")))))))
~~~
# FORMATTED
~~~roc
foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
	U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "foo")
		(ty-lookup (name "U64") (builtin)))
	(s-type-anno (name "bar")
		(ty-malformed))
	(s-type-anno (name "biz")
		(ty-tuple
			(ty-rigid-var (name "a"))
			(ty-rigid-var (name "b"))
			(ty-rigid-var (name "c"))))
	(s-type-anno (name "add_one")
		(ty-parens
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U16") (builtin))
				(ty-lookup (name "U32") (builtin)))))
	(s-type-anno (name "main!")
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-malformed))
			(ty-apply (name "Result") (external-module "Result")
				(ty-record)
				(ty-underscore)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
