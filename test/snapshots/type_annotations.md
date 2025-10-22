# META
~~~ini
description=Various type annotations
type=snippet
~~~
# SOURCE
~~~roc
foo : U64
bar : Thing(_a, _b, _)
baz : (_a, _b, _c)
add_one : (U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((_a, _b, _c))
~~~
# EXPECTED
UNDECLARED TYPE - type_annotations.md:2:7:2:12
UNDECLARED TYPE - type_annotations.md:5:14:5:20
UNDECLARED TYPE - type_annotations.md:6:13:6:18
# PROBLEMS
**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:2:7:2:12:**
```roc
bar : Thing(_a, _b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:5:14:5:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:6:13:6:18:**
```roc
tag_tuple : Value((_a, _b, _c))
```
            ^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,Underscore,CloseRound,
LowerIdent,OpColon,OpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,CloseRound,CloseRound,
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
				(underscore-ty-var (raw "_a"))
				(underscore-ty-var (raw "_b"))
				(_)))
		(s-type-anno (name "baz")
			(ty-tuple
				(underscore-ty-var (raw "_a"))
				(underscore-ty-var (raw "_b"))
				(underscore-ty-var (raw "_c"))))
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
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b"))
					(underscore-ty-var (raw "_c")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "foo")
		(ty-lookup (name "U64") (builtin)))
	(s-type-anno (name "bar")
		(ty-malformed))
	(s-type-anno (name "baz")
		(ty-tuple
			(ty-rigid-var (name "_a"))
			(ty-rigid-var (name "_b"))
			(ty-rigid-var (name "_c"))))
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
