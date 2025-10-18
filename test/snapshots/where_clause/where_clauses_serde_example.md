# META
~~~ini
description=Module dispatch in where clause
type=snippet
~~~
# SOURCE
~~~roc
deserialize : List(U8) -> Result(a, [DecodeErr])
	where [a.decode : List(U8) -> Result(a, [DecodeErr])]
deserialize = |_| ...
~~~
# EXPECTED
UNDECLARED TYPE - where_clauses_serde_example.md:1:27:1:33
UNDECLARED TYPE - where_clauses_serde_example.md:2:32:2:38
# PROBLEMS
**UNDECLARED TYPE**
The type _Result_ is not declared in this scope.

This type is referenced here:
**where_clauses_serde_example.md:1:27:1:33:**
```roc
deserialize : List(U8) -> Result(a, [DecodeErr])
```
                          ^^^^^^


**UNDECLARED TYPE**
The type _Result_ is not declared in this scope.

This type is referenced here:
**where_clauses_serde_example.md:2:32:2:38:**
```roc
	where [a.decode : List(U8) -> Result(a, [DecodeErr])]
```
	                              ^^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "deserialize")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "U8")))
				(ty-apply
					(ty (name "Result"))
					(ty-var (raw "a"))
					(ty-tag-union
						(tags
							(ty (name "DecodeErr"))))))
			(where
				(method (module-of "a") (name "decode")
					(args
						(ty-apply
							(ty (name "List"))
							(ty (name "U8"))))
					(ty-apply
						(ty (name "Result"))
						(ty-var (raw "a"))
						(ty-tag-union
							(tags
								(ty (name "DecodeErr"))))))))
		(s-decl
			(p-ident (raw "deserialize"))
			(e-lambda
				(args
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "deserialize"))
		(e-lambda
			(args
				(p-underscore))
			(e-not-implemented))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U8") (builtin)))
					(ty-malformed)))))
	(s-type-anno (name "deserialize")
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "U8") (builtin)))
			(ty-malformed))
		(where
			(method (module-of "a") (ident "decode")
				(args
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U8") (builtin))))
				(ty-malformed))))
	(ext-decl (ident "a.decode") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Num(Int(Unsigned8))) -> Error")))
	(expressions
		(expr (type "List(Num(Int(Unsigned8))) -> Error"))))
~~~
