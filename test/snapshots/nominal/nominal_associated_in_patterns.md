# META
~~~ini
description=Using qualified types in pattern matching and function parameters
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Result := [Success(U64), Failure(Str)]
}

handleSuccess : Foo.Result -> Str
handleSuccess = |res| "success"
~~~
# EXPECTED
UNUSED VARIABLE - nominal_associated_in_patterns.md:6:18:6:21
# PROBLEMS
**UNUSED VARIABLE**
Variable `res` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_res` to suppress this warning.
The unused variable is declared here:
**nominal_associated_in_patterns.md:6:18:6:21:**
```roc
handleSuccess = |res| "success"
```
                 ^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Result")
						(args))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Success"))
								(ty (name "U64")))
							(ty-apply
								(ty (name "Failure"))
								(ty (name "Str"))))))))
		(s-type-anno (name "handleSuccess")
			(ty-fn
				(ty (name "Foo.Result"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handleSuccess"))
			(e-lambda
				(args
					(p-ident (raw "res")))
				(e-string
					(e-string-part (raw "success")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Result := [Success(U64), Failure(Str)]
}

handleSuccess : Foo.Result -> Str
handleSuccess = |res| "success"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "handleSuccess"))
		(e-lambda
			(args
				(p-assign (ident "res")))
			(e-string
				(e-literal (string "success"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Foo.Result") (local))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Result"))
		(ty-tag-union
			(ty-tag-name (name "Success")
				(ty-lookup (name "U64") (builtin)))
			(ty-tag-name (name "Failure")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Result -> Str")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Result")
			(ty-header (name "Foo.Result"))))
	(expressions
		(expr (type "Foo.Result -> Str"))))
~~~
