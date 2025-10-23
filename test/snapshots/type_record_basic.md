# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({namee: "luke", age:21})
~~~
# EXPECTED
TYPE MISMATCH - type_record_basic.md:6:21:6:44
# PROBLEMS
**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**type_record_basic.md:6:21:6:44:**
```roc
main! = |_| getName({namee: "luke", age:21})
```
                    ^^^^^^^^^^^^^^^^^^^^^^^

This argument has the type:
    _{ age: Num(_size), namee: Str }_

But `getName` needs the first argument to be:
    _{ age: Num(Int(Unsigned64)), name: Str }_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "getName")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "age")
						(ty (name "U64"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getName"))
			(e-lambda
				(args
					(p-ident (raw "_person")))
				(e-string
					(e-string-part (raw "hello")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "getName"))
					(e-record
						(field (field "namee")
							(e-string
								(e-string-part (raw "luke"))))
						(field (field "age")
							(e-int (raw "21")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name : Str, age : U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({ namee: "luke", age: 21 })
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getName"))
		(e-lambda
			(args
				(p-assign (ident "_person")))
			(e-string
				(e-literal (string "hello"))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (external-module "Str")))
					(field (field "age")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "getName")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "getName")))
					(e-record
						(fields
							(field (name "namee")
								(e-string
									(e-literal (string "luke"))))
							(field (name "age")
								(e-num (value "21"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(expr (type "_arg -> Error"))))
~~~
