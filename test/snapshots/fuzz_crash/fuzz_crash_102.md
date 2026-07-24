# META
~~~ini
description=Canonicalize panic in canonical_type_keys invariant
type=file
~~~
# SOURCE
~~~roc
main!=|G|"""
.S
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,UpperIdent,OpBar,MultilineStringStart,StringPart,
DotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-tag (raw "G")))
				(e-typed-multiline-string (type "S")
					(e-string-part (raw "")))))))
~~~
# FORMATTED
~~~roc
main! = |G| \\
	.S
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "[G] -> Error")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "[G] -> Error"))))
~~~
