# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

printName : { name: Str, age: U64 } => Str
printName = |person| {
    Stdout.line!(person.name)
    person.name
}
main! = |_| {}
~~~
# EXPECTED
MODULE NOT FOUND - type_record_effectful.md:3:1:3:17
UNDEFINED VARIABLE - type_record_effectful.md:7:5:7:17
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**type_record_effectful.md:3:1:3:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**type_record_effectful.md:7:5:7:17:**
```roc
    Stdout.line!(person.name)
```
    ^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
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
		(s-import (raw "pf.Stdout"))
		(s-type-anno (name "printName")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "age")
						(ty (name "U64"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "printName"))
			(e-lambda
				(args
					(p-ident (raw "person")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-field-access
								(e-ident (raw "person"))
								(e-ident (raw "name"))))
						(e-field-access
							(e-ident (raw "person"))
							(e-ident (raw "name")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

printName : { name : Str, age : U64 } => Str
printName = |person| {
	Stdout.line!(person.name)
	person.name
}
main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "printName"))
		(e-lambda
			(args
				(p-assign (ident "person")))
			(e-block
				(s-expr
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-dot-access (field "name")
							(receiver
								(e-lookup-local
									(p-assign (ident "person")))))))
				(e-dot-access (field "name")
					(receiver
						(e-lookup-local
							(p-assign (ident "person")))))))
		(annotation
			(ty-fn (effectful true)
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (external-module "Str")))
					(field (field "age")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-import (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: Num(Int(Unsigned64)), name: Error } => Error"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "{ age: Num(Int(Unsigned64)), name: Error } => Error"))
		(expr (type "_arg -> {}"))))
~~~
