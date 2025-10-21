# META
~~~ini
description=Function with no type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Pure function with no annotation
multiply = |x, y| x * y

# Function with no type annotation - should infer effectfulness from body
print_number! = |n| Stdout.line!(n)

# Another effectful function with no annotation
process! = |x| print_number!(multiply(x, 2))

main! = process!(42)
~~~
# EXPECTED
MODULE NOT FOUND - function_no_annotation.md:3:1:3:17
UNDEFINED VARIABLE - function_no_annotation.md:9:21:9:33
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**function_no_annotation.md:3:1:3:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**function_no_annotation.md:9:21:9:33:**
```roc
print_number! = |n| Stdout.line!(n)
```
                    ^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpStar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout"))
		(s-decl
			(p-ident (raw "multiply"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-binop (op "*")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-decl
			(p-ident (raw "print_number!"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "Stdout.line!"))
					(e-ident (raw "n")))))
		(s-decl
			(p-ident (raw "process!"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "print_number!"))
					(e-apply
						(e-ident (raw "multiply"))
						(e-ident (raw "x"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-apply
				(e-ident (raw "process!"))
				(e-int (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "multiply"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "y"))))))
	(d-let
		(p-assign (ident "print_number!"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "n"))))))
	(d-let
		(p-assign (ident "process!"))
		(e-closure
			(captures
				(capture (ident "print_number!"))
				(capture (ident "multiply")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-call
					(e-lookup-local
						(p-assign (ident "print_number!")))
					(e-call
						(e-lookup-local
							(p-assign (ident "multiply")))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "2")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-call
			(e-lookup-local
				(p-assign (ident "process!")))
			(e-num (value "42"))))
	(s-import (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size), Num(_size2) -> Num(_size3)"))
		(patt (type "_arg -> _ret"))
		(patt (type "Num(_size) -> _ret"))
		(patt (type "_a")))
	(expressions
		(expr (type "Num(_size), Num(_size2) -> Num(_size3)"))
		(expr (type "_arg -> _ret"))
		(expr (type "Num(_size) -> _ret"))
		(expr (type "_a"))))
~~~
