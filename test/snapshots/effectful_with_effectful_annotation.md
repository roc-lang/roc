# META
~~~ini
description=Effectful function with effectful annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)

main! = print_msg!("Hello, world!")
~~~
# EXPECTED
NAME NOT IN SCOPE - effectful_with_effectful_annotation.md:7:20:7:32
EFFECTFUL TOP LEVEL VALUE - effectful_with_effectful_annotation.md:9:9:9:36
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `line!` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  print_msg! = |msg| Stdout.line!(msg)                                      │
 │                     ‾‾‾‾‾‾‾‾‾‾‾‾                                           │
 └─────────────────────────────── effectful_with_effectful_annotation.md:7:20 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────────────┐
│ EFFECTFUL TOP LEVEL VALUE ├─ This top-level definition performs an effect ──┐
└┬──────────────────────────┘  while initializing.                            │
 │                                                                            │
 │  main! = print_msg!("Hello, world!")                                       │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                       │
 └──────────────────────────────── effectful_with_effectful_annotation.md:9:9 ┘

    Move the effect into a function body so it runs when the function is called.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
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
		(s-type-anno (name "print_msg!")
			(ty-fn
				(ty (name "Str"))
				(ty-record)))
		(s-decl
			(p-ident (raw "print_msg!"))
			(e-lambda
				(args
					(p-ident (raw "msg")))
				(e-apply
					(e-ident (raw "Stdout.line!"))
					(e-ident (raw "msg")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-apply
				(e-ident (raw "print_msg!"))
				(e-string
					(e-string-part (raw "Hello, world!")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "print_msg!"))
		(e-lambda
			(args
				(p-assign (ident "msg")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "msg")))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-call (constraint-fn-var 55)
			(e-lookup-local
				(p-assign (ident "print_msg!")))
			(e-string
				(e-literal (string "Hello, world!")))))
	(s-import (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Str => Error"))
		(expr (type "Error"))))
~~~
