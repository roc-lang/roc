# META
~~~ini
description=A simple module with no blanks
type=file
~~~
# SOURCE
~~~roc
module [hello!, world]
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:22),CloseSquare(1:22-1:23),Newline(1:1-1:1),
KwImport(2:1-2:7),LowerIdent(2:8-2:10),NoSpaceDotUpperIdent(2:10-2:17),Newline(1:1-1:1),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),UpperIdent(3:10-3:16),NoSpaceDotLowerIdent(3:16-3:22),NoSpaceOpenRound(3:22-3:23),StringStart(3:23-3:24),StringPart(3:24-3:29),StringEnd(3:29-3:30),CloseRound(3:30-3:31),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),StringStart(4:9-4:10),StringPart(4:10-4:15),StringEnd(4:15-4:16),EndOfFile(4:16-4:16),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(module @1.1-1.23
		(exposes @1.8-1.23
			(exposed-lower-ident (text "hello!"))
			(exposed-lower-ident (text "world"))))
	(statements
		(s-import @2.1-2.17 (raw "pf.Stdout"))
		(s-decl @3.1-3.31
			(p-ident @3.1-3.7 (raw "hello!"))
			(e-apply @3.10-3.31
				(e-ident @3.10-3.22 (raw "Stdout.line!"))
				(e-string @3.23-3.30
					(e-string-part @3.24-3.29 (raw "Hello")))))
		(s-decl @4.1-4.16
			(p-ident @4.1-4.6 (raw "world"))
			(e-string @4.9-4.16
				(e-string-part @4.10-4.15 (raw "World"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "hello!"))
		(e-call @3.10-3.31
			(e-lookup-external @3.10-3.22
				(module-idx "0")
				(target-node-idx "0"))
			(e-string @3.23-3.30
				(e-literal @3.24-3.29 (string "Hello")))))
	(d-let
		(p-assign @4.1-4.6 (ident "world"))
		(e-string @4.9-4.16
			(e-literal @4.10-4.15 (string "World"))))
	(s-import @2.1-2.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "*"))
		(patt @4.1-4.6 (type "Str")))
	(expressions
		(expr @3.10-3.31 (type "*"))
		(expr @4.9-4.16 (type "Str"))))
~~~
