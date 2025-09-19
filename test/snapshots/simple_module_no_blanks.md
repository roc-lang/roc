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
MODULE NOT FOUND - simple_module_no_blanks.md:2:1:2:17
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**simple_module_no_blanks.md:2:1:2:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:22),CloseSquare(1:22-1:23),
KwImport(2:1-2:7),LowerIdent(2:8-2:10),NoSpaceDotUpperIdent(2:10-2:17),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),UpperIdent(3:10-3:16),NoSpaceDotLowerIdent(3:16-3:22),NoSpaceOpenRound(3:22-3:23),StringStart(3:23-3:24),StringPart(3:24-3:29),StringEnd(3:29-3:30),CloseRound(3:30-3:31),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),StringStart(4:9-4:10),StringPart(4:10-4:15),StringEnd(4:15-4:16),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(module @1.1-1.23
		(exposes @1.8-1.23
			(exposed-lower-ident @1.9-1.15
				(text "hello!"))
			(exposed-lower-ident @1.17-1.22
				(text "world"))))
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
			(e-string @3.23-3.30
				(e-literal @3.24-3.29 (string "Hello")))))
	(d-let
		(p-assign @4.1-4.6 (ident "world"))
		(e-string @4.9-4.16
			(e-literal @4.10-4.15 (string "World"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err"))))
	(s-import @2.1-2.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "_a"))
		(patt @4.1-4.6 (type "Str")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @3.10-3.31 (type "_a"))
		(expr @4.9-4.16 (type "Str"))))
~~~
