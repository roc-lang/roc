# META
~~~ini
description=A simple module with no blanks
type=file:SimpleModuleNoBlanks.roc
~~~
# SOURCE
~~~roc
SimpleModuleNoBlanks := {}
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
UpperIdent(1:1-1:21),OpColonEqual(1:22-1:24),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),
KwImport(2:1-2:7),LowerIdent(2:8-2:10),NoSpaceDotUpperIdent(2:10-2:17),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),UpperIdent(3:10-3:16),NoSpaceDotLowerIdent(3:16-3:22),NoSpaceOpenRound(3:22-3:23),StringStart(3:23-3:24),StringPart(3:24-3:29),StringEnd(3:29-3:30),CloseRound(3:30-3:31),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),StringStart(4:9-4:10),StringPart(4:10-4:15),StringEnd(4:15-4:16),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(type-module @1.1-1.21)
	(statements
		(s-type-decl @1.1-1.27
			(header @1.1-1.21 (name "SimpleModuleNoBlanks")
				(args))
			(ty-record @1.25-1.27))
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
	(s-nominal-decl @1.1-1.27
		(ty-header @1.1-1.21 (name "SimpleModuleNoBlanks"))
		(ty-record @1.25-1.27))
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
		(nominal @1.1-1.27 (type "SimpleModuleNoBlanks")
			(ty-header @1.1-1.21 (name "SimpleModuleNoBlanks"))))
	(expressions
		(expr @3.10-3.31 (type "_a"))
		(expr @4.9-4.16 (type "Str"))))
~~~
