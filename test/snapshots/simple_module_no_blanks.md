# META
~~~ini
description=A simple module with no blanks
type=snippet
~~~
# SOURCE
~~~roc
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
MODULE NOT FOUND - simple_module_no_blanks.md:1:1:1:17
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**simple_module_no_blanks.md:1:1:1:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:10),NoSpaceDotUpperIdent(1:10-1:17),
LowerIdent(2:1-2:7),OpAssign(2:8-2:9),UpperIdent(2:10-2:16),NoSpaceDotLowerIdent(2:16-2:22),NoSpaceOpenRound(2:22-2:23),StringStart(2:23-2:24),StringPart(2:24-2:29),StringEnd(2:29-2:30),CloseRound(2:30-2:31),
LowerIdent(3:1-3:6),OpAssign(3:7-3:8),StringStart(3:9-3:10),StringPart(3:10-3:15),StringEnd(3:15-3:16),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.16
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "pf.Stdout"))
		(s-decl @2.1-2.31
			(p-ident @2.1-2.7 (raw "hello!"))
			(e-apply @2.10-2.31
				(e-ident @2.10-2.22 (raw "Stdout.line!"))
				(e-string @2.23-2.30
					(e-string-part @2.24-2.29 (raw "Hello")))))
		(s-decl @3.1-3.16
			(p-ident @3.1-3.6 (raw "world"))
			(e-string @3.9-3.16
				(e-string-part @3.10-3.15 (raw "World"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.7 (ident "hello!"))
		(e-call @2.10-2.31
			(e-lookup-external @2.10-2.22
				(module-idx "4")
				(target-node-idx "0"))
			(e-string @2.23-2.30
				(e-literal @2.24-2.29 (string "Hello")))))
	(d-let
		(p-assign @3.1-3.6 (ident "world"))
		(e-string @3.9-3.16
			(e-literal @3.10-3.15 (string "World"))))
	(s-import @1.1-1.17 (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.7 (type "_a"))
		(patt @3.1-3.6 (type "Str")))
	(expressions
		(expr @2.10-2.31 (type "_a"))
		(expr @3.9-3.16 (type "Str"))))
~~~
