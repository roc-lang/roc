# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import u.R}g:r->R.a.E
~~~
# EXPECTED
OVER CLOSED BRACE - :0:0:0:0
# PROBLEMS
**OVER CLOSED BRACE**
There are too many closing braces here.

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'module_not_found' is not yet handled in report generation.
**/Users/jaredramirez/dev/github/roc-lang/roc/src/snapshots/fuzz_crash/fuzz_crash_042.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),KwImport(1:9-1:15),LowerIdent(1:16-1:17),NoSpaceDotUpperIdent(1:17-1:19),LowerIdent(1:20-1:21),OpColon(1:21-1:22),LowerIdent(1:22-1:23),OpArrow(1:23-1:25),UpperIdent(1:25-1:26),NoSpaceDotLowerIdent(1:26-1:28),NoSpaceDotUpperIdent(1:28-1:30),EndOfFile(1:30-1:30),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-import @1.9-1.19 (raw "u.R"))
		(s-type-anno @1.20-1.30 (name "g")
			(ty-fn @1.22-1.30
				(ty-var @1.22-1.23 (raw "r"))
				(ty @1.25-1.30 (name "R.a.E"))))))
~~~
# FORMATTED
~~~roc
module []
import u.R
g : r -> R.a.E
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.9-1.19 (module "u.R") (qualifier "u")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
