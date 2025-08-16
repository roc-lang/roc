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
PARSE ERROR - fuzz_crash_042.md:1:19:1:20
MODULE NOT FOUND - fuzz_crash_042.md:1:9:1:19
MODULE NOT IMPORTED - fuzz_crash_042.md:1:25:1:30
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_042.md:1:19:1:20:**
```roc
module[]import u.R}g:r->R.a.E
```
                  ^


**MODULE NOT FOUND**
The module `u.R` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_042.md:1:9:1:19:**
```roc
module[]import u.R}g:r->R.a.E
```
        ^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `module[]import u.R}g:r->R.a` imported into this Roc file.

You're attempting to use this module here:
**fuzz_crash_042.md:1:25:1:30:**
```roc
module[]import u.R}g:r->R.a.E
```
                        ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),KwImport(1:9-1:15),LowerIdent(1:16-1:17),NoSpaceDotUpperIdent(1:17-1:19),CloseCurly(1:19-1:20),LowerIdent(1:20-1:21),OpColon(1:21-1:22),LowerIdent(1:22-1:23),OpArrow(1:23-1:25),UpperIdent(1:25-1:26),NoSpaceDotLowerIdent(1:26-1:28),NoSpaceDotUpperIdent(1:28-1:30),EndOfFile(1:30-1:30),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-import @1.9-1.19 (raw "u.R"))
		(s-malformed @1.19-1.20 (tag "statement_unexpected_token"))
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
