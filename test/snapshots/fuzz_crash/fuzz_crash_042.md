# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
import u.R}g:r->R.a.E
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_042.md:1:11:1:12
MODULE NOT FOUND - fuzz_crash_042.md:1:1:1:11
MODULE NOT IMPORTED - fuzz_crash_042.md:1:17:1:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_042.md:1:11:1:12:**
```roc
import u.R}g:r->R.a.E
```
          ^


**MODULE NOT FOUND**
The module `u.R` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_042.md:1:1:1:11:**
```roc
import u.R}g:r->R.a.E
```
^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `R.a` imported into this Roc file.

You're attempting to use this module here:
**fuzz_crash_042.md:1:17:1:22:**
```roc
import u.R}g:r->R.a.E
```
                ^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:9),NoSpaceDotUpperIdent(1:9-1:11),CloseCurly(1:11-1:12),LowerIdent(1:12-1:13),OpColon(1:13-1:14),LowerIdent(1:14-1:15),OpArrow(1:15-1:17),UpperIdent(1:17-1:18),NoSpaceDotLowerIdent(1:18-1:20),NoSpaceDotUpperIdent(1:20-1:22),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.22
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.11 (raw "u.R"))
		(s-malformed @1.11-1.12 (tag "statement_unexpected_token"))
		(s-type-anno @1.12-1.22 (name "g")
			(ty-fn @1.14-1.22
				(ty-var @1.14-1.15 (raw "r"))
				(ty @1.17-1.22 (name "R.a.E"))))))
~~~
# FORMATTED
~~~roc
import u.R
g : r -> R.a.E
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.1-1.11 (module "u.R")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
