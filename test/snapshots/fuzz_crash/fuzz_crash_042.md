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
KwImport,LowerIdent,NoSpaceDotUpperIdent,CloseCurly,LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "u.R"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "g")
			(ty-fn
				(ty-var (raw "r"))
				(ty (name "R.a.E"))))))
~~~
# FORMATTED
~~~roc
import u.R
g : r -> R.a.E
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "u.R")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
