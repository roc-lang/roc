# META
~~~ini
description=fuzz crash
type=file:FuzzCrash050.roc
~~~
# SOURCE
~~~roc
FuzzCrash050 := {}

)
 
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_050.md:3:1:3:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_050.md:3:1:3:2:**
```roc
)
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
CloseRound(3:1-3:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash050")
				(args))
			(ty-record @1.17-1.19))
		(s-malformed @3.1-3.2 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
FuzzCrash050 := {}

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash050"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash050")
			(ty-header @1.1-1.13 (name "FuzzCrash050"))))
	(expressions))
~~~
