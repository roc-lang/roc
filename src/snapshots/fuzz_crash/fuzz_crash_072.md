# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({})(!{0})
~~~
# EXPECTED
INVALID STATEMENT - fuzz_crash_072.md:1:9:1:19
# PROBLEMS
**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_072.md:1:9:1:19:**
```roc
module[]({})(!{0})
```
        ^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),OpenCurly(1:10-1:11),CloseCurly(1:11-1:12),CloseRound(1:12-1:13),NoSpaceOpenRound(1:13-1:14),OpBang(1:14-1:15),OpenCurly(1:15-1:16),Int(1:16-1:17),CloseCurly(1:17-1:18),CloseRound(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(file @1.1-1.19
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-apply @1.9-1.19
			(e-tuple @1.9-1.13
				(e-record @1.10-1.12))
			(unary "!"
				(e-block @1.15-1.18
					(statements
						(e-int @1.16-1.17 (raw "0"))))))))
~~~
# FORMATTED
~~~roc
module []
({})(
	!{
		0
	},
)
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
