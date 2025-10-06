# META
~~~ini
description=fuzz crash
type=file:FuzzCrash052.roc
~~~
# SOURCE
~~~roc
FuzzCrash052 := {}

S
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:4:1:4:2
# PROBLEMS
**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_052.md:4:1:4:2:**
```roc
0
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
UpperIdent(3:1-3:2),
Int(4:1-4:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.2
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash052")
				(args))
			(ty-record @1.17-1.19))
		(s-malformed @4.1-4.2 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
FuzzCrash052 := {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash052"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash052")
			(ty-header @1.1-1.13 (name "FuzzCrash052"))))
	(expressions))
~~~
