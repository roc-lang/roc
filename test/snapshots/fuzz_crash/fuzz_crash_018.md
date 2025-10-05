# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_018.md:1:1:1:2
PARSE ERROR - fuzz_crash_018.md:2:1:2:3
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_018.md:1:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_018.md:1:1:1:2:**
```roc
0 b:S
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_018.md:2:1:2:3:**
```roc
.R
```
^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_018`.roc, but no top-level type declaration named `fuzz_crash_018` was found.

Add either:
`fuzz_crash_018 := ...` (nominal type)
or:
`fuzz_crash_018 : ...` (type alias)
**fuzz_crash_018.md:1:1:2:3:**
```roc
0 b:S
.R
```


**UNDECLARED TYPE**
The type _S_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_018.md:1:5:1:6:**
```roc
0 b:S
```
    ^


# TOKENS
~~~zig
Int(1:1-1:2),LowerIdent(1:3-1:4),OpColon(1:4-1:5),UpperIdent(1:5-1:6),
DotUpperIdent(2:1-2:3),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.3
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))
		(s-type-anno @1.3-1.6 (name "b")
			(ty @1.5-1.6 (name "S")))
		(s-malformed @2.1-2.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
b : S
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
