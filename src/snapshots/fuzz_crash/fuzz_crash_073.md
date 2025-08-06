# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]!0.t
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_073.md:1:12:1:14
INVALID STATEMENT - fuzz_crash_073.md:1:9:1:14
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**PARSE ERROR**
A parsing error occurred: `expr_dot_suffix_not_allowed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_073.md:1:12:1:14:**
```roc
module[]!0.t
```
           ^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_073.md:1:9:1:14:**
```roc
module[]!0.t
```
        ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpBang(1:9-1:10),Int(1:10-1:11),DotLowerIdent(1:12-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(file @1.1-1.14
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(unary "!"
			(e-malformed @1.12-1.14 (reason "expr_dot_suffix_not_allowed")))))
~~~
# FORMATTED
~~~roc
module []
!
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
