# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = if tru 0
~~~
# EXPECTED
PARSE ERROR - expr_if_missing_else.md:3:15:3:15
COMPILER DIAGNOSTIC - expr_if_missing_else.md:0:0:0:0
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**expr_if_missing_else.md:3:15:3:15:**
```roc
foo = if tru 0
```
              


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**expr_if_missing_else.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:13),Int(3:14-3:15),EndOfFile(3:15-3:15),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.15
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @3.15-3.15 (reason "no_else")))))
~~~
# FORMATTED
~~~roc
module []

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @3.1-3.4 (ident "foo")))
		(expr
			(e-runtime-error (tag "expr_not_canonicalized")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~
