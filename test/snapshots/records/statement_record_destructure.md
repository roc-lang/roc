# META
~~~ini
description=Record destructuring in assignment statement
type=snippet
~~~
# SOURCE
~~~roc
{ name, age, email } = person
~~~
# EXPECTED
INVALID STATEMENT - statement_record_destructure.md:1:1:1:21
# PROBLEMS
**INVALID STATEMENT**
The statement `destructuring declaration` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:1:1:21:**
```roc
{ name, age, email } = person
```
^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false))
				(field (name "email") (rest false)))
			(e-ident (raw "person")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
