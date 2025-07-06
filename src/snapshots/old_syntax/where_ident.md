# META
~~~ini
description=where_ident
type=expr
~~~
# SOURCE
~~~roc
where : {where: I32}
where = {where: 1}

where.where
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - where_ident.md:1:1:1:8
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **where :** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_ident.md:1:1:1:8:**
```roc
where : {where: I32}
```
^^^^^^^


# TOKENS
~~~zig
KwWhere(1:1-1:6),OpColon(1:7-1:8),OpenCurly(1:9-1:10),KwWhere(1:10-1:15),OpColon(1:15-1:16),UpperIdent(1:17-1:20),CloseCurly(1:20-1:21),Newline(1:1-1:1),
KwWhere(2:1-2:6),OpAssign(2:7-2:8),OpenCurly(2:9-2:10),KwWhere(2:10-2:15),OpColon(2:15-2:16),Int(2:17-2:18),CloseCurly(2:18-2:19),Newline(1:1-1:1),
Newline(1:1-1:1),
KwWhere(4:1-4:6),NoSpaceDotLowerIdent(4:6-4:12),EndOfFile(4:12-4:12),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.8 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

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
