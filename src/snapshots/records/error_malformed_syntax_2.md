# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
PARSE ERROR - error_malformed_syntax_2.md:1:17:1:20
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **42,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**error_malformed_syntax_2.md:1:8:1:11:**
```roc
{ age: 42, name = "Alice" }
```
       ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, name** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**error_malformed_syntax_2.md:1:10:1:16:**
```roc
{ age: 42, name = "Alice" }
```
         ^^^^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNUSED VARIABLE**
Variable ``name`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**error_malformed_syntax_2.md:1:12:1:16:**
```roc
{ age: 42, name = "Alice" }
```
           ^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:6),OpColon(1:6-1:7),Int(1:8-1:10),Comma(1:10-1:11),LowerIdent(1:12-1:16),OpAssign(1:17-1:18),StringStart(1:19-1:20),StringPart(1:20-1:25),StringEnd(1:25-1:26),CloseCurly(1:27-1:28),EndOfFile(1:28-1:28),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.28
	(statements
		(s-type-anno @1.3-1.11 (name "age")
			(ty-malformed @1.8-1.11 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.10-1.16 (reason "expr_unexpected_token"))
		(s-decl @1.12-1.26
			(p-ident @1.12-1.16 (raw "name"))
			(e-string @1.19-1.26
				(e-string-part @1.20-1.25 (raw "Alice"))))))
~~~
# FORMATTED
~~~roc
{
	age : 
	
	name = "Alice"
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.28
	(s-type-anno @1.3-1.11 (name "age")
		(ty-malformed @1.8-1.11))
	(s-let @1.12-1.26
		(p-assign @1.12-1.16 (ident "name"))
		(e-string @1.19-1.26
			(e-literal @1.20-1.25 (string "Alice"))))
	(e-string @1.19-1.26
		(e-literal @1.20-1.25 (string "Alice"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.28 (type "Str"))
~~~
