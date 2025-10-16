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
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED TOKEN IN EXPRESSION - error_malformed_syntax_2.md:1:10:1:11
MALFORMED TYPE - error_malformed_syntax_2.md:1:8:1:10
UNRECOGNIZED SYNTAX - error_malformed_syntax_2.md:1:10:1:11
UNUSED VARIABLE - error_malformed_syntax_2.md:1:12:1:16
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **42** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**error_malformed_syntax_2.md:1:8:1:10:**
```roc
{ age: 42, name = "Alice" }
```
       ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**error_malformed_syntax_2.md:1:10:1:11:**
```roc
{ age: 42, name = "Alice" }
```
         ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**error_malformed_syntax_2.md:1:8:1:10:**
```roc
{ age: 42, name = "Alice" }
```
       ^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**error_malformed_syntax_2.md:1:10:1:11:**
```roc
{ age: 42, name = "Alice" }
```
         ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNUSED VARIABLE**
Variable `name` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**error_malformed_syntax_2.md:1:12:1:16:**
```roc
{ age: 42, name = "Alice" }
```
           ^^^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "age")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-decl
			(p-ident (raw "name"))
			(e-string
				(e-string-part (raw "Alice"))))))
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
(e-block
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "name"))
		(e-string
			(e-literal (string "Alice"))))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
