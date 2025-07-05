# META
~~~ini
description=multiline_str_opt_field
type=expr
~~~
# SOURCE
~~~roc
{l?""""""}""
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_str_opt_field.md:1:4:1:7:**
```roc
{l?""""""}""
```
   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_str_opt_field.md:1:7:1:10:**
```roc
{l?""""""}""
```
      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_str_opt_field.md:1:7:1:11:**
```roc
{l?""""""}""
```
      ^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),NoSpaceOpQuestion(1:3-1:4),MultilineStringStart(1:4-1:7),StringPart(1:7-1:7),MultilineStringEnd(1:7-1:10),CloseCurly(1:10-1:11),StringStart(1:11-1:12),StringPart(1:12-1:12),StringEnd(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.11
	(statements
		(e-question-suffix @1.2-1.4
			(e-ident @1.2-1.3 (raw "l")))
		(e-malformed @1.4-1.7 (reason "expr_unexpected_token"))
		(e-malformed @1.7-1.10 (reason "expr_unexpected_token"))
		(e-malformed @1.7-1.11 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	l?
	
	
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.11
	(s-expr @1.2-1.7
		(e-runtime-error (tag "not_implemented")))
	(e-empty_record @1.1-1.11))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "{}"))
~~~
