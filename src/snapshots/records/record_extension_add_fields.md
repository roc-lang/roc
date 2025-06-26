# META
~~~ini
description=Record extension adding new fields
type=expr
~~~
# SOURCE
~~~roc
{ person & salary, department }
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& salary** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_add_fields.md:1:10:1:18:**
```roc
{ person & salary, department }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, department** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_add_fields.md:1:18:1:30:**
```roc
{ person & salary, department }
```


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `salary` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `department` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:9),OpAmpersand(1:10-1:11),LowerIdent(1:12-1:18),Comma(1:18-1:19),LowerIdent(1:20-1:30),CloseCurly(1:31-1:32),EndOfFile(1:32-1:32),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-32
	(statements
		(e-ident @1-3-1-9 (qaul "") (raw "person"))
		(e-malformed @1-10-1-18 (reason "expr_unexpected_token"))
		(e-ident @1-12-1-18 (qaul "") (raw "salary"))
		(e-malformed @1-18-1-30 (reason "expr_unexpected_token"))
		(e-ident @1-20-1-30 (qaul "") (raw "department"))))
~~~
# FORMATTED
~~~roc
{
	person
	
	salary
	
	department
}
~~~
# TYPES
~~~clojure
(expr (id 80) (type "*"))
~~~