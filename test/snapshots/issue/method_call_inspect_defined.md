# META
~~~ini
description=Method call syntax with .inspect() on string should give MISSING METHOD
type=expr
~~~
# SOURCE
~~~roc
{ x = "hello"; x.inspect() }
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - method_call_inspect_defined.md:1:14:1:15
UNRECOGNIZED SYNTAX - method_call_inspect_defined.md:1:14:1:15
MISSING METHOD - method_call_inspect_defined.md:1:18:1:25
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **;** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**method_call_inspect_defined.md:1:14:1:15:**
```roc
{ x = "hello"; x.inspect() }
```
             ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**method_call_inspect_defined.md:1:14:1:15:**
```roc
{ x = "hello"; x.inspect() }
```
             ^

This might be a syntax error, an unsupported language feature, or a typo.

**MISSING METHOD**
This **inspect** method is being called on a value whose type doesn't have that method:
**method_call_inspect_defined.md:1:18:1:25:**
```roc
{ x = "hello"; x.inspect() }
```
                 ^^^^^^^

The value's type, which does not have a method named **inspect**, is:

    _Str_

**Hint:** For this to work, the type would need to have a method named **inspect** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,MalformedUnknownToken,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-string
				(e-string-part (raw "hello"))))
		(e-malformed (reason "expr_unexpected_token"))
		(e-field-access
			(e-ident (raw "x"))
			(e-apply
				(e-ident (raw "inspect"))))))
~~~
# FORMATTED
~~~roc
{
	x = "hello"
		x.inspect()
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-string
			(e-literal (string "hello"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(e-dot-access (field "inspect")
		(receiver
			(e-lookup-local
				(p-assign (ident "x"))))
		(args)))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
