# META
~~~ini
description=Method call syntax with .inspect() on string should work now that Str.inspect exists
type=expr
~~~
# SOURCE
~~~roc
{ x = "hello"; x.inspect() }
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - method_call_inspect_defined.md:1:14:1:15
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  { x = "hello"; x.inspect() }                                              │
 │               ‾                                                            │
 └─────────────────────────────────────── method_call_inspect_defined.md:1:14 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `;` here.
    This token is malformed, so it cannot be used as ordinary Roc syntax.

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
		(e-method-call (method ".inspect")
			(receiver
				(e-ident (raw "x")))
			(args))))
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
	(e-dispatch-call (method "inspect") (constraint-fn-var 37)
		(receiver
			(e-lookup-local
				(p-assign (ident "x"))))
		(args)))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
