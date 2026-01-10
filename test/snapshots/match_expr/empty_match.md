# META
~~~ini
description=Match expression with no branches should produce error
type=expr
~~~
# SOURCE
~~~roc
|input|
    match input {}
~~~
# EXPECTED
PARSE ERROR - empty_match.md:2:5:2:10
INVALID LAMBDA - :0:0:0:0
UNUSED VARIABLE - empty_match.md:1:2:1:7
# PROBLEMS
**PARSE ERROR**
A match expression must have at least one branch.

**empty_match.md:2:5:2:10:**
```roc
    match input {}
```
    ^^^^^


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**UNUSED VARIABLE**
Variable `input` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_input` to suppress this warning.
The unused variable is declared here:
**empty_match.md:1:2:1:7:**
```roc
|input|
```
 ^^^^^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "input")))
	(e-malformed (reason "match_has_no_branches")))
~~~
# FORMATTED
~~~roc
|input|
	
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "lambda_body_not_canonicalized"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
