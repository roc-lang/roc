# META
~~~ini
description=compare_apply_record
type=expr
~~~
# SOURCE
~~~roc
x>
x{

}<r
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpGreaterThan(1:2-1:3),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpenCurly(2:2-2:3),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseCurly(4:1-4:2),OpLessThan(4:2-4:3),LowerIdent(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.3 (op ">")
	(e-ident @1.1-1.2 (qaul "") (raw "x"))
	(e-ident @2.1-2.2 (qaul "") (raw "x")))
~~~
# FORMATTED
~~~roc
x >
	x
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.3 (op "gt")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "*"))
~~~
