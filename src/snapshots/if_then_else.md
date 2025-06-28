# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if true A

    else {
    B
    }
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `true` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:14),UpperIdent(3:15-3:16),Newline(1:1-1:1),
Newline(1:1-1:1),
KwElse(5:5-5:9),OpenCurly(5:10-5:11),Newline(1:1-1:1),
UpperIdent(6:5-6:6),Newline(1:1-1:1),
CloseCurly(7:5-7:6),EndOfFile(7:6-7:6),
~~~
# PARSE
~~~clojure
(file @1-1-7-6
	(module @1-1-1-13
		(exposes @1-8-1-13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @3-1-7-6
			(p-ident @3-1-3-4 (raw "foo"))
			(e-if-then-else @3-7-7-6
				(e-ident @3-10-3-14 (qaul "") (raw "true"))
				(e-tag @3-15-3-16 (raw "A"))
				(e-block @5-10-7-6
					(statements
						(e-tag @6-5-6-6 (raw "B"))))))))
~~~
# FORMATTED
~~~roc
module [foo]

foo = if true A

	else {
		B
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 84)
		(p-assign @3-1-3-4 (ident "foo") (id 72))
		(e-if @3-7-7-6 (cond-var 0) (branch-var 0) (id 83)
			(if-branches
				(if-branch
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-tag @3-15-3-16 (ext-var 0) (name "A") (args "TODO"))))
			(if-else
				(e-block @5-10-7-6
					(e-tag @6-5-6-6 (ext-var 0) (name "B") (args "TODO")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "foo") (def_var 84) (type "*")))
	(expressions
		(expr @3-7-7-6 (type "*"))))
~~~
