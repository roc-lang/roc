# META
~~~ini
description=Polymorphic function with arity mismatch causing instantiation crash
type=expr
~~~
# SOURCE
~~~roc
match 42 {
    _ => {
        id = |x| x

        id(1, 2)
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_polymorphic_instantiation_crash.md:5:9:5:11:**
```roc
        id(1, 2)
```
        ^^

It is of type:
    _* -> *_

But you are trying to use it as:
    _Num(*), Num(*) -> *_

# TOKENS
~~~zig
KwMatch(1:1-1:6),Int(1:7-1:9),OpenCurly(1:10-1:11),Newline(1:1-1:1),
Underscore(2:5-2:6),OpFatArrow(2:7-2:9),OpenCurly(2:10-2:11),Newline(1:1-1:1),
LowerIdent(3:9-3:11),OpAssign(3:12-3:13),OpBar(3:14-3:15),LowerIdent(3:15-3:16),OpBar(3:16-3:17),LowerIdent(3:18-3:19),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:9-5:11),NoSpaceOpenRound(5:11-5:12),Int(5:12-5:13),Comma(5:13-5:14),Int(5:15-5:16),CloseRound(5:16-5:17),Newline(1:1-1:1),
CloseCurly(6:5-6:6),Newline(1:1-1:1),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-int @1.7-1.9 (raw "42"))
	(branches
		(branch @2.5-7.2
			(p-underscore)
			(e-block @2.10-6.6
				(statements
					(s-decl @3.9-3.19
						(p-ident @3.9-3.11 (raw "id"))
						(e-lambda @3.14-3.19
							(args
								(p-ident @3.15-3.16 (raw "x")))
							(e-ident @3.18-3.19 (raw "x"))))
					(e-apply @5.9-5.17
						(e-ident @5.9-5.11 (raw "id"))
						(e-int @5.12-5.13 (raw "1"))
						(e-int @5.15-5.16 (raw "2"))))))))
~~~
# FORMATTED
~~~roc
match 42 {
	_ => {
		id = |x| x

		id(1, 2)
	}
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-7.2
	(match @1.1-7.2
		(cond
			(e-int @1.7-1.9 (value "42")))
		(branches
			(branch
				(patterns
					(p-underscore @2.5-2.6 (degenerate false)))
				(value
					(e-block @2.10-6.6
						(s-let @3.9-3.19
							(p-assign @3.9-3.11 (ident "id"))
							(e-lambda @3.14-3.19
								(args
									(p-assign @3.15-3.16 (ident "x")))
								(e-lookup-local @3.18-3.19
									(pattern @3.15-3.16))))
						(e-call @5.9-5.17
							(e-lookup-local @5.9-5.11
								(pattern @3.9-3.11))
							(e-int @5.12-5.13 (value "1"))
							(e-int @5.15-5.16 (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "*"))
~~~
