# META
~~~ini
description=Match expression with empty list pattern followed by list rest pattern (segfault regression test)
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] => Err(EmptyList)
    [.., e] => Ok(e)
}
~~~
# EXPECTED
UNDEFINED VARIABLE - empty_list_before_rest_pattern.md:1:7:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `l` in this scope.
Is there an `import` or `exposing` missing up-top?

**empty_list_before_rest_pattern.md:1:7:1:8:**
```roc
match l {
```
      ^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:8),OpenCurly(1:9-1:10),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),UpperIdent(2:11-2:14),NoSpaceOpenRound(2:14-2:15),UpperIdent(2:15-2:24),CloseRound(2:24-2:25),
OpenSquare(3:5-3:6),DoubleDot(3:6-3:8),Comma(3:8-3:9),LowerIdent(3:10-3:11),CloseSquare(3:11-3:12),OpFatArrow(3:13-3:15),UpperIdent(3:16-3:18),NoSpaceOpenRound(3:18-3:19),LowerIdent(3:19-3:20),CloseRound(3:20-3:21),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.8 (raw "l"))
	(branches
		(branch @2.5-2.25
			(p-list @2.5-2.7)
			(e-apply @2.11-2.25
				(e-tag @2.11-2.14 (raw "Err"))
				(e-tag @2.15-2.24 (raw "EmptyList"))))
		(branch @3.5-3.21
			(p-list @3.5-3.12
				(p-list-rest @3.6-3.8)
				(p-ident @3.10-3.11 (raw "e")))
			(e-apply @3.16-3.21
				(e-tag @3.16-3.18 (raw "Ok"))
				(e-ident @3.19-3.20 (raw "e"))))))
~~~
# FORMATTED
~~~roc
match l {
	[] => Err(EmptyList)
	[.., e] => Ok(e)
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.7
							(patterns))))
				(value
					(e-tag @2.11-2.14 (name "Err")
						(args
							(e-tag @2.15-2.24 (name "EmptyList"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.12
							(patterns
								(p-assign @3.10-3.11 (ident "e")))
							(rest-at (index 0)))))
				(value
					(e-tag @3.16-3.18 (name "Ok")
						(args
							(e-lookup-local @3.19-3.20
								(p-assign @3.10-3.11 (ident "e"))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "[Err([EmptyList]_others), Ok(_a)]_others2"))
~~~
