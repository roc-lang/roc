# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_patterns_err_multiple_rest.md:1:7:1:14
INVALID PATTERN - :0:0:0:0
UNUSED VARIABLE - list_patterns_err_multiple_rest.md:2:10:2:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_patterns_err_multiple_rest.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNUSED VARIABLE**
Variable `middle` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like _middle to suppress this warning.
The unused variable is declared here:
**list_patterns_err_multiple_rest.md:2:10:2:16:**
```roc
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
```
         ^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),
OpenSquare(2:5-2:6),DoubleDot(2:6-2:8),Comma(2:8-2:9),LowerIdent(2:10-2:16),Comma(2:16-2:17),DoubleDot(2:18-2:20),CloseSquare(2:20-2:21),OpFatArrow(2:22-2:24),TripleDot(2:25-2:28),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.14 (raw "numbers"))
	(branches
		(branch @2.5-2.28
			(p-list @2.5-2.21
				(p-list-rest @2.6-2.8)
				(p-ident @2.10-2.16 (raw "middle"))
				(p-list-rest @2.18-2.20))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
match numbers {
	[.., middle, ..] => ...
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.21
							(patterns
								(p-assign @2.10-2.16 (ident "middle")))
							(rest-at (index 0)))))
				(value
					(e-not-implemented @1.1-1.1))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "_a"))
~~~
