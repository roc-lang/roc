# META
~~~ini
description=Match expression with tag patterns and variable catch-all pattern
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - wildcard_patterns.md:1:7:1:12
UNUSED VARIABLE - wildcard_patterns.md:4:5:4:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**wildcard_patterns.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNUSED VARIABLE**
Variable `other` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:
**wildcard_patterns.md:4:5:4:10:**
```roc
    other => "something else"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
UpperIdent(2:5-2:11),OpFatArrow(2:12-2:14),StringStart(2:15-2:16),StringPart(2:16-2:26),StringEnd(2:26-2:27),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),StringStart(3:13-3:14),StringPart(3:14-3:18),StringEnd(3:18-3:19),
LowerIdent(4:5-4:10),OpFatArrow(4:11-4:13),StringStart(4:14-4:15),StringPart(4:15-4:29),StringEnd(4:29-4:30),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "value"))
	(branches
		(branch @2.5-2.27
			(p-tag @2.5-2.11 (raw "Answer"))
			(e-string @2.15-2.27
				(e-string-part @2.16-2.26 (raw "the answer"))))
		(branch @3.5-3.19
			(p-tag @3.5-3.9 (raw "Zero"))
			(e-string @3.13-3.19
				(e-string-part @3.14-3.18 (raw "zero"))))
		(branch @4.5-4.30
			(p-ident @4.5-4.10 (raw "other"))
			(e-string @4.14-4.30
				(e-string-part @4.15-4.29 (raw "something else"))))))
~~~
# FORMATTED
~~~roc
match value {
	Answer => "the answer"
	Zero => "zero"
	other => "something else"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @2.5-2.11)))
				(value
					(e-string @2.15-2.27
						(e-literal @2.16-2.26 (string "the answer")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @3.5-3.9)))
				(value
					(e-string @3.13-3.19
						(e-literal @3.14-3.18 (string "zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @4.5-4.10 (ident "other"))))
				(value
					(e-string @4.14-4.30
						(e-literal @4.15-4.29 (string "something else"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
