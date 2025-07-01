# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    Other => 4
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**INCOMPATIBLE MATCH BRANCHES**
The second branch's type in this `match` is different from the previous ones:
**literal_patterns.md:1:1:**
```roc
match value {
    Answer => 1
    Zero => "hello"
```
            ^^^^^^^

The second branch has this type;
    _Str_

But the previous branch has this type:
    _Num(*)_

All branches in an `match` must have compatible types.

Note: You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:11),OpFatArrow(2:12-2:14),Int(2:15-2:16),Newline(1:1-1:1),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),StringStart(3:13-3:14),StringPart(3:14-3:19),StringEnd(3:19-3:20),Newline(1:1-1:1),
UpperIdent(4:5-4:13),OpFatArrow(4:14-4:16),Int(4:17-4:18),Newline(1:1-1:1),
UpperIdent(5:5-5:10),OpFatArrow(5:11-5:13),Int(5:14-5:15),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (qaul "") (raw "value"))
	(branches
		(branch @2.5-3.9
			(p-tag @2.5-2.11 (raw "Answer"))
			(e-int @2.15-2.16 (raw "1")))
		(branch @1.1-1.1
			(p-tag @3.5-3.9 (raw "Zero"))
			(e-string @3.13-3.20
				(e-string-part @3.14-3.19 (raw "hello"))))
		(branch @4.5-5.10
			(p-tag @4.5-4.13 (raw "Greeting"))
			(e-int @4.17-4.18 (raw "3")))
		(branch @5.5-6.2
			(p-tag @5.5-5.10 (raw "Other"))
			(e-int @5.14-5.15 (raw "4")))))
~~~
# FORMATTED
~~~roc
match value {
	Answer => 1
	Zero => "hello"
	Greeting => 3
	Other => 4
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-6.2
	(match @1.1-6.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-applied-tag @2.5-2.11 (degenerate false)))
				(value
					(e-int @2.15-2.16 (value "1"))))
			(branch
				(patterns
					(p-applied-tag @3.5-3.9 (degenerate false)))
				(value
					(e-string @3.13-3.20
						(e-literal @3.14-3.19 (string "hello")))))
			(branch
				(patterns
					(p-applied-tag @4.5-4.13 (degenerate false)))
				(value
					(e-int @4.17-4.18 (value "3"))))
			(branch
				(patterns
					(p-applied-tag @5.5-5.10 (degenerate false)))
				(value
					(e-int @5.14-5.15 (value "4")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
