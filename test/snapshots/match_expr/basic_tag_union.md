# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_tag_union.md:1:7:1:12
INCOMPATIBLE MATCH BRANCHES - basic_tag_union.md:1:1:1:1
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `color` in this scope.
Is there an `import` or `exposing` missing up-top?

**basic_tag_union.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


**INCOMPATIBLE MATCH BRANCHES**
The third branch's type in this `match` is different from the previous ones:
**basic_tag_union.md:1:1:**
```roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
```
          ^^^

The third branch has this type;
    _Str_

But all the previous branches have this type:
    _Num(_size)_

All branches in an `match` must have compatible types.

Note: You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
UpperIdent(2:2-2:5),OpFatArrow(2:6-2:8),Int(2:9-2:10),
UpperIdent(3:2-3:6),OpFatArrow(3:7-3:9),Int(3:10-3:11),
UpperIdent(4:2-4:7),OpFatArrow(4:8-4:10),StringStart(4:11-4:12),StringPart(4:12-4:13),StringEnd(4:13-4:14),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "color"))
	(branches
		(branch @2.2-2.10
			(p-tag @2.2-2.5 (raw "Red"))
			(e-int @2.9-2.10 (raw "1")))
		(branch @3.2-3.11
			(p-tag @3.2-3.6 (raw "Blue"))
			(e-int @3.10-3.11 (raw "2")))
		(branch @4.2-4.14
			(p-tag @4.2-4.7 (raw "Green"))
			(e-string @4.11-4.14
				(e-string-part @4.12-4.13 (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
						(p-applied-tag @2.2-2.5)))
				(value
					(e-int @2.9-2.10 (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @3.2-3.6)))
				(value
					(e-int @3.10-3.11 (value "2"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @4.2-4.7)))
				(value
					(e-string @4.11-4.14
						(e-literal @4.12-4.13 (string "3"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Error"))
~~~
