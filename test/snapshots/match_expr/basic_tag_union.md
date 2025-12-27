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
MISSING METHOD - basic_tag_union.md:3:10:3:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `color` in this scope.
Is there an `import` or `exposing` missing up-top?

**basic_tag_union.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**basic_tag_union.md:3:10:3:11:**
```roc
	Blue => 2
```
	        ^

The value's type, which does not have a method named **from_numeral**, is:

    Str

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "color"))
	(branches
		(branch
			(p-tag (raw "Red"))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Blue"))
			(e-int (raw "2")))
		(branch
			(p-tag (raw "Green"))
			(e-string
				(e-string-part (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "2"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "3"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
