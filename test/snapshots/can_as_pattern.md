# META
~~~ini
description=Test as pattern in match
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    Some(v) as whole => whole
    None => None
}
~~~
# EXPECTED
UNUSED VARIABLE - can_as_pattern.md:2:10:2:11
# PROBLEMS
**UNUSED VARIABLE**
Variable `v` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_v` to suppress this warning.
The unused variable is declared here:
**can_as_pattern.md:2:10:2:11:**
```roc
    Some(v) as whole => whole
```
         ^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,KwAs,LowerIdent,OpFatArrow,LowerIdent,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-match
		(e-ident (raw "x"))
		(branches
			(branch
				(p-as (name "whole")
					(p-tag (raw "Some")
						(p-ident (raw "v"))))
				(e-ident (raw "whole")))
			(branch
				(p-tag (raw "None"))
				(e-tag (raw "None"))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	Some(v) as whole => whole
	None => None
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "x"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-as (as "whole")
								(p-applied-tag))))
					(value
						(e-lookup-local
							(p-as (as "whole")
								(p-applied-tag)))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-tag (name "None"))))))))
~~~
# TYPES
~~~clojure
(expr (type "[None, ..[Some(a), ..b]] -> [None, ..[Some(a), ..b]]"))
~~~
