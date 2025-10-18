# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_patterns.md:3:13:3:19
UNDEFINED VARIABLE - list_patterns.md:1:7:1:14
UNDEFINED VARIABLE - list_patterns.md:2:11:2:14
UNUSED VARIABLE - list_patterns.md:3:6:3:11
UNUSED VARIABLE - list_patterns.md:3:15:3:15
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_patterns.md:3:13:3:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_patterns.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `acc` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_patterns.md:2:11:2:14:**
```roc
    [] => acc
```
          ^^^


**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**list_patterns.md:3:6:3:11:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
     ^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_patterns.md:3:15:3:15:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
              ^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "numbers"))
	(branches
		(branch
			(p-list)
			(e-ident (raw "acc")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match numbers {
	[] => acc
	[first, .. as rest] => 0
}
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
						(p-list
							(patterns))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
