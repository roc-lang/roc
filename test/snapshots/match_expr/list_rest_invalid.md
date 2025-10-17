# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:4:9:4:15
UNDEFINED VARIABLE - list_rest_invalid.md:1:7:1:12
UNUSED VARIABLE - list_rest_invalid.md:2:6:2:11
UNUSED VARIABLE - list_rest_invalid.md:2:15:2:15
UNUSED VARIABLE - list_rest_invalid.md:3:8:3:8
UNUSED VARIABLE - list_rest_invalid.md:3:14:3:18
UNUSED VARIABLE - list_rest_invalid.md:4:6:4:7
UNUSED VARIABLE - list_rest_invalid.md:4:11:4:11
UNUSED VARIABLE - list_rest_invalid.md:4:17:4:18
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_invalid.md:2:13:2:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_invalid.md:3:6:3:12:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
     ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_invalid.md:4:9:4:15:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_rest_invalid.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:2:6:2:11:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
     ^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:2:15:2:15:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
              ^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:3:8:3:8:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
       ^


**UNUSED VARIABLE**
Variable `last` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_last` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:3:14:3:18:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
             ^^^^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:6:4:7:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
     ^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:11:4:11:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
          ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:17:4:18:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
                ^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))
		(branch
			(p-list
				(p-list-rest (name "rest"))
				(p-ident (raw "last")))
			(e-int (raw "1")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list-rest (name "rest"))
				(p-ident (raw "y")))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => 0 # invalid rest pattern should error
	[.. as rest, last] => 1 # invalid rest pattern should error
	[x, .. as rest, y] => 2
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
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "2")))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
