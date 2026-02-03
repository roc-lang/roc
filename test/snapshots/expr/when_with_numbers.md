# META
~~~ini
description=When is old syntax use match instead (should error)
type=snippet
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# EXPECTED
PARSE ERROR - when_with_numbers.md:1:1:1:5
PARSE ERROR - when_with_numbers.md:1:6:1:7
PARSE ERROR - when_with_numbers.md:1:8:1:10
PARSE ERROR - when_with_numbers.md:2:2:2:3
PARSE ERROR - when_with_numbers.md:2:4:2:6
PARSE ERROR - when_with_numbers.md:2:7:2:8
PARSE ERROR - when_with_numbers.md:3:2:3:3
PARSE ERROR - when_with_numbers.md:3:4:3:6
PARSE ERROR - when_with_numbers.md:3:7:3:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:1:1:1:5:**
```roc
when x is
```
^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:1:6:1:7:**
```roc
when x is
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:1:8:1:10:**
```roc
when x is
```
       ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:2:2:2:3:**
```roc
 1 -> 2
```
 ^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**when_with_numbers.md:2:4:2:6:**
```roc
 1 -> 2
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:2:7:2:8:**
```roc
 1 -> 2
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:3:2:3:3:**
```roc
 3 -> 4
```
 ^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**when_with_numbers.md:3:4:3:6:**
```roc
 3 -> 4
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_with_numbers.md:3:7:3:8:**
```roc
 3 -> 4
```
      ^


# TOKENS
~~~zig
LowerIdent,LowerIdent,LowerIdent,
Int,OpArrow,Int,
Int,OpArrow,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc


~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
