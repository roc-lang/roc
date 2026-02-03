# META
~~~ini
description=Pizza operator (|>) parsing
type=snippet
~~~
# SOURCE
~~~roc
1 |> add 2 |> mul 3
~~~
# EXPECTED
PARSE ERROR - parse_pizza_operator.md:1:1:1:2
PARSE ERROR - parse_pizza_operator.md:1:3:1:5
PARSE ERROR - parse_pizza_operator.md:1:6:1:9
PARSE ERROR - parse_pizza_operator.md:1:10:1:11
PARSE ERROR - parse_pizza_operator.md:1:12:1:14
PARSE ERROR - parse_pizza_operator.md:1:15:1:18
PARSE ERROR - parse_pizza_operator.md:1:19:1:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:1:1:2:**
```roc
1 |> add 2 |> mul 3
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:3:1:5:**
```roc
1 |> add 2 |> mul 3
```
  ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:6:1:9:**
```roc
1 |> add 2 |> mul 3
```
     ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:10:1:11:**
```roc
1 |> add 2 |> mul 3
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:12:1:14:**
```roc
1 |> add 2 |> mul 3
```
           ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:15:1:18:**
```roc
1 |> add 2 |> mul 3
```
              ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_pizza_operator.md:1:19:1:20:**
```roc
1 |> add 2 |> mul 3
```
                  ^


# TOKENS
~~~zig
Int,OpPizza,LowerIdent,Int,OpPizza,LowerIdent,Int,
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
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
