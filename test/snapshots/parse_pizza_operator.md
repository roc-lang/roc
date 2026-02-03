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
PIZZA OPERATOR NOT SUPPORTED - parse_pizza_operator.md:1:1:1:20
# PROBLEMS
**PIZZA OPERATOR NOT SUPPORTED**
Roc doesn't use the pizza operator (**|>**).

Instead, use the arrow syntax **->** to chain function calls:

`list |> List.map(f) |> List.join`

becomes:

**list->List.map(f)->List.join**

**parse_pizza_operator.md:1:1:1:20:**
```roc
1 |> add 2 |> mul 3
```
^^^^^^^^^^^^^^^^^^^


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
		(s-malformed (tag "pizza_operator_not_supported"))))
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
