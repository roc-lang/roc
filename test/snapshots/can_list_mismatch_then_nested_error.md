# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:5:1:5
MISSING METHOD - can_list_mismatch_then_nested_error.md:1:2:1:3
MISSING METHOD - can_list_mismatch_then_nested_error.md:1:15:1:16
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:5:**
```roc
[1, "hello", [3, "world"]]
```
    ^^^^^^^  ^^^^^^^^^^^^

The second element has this type:

    Str

However, the third element has this type:

    List(Str)

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**can_list_mismatch_then_nested_error.md:1:2:1:3:**
```roc
[1, "hello", [3, "world"]]
```
 ^

The value's type, which does not have a method named **from_numeral**, is:

    Str

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**can_list_mismatch_then_nested_error.md:1:15:1:16:**
```roc
[1, "hello", [3, "world"]]
```
              ^

The value's type, which does not have a method named **from_numeral**, is:

    Str

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
	(e-list
		(e-int (raw "3"))
		(e-string
			(e-string-part (raw "world")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))
		(e-list
			(elems
				(e-num (value "3"))
				(e-string
					(e-literal (string "world")))))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
