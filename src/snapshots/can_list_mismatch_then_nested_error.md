# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The first two elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:2:**
```roc
[1, "hello", [3, "world"]]
```
 ^  ^^^^^^^

The first element has this type:
    _Num(*)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**INCOMPATIBLE LIST ELEMENTS**
The two elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:15:**
```roc
[1, "hello", [3, "world"]]
```
              ^  ^^^^^^^

The first element has this type:
    _Num(*)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),Comma(1:12-1:13),OpenSquare(1:14-1:15),Int(1:15-1:16),Comma(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:24),StringEnd(1:24-1:25),CloseSquare(1:25-1:26),CloseSquare(1:26-1:27),EndOfFile(1:27-1:27),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.27
	(e-int @1.2-1.3 (raw "1"))
	(e-string @1.5-1.12
		(e-string-part @1.6-1.11 (raw "hello")))
	(e-list @1.14-1.26
		(e-int @1.15-1.16 (raw "3"))
		(e-string @1.18-1.25
			(e-string-part @1.19-1.24 (raw "world")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.27 (elem-var 73) (id 80)
	(elems
		(e-int @1.2-1.3 (value "1"))
		(e-string @1.5-1.12
			(e-literal @1.6-1.11 (string "hello")))
		(e-list @1.14-1.26 (elem-var 76)
			(elems
				(e-int @1.15-1.16 (value "3"))
				(e-string @1.18-1.25
					(e-literal @1.19-1.24 (string "world")))))))
~~~
# TYPES
~~~clojure
(expr (id 80) (type "List(Error)"))
~~~
