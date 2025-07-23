# META
~~~ini
description=Tuple pattern matching tests
type=expr
~~~
# SOURCE
~~~roc
{

    # Simple tuple destructuring
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")

    {}
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - tuple_patterns.md:7:22:7:23
UNDEFINED VARIABLE - tuple_patterns.md:7:7:7:8
UNDEFINED VARIABLE - tuple_patterns.md:7:10:7:11
UNDEFINED VARIABLE - tuple_patterns.md:7:15:7:16
UNDEFINED VARIABLE - tuple_patterns.md:7:18:7:19
UNUSED VARIABLE - tuple_patterns.md:13:20:13:27
UNUSED VARIABLE - tuple_patterns.md:16:6:16:10
UNUSED VARIABLE - tuple_patterns.md:4:6:4:7
UNUSED VARIABLE - tuple_patterns.md:10:6:10:11
UNUSED VARIABLE - tuple_patterns.md:4:9:4:10
UNUSED VARIABLE - tuple_patterns.md:13:6:13:10
UNUSED VARIABLE - tuple_patterns.md:13:12:13:18
UNUSED VARIABLE - tuple_patterns.md:10:13:10:19
UNUSED VARIABLE - tuple_patterns.md:16:12:16:17
UNUSED VARIABLE - tuple_patterns.md:10:21:10:26
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:7:22:7:23:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
                     ^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'a' is not defined:
**tuple_patterns.md:7:7:7:8:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
      ^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'b' is not defined:
**tuple_patterns.md:7:10:7:11:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
         ^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'c' is not defined:
**tuple_patterns.md:7:15:7:16:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
              ^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'd' is not defined:
**tuple_patterns.md:7:18:7:19:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
                 ^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'boolean' is defined but never used:
**tuple_patterns.md:13:20:13:27:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)
```
                   ^^^^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'list' is defined but never used:
**tuple_patterns.md:16:6:16:10:**
```roc
    (list, hello) = ([1, 2, 3], "hello")
```
     ^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'x' is defined but never used:
**tuple_patterns.md:4:6:4:7:**
```roc
    (x, y) = (1, 2)
```
     ^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'first' is defined but never used:
**tuple_patterns.md:10:6:10:11:**
```roc
    (first, second, third) = (100, 42, 200)
```
     ^^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'y' is defined but never used:
**tuple_patterns.md:4:9:4:10:**
```roc
    (x, y) = (1, 2)
```
        ^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'name' is defined but never used:
**tuple_patterns.md:13:6:13:10:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)
```
     ^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'string' is defined but never used:
**tuple_patterns.md:13:12:13:18:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)
```
           ^^^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'second' is defined but never used:
**tuple_patterns.md:10:13:10:19:**
```roc
    (first, second, third) = (100, 42, 200)
```
            ^^^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'hello' is defined but never used:
**tuple_patterns.md:16:12:16:17:**
```roc
    (list, hello) = ([1, 2, 3], "hello")
```
           ^^^^^


**UNUSED VARIABLE**

**Unused Variable**
The variable 'third' is defined but never used:
**tuple_patterns.md:10:21:10:26:**
```roc
    (first, second, third) = (100, 42, 200)
```
                    ^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
OpenRound(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),LowerIdent(4:9-4:10),CloseRound(4:10-4:11),OpAssign(4:12-4:13),OpenRound(4:14-4:15),Int(4:15-4:16),Comma(4:16-4:17),Int(4:18-4:19),CloseRound(4:19-4:20),
OpenRound(7:5-7:6),NoSpaceOpenRound(7:6-7:7),LowerIdent(7:7-7:8),Comma(7:8-7:9),LowerIdent(7:10-7:11),CloseRound(7:11-7:12),Comma(7:12-7:13),OpenRound(7:14-7:15),LowerIdent(7:15-7:16),Comma(7:16-7:17),LowerIdent(7:18-7:19),CloseRound(7:19-7:20),CloseRound(7:20-7:21),OpAssign(7:22-7:23),OpenRound(7:24-7:25),NoSpaceOpenRound(7:25-7:26),Int(7:26-7:28),Comma(7:28-7:29),Int(7:30-7:32),CloseRound(7:32-7:33),Comma(7:33-7:34),OpenRound(7:35-7:36),Int(7:36-7:38),Comma(7:38-7:39),Int(7:40-7:42),CloseRound(7:42-7:43),CloseRound(7:43-7:44),
OpenRound(10:5-10:6),LowerIdent(10:6-10:11),Comma(10:11-10:12),LowerIdent(10:13-10:19),Comma(10:19-10:20),LowerIdent(10:21-10:26),CloseRound(10:26-10:27),OpAssign(10:28-10:29),OpenRound(10:30-10:31),Int(10:31-10:34),Comma(10:34-10:35),Int(10:36-10:38),Comma(10:38-10:39),Int(10:40-10:43),CloseRound(10:43-10:44),
OpenRound(13:5-13:6),LowerIdent(13:6-13:10),Comma(13:10-13:11),LowerIdent(13:12-13:18),Comma(13:18-13:19),LowerIdent(13:20-13:27),CloseRound(13:27-13:28),OpAssign(13:29-13:30),OpenRound(13:31-13:32),StringStart(13:32-13:33),StringPart(13:33-13:38),StringEnd(13:38-13:39),Comma(13:39-13:40),StringStart(13:41-13:42),StringPart(13:42-13:47),StringEnd(13:47-13:48),Comma(13:48-13:49),UpperIdent(13:50-13:54),CloseRound(13:54-13:55),
OpenRound(16:5-16:6),LowerIdent(16:6-16:10),Comma(16:10-16:11),LowerIdent(16:12-16:17),CloseRound(16:17-16:18),OpAssign(16:19-16:20),OpenRound(16:21-16:22),OpenSquare(16:22-16:23),Int(16:23-16:24),Comma(16:24-16:25),Int(16:26-16:27),Comma(16:27-16:28),Int(16:29-16:30),CloseSquare(16:30-16:31),Comma(16:31-16:32),StringStart(16:33-16:34),StringPart(16:34-16:39),StringEnd(16:39-16:40),CloseRound(16:40-16:41),
OpenCurly(18:5-18:6),CloseCurly(18:6-18:7),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-19.2
	(statements
		(s-decl @4.5-4.20
			(p-tuple @4.5-4.11
				(p-ident @4.6-4.7 (raw "x"))
				(p-ident @4.9-4.10 (raw "y")))
			(e-tuple @4.14-4.20
				(e-int @4.15-4.16 (raw "1"))
				(e-int @4.18-4.19 (raw "2"))))
		(e-tuple @7.5-7.21
			(e-tuple @7.6-7.12
				(e-ident @7.7-7.8 (raw "a"))
				(e-ident @7.10-7.11 (raw "b")))
			(e-tuple @7.14-7.20
				(e-ident @7.15-7.16 (raw "c"))
				(e-ident @7.18-7.19 (raw "d"))))
		(e-malformed @7.22-7.23 (reason "expr_unexpected_token"))
		(e-tuple @7.24-7.44
			(e-tuple @7.25-7.33
				(e-int @7.26-7.28 (raw "10"))
				(e-int @7.30-7.32 (raw "20")))
			(e-tuple @7.35-7.43
				(e-int @7.36-7.38 (raw "30"))
				(e-int @7.40-7.42 (raw "40"))))
		(s-decl @10.5-10.44
			(p-tuple @10.5-10.27
				(p-ident @10.6-10.11 (raw "first"))
				(p-ident @10.13-10.19 (raw "second"))
				(p-ident @10.21-10.26 (raw "third")))
			(e-tuple @10.30-10.44
				(e-int @10.31-10.34 (raw "100"))
				(e-int @10.36-10.38 (raw "42"))
				(e-int @10.40-10.43 (raw "200"))))
		(s-decl @13.5-13.55
			(p-tuple @13.5-13.28
				(p-ident @13.6-13.10 (raw "name"))
				(p-ident @13.12-13.18 (raw "string"))
				(p-ident @13.20-13.27 (raw "boolean")))
			(e-tuple @13.31-13.55
				(e-string @13.32-13.39
					(e-string-part @13.33-13.38 (raw "Alice")))
				(e-string @13.41-13.48
					(e-string-part @13.42-13.47 (raw "fixed")))
				(e-tag @13.50-13.54 (raw "True"))))
		(s-decl @16.5-16.41
			(p-tuple @16.5-16.18
				(p-ident @16.6-16.10 (raw "list"))
				(p-ident @16.12-16.17 (raw "hello")))
			(e-tuple @16.21-16.41
				(e-list @16.22-16.31
					(e-int @16.23-16.24 (raw "1"))
					(e-int @16.26-16.27 (raw "2"))
					(e-int @16.29-16.30 (raw "3")))
				(e-string @16.33-16.40
					(e-string-part @16.34-16.39 (raw "hello")))))
		(e-record @18.5-18.7)))
~~~
# FORMATTED
~~~roc
{

	# Simple tuple destructuring
	(x, y) = (1, 2)

	# Nested tuple patterns
	((a, b), (c, d))
	
	((10, 20), (30, 40))

	# Mixed patterns with literals
	(first, second, third) = (100, 42, 200)

	# Tuple with string and tag patterns
	(name, string, boolean) = ("Alice", "fixed", True)

	# Tuple with list pattern
	(list, hello) = ([1, 2, 3], "hello")

	{}
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-19.2
	(s-let @4.5-4.20
		(p-tuple @4.5-4.11
			(patterns
				(p-assign @4.6-4.7 (ident "x"))
				(p-assign @4.9-4.10 (ident "y"))))
		(e-tuple @4.14-4.20
			(elems
				(e-int @4.15-4.16 (value "1"))
				(e-int @4.18-4.19 (value "2")))))
	(s-expr @7.5-7.21
		(e-tuple @7.5-7.21
			(elems
				(e-tuple @7.6-7.12
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope"))))
				(e-tuple @7.14-7.20
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope")))))))
	(s-expr @7.24-7.44
		(e-tuple @7.24-7.44
			(elems
				(e-tuple @7.25-7.33
					(elems
						(e-int @7.26-7.28 (value "10"))
						(e-int @7.30-7.32 (value "20"))))
				(e-tuple @7.35-7.43
					(elems
						(e-int @7.36-7.38 (value "30"))
						(e-int @7.40-7.42 (value "40")))))))
	(s-let @10.5-10.44
		(p-tuple @10.5-10.27
			(patterns
				(p-assign @10.6-10.11 (ident "first"))
				(p-assign @10.13-10.19 (ident "second"))
				(p-assign @10.21-10.26 (ident "third"))))
		(e-tuple @10.30-10.44
			(elems
				(e-int @10.31-10.34 (value "100"))
				(e-int @10.36-10.38 (value "42"))
				(e-int @10.40-10.43 (value "200")))))
	(s-let @13.5-13.55
		(p-tuple @13.5-13.28
			(patterns
				(p-assign @13.6-13.10 (ident "name"))
				(p-assign @13.12-13.18 (ident "string"))
				(p-assign @13.20-13.27 (ident "boolean"))))
		(e-tuple @13.31-13.55
			(elems
				(e-string @13.32-13.39
					(e-literal @13.33-13.38 (string "Alice")))
				(e-string @13.41-13.48
					(e-literal @13.42-13.47 (string "fixed")))
				(e-nominal @13.50-13.54 (nominal "Bool")
					(e-tag @13.50-13.54 (name "True"))))))
	(s-let @16.5-16.41
		(p-tuple @16.5-16.18
			(patterns
				(p-assign @16.6-16.10 (ident "list"))
				(p-assign @16.12-16.17 (ident "hello"))))
		(e-tuple @16.21-16.41
			(elems
				(e-list @16.22-16.31
					(elems
						(e-int @16.23-16.24 (value "1"))
						(e-int @16.26-16.27 (value "2"))
						(e-int @16.29-16.30 (value "3"))))
				(e-string @16.33-16.40
					(e-literal @16.34-16.39 (string "hello"))))))
	(e-empty_record @18.5-18.7))
~~~
# TYPES
~~~clojure
(expr @1.1-19.2 (type "{}"))
~~~
