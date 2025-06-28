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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:4:12:4:15:**
```roc
    (x, y) = (1, 2)
```
           ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:7:22:7:25:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```
                     ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:10:28:10:31:**
```roc
    (first, second, third) = (100, 42, 200)
```
                           ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:13:29:13:32:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)
```
                            ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:16:19:16:22:**
```roc
    (list, hello) = ([1, 2, 3], "hello")
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `c` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `second` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `third` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `name` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `string` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `boolean` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `hello` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:6-3:33),
OpenRound(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),LowerIdent(4:9-4:10),CloseRound(4:10-4:11),OpAssign(4:12-4:13),OpenRound(4:14-4:15),Int(4:15-4:16),Comma(4:16-4:17),Int(4:18-4:19),CloseRound(4:19-4:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:6-6:28),
OpenRound(7:5-7:6),NoSpaceOpenRound(7:6-7:7),LowerIdent(7:7-7:8),Comma(7:8-7:9),LowerIdent(7:10-7:11),CloseRound(7:11-7:12),Comma(7:12-7:13),OpenRound(7:14-7:15),LowerIdent(7:15-7:16),Comma(7:16-7:17),LowerIdent(7:18-7:19),CloseRound(7:19-7:20),CloseRound(7:20-7:21),OpAssign(7:22-7:23),OpenRound(7:24-7:25),NoSpaceOpenRound(7:25-7:26),Int(7:26-7:28),Comma(7:28-7:29),Int(7:30-7:32),CloseRound(7:32-7:33),Comma(7:33-7:34),OpenRound(7:35-7:36),Int(7:36-7:38),Comma(7:38-7:39),Int(7:40-7:42),CloseRound(7:42-7:43),CloseRound(7:43-7:44),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:6-9:35),
OpenRound(10:5-10:6),LowerIdent(10:6-10:11),Comma(10:11-10:12),LowerIdent(10:13-10:19),Comma(10:19-10:20),LowerIdent(10:21-10:26),CloseRound(10:26-10:27),OpAssign(10:28-10:29),OpenRound(10:30-10:31),Int(10:31-10:34),Comma(10:34-10:35),Int(10:36-10:38),Comma(10:38-10:39),Int(10:40-10:43),CloseRound(10:43-10:44),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:6-12:41),
OpenRound(13:5-13:6),LowerIdent(13:6-13:10),Comma(13:10-13:11),LowerIdent(13:12-13:18),Comma(13:18-13:19),LowerIdent(13:20-13:27),CloseRound(13:27-13:28),OpAssign(13:29-13:30),OpenRound(13:31-13:32),StringStart(13:32-13:33),StringPart(13:33-13:38),StringEnd(13:38-13:39),Comma(13:39-13:40),StringStart(13:41-13:42),StringPart(13:42-13:47),StringEnd(13:47-13:48),Comma(13:48-13:49),UpperIdent(13:50-13:54),CloseRound(13:54-13:55),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:6-15:30),
OpenRound(16:5-16:6),LowerIdent(16:6-16:10),Comma(16:10-16:11),LowerIdent(16:12-16:17),CloseRound(16:17-16:18),OpAssign(16:19-16:20),OpenRound(16:21-16:22),OpenSquare(16:22-16:23),Int(16:23-16:24),Comma(16:24-16:25),Int(16:26-16:27),Comma(16:27-16:28),Int(16:29-16:30),CloseSquare(16:30-16:31),Comma(16:31-16:32),StringStart(16:33-16:34),StringPart(16:34-16:39),StringEnd(16:39-16:40),CloseRound(16:40-16:41),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenCurly(18:5-18:6),CloseCurly(18:6-18:7),Newline(1:1-1:1),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(e-block @1-1-19-2
	(statements
		(e-tuple @4-5-4-11
			(e-ident @4-6-4-7 (qaul "") (raw "x"))
			(e-ident @4-9-4-10 (qaul "") (raw "y")))
		(e-malformed @4-12-4-15 (reason "expr_unexpected_token"))
		(e-tuple @4-14-4-20
			(e-int @4-15-4-16 (raw "1"))
			(e-int @4-18-4-19 (raw "2")))
		(e-tuple @7-5-7-21
			(e-tuple @7-6-7-12
				(e-ident @7-7-7-8 (qaul "") (raw "a"))
				(e-ident @7-10-7-11 (qaul "") (raw "b")))
			(e-tuple @7-14-7-20
				(e-ident @7-15-7-16 (qaul "") (raw "c"))
				(e-ident @7-18-7-19 (qaul "") (raw "d"))))
		(e-malformed @7-22-7-25 (reason "expr_unexpected_token"))
		(e-tuple @7-24-7-44
			(e-tuple @7-25-7-33
				(e-int @7-26-7-28 (raw "10"))
				(e-int @7-30-7-32 (raw "20")))
			(e-tuple @7-35-7-43
				(e-int @7-36-7-38 (raw "30"))
				(e-int @7-40-7-42 (raw "40"))))
		(e-tuple @10-5-10-27
			(e-ident @10-6-10-11 (qaul "") (raw "first"))
			(e-ident @10-13-10-19 (qaul "") (raw "second"))
			(e-ident @10-21-10-26 (qaul "") (raw "third")))
		(e-malformed @10-28-10-31 (reason "expr_unexpected_token"))
		(e-tuple @10-30-10-44
			(e-int @10-31-10-34 (raw "100"))
			(e-int @10-36-10-38 (raw "42"))
			(e-int @10-40-10-43 (raw "200")))
		(e-tuple @13-5-13-28
			(e-ident @13-6-13-10 (qaul "") (raw "name"))
			(e-ident @13-12-13-18 (qaul "") (raw "string"))
			(e-ident @13-20-13-27 (qaul "") (raw "boolean")))
		(e-malformed @13-29-13-32 (reason "expr_unexpected_token"))
		(e-tuple @13-31-13-55
			(e-string @13-32-13-39
				(e-string-part @13-33-13-38 (raw "Alice")))
			(e-string @13-41-13-48
				(e-string-part @13-42-13-47 (raw "fixed")))
			(e-tag @13-50-13-54 (raw "True")))
		(e-tuple @16-5-16-18
			(e-ident @16-6-16-10 (qaul "") (raw "list"))
			(e-ident @16-12-16-17 (qaul "") (raw "hello")))
		(e-malformed @16-19-16-22 (reason "expr_unexpected_token"))
		(e-tuple @16-21-16-41
			(e-list @16-22-16-31
				(e-int @16-23-16-24 (raw "1"))
				(e-int @16-26-16-27 (raw "2"))
				(e-int @16-29-16-30 (raw "3")))
			(e-string @16-33-16-40
				(e-string-part @16-34-16-39 (raw "hello"))))
		(e-record @18-5-18-7)))
~~~
# FORMATTED
~~~roc
{

	# Simple tuple destructuring
	(x, y)
	
	(1, 2)

	# Nested tuple patterns
	((a, b), (c, d))
	
	((10, 20), (30, 40))

	# Mixed patterns with literals
	(first, second, third)
	
	(100, 42, 200)

	# Tuple with string and tag patterns
	(name, string, boolean)
	
	("Alice", "fixed", True)

	# Tuple with list pattern
	(list, hello)
	
	([1, 2, 3], "hello")

	{}
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1-1-19-2 (id 146)
	(s-expr @4-5-4-13
		(e-tuple @4-5-4-11
			(elems
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope")))))
	(s-expr @4-14-7-6
		(e-tuple @4-14-4-20
			(elems
				(e-int @4-15-4-16 (value "1"))
				(e-int @4-18-4-19 (value "2")))))
	(s-expr @7-5-7-23
		(e-tuple @7-5-7-21
			(elems
				(e-tuple @7-6-7-12
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope"))))
				(e-tuple @7-14-7-20
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope")))))))
	(s-expr @7-24-10-6
		(e-tuple @7-24-7-44
			(elems
				(e-tuple @7-25-7-33
					(elems
						(e-int @7-26-7-28 (value "10"))
						(e-int @7-30-7-32 (value "20"))))
				(e-tuple @7-35-7-43
					(elems
						(e-int @7-36-7-38 (value "30"))
						(e-int @7-40-7-42 (value "40")))))))
	(s-expr @10-5-10-29
		(e-tuple @10-5-10-27
			(elems
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope")))))
	(s-expr @10-30-13-6
		(e-tuple @10-30-10-44
			(elems
				(e-int @10-31-10-34 (value "100"))
				(e-int @10-36-10-38 (value "42"))
				(e-int @10-40-10-43 (value "200")))))
	(s-expr @13-5-13-30
		(e-tuple @13-5-13-28
			(elems
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope")))))
	(s-expr @13-31-16-6
		(e-tuple @13-31-13-55
			(elems
				(e-string @13-32-13-39
					(e-literal @13-33-13-38 (string "Alice")))
				(e-string @13-41-13-48
					(e-literal @13-42-13-47 (string "fixed")))
				(e-tag @13-50-13-54 (ext-var 0) (name "True") (args "TODO")))))
	(s-expr @16-5-16-20
		(e-tuple @16-5-16-18
			(elems
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope")))))
	(s-expr @16-21-18-6
		(e-tuple @16-21-16-41
			(elems
				(e-list @16-22-16-31 (elem-var 137)
					(elems
						(e-int @16-23-16-24 (value "1"))
						(e-int @16-26-16-27 (value "2"))
						(e-int @16-29-16-30 (value "3"))))
				(e-string @16-33-16-40
					(e-literal @16-34-16-39 (string "hello"))))))
	(e-empty_record @18-5-18-7))
~~~
# TYPES
~~~clojure
(expr (id 146) (type "*"))
~~~
