# META
~~~ini
description=Various type declarations
type=file
~~~
# SOURCE
~~~roc
module [Map, Foo, Some, Maybe, SomeFunc, add_one, main!]

Map(a, b) : List(a), (a -> b) -> List(b)

Foo : (Bar, Baz)

Some(a) : { foo : Ok(a), bar : Something }

Maybe(a) : [Some(a), None]

SomeFunc(a) : Maybe(a), a -> Maybe(a)

MyType : U64

MyType2 : Module.Thingy
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``Bar`` is not declared in this scope.

This type is referenced here:
**type_declarations.md:5:8:5:11:**
```roc
Foo : (Bar, Baz)
```


**UNDECLARED TYPE**
The type ``Baz`` is not declared in this scope.

This type is referenced here:
**type_declarations.md:5:13:5:16:**
```roc
Foo : (Bar, Baz)
```


**UNDECLARED TYPE**
The type ``Something`` is not declared in this scope.

This type is referenced here:
**type_declarations.md:7:32:7:41:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```


**UNDECLARED TYPE**
The type ``None`` is not declared in this scope.

This type is referenced here:
**type_declarations.md:9:22:9:26:**
```roc
Maybe(a) : [Some(a), None]
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:23),Comma(1:23-1:24),UpperIdent(1:25-1:30),Comma(1:30-1:31),UpperIdent(1:32-1:40),Comma(1:40-1:41),LowerIdent(1:42-1:49),Comma(1:49-1:50),LowerIdent(1:51-1:56),CloseSquare(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:4),NoSpaceOpenRound(3:4-3:5),LowerIdent(3:5-3:6),Comma(3:6-3:7),LowerIdent(3:8-3:9),CloseRound(3:9-3:10),OpColon(3:11-3:12),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Comma(3:20-3:21),OpenRound(3:22-3:23),LowerIdent(3:23-3:24),OpArrow(3:25-3:27),LowerIdent(3:28-3:29),CloseRound(3:29-3:30),OpArrow(3:31-3:33),UpperIdent(3:34-3:38),NoSpaceOpenRound(3:38-3:39),LowerIdent(3:39-3:40),CloseRound(3:40-3:41),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(5:1-5:4),OpColon(5:5-5:6),OpenRound(5:7-5:8),UpperIdent(5:8-5:11),Comma(5:11-5:12),UpperIdent(5:13-5:16),CloseRound(5:16-5:17),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(7:1-7:5),NoSpaceOpenRound(7:5-7:6),LowerIdent(7:6-7:7),CloseRound(7:7-7:8),OpColon(7:9-7:10),OpenCurly(7:11-7:12),LowerIdent(7:13-7:16),OpColon(7:17-7:18),UpperIdent(7:19-7:21),NoSpaceOpenRound(7:21-7:22),LowerIdent(7:22-7:23),CloseRound(7:23-7:24),Comma(7:24-7:25),LowerIdent(7:26-7:29),OpColon(7:30-7:31),UpperIdent(7:32-7:41),CloseCurly(7:42-7:43),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(9:1-9:6),NoSpaceOpenRound(9:6-9:7),LowerIdent(9:7-9:8),CloseRound(9:8-9:9),OpColon(9:10-9:11),OpenSquare(9:12-9:13),UpperIdent(9:13-9:17),NoSpaceOpenRound(9:17-9:18),LowerIdent(9:18-9:19),CloseRound(9:19-9:20),Comma(9:20-9:21),UpperIdent(9:22-9:26),CloseSquare(9:26-9:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(11:1-11:9),NoSpaceOpenRound(11:9-11:10),LowerIdent(11:10-11:11),CloseRound(11:11-11:12),OpColon(11:13-11:14),UpperIdent(11:15-11:20),NoSpaceOpenRound(11:20-11:21),LowerIdent(11:21-11:22),CloseRound(11:22-11:23),Comma(11:23-11:24),LowerIdent(11:25-11:26),OpArrow(11:27-11:29),UpperIdent(11:30-11:35),NoSpaceOpenRound(11:35-11:36),LowerIdent(11:36-11:37),CloseRound(11:37-11:38),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(13:1-13:7),OpColon(13:8-13:9),UpperIdent(13:10-13:13),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(15:1-15:8),OpColon(15:9-15:10),UpperIdent(15:11-15:17),NoSpaceDotUpperIdent(15:17-15:24),EndOfFile(15:24-15:24),
~~~
# PARSE
~~~clojure
(file (1:1-15:24)
	(module (1:1-1:57)
		(exposes (1:8-1:57)
			(exposed_item (upper_ident "Map"))
			(exposed_item (upper_ident "Foo"))
			(exposed_item (upper_ident "Some"))
			(exposed_item (upper_ident "Maybe"))
			(exposed_item (upper_ident "SomeFunc"))
			(exposed_item (lower_ident "add_one"))
			(exposed_item (lower_ident "main!"))))
	(statements
		(type_decl (3:1-5:4)
			(header (3:1-3:10)
				"Map"
				(args
					(ty_var (3:5-3:6) "a")
					(ty_var (3:8-3:9) "b")))
			(fn (3:13-3:41)
				(apply (3:13-3:20)
					(ty "List")
					(ty_var (3:18-3:19) "a"))
				(fn (3:23-3:29)
					(ty_var (3:23-3:24) "a")
					(ty_var (3:28-3:29) "b"))
				(apply (3:34-3:41)
					(ty "List")
					(ty_var (3:39-3:40) "b"))))
		(type_decl (5:1-7:5)
			(header (5:1-5:4) "Foo" (args))
			(tuple (5:7-5:17)
				(ty "Bar")
				(ty "Baz")))
		(type_decl (7:1-9:6)
			(header (7:1-7:8)
				"Some"
				(args (ty_var (7:6-7:7) "a")))
			(record (7:11-7:43)
				(anno_record_field (7:13-7:25)
					"foo"
					(apply (7:19-7:24)
						(ty "Ok")
						(ty_var (7:22-7:23) "a")))
				(anno_record_field (7:26-7:43) "bar" (ty "Something"))))
		(type_decl (9:1-11:9)
			(header (9:1-9:9)
				"Maybe"
				(args (ty_var (9:7-9:8) "a")))
			(tag_union (9:12-9:27)
				(tags
					(apply (9:13-9:20)
						(ty "Some")
						(ty_var (9:18-9:19) "a"))
					(ty "None"))))
		(type_decl (11:1-13:7)
			(header (11:1-11:12)
				"SomeFunc"
				(args (ty_var (11:10-11:11) "a")))
			(fn (11:15-11:38)
				(apply (11:15-11:23)
					(ty "Maybe")
					(ty_var (11:21-11:22) "a"))
				(ty_var (11:25-11:26) "a")
				(apply (11:30-11:38)
					(ty "Maybe")
					(ty_var (11:36-11:37) "a"))))
		(type_decl (13:1-15:8)
			(header (13:1-13:7) "MyType" (args))
			(ty "U64"))
		(type_decl (15:1-15:24)
			(header (15:1-15:8) "MyType2" (args))
			(mod_ty "Module" ".Thingy"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(s_type_decl (3:1-5:4)
		(type_header (3:1-3:10)
			"Map"
			(args
				(ty_var (3:5-3:6) "a")
				(ty_var (3:8-3:9) "b")))
		(fn (3:13-3:41)
			(apply (3:13-3:20)
				"List"
				(ty_var (3:18-3:19) "a"))
			(parens (3:22-3:30)
				(fn (3:23-3:29)
					(ty_var (3:23-3:24) "a")
					(ty_var (3:28-3:29) "b")
					"false"))
			(apply (3:34-3:41)
				"List"
				(ty_var (3:39-3:40) "b"))
			"false"))
	(s_type_decl (5:1-7:5)
		(type_header (5:1-5:4) "Foo")
		(tuple (5:7-5:17)
			(ty (5:8-5:11) "Bar")
			(ty (5:13-5:16) "Baz")))
	(s_type_decl (7:1-9:6)
		(type_header (7:1-7:8)
			"Some"
			(args (ty_var (7:6-7:7) "a")))
		(record (7:11-7:43)
			(record_field
				"foo"
				(apply (7:19-7:24)
					"Ok"
					(ty_var (7:22-7:23) "a")))
			(record_field "bar" (ty (7:32-7:41) "Something"))))
	(s_type_decl (9:1-11:9)
		(type_header (9:1-9:9)
			"Maybe"
			(args (ty_var (9:7-9:8) "a")))
		(tag_union (9:12-9:27)
			(apply (9:13-9:20)
				"Some"
				(ty_var (9:18-9:19) "a"))
			(ty (9:22-9:26) "None")))
	(s_type_decl (11:1-13:7)
		(type_header (11:1-11:12)
			"SomeFunc"
			(args (ty_var (11:10-11:11) "a")))
		(fn (11:15-11:38)
			(apply (11:15-11:23)
				"Maybe"
				(ty_var (11:21-11:22) "a"))
			(ty_var (11:25-11:26) "a")
			(apply (11:30-11:38)
				"Maybe"
				(ty_var (11:36-11:37) "a"))
			"false"))
	(s_type_decl (13:1-15:8)
		(type_header (13:1-13:7) "MyType")
		(ty (13:10-13:13) "U64"))
	(s_type_decl (15:1-15:24)
		(type_header (15:1-15:8) "MyType2")
		(mod_ty (15:11-15:24) "Thingy" "Module")))
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~