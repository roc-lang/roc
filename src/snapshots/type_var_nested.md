# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variables nested in Result and List types
mapResult : Result(a, e), (a -> b) -> Result(b, e)
mapResult = |result, transform| {
    when result is
        Ok(value) -> Ok(transform(value))
        Err(error) -> Err(error)
}

# Type variables nested in Maybe and List
filterMaybes : List(Maybe(t)) -> List(t)
filterMaybes = |list| {
    List.keepOks(list)
}

main! = |_| {}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `is` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

**UNUSED VARIABLE**
Variable ``transform`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_transform` to suppress this warning.
The unused variable is declared here:
**type_var_nested.md:5:22:5:31:**
```roc
mapResult = |result, transform| {
```


**UNDEFINED VARIABLE**
Nothing is named `keepOks` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:49),
LowerIdent(4:1-4:10),OpColon(4:11-4:12),UpperIdent(4:13-4:19),NoSpaceOpenRound(4:19-4:20),LowerIdent(4:20-4:21),Comma(4:21-4:22),LowerIdent(4:23-4:24),CloseRound(4:24-4:25),Comma(4:25-4:26),OpenRound(4:27-4:28),LowerIdent(4:28-4:29),OpArrow(4:30-4:32),LowerIdent(4:33-4:34),CloseRound(4:34-4:35),OpArrow(4:36-4:38),UpperIdent(4:39-4:45),NoSpaceOpenRound(4:45-4:46),LowerIdent(4:46-4:47),Comma(4:47-4:48),LowerIdent(4:49-4:50),CloseRound(4:50-4:51),Newline(1:1-1:1),
LowerIdent(5:1-5:10),OpAssign(5:11-5:12),OpBar(5:13-5:14),LowerIdent(5:14-5:20),Comma(5:20-5:21),LowerIdent(5:22-5:31),OpBar(5:31-5:32),OpenCurly(5:33-5:34),Newline(1:1-1:1),
LowerIdent(6:5-6:9),LowerIdent(6:10-6:16),LowerIdent(6:17-6:19),Newline(1:1-1:1),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:17),CloseRound(7:17-7:18),OpArrow(7:19-7:21),UpperIdent(7:22-7:24),NoSpaceOpenRound(7:24-7:25),LowerIdent(7:25-7:34),NoSpaceOpenRound(7:34-7:35),LowerIdent(7:35-7:40),CloseRound(7:40-7:41),CloseRound(7:41-7:42),Newline(1:1-1:1),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),LowerIdent(8:13-8:18),CloseRound(8:18-8:19),OpArrow(8:20-8:22),UpperIdent(8:23-8:26),NoSpaceOpenRound(8:26-8:27),LowerIdent(8:27-8:32),CloseRound(8:32-8:33),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:42),
LowerIdent(12:1-12:13),OpColon(12:14-12:15),UpperIdent(12:16-12:20),NoSpaceOpenRound(12:20-12:21),UpperIdent(12:21-12:26),NoSpaceOpenRound(12:26-12:27),LowerIdent(12:27-12:28),CloseRound(12:28-12:29),CloseRound(12:29-12:30),OpArrow(12:31-12:33),UpperIdent(12:34-12:38),NoSpaceOpenRound(12:38-12:39),LowerIdent(12:39-12:40),CloseRound(12:40-12:41),Newline(1:1-1:1),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:21),OpBar(13:21-13:22),OpenCurly(13:23-13:24),Newline(1:1-1:1),
UpperIdent(14:5-14:9),NoSpaceDotLowerIdent(14:9-14:17),NoSpaceOpenRound(14:17-14:18),LowerIdent(14:18-14:22),CloseRound(14:22-14:23),Newline(1:1-1:1),
CloseCurly(15:1-15:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(17:1-17:6),OpAssign(17:7-17:8),OpBar(17:9-17:10),Underscore(17:10-17:11),OpBar(17:11-17:12),OpenCurly(17:13-17:14),CloseCurly(17:14-17:15),EndOfFile(17:15-17:15),
~~~
# PARSE
~~~clojure
(file @1-1-17-15
	(app @1-1-1-57
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-57 (name "pf")
			(e-string @1-28-1-55
				(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))
		(packages @1-13-1-57
			(record-field @1-15-1-57 (name "pf")
				(e-string @1-28-1-55
					(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4-1-5-10 (name "mapResult")
			(ty-fn @4-13-4-51
				(ty-apply @4-13-4-25
					(ty (name "Result"))
					(ty-var @4-20-4-21 (raw "a"))
					(ty-var @4-23-4-24 (raw "e")))
				(ty-fn @4-28-4-34
					(ty-var @4-28-4-29 (raw "a"))
					(ty-var @4-33-4-34 (raw "b")))
				(ty-apply @4-39-4-51
					(ty (name "Result"))
					(ty-var @4-46-4-47 (raw "b"))
					(ty-var @4-49-4-50 (raw "e")))))
		(s-decl @5-1-9-2
			(p-ident @5-1-5-10 (raw "mapResult"))
			(e-lambda @5-13-9-2
				(args
					(p-ident @5-14-5-20 (raw "result"))
					(p-ident @5-22-5-31 (raw "transform")))
				(e-block @5-33-9-2
					(statements
						(e-ident @6-5-6-9 (qaul "") (raw "when"))
						(e-ident @6-10-6-16 (qaul "") (raw "result"))
						(e-ident @6-17-6-19 (qaul "") (raw "is"))
						(e-local-dispatch @7-9-8-12
							(e-apply @7-9-7-18
								(e-tag @7-9-7-11 (raw "Ok"))
								(e-ident @7-12-7-17 (qaul "") (raw "value")))
							(e-apply @7-19-7-42
								(e-tag @7-22-7-24 (raw "Ok"))
								(e-apply @7-25-7-41
									(e-ident @7-25-7-34 (qaul "") (raw "transform"))
									(e-ident @7-35-7-40 (qaul "") (raw "value")))))
						(e-local-dispatch @8-9-9-2
							(e-apply @8-9-8-19
								(e-tag @8-9-8-12 (raw "Err"))
								(e-ident @8-13-8-18 (qaul "") (raw "error")))
							(e-apply @8-20-8-33
								(e-tag @8-23-8-26 (raw "Err"))
								(e-ident @8-27-8-32 (qaul "") (raw "error"))))))))
		(s-type-anno @12-1-13-13 (name "filterMaybes")
			(ty-fn @12-16-12-41
				(ty-apply @12-16-12-30
					(ty (name "List"))
					(ty-apply @12-21-12-29
						(ty (name "Maybe"))
						(ty-var @12-27-12-28 (raw "t"))))
				(ty-apply @12-34-12-41
					(ty (name "List"))
					(ty-var @12-39-12-40 (raw "t")))))
		(s-decl @13-1-15-2
			(p-ident @13-1-13-13 (raw "filterMaybes"))
			(e-lambda @13-16-15-2
				(args
					(p-ident @13-17-13-21 (raw "list")))
				(e-block @13-23-15-2
					(statements
						(e-apply @14-5-14-23
							(e-ident @14-5-14-17 (qaul "List") (raw ".keepOks"))
							(e-ident @14-18-14-22 (qaul "") (raw "list")))))))
		(s-decl @17-1-17-15
			(p-ident @17-1-17-6 (raw "main!"))
			(e-lambda @17-9-17-15
				(args
					(p-underscore))
				(e-record @17-13-17-15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variables nested in Result and List types
mapResult : Result(a, e), (a -> b) -> Result(b, e)
mapResult = |result, transform| {
	when
	result
	is
	Ok(value)->Ok(transform(value))
	Err(error)->Err(error)
}

# Type variables nested in Maybe and List
filterMaybes : List(Maybe(t)) -> List(t)
filterMaybes = |list| {
	List.keepOks(list)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 122)
		(p-assign @5-1-5-10 (ident "mapResult") (id 90))
		(e-lambda @5-13-9-2 (id 108)
			(args
				(p-assign @5-14-5-20 (ident "result") (id 91))
				(p-assign @5-22-5-31 (ident "transform") (id 92)))
			(e-block @5-33-9-2
				(s-expr @6-5-6-16
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-expr @6-10-6-19
					(e-lookup-local @6-10-6-16
						(pattern (id 91))))
				(s-expr @6-17-7-11
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-expr @7-9-8-12
					(e-runtime-error (tag "not_implemented")))
				(e-runtime-error (tag "not_implemented"))))
		(annotation @5-1-5-10 (signature 120) (id 121)
			(declared-type
				(ty-fn @4-13-4-51 (effectful false)
					(ty-apply @4-13-4-25 (symbol "Result")
						(ty-var @4-20-4-21 (name "a"))
						(ty-var @4-23-4-24 (name "e")))
					(ty-parens @4-27-4-35
						(ty-fn @4-28-4-34 (effectful false)
							(ty-var @4-28-4-29 (name "a"))
							(ty-var @4-33-4-34 (name "b"))))
					(ty-apply @4-39-4-51 (symbol "Result")
						(ty-var @4-46-4-47 (name "b"))
						(ty-var @4-49-4-50 (name "e")))))))
	(d-let (id 147)
		(p-assign @13-1-13-13 (ident "filterMaybes") (id 131))
		(e-lambda @13-16-15-2 (id 140)
			(args
				(p-assign @13-17-13-21 (ident "list") (id 132)))
			(e-block @13-23-15-2
				(e-call @14-5-14-23
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local @14-18-14-22
						(pattern (id 132))))))
		(annotation @13-1-13-13 (signature 145) (id 146)
			(declared-type
				(ty-fn @12-16-12-41 (effectful false)
					(ty-apply @12-16-12-30 (symbol "List")
						(ty-apply @12-21-12-29 (symbol "Maybe")
							(ty-var @12-27-12-28 (name "t"))))
					(ty-apply @12-34-12-41 (symbol "List")
						(ty-var @12-39-12-40 (name "t")))))))
	(d-let (id 153)
		(p-assign @17-1-17-6 (ident "main!") (id 148))
		(e-lambda @17-9-17-15 (id 152)
			(args
				(p-underscore @17-10-17-11 (id 149)))
			(e-empty_record @17-13-17-15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "mapResult") (def_var 122) (type "Result, a -> b -> Error"))
		(d_assign (name "filterMaybes") (def_var 147) (type "List -> List"))
		(d_assign (name "main!") (def_var 153) (type "* ? {}")))
	(expressions
		(expr @5-13-9-2 (type "Result, a -> b -> Error"))
		(expr @13-16-15-2 (type "List -> List"))
		(expr @17-9-17-15 (type "* ? {}"))))
~~~
