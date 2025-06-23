# META
~~~ini
description=Type redeclaration in same scope should produce error
type=file
~~~
# SOURCE
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``None`` is not declared in this scope.

This type is referenced here:
**type_redeclaration_same_scope.md:3:22:3:26:**
```roc
Maybe(a) : [Some(a), None]
```


**UNDECLARED TYPE**
The type ``Err`` is not declared in this scope.

This type is referenced here:
**type_redeclaration_same_scope.md:4:20:4:23:**
```roc
Maybe(a) : [Ok(a), Err]
```


**TYPE REDECLARED**
The type ``Maybe`` is being redeclared.

The redeclaration is here:
**type_redeclaration_same_scope.md:4:1:4:24:**
```roc
Maybe(a) : [Ok(a), Err]
```

But ``Maybe`` was already declared here:
**type_redeclaration_same_scope.md:3:1:4:6:**
```roc
Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColon(3:10-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),CloseSquare(3:26-3:27),Newline(1:1-1:1),
UpperIdent(4:1-4:6),NoSpaceOpenRound(4:6-4:7),LowerIdent(4:7-4:8),CloseRound(4:8-4:9),OpColon(4:10-4:11),OpenSquare(4:12-4:13),UpperIdent(4:13-4:15),NoSpaceOpenRound(4:15-4:16),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),Comma(4:18-4:19),UpperIdent(4:20-4:23),CloseSquare(4:23-4:24),EndOfFile(4:24-4:24),
~~~
# PARSE
~~~clojure
(file (1:1-4:24)
	(module (1:1-1:15)
		(exposes (1:8-1:15) (exposed_item (upper_ident "Maybe"))))
	(statements
		(type_decl (3:1-4:6)
			(header (3:1-3:9)
				"Maybe"
				(args (ty_var (3:7-3:8) "a")))
			(tag_union (3:12-3:27)
				(tags
					(apply (3:13-3:20)
						(ty "Some")
						(ty_var (3:18-3:19) "a"))
					(ty "None"))))
		(type_decl (4:1-4:24)
			(header (4:1-4:9)
				"Maybe"
				(args (ty_var (4:7-4:8) "a")))
			(tag_union (4:12-4:24)
				(tags
					(apply (4:13-4:18)
						(ty "Ok")
						(ty_var (4:16-4:17) "a"))
					(ty "Err"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(s_type_decl (3:1-4:6)
		(type_header (3:1-3:9)
			"Maybe"
			(args (ty_var (3:7-3:8) "a")))
		(tag_union (3:12-3:27)
			(apply (3:13-3:20)
				"Some"
				(ty_var (3:18-3:19) "a"))
			(ty (3:22-3:26) "None")))
	(s_type_decl (4:1-4:24)
		(type_header (4:1-4:9)
			"Maybe"
			(args (ty_var (4:7-4:8) "a")))
		(tag_union (4:12-4:24)
			(apply (4:13-4:18)
				"Ok"
				(ty_var (4:16-4:17) "a"))
			(ty (4:20-4:23) "Err"))))
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~