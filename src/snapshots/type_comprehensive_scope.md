# META
~~~ini
description=Comprehensive type scope validation - built-ins, user types, redeclaration, forward refs
type=file
~~~
# SOURCE
~~~roc
module [MyU64, Person, Result, Tree, Node]

# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name: Str, age: U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value: a, children: List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
    person: Person,
    result: Result(Bool, Str),
    tree: Tree(U64)
}
~~~
# PROBLEMS
**TYPE REDECLARED**
The type ``Result`` is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:12:1:15:5:**
```roc
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]
```

But ``Result`` was already declared here:
**type_comprehensive_scope.md:1:1:1:1:**
```roc
module [MyU64, Person, Result, Tree, Node]
```


**TYPE REDECLARED**
The type ``Person`` is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:24:1:27:8:**
```roc
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType
```

But ``Person`` was already declared here:
**type_comprehensive_scope.md:9:1:12:7:**
```roc
Person : { name: Str, age: U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]
```


**UNDECLARED TYPE**
The type ``SomeUndeclaredType`` is not declared in this scope.

This type is referenced here:
**type_comprehensive_scope.md:27:11:27:29:**
```roc
BadType : SomeUndeclaredType
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:22),Comma(1:22-1:23),UpperIdent(1:24-1:30),Comma(1:30-1:31),UpperIdent(1:32-1:36),Comma(1:36-1:37),UpperIdent(1:38-1:42),CloseSquare(1:42-1:43),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:29),
UpperIdent(4:1-4:6),OpColon(4:7-4:8),UpperIdent(4:9-4:12),Newline(1:1-1:1),
UpperIdent(5:1-5:9),OpColon(5:10-5:11),UpperIdent(5:12-5:15),Newline(1:1-1:1),
UpperIdent(6:1-6:7),OpColon(6:8-6:9),UpperIdent(6:10-6:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:2-8:27),
UpperIdent(9:1-9:7),OpColon(9:8-9:9),OpenCurly(9:10-9:11),LowerIdent(9:12-9:16),OpColon(9:16-9:17),UpperIdent(9:18-9:21),Comma(9:21-9:22),LowerIdent(9:23-9:26),OpColon(9:26-9:27),UpperIdent(9:28-9:31),CloseCurly(9:32-9:33),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:23),
UpperIdent(12:1-12:7),NoSpaceOpenRound(12:7-12:8),LowerIdent(12:8-12:10),Comma(12:10-12:11),LowerIdent(12:12-12:15),CloseRound(12:15-12:16),OpColon(12:17-12:18),OpenSquare(12:19-12:20),UpperIdent(12:20-12:22),NoSpaceOpenRound(12:22-12:23),LowerIdent(12:23-12:25),CloseRound(12:25-12:26),Comma(12:26-12:27),UpperIdent(12:28-12:31),NoSpaceOpenRound(12:31-12:32),LowerIdent(12:32-12:35),CloseRound(12:35-12:36),CloseSquare(12:36-12:37),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(14:2-14:66),
UpperIdent(15:1-15:5),NoSpaceOpenRound(15:5-15:6),LowerIdent(15:6-15:7),CloseRound(15:7-15:8),OpColon(15:9-15:10),OpenSquare(15:11-15:12),UpperIdent(15:12-15:18),NoSpaceOpenRound(15:18-15:19),UpperIdent(15:19-15:23),NoSpaceOpenRound(15:23-15:24),LowerIdent(15:24-15:25),CloseRound(15:25-15:26),CloseRound(15:26-15:27),Comma(15:27-15:28),UpperIdent(15:29-15:33),NoSpaceOpenRound(15:33-15:34),LowerIdent(15:34-15:35),CloseRound(15:35-15:36),CloseSquare(15:36-15:37),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(17:2-17:35),
UpperIdent(18:1-18:5),NoSpaceOpenRound(18:5-18:6),LowerIdent(18:6-18:7),CloseRound(18:7-18:8),OpColon(18:9-18:10),OpenCurly(18:11-18:12),LowerIdent(18:13-18:18),OpColon(18:18-18:19),LowerIdent(18:20-18:21),Comma(18:21-18:22),LowerIdent(18:23-18:31),OpColon(18:31-18:32),UpperIdent(18:33-18:37),NoSpaceOpenRound(18:37-18:38),UpperIdent(18:38-18:42),NoSpaceOpenRound(18:42-18:43),LowerIdent(18:43-18:44),CloseRound(18:44-18:45),CloseRound(18:45-18:46),CloseCurly(18:47-18:48),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(20:2-20:34),
UpperIdent(21:1-21:9),OpColon(21:10-21:11),UpperIdent(21:12-21:18),NoSpaceOpenRound(21:18-21:19),UpperIdent(21:19-21:22),Comma(21:22-21:23),UpperIdent(21:24-21:27),CloseRound(21:27-21:28),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(23:2-23:36),
UpperIdent(24:1-24:7),OpColon(24:8-24:9),UpperIdent(24:10-24:13),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(26:2-26:42),
UpperIdent(27:1-27:8),OpColon(27:9-27:10),UpperIdent(27:11-27:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(29:2-29:39),
UpperIdent(30:1-30:7),OpColon(30:8-30:9),UpperIdent(30:10-30:14),NoSpaceOpenRound(30:14-30:15),UpperIdent(30:15-30:18),CloseRound(30:18-30:19),Newline(1:1-1:1),
UpperIdent(31:1-31:7),OpColon(31:8-31:9),UpperIdent(31:10-31:14),NoSpaceOpenRound(31:14-31:15),UpperIdent(31:15-31:18),Comma(31:18-31:19),UpperIdent(31:20-31:23),CloseRound(31:23-31:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(33:2-33:52),
UpperIdent(34:1-34:8),OpColon(34:9-34:10),OpenCurly(34:11-34:12),Newline(1:1-1:1),
LowerIdent(35:5-35:11),OpColon(35:11-35:12),UpperIdent(35:13-35:19),Comma(35:19-35:20),Newline(1:1-1:1),
LowerIdent(36:5-36:11),OpColon(36:11-36:12),UpperIdent(36:13-36:19),NoSpaceOpenRound(36:19-36:20),UpperIdent(36:20-36:24),Comma(36:24-36:25),UpperIdent(36:26-36:29),CloseRound(36:29-36:30),Comma(36:30-36:31),Newline(1:1-1:1),
LowerIdent(37:5-37:9),OpColon(37:9-37:10),UpperIdent(37:11-37:15),NoSpaceOpenRound(37:15-37:16),UpperIdent(37:16-37:19),CloseRound(37:19-37:20),Newline(1:1-1:1),
CloseCurly(38:1-38:2),EndOfFile(38:2-38:2),
~~~
# PARSE
~~~clojure
(file (1:1-38:2)
	(module (1:1-1:43)
		(exposes (1:8-1:43)
			(exposed_item (upper_ident "MyU64"))
			(exposed_item (upper_ident "Person"))
			(exposed_item (upper_ident "Result"))
			(exposed_item (upper_ident "Tree"))
			(exposed_item (upper_ident "Node"))))
	(statements
		(type_decl (4:1-5:9)
			(header (4:1-4:6) "MyU64" (args))
			(ty "U64"))
		(type_decl (5:1-6:7)
			(header (5:1-5:9) "MyString" (args))
			(ty "Str"))
		(type_decl (6:1-9:7)
			(header (6:1-6:7) "MyBool" (args))
			(ty "Bool"))
		(type_decl (9:1-12:7)
			(header (9:1-9:7) "Person" (args))
			(record (9:10-9:33)
				(anno_record_field (9:12-9:22) "name" (ty "Str"))
				(anno_record_field (9:23-9:33) "age" (ty "U64"))))
		(type_decl (12:1-15:5)
			(header (12:1-12:16)
				"Result"
				(args
					(ty_var (12:8-12:10) "ok")
					(ty_var (12:12-12:15) "err")))
			(tag_union (12:19-12:37)
				(tags
					(apply (12:20-12:26)
						(ty "Ok")
						(ty_var (12:23-12:25) "ok"))
					(apply (12:28-12:36)
						(ty "Err")
						(ty_var (12:32-12:35) "err")))))
		(type_decl (15:1-18:5)
			(header (15:1-15:8)
				"Tree"
				(args (ty_var (15:6-15:7) "a")))
			(tag_union (15:11-15:37)
				(tags
					(apply (15:12-15:27)
						(ty "Branch")
						(apply (15:19-15:26)
							(ty "Node")
							(ty_var (15:24-15:25) "a")))
					(apply (15:29-15:36)
						(ty "Leaf")
						(ty_var (15:34-15:35) "a")))))
		(type_decl (18:1-21:9)
			(header (18:1-18:8)
				"Node"
				(args (ty_var (18:6-18:7) "a")))
			(record (18:11-18:48)
				(anno_record_field (18:13-18:22)
					"value"
					(ty_var (18:20-18:21) "a"))
				(anno_record_field (18:23-18:48)
					"children"
					(apply (18:33-18:46)
						(ty "List")
						(apply (18:38-18:45)
							(ty "Tree")
							(ty_var (18:43-18:44) "a"))))))
		(type_decl (21:1-24:7)
			(header (21:1-21:9) "MyResult" (args))
			(apply (21:12-21:28)
				(ty "Result")
				(ty "Str")
				(ty "U64")))
		(type_decl (24:1-27:8)
			(header (24:1-24:7) "Person" (args))
			(ty "U64"))
		(type_decl (27:1-30:7)
			(header (27:1-27:8) "BadType" (args))
			(ty "SomeUndeclaredType"))
		(type_decl (30:1-31:7)
			(header (30:1-30:7) "MyList" (args))
			(apply (30:10-30:19)
				(ty "List")
				(ty "Str")))
		(type_decl (31:1-34:8)
			(header (31:1-31:7) "MyDict" (args))
			(apply (31:10-31:24)
				(ty "Dict")
				(ty "Str")
				(ty "U64")))
		(type_decl (34:1-38:2)
			(header (34:1-34:8) "Complex" (args))
			(record (34:11-38:2)
				(anno_record_field (35:5-35:20) "person" (ty "Person"))
				(anno_record_field (36:5-36:31)
					"result"
					(apply (36:13-36:30)
						(ty "Result")
						(ty "Bool")
						(ty "Str")))
				(anno_record_field (37:5-38:2)
					"tree"
					(apply (37:11-37:20)
						(ty "Tree")
						(ty "U64")))))))
~~~
# FORMATTED
~~~roc
module [MyU64, Person, Result, Tree, Node]

# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name : Str, age : U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value : a, children : List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
	person : Person,
	result : Result(Bool, Str),
	tree : Tree(U64),
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(s_type_decl (4:1-5:9)
		(type_header (4:1-4:6) "MyU64")
		(ty (4:9-4:12) "U64"))
	(s_type_decl (5:1-6:7)
		(type_header (5:1-5:9) "MyString")
		(ty (5:12-5:15) "Str"))
	(s_type_decl (6:1-9:7)
		(type_header (6:1-6:7) "MyBool")
		(ty (6:10-6:14) "Bool"))
	(s_type_decl (9:1-12:7)
		(type_header (9:1-9:7) "Person")
		(record (9:10-9:33)
			(record_field "name" (ty (9:18-9:21) "Str"))
			(record_field "age" (ty (9:28-9:31) "U64"))))
	(s_type_decl (12:1-15:5)
		(type_header (12:1-12:16)
			"Result"
			(args
				(ty_var (12:8-12:10) "ok")
				(ty_var (12:12-12:15) "err")))
		(tag_union (12:19-12:37)
			(apply (12:20-12:26)
				"Ok"
				(ty_var (12:23-12:25) "ok"))
			(apply (12:28-12:36)
				"Err"
				(ty_var (12:32-12:35) "err"))))
	(s_type_decl (15:1-18:5)
		(type_header (15:1-15:8)
			"Tree"
			(args (ty_var (15:6-15:7) "a")))
		(tag_union (15:11-15:37)
			(apply (15:12-15:27)
				"Branch"
				(apply (15:19-15:26)
					"Node"
					(ty_var (15:24-15:25) "a")))
			(apply (15:29-15:36)
				"Leaf"
				(ty_var (15:34-15:35) "a"))))
	(s_type_decl (18:1-21:9)
		(type_header (18:1-18:8)
			"Node"
			(args (ty_var (18:6-18:7) "a")))
		(record (18:11-18:48)
			(record_field "value" (ty_var (18:20-18:21) "a"))
			(record_field
				"children"
				(apply (18:33-18:46)
					"List"
					(apply (18:38-18:45)
						"Tree"
						(ty_var (18:43-18:44) "a"))))))
	(s_type_decl (21:1-24:7)
		(type_header (21:1-21:9) "MyResult")
		(apply (21:12-21:28)
			"Result"
			(ty (21:19-21:22) "Str")
			(ty (21:24-21:27) "U64")))
	(s_type_decl (24:1-27:8)
		(type_header (24:1-24:7) "Person")
		(ty (24:10-24:13) "U64"))
	(s_type_decl (27:1-30:7)
		(type_header (27:1-27:8) "BadType")
		(ty (27:11-27:29) "SomeUndeclaredType"))
	(s_type_decl (30:1-31:7)
		(type_header (30:1-30:7) "MyList")
		(apply (30:10-30:19)
			"List"
			(ty (30:15-30:18) "Str")))
	(s_type_decl (31:1-34:8)
		(type_header (31:1-31:7) "MyDict")
		(apply (31:10-31:24)
			"Dict"
			(ty (31:15-31:18) "Str")
			(ty (31:20-31:23) "U64")))
	(s_type_decl (34:1-38:2)
		(type_header (34:1-34:8) "Complex")
		(record (34:11-38:2)
			(record_field "person" (ty (35:13-35:19) "Person"))
			(record_field
				"result"
				(apply (36:13-36:30)
					"Result"
					(ty (36:20-36:24) "Bool")
					(ty (36:26-36:29) "Str")))
			(record_field
				"tree"
				(apply (37:11-37:20)
					"Tree"
					(ty (37:16-37:19) "U64"))))))
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~