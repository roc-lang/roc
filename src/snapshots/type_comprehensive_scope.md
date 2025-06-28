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
          ^^^^^^^^^^^^^^^^^^


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
(file @1-1-38-2
	(module @1-1-1-43
		(exposes @1-8-1-43
			(exposed-upper-ident (text "MyU64"))
			(exposed-upper-ident (text "Person"))
			(exposed-upper-ident (text "Result"))
			(exposed-upper-ident (text "Tree"))
			(exposed-upper-ident (text "Node"))))
	(statements
		(s-type-decl @4-1-5-9
			(header @4-1-4-6 (name "MyU64")
				(args))
			(ty (name "U64")))
		(s-type-decl @5-1-6-7
			(header @5-1-5-9 (name "MyString")
				(args))
			(ty (name "Str")))
		(s-type-decl @6-1-9-7
			(header @6-1-6-7 (name "MyBool")
				(args))
			(ty (name "Bool")))
		(s-type-decl @9-1-12-7
			(header @9-1-9-7 (name "Person")
				(args))
			(ty-record @9-10-9-33
				(anno-record-field @9-12-9-22 (name "name")
					(ty (name "Str")))
				(anno-record-field @9-23-9-33 (name "age")
					(ty (name "U64")))))
		(s-type-decl @12-1-15-5
			(header @12-1-12-16 (name "Result")
				(args
					(ty-var @12-8-12-10 (raw "ok"))
					(ty-var @12-12-12-15 (raw "err"))))
			(ty-tag-union @12-19-12-37
				(tags
					(ty-apply @12-20-12-26
						(ty (name "Ok"))
						(ty-var @12-23-12-25 (raw "ok")))
					(ty-apply @12-28-12-36
						(ty (name "Err"))
						(ty-var @12-32-12-35 (raw "err"))))))
		(s-type-decl @15-1-18-5
			(header @15-1-15-8 (name "Tree")
				(args
					(ty-var @15-6-15-7 (raw "a"))))
			(ty-tag-union @15-11-15-37
				(tags
					(ty-apply @15-12-15-27
						(ty (name "Branch"))
						(ty-apply @15-19-15-26
							(ty (name "Node"))
							(ty-var @15-24-15-25 (raw "a"))))
					(ty-apply @15-29-15-36
						(ty (name "Leaf"))
						(ty-var @15-34-15-35 (raw "a"))))))
		(s-type-decl @18-1-21-9
			(header @18-1-18-8 (name "Node")
				(args
					(ty-var @18-6-18-7 (raw "a"))))
			(ty-record @18-11-18-48
				(anno-record-field @18-13-18-22 (name "value")
					(ty-var @18-20-18-21 (raw "a")))
				(anno-record-field @18-23-18-48 (name "children")
					(ty-apply @18-33-18-46
						(ty (name "List"))
						(ty-apply @18-38-18-45
							(ty (name "Tree"))
							(ty-var @18-43-18-44 (raw "a")))))))
		(s-type-decl @21-1-24-7
			(header @21-1-21-9 (name "MyResult")
				(args))
			(ty-apply @21-12-21-28
				(ty (name "Result"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl @24-1-27-8
			(header @24-1-24-7 (name "Person")
				(args))
			(ty (name "U64")))
		(s-type-decl @27-1-30-7
			(header @27-1-27-8 (name "BadType")
				(args))
			(ty (name "SomeUndeclaredType")))
		(s-type-decl @30-1-31-7
			(header @30-1-30-7 (name "MyList")
				(args))
			(ty-apply @30-10-30-19
				(ty (name "List"))
				(ty (name "Str"))))
		(s-type-decl @31-1-34-8
			(header @31-1-31-7 (name "MyDict")
				(args))
			(ty-apply @31-10-31-24
				(ty (name "Dict"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl @34-1-38-2
			(header @34-1-34-8 (name "Complex")
				(args))
			(ty-record @34-11-38-2
				(anno-record-field @35-5-35-20 (name "person")
					(ty (name "Person")))
				(anno-record-field @36-5-36-31 (name "result")
					(ty-apply @36-13-36-30
						(ty (name "Result"))
						(ty (name "Bool"))
						(ty (name "Str"))))
				(anno-record-field @37-5-38-2 (name "tree")
					(ty-apply @37-11-37-20
						(ty (name "Tree"))
						(ty (name "U64"))))))))
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
(can-ir
	(s-type-decl @4-1-5-9 (id 74)
		(ty-header @4-1-4-6 (name "MyU64"))
		(ty @4-9-4-12 (name "U64")))
	(s-type-decl @5-1-6-7 (id 77)
		(ty-header @5-1-5-9 (name "MyString"))
		(ty @5-12-5-15 (name "Str")))
	(s-type-decl @6-1-9-7 (id 80)
		(ty-header @6-1-6-7 (name "MyBool"))
		(ty @6-10-6-14 (name "Bool")))
	(s-type-decl @9-1-12-7 (id 87)
		(ty-header @9-1-9-7 (name "Person"))
		(ty-record @9-10-9-33
			(field (field "name")
				(ty @9-18-9-21 (name "Str")))
			(field (field "age")
				(ty @9-28-9-31 (name "U64")))))
	(s-type-decl @12-1-15-5 (id 96)
		(ty-header @12-1-12-16 (name "Result")
			(ty-args
				(ty-var @12-8-12-10 (name "ok"))
				(ty-var @12-12-12-15 (name "err"))))
		(ty-tag-union @12-19-12-37
			(ty-apply @12-20-12-26 (symbol "Ok")
				(ty-var @12-23-12-25 (name "ok")))
			(ty-apply @12-28-12-36 (symbol "Err")
				(ty-var @12-32-12-35 (name "err")))))
	(s-type-decl @15-1-18-5 (id 106)
		(ty-header @15-1-15-8 (name "Tree")
			(ty-args
				(ty-var @15-6-15-7 (name "a"))))
		(ty-tag-union @15-11-15-37
			(ty-apply @15-12-15-27 (symbol "Branch")
				(ty-apply @15-19-15-26 (symbol "Node")
					(ty-var @15-24-15-25 (name "a"))))
			(ty-apply @15-29-15-36 (symbol "Leaf")
				(ty-var @15-34-15-35 (name "a")))))
	(s-type-decl @18-1-21-9 (id 116)
		(ty-header @18-1-18-8 (name "Node")
			(ty-args
				(ty-var @18-6-18-7 (name "a"))))
		(ty-record @18-11-18-48
			(field (field "value")
				(ty-var @18-20-18-21 (name "a")))
			(field (field "children")
				(ty-apply @18-33-18-46 (symbol "List")
					(ty-apply @18-38-18-45 (symbol "Tree")
						(ty-var @18-43-18-44 (name "a")))))))
	(s-type-decl @21-1-24-7 (id 121)
		(ty-header @21-1-21-9 (name "MyResult"))
		(ty-apply @21-12-21-28 (symbol "Result")
			(ty @21-19-21-22 (name "Str"))
			(ty @21-24-21-27 (name "U64"))))
	(s-type-decl @24-1-27-8 (id 124)
		(ty-header @24-1-24-7 (name "Person"))
		(ty @24-10-24-13 (name "U64")))
	(s-type-decl @27-1-30-7 (id 129)
		(ty-header @27-1-27-8 (name "BadType"))
		(ty @27-11-27-29 (name "SomeUndeclaredType")))
	(s-type-decl @30-1-31-7 (id 133)
		(ty-header @30-1-30-7 (name "MyList"))
		(ty-apply @30-10-30-19 (symbol "List")
			(ty @30-15-30-18 (name "Str"))))
	(s-type-decl @31-1-34-8 (id 138)
		(ty-header @31-1-31-7 (name "MyDict"))
		(ty-apply @31-10-31-24 (symbol "Dict")
			(ty @31-15-31-18 (name "Str"))
			(ty @31-20-31-23 (name "U64"))))
	(s-type-decl @34-1-38-2 (id 150)
		(ty-header @34-1-34-8 (name "Complex"))
		(ty-record @34-11-38-2
			(field (field "person")
				(ty @35-13-35-19 (name "Person")))
			(field (field "result")
				(ty-apply @36-13-36-30 (symbol "Result")
					(ty @36-20-36-24 (name "Bool"))
					(ty @36-26-36-29 (name "Str"))))
			(field (field "tree")
				(ty-apply @37-11-37-20 (symbol "Tree")
					(ty @37-16-37-19 (name "U64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
