# META
~~~ini
description=Comprehensive type scope validation - built-ins, user types, redeclaration, forward refs
type=snippet
~~~
# SOURCE
~~~roc
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
# EXPECTED
TYPE REDECLARED - type_comprehensive_scope.md:10:1:10:37
UNDECLARED TYPE - type_comprehensive_scope.md:13:19:13:23
TYPE REDECLARED - type_comprehensive_scope.md:22:1:22:13
UNDECLARED TYPE - type_comprehensive_scope.md:25:11:25:29
# PROBLEMS
**TYPE REDECLARED**
The type _Result_ is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:10:1:10:37:**
```roc
Result(ok, err) : [Ok(ok), Err(err)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But _Result_ was already declared here:
**type_comprehensive_scope.md:1:1:1:1:**
```roc
# Built-in types should work
```
^


**UNDECLARED TYPE**
The type _Node_ is not declared in this scope.

This type is referenced here:
**type_comprehensive_scope.md:13:19:13:23:**
```roc
Tree(a) : [Branch(Node(a)), Leaf(a)]
```
                  ^^^^


**TYPE REDECLARED**
The type _Person_ is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:22:1:22:13:**
```roc
Person : U64
```
^^^^^^^^^^^^

But _Person_ was already declared here:
**type_comprehensive_scope.md:7:1:7:33:**
```roc
Person : { name: Str, age: U64 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _SomeUndeclaredType_ is not declared in this scope.

This type is referenced here:
**type_comprehensive_scope.md:25:11:25:29:**
```roc
BadType : SomeUndeclaredType
```
          ^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyU64")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "MyString")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "MyBool")
				(args))
			(ty (name "Bool")))
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record
				(anno-record-field (name "name")
					(ty (name "Str")))
				(anno-record-field (name "age")
					(ty (name "U64")))))
		(s-type-decl
			(header (name "Result")
				(args
					(ty-var (raw "ok"))
					(ty-var (raw "err"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "ok")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "err"))))))
		(s-type-decl
			(header (name "Tree")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Branch"))
						(ty-apply
							(ty (name "Node"))
							(ty-var (raw "a"))))
					(ty-apply
						(ty (name "Leaf"))
						(ty-var (raw "a"))))))
		(s-type-decl
			(header (name "Node")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "value")
					(ty-var (raw "a")))
				(anno-record-field (name "children")
					(ty-apply
						(ty (name "List"))
						(ty-apply
							(ty (name "Tree"))
							(ty-var (raw "a")))))))
		(s-type-decl
			(header (name "MyResult")
				(args))
			(ty-apply
				(ty (name "Result"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl
			(header (name "Person")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "BadType")
				(args))
			(ty (name "SomeUndeclaredType")))
		(s-type-decl
			(header (name "MyList")
				(args))
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
		(s-type-decl
			(header (name "MyDict")
				(args))
			(ty-apply
				(ty (name "Dict"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl
			(header (name "Complex")
				(args))
			(ty-record
				(anno-record-field (name "person")
					(ty (name "Person")))
				(anno-record-field (name "result")
					(ty-apply
						(ty (name "Result"))
						(ty (name "Bool"))
						(ty (name "Str"))))
				(anno-record-field (name "tree")
					(ty-apply
						(ty (name "Tree"))
						(ty (name "U64"))))))))
~~~
# FORMATTED
~~~roc
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
	(s-alias-decl
		(ty-header (name "MyU64"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "MyString"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "MyBool"))
		(ty-lookup (name "Bool") (external (module-idx "2") (target-node-idx "0"))))
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "name")
				(ty-lookup (name "Str") (builtin)))
			(field (field "age")
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "Result")
			(ty-args
				(ty-rigid-var (name "ok"))
				(ty-rigid-var (name "err"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "err"))))))
	(s-alias-decl
		(ty-header (name "Tree")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Branch")
				(ty-malformed))
			(ty-tag-name (name "Leaf")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-alias-decl
		(ty-header (name "Node")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "value")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(field (field "children")
				(ty-apply (name "List") (builtin)
					(ty-apply (name "Tree") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(s-alias-decl
		(ty-header (name "MyResult"))
		(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "0"))
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "U64") (builtin))))
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "BadType"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "MyList"))
		(ty-apply (name "List") (builtin)
			(ty-lookup (name "Str") (builtin))))
	(s-alias-decl
		(ty-header (name "MyDict"))
		(ty-apply (name "Dict") (external (module-idx "0") (target-node-idx "0"))
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "U64") (builtin))))
	(s-alias-decl
		(ty-header (name "Complex"))
		(ty-record
			(field (field "person")
				(ty-lookup (name "Person") (local)))
			(field (field "result")
				(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "0"))
					(ty-lookup (name "Bool") (external (module-idx "2") (target-node-idx "0")))
					(ty-lookup (name "Str") (builtin))))
			(field (field "tree")
				(ty-apply (name "Tree") (local)
					(ty-lookup (name "U64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "MyU64")
			(ty-header (name "MyU64")))
		(alias (type "MyString")
			(ty-header (name "MyString")))
		(alias (type "MyBool")
			(ty-header (name "MyBool")))
		(alias (type "Person")
			(ty-header (name "Person")))
		(alias (type "Result(ok, err)")
			(ty-header (name "Result")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err")))))
		(alias (type "Tree(a)")
			(ty-header (name "Tree")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Node(a)")
			(ty-header (name "Node")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "MyResult")
			(ty-header (name "MyResult")))
		(alias (type "Person")
			(ty-header (name "Person")))
		(alias (type "BadType")
			(ty-header (name "BadType")))
		(alias (type "MyList")
			(ty-header (name "MyList")))
		(alias (type "MyDict")
			(ty-header (name "MyDict")))
		(alias (type "Complex")
			(ty-header (name "Complex"))))
	(expressions))
~~~
