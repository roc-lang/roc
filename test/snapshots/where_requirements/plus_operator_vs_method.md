# META
~~~ini
description=Demonstrates error messages for + operator vs .plus() method on type without plus
type=file:MyType.roc
~~~
# SOURCE
~~~roc
MyType := [Val(U64)].{}

a : MyType
a = MyType.Val(5)

b : MyType
b = MyType.Val(10)

# Using the + operator - should show "Cannot use the + operator" message
result1 : MyType
result1 = a + b

c : MyType
c = MyType.Val(5)

d : MyType
d = MyType.Val(10)

# Using the .plus() method directly - should show generic "does not have a plus method" message
result2 : MyType
result2 = c.plus(d)
~~~
# EXPECTED
MISSING MEMBER - plus_operator_vs_method.md:11:11:11:16
+ - :0:0:0:0
MISSING MEMBER - plus_operator_vs_method.md:21:13:21:17
# PROBLEMS
**MISSING MEMBER**
The value before this **+** operator has a type that doesn't have a **plus** method:
**plus_operator_vs_method.md:11:11:11:16:**
```roc
result1 = a + b
```
          ^^^^^

The value's type, which does not have a member named **plus**, is:

    MyType

**Hint:** The **+** operator calls a member named **plus** on the value preceding it, passing the value after the operator as the one argument.

**MISSING MEMBER**
This **plus** member is being used on a value whose type doesn't provide that member:
**plus_operator_vs_method.md:21:13:21:17:**
```roc
result2 = c.plus(d)
```
            ^^^^

The value's type, which does not have a member named **plus**, is:

    MyType

**Hint:** For this to work, the type would need to have a member named **plus** in the type's declaration.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty (name "U64")))))
			(associated))
		(s-type-anno (name "a")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "a"))
			(e-apply
				(e-tag (raw "MyType.Val"))
				(e-int (raw "5"))))
		(s-type-anno (name "b")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "b"))
			(e-apply
				(e-tag (raw "MyType.Val"))
				(e-int (raw "10"))))
		(s-type-anno (name "result1")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "result1"))
			(e-binop (op "+")
				(e-ident (raw "a"))
				(e-ident (raw "b"))))
		(s-type-anno (name "c")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "c"))
			(e-apply
				(e-tag (raw "MyType.Val"))
				(e-int (raw "5"))))
		(s-type-anno (name "d")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "d"))
			(e-apply
				(e-tag (raw "MyType.Val"))
				(e-int (raw "10"))))
		(s-type-anno (name "result2")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "result2"))
			(e-field-access
				(e-ident (raw "c"))
				(e-apply
					(e-ident (raw "plus"))
					(e-ident (raw "d")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-nominal (nominal "MyType")
			(e-tag (name "Val")
				(args
					(e-num (value "5")))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "b"))
		(e-nominal (nominal "MyType")
			(e-tag (name "Val")
				(args
					(e-num (value "10")))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "result1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "a")))
			(e-lookup-local
				(p-assign (ident "b"))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "c"))
		(e-nominal (nominal "MyType")
			(e-tag (name "Val")
				(args
					(e-num (value "5")))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "d"))
		(e-nominal (nominal "MyType")
			(e-tag (name "Val")
				(args
					(e-num (value "10")))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "result2"))
		(e-field-access (field "plus")
			(receiver
				(e-lookup-local
					(p-assign (ident "c"))))
			(args
				(e-lookup-local
					(p-assign (ident "d")))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(s-nominal-decl
		(ty-header (name "MyType"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "MyType"))
		(patt (type "Error"))
		(patt (type "MyType"))
		(patt (type "MyType"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "Error"))
		(expr (type "MyType"))
		(expr (type "Error"))
		(expr (type "MyType"))
		(expr (type "MyType"))
		(expr (type "Error"))))
~~~
