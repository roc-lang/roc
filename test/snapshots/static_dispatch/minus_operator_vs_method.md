# META
~~~ini
description=Demonstrates error messages for - operator vs .minus() method on type without minus
type=file:MyType.roc
~~~
# SOURCE
~~~roc
MyType := [Val(U64)].{}

a : MyType
a = MyType.Val(5)

b : MyType
b = MyType.Val(10)

# Using the - operator - should show "Cannot use the - operator" message
result1 : MyType
result1 = a - b

# Using the .minus() method directly - should show generic "does not have a minus method" message
result2 : MyType
result2 = a.minus(b)
~~~
# EXPECTED
MISSING METHOD - minus_operator_vs_method.md:11:11:11:16
- - :0:0:0:0
MISSING METHOD - minus_operator_vs_method.md:15:11:15:21
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**minus_operator_vs_method.md:11:15:11:16:**
```roc
result1 = a - b
```
              ^

It has the type:
    _MyType_

But I expected it to be:
    __size_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**minus_operator_vs_method.md:11:11:11:16:**
```roc
result1 = a - b
```
          ^^^^^

It has the type:
    __size_

But the type annotation says it should have the type:
    _MyType_

**MISSING METHOD**
The value before this **-** operator has the type **MyType**, which has no **minus** method:
**minus_operator_vs_method.md:11:11:11:16:**
```roc
result1 = a - b
```
          ^^^^^


**Hint: **The **-** operator calls a method named **minus** on the value preceding it, passing the value after the operator as the one argument.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**minus_operator_vs_method.md:15:19:15:20:**
```roc
result2 = a.minus(b)
```
                  ^

It has the type:
    _MyType_

But I expected it to be:
    __size_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**minus_operator_vs_method.md:15:11:15:21:**
```roc
result2 = a.minus(b)
```
          ^^^^^^^^^^

It has the type:
    __size_

But the type annotation says it should have the type:
    _MyType_

**MISSING METHOD**
This **minus** method is being called on the type **MyType**, which has no method with that name:
**minus_operator_vs_method.md:15:11:15:21:**
```roc
result2 = a.minus(b)
```
          ^^^^^^^^^^


**Hint: **For this to work, the type would need to have a method named **minus** associated with it in the type's declaration.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
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
			(e-binop (op "-")
				(e-ident (raw "a"))
				(e-ident (raw "b"))))
		(s-type-anno (name "result2")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "result2"))
			(e-field-access
				(e-ident (raw "a"))
				(e-apply
					(e-ident (raw "minus"))
					(e-ident (raw "b")))))))
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
		(e-binop (op "sub")
			(e-lookup-local
				(p-assign (ident "a")))
			(e-lookup-local
				(p-assign (ident "b"))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "result2"))
		(e-dot-access (field "minus")
			(receiver
				(e-lookup-local
					(p-assign (ident "a"))))
			(args
				(e-lookup-local
					(p-assign (ident "b")))))
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
		(patt (type "MyType"))
		(patt (type "MyType"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))
		(expr (type "MyType"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
