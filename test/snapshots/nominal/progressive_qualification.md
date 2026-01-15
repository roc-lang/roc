# META
~~~ini
description=Test progressive qualification of nested associated items per can_plan.md
type=snippet
~~~
# SOURCE
~~~roc
# ============================================================================
# Test progressive qualification per can_plan.md
#
# The plan states:
# - Module scope gets "Foo.Bar.baz" (user-facing fully qualified)
# - Foo's scope gets "Bar.baz" (partially qualified)
# - Bar's scope gets "baz" (unqualified)
# ============================================================================

# --------------------------------------------------------------------------
# Test 1: Two-level nesting - access from each scope level
# --------------------------------------------------------------------------

Outer := [O].{
    Inner := [I].{
        value = 42

        # From Inner's scope: unqualified access
        fromInner = value
    }

    # From Outer's scope: partially qualified access (Inner.value)
    fromOuter = Inner.value
}

# From module scope: fully qualified access (Outer.Inner.value)
fromModule = Outer.Inner.value

# Verify all access patterns work
test1_inner = Outer.Inner.fromInner     # Should be 42
test1_outer = Outer.fromOuter           # Should be 42
test1_module = fromModule               # Should be 42

# --------------------------------------------------------------------------
# Test 2: Three-level nesting - access from each scope level
# --------------------------------------------------------------------------

Level1 := [L1].{
    Level2 := [L2].{
        Level3 := [L3].{
            deepValue = 100

            # From Level3's scope: unqualified
            accessFromL3 = deepValue
        }

        # From Level2's scope: partial qualification (Level3.deepValue)
        accessFromL2 = Level3.deepValue
    }

    # From Level1's scope: more qualification (Level2.Level3.deepValue)
    accessFromL1 = Level2.Level3.deepValue
}

# From module scope: full qualification (Level1.Level2.Level3.deepValue)
accessFromModule = Level1.Level2.Level3.deepValue

# Verify
test2_l3 = Level1.Level2.Level3.accessFromL3    # 100
test2_l2 = Level1.Level2.accessFromL2           # 100
test2_l1 = Level1.accessFromL1                  # 100
test2_mod = accessFromModule                    # 100

# --------------------------------------------------------------------------
# Test 3: Cross-references between siblings at different nesting levels
# --------------------------------------------------------------------------

Parent := [P].{
    sharedVal = 999

    Child1 := [C1].{
        c1Val = 10
        # Access parent's value with partial qualification
        useParent = Parent.sharedVal
    }

    Child2 := [C2].{
        c2Val = 20
        # Access sibling's value - need to go through parent scope
        useSibling = Child1.c1Val
        useParent = Parent.sharedVal
    }

    # Parent can access both children with partial qualification
    sumChildren = Child1.c1Val + Child2.c2Val
}

test3_c1 = Parent.Child1.useParent      # 999
test3_c2_sib = Parent.Child2.useSibling # 10
test3_c2_par = Parent.Child2.useParent  # 999
test3_sum = Parent.sumChildren          # 30

# --------------------------------------------------------------------------
# Test 4: Name reuse in non-overlapping scopes (no shadowing)
# --------------------------------------------------------------------------

TypeA := [TA].{
    helper = 111
    useHelper = helper
}

TypeB := [TB].{
    helper = 222
    useHelper = helper
}

# Each type has its own "helper" - no conflict
test4_a = TypeA.useHelper   # 111
test4_b = TypeB.useHelper   # 222
test4_a_direct = TypeA.helper   # 111
test4_b_direct = TypeB.helper   # 222

# --------------------------------------------------------------------------
# Test 5: Same nested type names in different parents
# --------------------------------------------------------------------------

Parent1 := [P1].{
    Nested := [N1].{
        val = 1
    }
}

Parent2 := [P2].{
    Nested := [N2].{
        val = 2
    }
}

# Both Parent1.Nested and Parent2.Nested exist independently
test5_p1 = Parent1.Nested.val   # 1
test5_p2 = Parent2.Nested.val   # 2

# --------------------------------------------------------------------------
# Test 6: Inner accessing outer's values (scope chain lookup)
# --------------------------------------------------------------------------

Container := [CONT].{
    outerA = 10
    outerB = 20

    Nested := [NEST].{
        innerVal = 5

        # Access outer values - these should be visible via scope chain
        useOuterA = outerA
        useOuterB = outerB
        combined = innerVal + outerA + outerB
    }
}

test6_a = Container.Nested.useOuterA    # 10
test6_b = Container.Nested.useOuterB    # 20
test6_combined = Container.Nested.combined  # 35
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags
					(ty (name "O"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "I"))))
					(associated
						(s-decl
							(p-ident (raw "value"))
							(e-int (raw "42")))
						(s-decl
							(p-ident (raw "fromInner"))
							(e-ident (raw "value")))))
				(s-decl
					(p-ident (raw "fromOuter"))
					(e-ident (raw "Inner.value")))))
		(s-decl
			(p-ident (raw "fromModule"))
			(e-ident (raw "Outer.Inner.value")))
		(s-decl
			(p-ident (raw "test1_inner"))
			(e-ident (raw "Outer.Inner.fromInner")))
		(s-decl
			(p-ident (raw "test1_outer"))
			(e-ident (raw "Outer.fromOuter")))
		(s-decl
			(p-ident (raw "test1_module"))
			(e-ident (raw "fromModule")))
		(s-type-decl
			(header (name "Level1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "L1"))))
			(associated
				(s-type-decl
					(header (name "Level2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "L2"))))
					(associated
						(s-type-decl
							(header (name "Level3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "L3"))))
							(associated
								(s-decl
									(p-ident (raw "deepValue"))
									(e-int (raw "100")))
								(s-decl
									(p-ident (raw "accessFromL3"))
									(e-ident (raw "deepValue")))))
						(s-decl
							(p-ident (raw "accessFromL2"))
							(e-ident (raw "Level3.deepValue")))))
				(s-decl
					(p-ident (raw "accessFromL1"))
					(e-ident (raw "Level2.Level3.deepValue")))))
		(s-decl
			(p-ident (raw "accessFromModule"))
			(e-ident (raw "Level1.Level2.Level3.deepValue")))
		(s-decl
			(p-ident (raw "test2_l3"))
			(e-ident (raw "Level1.Level2.Level3.accessFromL3")))
		(s-decl
			(p-ident (raw "test2_l2"))
			(e-ident (raw "Level1.Level2.accessFromL2")))
		(s-decl
			(p-ident (raw "test2_l1"))
			(e-ident (raw "Level1.accessFromL1")))
		(s-decl
			(p-ident (raw "test2_mod"))
			(e-ident (raw "accessFromModule")))
		(s-type-decl
			(header (name "Parent")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P"))))
			(associated
				(s-decl
					(p-ident (raw "sharedVal"))
					(e-int (raw "999")))
				(s-type-decl
					(header (name "Child1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "C1"))))
					(associated
						(s-decl
							(p-ident (raw "c1Val"))
							(e-int (raw "10")))
						(s-decl
							(p-ident (raw "useParent"))
							(e-ident (raw "Parent.sharedVal")))))
				(s-type-decl
					(header (name "Child2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "C2"))))
					(associated
						(s-decl
							(p-ident (raw "c2Val"))
							(e-int (raw "20")))
						(s-decl
							(p-ident (raw "useSibling"))
							(e-ident (raw "Child1.c1Val")))
						(s-decl
							(p-ident (raw "useParent"))
							(e-ident (raw "Parent.sharedVal")))))
				(s-decl
					(p-ident (raw "sumChildren"))
					(e-binop (op "+")
						(e-ident (raw "Child1.c1Val"))
						(e-ident (raw "Child2.c2Val"))))))
		(s-decl
			(p-ident (raw "test3_c1"))
			(e-ident (raw "Parent.Child1.useParent")))
		(s-decl
			(p-ident (raw "test3_c2_sib"))
			(e-ident (raw "Parent.Child2.useSibling")))
		(s-decl
			(p-ident (raw "test3_c2_par"))
			(e-ident (raw "Parent.Child2.useParent")))
		(s-decl
			(p-ident (raw "test3_sum"))
			(e-ident (raw "Parent.sumChildren")))
		(s-type-decl
			(header (name "TypeA")
				(args))
			(ty-tag-union
				(tags
					(ty (name "TA"))))
			(associated
				(s-decl
					(p-ident (raw "helper"))
					(e-int (raw "111")))
				(s-decl
					(p-ident (raw "useHelper"))
					(e-ident (raw "helper")))))
		(s-type-decl
			(header (name "TypeB")
				(args))
			(ty-tag-union
				(tags
					(ty (name "TB"))))
			(associated
				(s-decl
					(p-ident (raw "helper"))
					(e-int (raw "222")))
				(s-decl
					(p-ident (raw "useHelper"))
					(e-ident (raw "helper")))))
		(s-decl
			(p-ident (raw "test4_a"))
			(e-ident (raw "TypeA.useHelper")))
		(s-decl
			(p-ident (raw "test4_b"))
			(e-ident (raw "TypeB.useHelper")))
		(s-decl
			(p-ident (raw "test4_a_direct"))
			(e-ident (raw "TypeA.helper")))
		(s-decl
			(p-ident (raw "test4_b_direct"))
			(e-ident (raw "TypeB.helper")))
		(s-type-decl
			(header (name "Parent1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P1"))))
			(associated
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "N1"))))
					(associated
						(s-decl
							(p-ident (raw "val"))
							(e-int (raw "1")))))))
		(s-type-decl
			(header (name "Parent2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P2"))))
			(associated
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "N2"))))
					(associated
						(s-decl
							(p-ident (raw "val"))
							(e-int (raw "2")))))))
		(s-decl
			(p-ident (raw "test5_p1"))
			(e-ident (raw "Parent1.Nested.val")))
		(s-decl
			(p-ident (raw "test5_p2"))
			(e-ident (raw "Parent2.Nested.val")))
		(s-type-decl
			(header (name "Container")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CONT"))))
			(associated
				(s-decl
					(p-ident (raw "outerA"))
					(e-int (raw "10")))
				(s-decl
					(p-ident (raw "outerB"))
					(e-int (raw "20")))
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "NEST"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-int (raw "5")))
						(s-decl
							(p-ident (raw "useOuterA"))
							(e-ident (raw "outerA")))
						(s-decl
							(p-ident (raw "useOuterB"))
							(e-ident (raw "outerB")))
						(s-decl
							(p-ident (raw "combined"))
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "innerVal"))
									(e-ident (raw "outerA")))
								(e-ident (raw "outerB"))))))))
		(s-decl
			(p-ident (raw "test6_a"))
			(e-ident (raw "Container.Nested.useOuterA")))
		(s-decl
			(p-ident (raw "test6_b"))
			(e-ident (raw "Container.Nested.useOuterB")))
		(s-decl
			(p-ident (raw "test6_combined"))
			(e-ident (raw "Container.Nested.combined")))))
~~~
# FORMATTED
~~~roc
# ============================================================================
# Test progressive qualification per can_plan.md
#
# The plan states:
# - Module scope gets "Foo.Bar.baz" (user-facing fully qualified)
# - Foo's scope gets "Bar.baz" (partially qualified)
# - Bar's scope gets "baz" (unqualified)
# ============================================================================

# --------------------------------------------------------------------------
# Test 1: Two-level nesting - access from each scope level
# --------------------------------------------------------------------------

Outer := [O].{
	Inner := [I].{
		value = 42

		# From Inner's scope: unqualified access
		fromInner = value
	}

	# From Outer's scope: partially qualified access (Inner.value)
	fromOuter = Inner.value
}

# From module scope: fully qualified access (Outer.Inner.value)
fromModule = Outer.Inner.value

# Verify all access patterns work
test1_inner = Outer.Inner.fromInner # Should be 42
test1_outer = Outer.fromOuter # Should be 42
test1_module = fromModule # Should be 42

# --------------------------------------------------------------------------
# Test 2: Three-level nesting - access from each scope level
# --------------------------------------------------------------------------

Level1 := [L1].{
	Level2 := [L2].{
		Level3 := [L3].{
			deepValue = 100

			# From Level3's scope: unqualified
			accessFromL3 = deepValue
		}

		# From Level2's scope: partial qualification (Level3.deepValue)
		accessFromL2 = Level3.deepValue
	}

	# From Level1's scope: more qualification (Level2.Level3.deepValue)
	accessFromL1 = Level2.Level3.deepValue
}

# From module scope: full qualification (Level1.Level2.Level3.deepValue)
accessFromModule = Level1.Level2.Level3.deepValue

# Verify
test2_l3 = Level1.Level2.Level3.accessFromL3 # 100
test2_l2 = Level1.Level2.accessFromL2 # 100
test2_l1 = Level1.accessFromL1 # 100
test2_mod = accessFromModule # 100

# --------------------------------------------------------------------------
# Test 3: Cross-references between siblings at different nesting levels
# --------------------------------------------------------------------------

Parent := [P].{
	sharedVal = 999

	Child1 := [C1].{
		c1Val = 10
		# Access parent's value with partial qualification
		useParent = Parent.sharedVal
	}

	Child2 := [C2].{
		c2Val = 20
		# Access sibling's value - need to go through parent scope
		useSibling = Child1.c1Val
		useParent = Parent.sharedVal
	}

	# Parent can access both children with partial qualification
	sumChildren = Child1.c1Val + Child2.c2Val
}

test3_c1 = Parent.Child1.useParent # 999
test3_c2_sib = Parent.Child2.useSibling # 10
test3_c2_par = Parent.Child2.useParent # 999
test3_sum = Parent.sumChildren # 30

# --------------------------------------------------------------------------
# Test 4: Name reuse in non-overlapping scopes (no shadowing)
# --------------------------------------------------------------------------

TypeA := [TA].{
	helper = 111
	useHelper = helper
}

TypeB := [TB].{
	helper = 222
	useHelper = helper
}

# Each type has its own "helper" - no conflict
test4_a = TypeA.useHelper # 111
test4_b = TypeB.useHelper # 222
test4_a_direct = TypeA.helper # 111
test4_b_direct = TypeB.helper # 222

# --------------------------------------------------------------------------
# Test 5: Same nested type names in different parents
# --------------------------------------------------------------------------

Parent1 := [P1].{
	Nested := [N1].{
		val = 1
	}
}

Parent2 := [P2].{
	Nested := [N2].{
		val = 2
	}
}

# Both Parent1.Nested and Parent2.Nested exist independently
test5_p1 = Parent1.Nested.val # 1
test5_p2 = Parent2.Nested.val # 2

# --------------------------------------------------------------------------
# Test 6: Inner accessing outer's values (scope chain lookup)
# --------------------------------------------------------------------------

Container := [CONT].{
	outerA = 10
	outerB = 20

	Nested := [NEST].{
		innerVal = 5

		# Access outer values - these should be visible via scope chain
		useOuterA = outerA
		useOuterB = outerB
		combined = innerVal + outerA + outerB
	}
}

test6_a = Container.Nested.useOuterA # 10
test6_b = Container.Nested.useOuterB # 20
test6_combined = Container.Nested.combined # 35
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "progressive_qualification.Outer.Inner.value"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "progressive_qualification.Outer.Inner.fromInner"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Outer.Inner.value"))))
	(d-let
		(p-assign (ident "progressive_qualification.Outer.fromOuter"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Outer.Inner.value"))))
	(d-let
		(p-assign (ident "progressive_qualification.Level1.Level2.Level3.deepValue"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "progressive_qualification.Level1.Level2.Level3.accessFromL3"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.Level3.deepValue"))))
	(d-let
		(p-assign (ident "progressive_qualification.Level1.Level2.accessFromL2"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.Level3.deepValue"))))
	(d-let
		(p-assign (ident "progressive_qualification.Level1.accessFromL1"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.Level3.deepValue"))))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.Child1.c1Val"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.Child1.useParent"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.sharedVal"))))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.Child2.c2Val"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.Child2.useSibling"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.Child1.c1Val"))))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.Child2.useParent"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.sharedVal"))))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.sharedVal"))
		(e-num (value "999")))
	(d-let
		(p-assign (ident "progressive_qualification.Parent.sumChildren"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "progressive_qualification.Parent.Child1.c1Val")))
			(e-lookup-local
				(p-assign (ident "progressive_qualification.Parent.Child2.c2Val")))))
	(d-let
		(p-assign (ident "progressive_qualification.TypeA.helper"))
		(e-num (value "111")))
	(d-let
		(p-assign (ident "progressive_qualification.TypeA.useHelper"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeA.helper"))))
	(d-let
		(p-assign (ident "progressive_qualification.TypeB.helper"))
		(e-num (value "222")))
	(d-let
		(p-assign (ident "progressive_qualification.TypeB.useHelper"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeB.helper"))))
	(d-let
		(p-assign (ident "progressive_qualification.Parent1.Nested.val"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "progressive_qualification.Parent2.Nested.val"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "progressive_qualification.Container.Nested.innerVal"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "progressive_qualification.Container.Nested.useOuterA"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Container.outerA"))))
	(d-let
		(p-assign (ident "progressive_qualification.Container.Nested.useOuterB"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Container.outerB"))))
	(d-let
		(p-assign (ident "progressive_qualification.Container.Nested.combined"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "progressive_qualification.Container.Nested.innerVal")))
				(e-lookup-local
					(p-assign (ident "progressive_qualification.Container.outerA"))))
			(e-lookup-local
				(p-assign (ident "progressive_qualification.Container.outerB")))))
	(d-let
		(p-assign (ident "progressive_qualification.Container.outerA"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "progressive_qualification.Container.outerB"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "fromModule"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Outer.Inner.value"))))
	(d-let
		(p-assign (ident "test1_inner"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Outer.Inner.fromInner"))))
	(d-let
		(p-assign (ident "test1_outer"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Outer.fromOuter"))))
	(d-let
		(p-assign (ident "test1_module"))
		(e-lookup-local
			(p-assign (ident "fromModule"))))
	(d-let
		(p-assign (ident "accessFromModule"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.Level3.deepValue"))))
	(d-let
		(p-assign (ident "test2_l3"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.Level3.accessFromL3"))))
	(d-let
		(p-assign (ident "test2_l2"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.Level2.accessFromL2"))))
	(d-let
		(p-assign (ident "test2_l1"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Level1.accessFromL1"))))
	(d-let
		(p-assign (ident "test2_mod"))
		(e-lookup-local
			(p-assign (ident "accessFromModule"))))
	(d-let
		(p-assign (ident "test3_c1"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.Child1.useParent"))))
	(d-let
		(p-assign (ident "test3_c2_sib"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.Child2.useSibling"))))
	(d-let
		(p-assign (ident "test3_c2_par"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.Child2.useParent"))))
	(d-let
		(p-assign (ident "test3_sum"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent.sumChildren"))))
	(d-let
		(p-assign (ident "test4_a"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeA.useHelper"))))
	(d-let
		(p-assign (ident "test4_b"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeB.useHelper"))))
	(d-let
		(p-assign (ident "test4_a_direct"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeA.helper"))))
	(d-let
		(p-assign (ident "test4_b_direct"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.TypeB.helper"))))
	(d-let
		(p-assign (ident "test5_p1"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent1.Nested.val"))))
	(d-let
		(p-assign (ident "test5_p2"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Parent2.Nested.val"))))
	(d-let
		(p-assign (ident "test6_a"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Container.Nested.useOuterA"))))
	(d-let
		(p-assign (ident "test6_b"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Container.Nested.useOuterB"))))
	(d-let
		(p-assign (ident "test6_combined"))
		(e-lookup-local
			(p-assign (ident "progressive_qualification.Container.Nested.combined"))))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union
			(ty-tag-name (name "O"))))
	(s-nominal-decl
		(ty-header (name "Level1"))
		(ty-tag-union
			(ty-tag-name (name "L1"))))
	(s-nominal-decl
		(ty-header (name "Parent"))
		(ty-tag-union
			(ty-tag-name (name "P"))))
	(s-nominal-decl
		(ty-header (name "TypeA"))
		(ty-tag-union
			(ty-tag-name (name "TA"))))
	(s-nominal-decl
		(ty-header (name "TypeB"))
		(ty-tag-union
			(ty-tag-name (name "TB"))))
	(s-nominal-decl
		(ty-header (name "Parent1"))
		(ty-tag-union
			(ty-tag-name (name "P1"))))
	(s-nominal-decl
		(ty-header (name "Parent2"))
		(ty-tag-union
			(ty-tag-name (name "P2"))))
	(s-nominal-decl
		(ty-header (name "Container"))
		(ty-tag-union
			(ty-tag-name (name "CONT"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Outer.Inner"))
		(ty-tag-union
			(ty-tag-name (name "I"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Level1.Level2"))
		(ty-tag-union
			(ty-tag-name (name "L2"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Level1.Level2.Level3"))
		(ty-tag-union
			(ty-tag-name (name "L3"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Parent.Child1"))
		(ty-tag-union
			(ty-tag-name (name "C1"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Parent.Child2"))
		(ty-tag-union
			(ty-tag-name (name "C2"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Parent1.Nested"))
		(ty-tag-union
			(ty-tag-name (name "N1"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Parent2.Nested"))
		(ty-tag-union
			(ty-tag-name (name "N2"))))
	(s-nominal-decl
		(ty-header (name "progressive_qualification.Container.Nested"))
		(ty-tag-union
			(ty-tag-name (name "NEST")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Level1")
			(ty-header (name "Level1")))
		(nominal (type "Parent")
			(ty-header (name "Parent")))
		(nominal (type "TypeA")
			(ty-header (name "TypeA")))
		(nominal (type "TypeB")
			(ty-header (name "TypeB")))
		(nominal (type "Parent1")
			(ty-header (name "Parent1")))
		(nominal (type "Parent2")
			(ty-header (name "Parent2")))
		(nominal (type "Container")
			(ty-header (name "Container")))
		(nominal (type "Outer.Inner")
			(ty-header (name "progressive_qualification.Outer.Inner")))
		(nominal (type "Level1.Level2")
			(ty-header (name "progressive_qualification.Level1.Level2")))
		(nominal (type "Level1.Level2.Level3")
			(ty-header (name "progressive_qualification.Level1.Level2.Level3")))
		(nominal (type "Parent.Child1")
			(ty-header (name "progressive_qualification.Parent.Child1")))
		(nominal (type "Parent.Child2")
			(ty-header (name "progressive_qualification.Parent.Child2")))
		(nominal (type "Parent1.Nested")
			(ty-header (name "progressive_qualification.Parent1.Nested")))
		(nominal (type "Parent2.Nested")
			(ty-header (name "progressive_qualification.Parent2.Nested")))
		(nominal (type "Container.Nested")
			(ty-header (name "progressive_qualification.Container.Nested"))))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
