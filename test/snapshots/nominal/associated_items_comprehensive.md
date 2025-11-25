# META
~~~ini
description=Comprehensive test of associated items: all depths, forward refs, scoping rules
type=snippet
~~~
# SOURCE
~~~roc
# ============================================================================
# DEPTH 1: Basic associated items
# ============================================================================

# Test 1.1: Simple associated value
Simple := [A].{
    value = 1
}
simple1 = Simple.value  # Should work

# Test 1.2: Forward reference within associated block
Forward := [B].{
    first = second      # Forward reference - should work
    second = 2
}
forward1 = Forward.first   # Should be 2
forward2 = Forward.second  # Should be 2

# Test 1.3: Multiple items referencing each other
Multi := [C].{
    a = 10
    b = a + 5          # Reference to sibling
    c = b * 2          # Reference to another sibling
}
multi1 = Multi.a  # 10
multi2 = Multi.b  # 15
multi3 = Multi.c  # 30

# Test 1.4: External references cannot see unqualified names
# (We test this by ensuring "value" alone doesn't resolve to Simple.value)
externalTest1 = Simple.value  # Must use qualified name

# ============================================================================
# DEPTH 2: One level of nesting
# ============================================================================

# Test 2.1: Nested type with associated items
Outer1 := [D].{
    outerVal = 100

    Inner1 := [E].{
        innerVal = 200
    }
}
depth2_1 = Outer1.outerVal              # 100
depth2_2 = Outer1.Inner1.innerVal       # 200

# Test 2.2: Nested items referencing outer items
Outer2 := [F].{
    shared = 50

    Inner2 := [G].{
        usesOuter = Outer2.shared       # Can reference outer scope
        doubled = usesOuter * 2
    }
}
depth2_3 = Outer2.shared                # 50
depth2_4 = Outer2.Inner2.usesOuter      # 50
depth2_5 = Outer2.Inner2.doubled        # 100

# Test 2.3: Forward references at depth 2
Outer3 := [H].{
    first = Outer3.Inner3.nested        # Forward ref to nested item

    Inner3 := [I].{
        nested = 42
    }
}
depth2_6 = Outer3.first                 # 42
depth2_7 = Outer3.Inner3.nested         # 42

# Test 2.4: Multiple nested types at same level
Outer4 := [J].{
    val = 1

    InnerA := [K].{
        valA = 2
    }

    InnerB := [L].{
        valB = Outer4.InnerA.valA + 1   # Reference sibling nested type
    }
}
depth2_8 = Outer4.val                   # 1
depth2_9 = Outer4.InnerA.valA           # 2
depth2_10 = Outer4.InnerB.valB          # 3

# ============================================================================
# DEPTH 3: Two levels of nesting
# ============================================================================

# Test 3.1: Three-level hierarchy
Level1 := [M].{
    val1 = 10

    Level2 := [N].{
        val2 = 20

        Level3 := [O].{
            val3 = 30
        }
    }
}
depth3_1 = Level1.val1                          # 10
depth3_2 = Level1.Level2.val2                   # 20
depth3_3 = Level1.Level2.Level3.val3            # 30

# Test 3.2: Cross-level references
CrossRef := [P].{
    top = 5

    Mid := [Q].{
        middle = CrossRef.top * 2               # Ref to grandparent

        Deep := [R].{
            deep = CrossRef.Mid.middle + CrossRef.top  # Ref to parent and grandparent
        }
    }
}
depth3_4 = CrossRef.top                         # 5
depth3_5 = CrossRef.Mid.middle                  # 10
depth3_6 = CrossRef.Mid.Deep.deep               # 15

# Test 3.3: Forward references spanning multiple levels
ForwardDeep := [S].{
    usesDeepNested = ForwardDeep.M1.M2.deepVal  # Forward ref through 2 levels

    M1 := [T].{
        M2 := [U].{
            deepVal = 99
        }
    }
}
depth3_7 = ForwardDeep.usesDeepNested           # 99
depth3_8 = ForwardDeep.M1.M2.deepVal            # 99

# ============================================================================
# DEPTH 4: Three levels of nesting
# ============================================================================

# Test 4.1: Four-level hierarchy with values at each level
D1 := [V].{
    v1 = 1000

    D2 := [W].{
        v2 = 2000

        D3 := [X].{
            v3 = 3000

            D4 := [Y].{
                v4 = 4000
            }
        }
    }
}
depth4_1 = D1.v1                    # 1000
depth4_2 = D1.D2.v2                 # 2000
depth4_3 = D1.D2.D3.v3              # 3000
depth4_4 = D1.D2.D3.D4.v4           # 4000

# Test 4.2: Deep forward reference
DeepForward := [Z].{
    usesDeeply = DeepForward.A.B.C.deepest

    A := [AA].{
        B := [BB].{
            C := [CC].{
                deepest = 777
            }
        }
    }
}
depth4_5 = DeepForward.usesDeeply           # 777

# ============================================================================
# DEPTH 5: Four levels of nesting (maximum test depth)
# ============================================================================

# Test 5.1: Five-level hierarchy
Max1 := [DD].{
    Max2 := [EE].{
        Max3 := [FF].{
            Max4 := [GG].{
                Max5 := [HH].{
                    deepestValue = 5555
                }
            }
        }
    }
}
depth5_1 = Max1.Max2.Max3.Max4.Max5.deepestValue    # 5555

# Test 5.2: Values at each level of 5-deep hierarchy
Full := [II].{
    val1 = 1

    L2 := [JJ].{
        val2 = Full.val1 + 1

        L3 := [KK].{
            val3 = Full.L2.val2 + 1

            L4 := [LL].{
                val4 = Full.L2.L3.val3 + 1

                L5 := [MM].{
                    val5 = Full.L2.L3.L4.val4 + 1
                }
            }
        }
    }
}
depth5_2 = Full.val1                        # 1
depth5_3 = Full.L2.val2                     # 2
depth5_4 = Full.L2.L3.val3                  # 3
depth5_5 = Full.L2.L3.L4.val4               # 4
depth5_6 = Full.L2.L3.L4.L5.val5            # 5

# ============================================================================
# SCOPING EDGE CASES
# ============================================================================

# Test: Unqualified names only work within associated block
ScopeTest := [NN].{
    innerOnly = 888

    # Within this block, "innerOnly" works unqualified
    canUse = innerOnly      # This works
}
# Outside the block, must use qualified name
scopeOuter = ScopeTest.innerOnly    # Must qualify
scopeAlias = ScopeTest.canUse       # 888

# Test: Nested block can access outer unqualified, but outer cannot access inner
ScopeNested := [OO].{
    outer = 111

    Nested := [PP].{
        # Can use "outer" unqualified because we're inside ScopeNested's block
        usesOuter = outer
        inner = 222
    }

    # Cannot use "inner" here - would need ScopeNested.Nested.inner
    usesNested = ScopeNested.Nested.inner
}

# ============================================================================
# TYPE ANNOTATIONS WITH ASSOCIATED ITEMS
# ============================================================================

# Test: Type annotations on associated items
Annotated := [QQ].{
    typed : U64
    typed = 999

    NestedAnnotated := [RR].{
        alsoTyped : U64
        alsoTyped = 111
    }
}
annoTest1 = Annotated.typed                         # 999
annoTest2 = Annotated.NestedAnnotated.alsoTyped     # 111

# ============================================================================
# COMPLEX DEPENDENCY CHAINS
# ============================================================================

# Test: Complex dependency chains across multiple nesting levels
Chain := [SS].{
    start = 1

    Mid := [TT].{
        step1 = Chain.start * 2

        Deep := [UU].{
            step2 = Chain.Mid.step1 * 3

            Deeper := [VV].{
                step3 = Chain.Mid.Deep.step2 * 4

                Deepest := [WW].{
                    final = Chain.Mid.Deep.Deeper.step3 * 5
                }
            }
        }
    }
}
chain1 = Chain.start                                    # 1
chain2 = Chain.Mid.step1                                # 2
chain3 = Chain.Mid.Deep.step2                           # 6
chain4 = Chain.Mid.Deep.Deeper.step3                    # 24
chain5 = Chain.Mid.Deep.Deeper.Deepest.final            # 120

# ============================================================================
# MULTIPLE FORWARD REFERENCES
# ============================================================================

# Test: Multiple forward references in circular dependency
Circular := [XX].{
    # All of these reference things defined later
    sum = Circular.a + Circular.b + Circular.c

    a = 10
    b = 20
    c = 30
}
circTest = Circular.sum     # 60

# ============================================================================
# MIXED SCENARIOS
# ============================================================================

# Test: Everything at once - deep nesting, forward refs, cross-level refs, annotations
Ultimate := [YY].{
    base : U64
    base = 100

    Branch1 := [ZZ].{
        b1val = Ultimate.base + Ultimate.Branch2.b2forward

        Branch1Inner := [AAA].{
            innerSum : U64
            innerSum = Ultimate.base + Ultimate.Branch1.b1val
        }
    }

    Branch2 := [BBB].{
        b2forward : U64
        b2forward = 50

        Branch2Inner := [CCC].{
            usesEverything = Ultimate.base + Ultimate.Branch1.b1val + Ultimate.Branch2.b2forward + Ultimate.Branch1.Branch1Inner.innerSum
        }
    }
}
ultimate1 = Ultimate.base                                       # 100
ultimate2 = Ultimate.Branch1.b1val                              # 150
ultimate3 = Ultimate.Branch2.b2forward                          # 50
ultimate4 = Ultimate.Branch1.Branch1Inner.innerSum              # 250
ultimate5 = Ultimate.Branch2.Branch2Inner.usesEverything        # 550

# ============================================================================
# INVALID LOOKUPS - These MUST produce errors
# ============================================================================

# Error 1: Module-level trying to access associated item unqualified
# "value" is only defined inside Simple's associated block
errModuleUnqualified = value  # ERROR: 'value' not in scope at module level

# Error 2: Outer scope trying to access inner scope item unqualified
ErrOuterAccessInner := [ERR1].{
    outerItem = 10

    InnerScope := [ERR2].{
        innerItem = 20
    }

    # This MUST fail - innerItem is only in InnerScope's block
    badAccess = innerItem  # ERROR: 'innerItem' not in scope here
}

# Error 3: Sibling nested types cannot access each other's items unqualified
ErrSiblingAccess := [ERR3].{
    SiblingA := [ERR4].{
        sibAVal = 100
    }

    SiblingB := [ERR5].{
        # Cannot access sibAVal unqualified - it's in SiblingA's scope, not here
        badSiblingAccess = sibAVal  # ERROR: 'sibAVal' not in scope
    }
}

# Error 4: Deeply nested trying to access cousin's items unqualified
ErrCousinAccess := [ERR6].{
    Branch1 := [ERR7].{
        Leaf1 := [ERR8].{
            leaf1Val = 1
        }
    }

    Branch2 := [ERR9].{
        Leaf2 := [ERR10].{
            # Cannot access leaf1Val unqualified - it's in a different branch
            badCousinAccess = leaf1Val  # ERROR: 'leaf1Val' not in scope
        }
    }
}

# Error 5: Parent trying to access grandchild's items unqualified
ErrGrandchildAccess := [ERR11].{
    Child := [ERR12].{
        Grandchild := [ERR13].{
            grandchildVal = 999
        }
    }

    # Cannot access grandchildVal unqualified - need Child.Grandchild.grandchildVal
    badGrandchildAccess = grandchildVal  # ERROR: 'grandchildVal' not in scope
}

# Error 6: Three levels deep - inner trying to access outer's sibling
ErrDeepSiblingAccess := [ERR14].{
    outerSibling = 50

    Level1 := [ERR15].{
        Level2 := [ERR16].{
            Level3 := [ERR17].{
                # This works - outerSibling is in an ancestor scope
                goodAccess = outerSibling  # OK - ancestor scope
            }
        }

        OtherBranch := [ERR18].{
            otherVal = 77
        }
    }

    Level1Alt := [ERR19].{
        # Cannot access otherVal - it's in Level1.OtherBranch, not an ancestor
        badDeepAccess = otherVal  # ERROR: 'otherVal' not in scope
    }
}

# Error 7: Module level trying various unqualified accesses
errTryOuter = outerItem      # ERROR: not in scope
errTrySibA = sibAVal         # ERROR: not in scope
errTryLeaf = leaf1Val        # ERROR: not in scope
errTryGrand = grandchildVal  # ERROR: not in scope
~~~
# EXPECTED
UNDEFINED VARIABLE - associated_items_comprehensive.md:361:17:361:26
UNUSED VARIABLE - associated_items_comprehensive.md:361:17:361:26
UNDEFINED VARIABLE - associated_items_comprehensive.md:372:28:372:35
UNUSED VARIABLE - associated_items_comprehensive.md:372:28:372:35
UNDEFINED VARIABLE - associated_items_comprehensive.md:387:31:387:39
UNUSED VARIABLE - associated_items_comprehensive.md:387:31:387:39
UNDEFINED VARIABLE - associated_items_comprehensive.md:401:27:401:40
UNUSED VARIABLE - associated_items_comprehensive.md:401:27:401:40
UNDEFINED VARIABLE - associated_items_comprehensive.md:423:25:423:33
UNUSED VARIABLE - associated_items_comprehensive.md:423:25:423:33
UNDEFINED VARIABLE - associated_items_comprehensive.md:350:24:350:29
UNDEFINED VARIABLE - associated_items_comprehensive.md:428:15:428:24
UNDEFINED VARIABLE - associated_items_comprehensive.md:429:14:429:21
UNDEFINED VARIABLE - associated_items_comprehensive.md:430:14:430:22
UNDEFINED VARIABLE - associated_items_comprehensive.md:431:15:431:28
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `innerItem` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:361:17:361:26:**
```roc
    badAccess = innerItem  # ERROR: 'innerItem' not in scope here
```
                ^^^^^^^^^


**UNUSED VARIABLE**
Variable `innerItem` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_innerItem` to suppress this warning.
The unused variable is declared here:
**associated_items_comprehensive.md:361:17:361:26:**
```roc
    badAccess = innerItem  # ERROR: 'innerItem' not in scope here
```
                ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `sibAVal` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:372:28:372:35:**
```roc
        badSiblingAccess = sibAVal  # ERROR: 'sibAVal' not in scope
```
                           ^^^^^^^


**UNUSED VARIABLE**
Variable `sibAVal` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_sibAVal` to suppress this warning.
The unused variable is declared here:
**associated_items_comprehensive.md:372:28:372:35:**
```roc
        badSiblingAccess = sibAVal  # ERROR: 'sibAVal' not in scope
```
                           ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `leaf1Val` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:387:31:387:39:**
```roc
            badCousinAccess = leaf1Val  # ERROR: 'leaf1Val' not in scope
```
                              ^^^^^^^^


**UNUSED VARIABLE**
Variable `leaf1Val` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_leaf1Val` to suppress this warning.
The unused variable is declared here:
**associated_items_comprehensive.md:387:31:387:39:**
```roc
            badCousinAccess = leaf1Val  # ERROR: 'leaf1Val' not in scope
```
                              ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `grandchildVal` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:401:27:401:40:**
```roc
    badGrandchildAccess = grandchildVal  # ERROR: 'grandchildVal' not in scope
```
                          ^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `grandchildVal` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_grandchildVal` to suppress this warning.
The unused variable is declared here:
**associated_items_comprehensive.md:401:27:401:40:**
```roc
    badGrandchildAccess = grandchildVal  # ERROR: 'grandchildVal' not in scope
```
                          ^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `otherVal` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:423:25:423:33:**
```roc
        badDeepAccess = otherVal  # ERROR: 'otherVal' not in scope
```
                        ^^^^^^^^


**UNUSED VARIABLE**
Variable `otherVal` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_otherVal` to suppress this warning.
The unused variable is declared here:
**associated_items_comprehensive.md:423:25:423:33:**
```roc
        badDeepAccess = otherVal  # ERROR: 'otherVal' not in scope
```
                        ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:350:24:350:29:**
```roc
errModuleUnqualified = value  # ERROR: 'value' not in scope at module level
```
                       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `outerItem` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:428:15:428:24:**
```roc
errTryOuter = outerItem      # ERROR: not in scope
```
              ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `sibAVal` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:429:14:429:21:**
```roc
errTrySibA = sibAVal         # ERROR: not in scope
```
             ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `leaf1Val` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:430:14:430:22:**
```roc
errTryLeaf = leaf1Val        # ERROR: not in scope
```
             ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `grandchildVal` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_comprehensive.md:431:15:431:28:**
```roc
errTryGrand = grandchildVal  # ERROR: not in scope
```
              ^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Simple")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-decl
					(p-ident (raw "value"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "simple1"))
			(e-ident (raw "Simple.value")))
		(s-type-decl
			(header (name "Forward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "B"))))
			(associated
				(s-decl
					(p-ident (raw "first"))
					(e-ident (raw "second")))
				(s-decl
					(p-ident (raw "second"))
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "forward1"))
			(e-ident (raw "Forward.first")))
		(s-decl
			(p-ident (raw "forward2"))
			(e-ident (raw "Forward.second")))
		(s-type-decl
			(header (name "Multi")
				(args))
			(ty-tag-union
				(tags
					(ty (name "C"))))
			(associated
				(s-decl
					(p-ident (raw "a"))
					(e-int (raw "10")))
				(s-decl
					(p-ident (raw "b"))
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-int (raw "5"))))
				(s-decl
					(p-ident (raw "c"))
					(e-binop (op "*")
						(e-ident (raw "b"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "multi1"))
			(e-ident (raw "Multi.a")))
		(s-decl
			(p-ident (raw "multi2"))
			(e-ident (raw "Multi.b")))
		(s-decl
			(p-ident (raw "multi3"))
			(e-ident (raw "Multi.c")))
		(s-decl
			(p-ident (raw "externalTest1"))
			(e-ident (raw "Simple.value")))
		(s-type-decl
			(header (name "Outer1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "D"))))
			(associated
				(s-decl
					(p-ident (raw "outerVal"))
					(e-int (raw "100")))
				(s-type-decl
					(header (name "Inner1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "E"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-int (raw "200")))))))
		(s-decl
			(p-ident (raw "depth2_1"))
			(e-ident (raw "Outer1.outerVal")))
		(s-decl
			(p-ident (raw "depth2_2"))
			(e-ident (raw "Outer1.Inner1.innerVal")))
		(s-type-decl
			(header (name "Outer2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "F"))))
			(associated
				(s-decl
					(p-ident (raw "shared"))
					(e-int (raw "50")))
				(s-type-decl
					(header (name "Inner2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "G"))))
					(associated
						(s-decl
							(p-ident (raw "usesOuter"))
							(e-ident (raw "Outer2.shared")))
						(s-decl
							(p-ident (raw "doubled"))
							(e-binop (op "*")
								(e-ident (raw "usesOuter"))
								(e-int (raw "2"))))))))
		(s-decl
			(p-ident (raw "depth2_3"))
			(e-ident (raw "Outer2.shared")))
		(s-decl
			(p-ident (raw "depth2_4"))
			(e-ident (raw "Outer2.Inner2.usesOuter")))
		(s-decl
			(p-ident (raw "depth2_5"))
			(e-ident (raw "Outer2.Inner2.doubled")))
		(s-type-decl
			(header (name "Outer3")
				(args))
			(ty-tag-union
				(tags
					(ty (name "H"))))
			(associated
				(s-decl
					(p-ident (raw "first"))
					(e-ident (raw "Outer3.Inner3.nested")))
				(s-type-decl
					(header (name "Inner3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "I"))))
					(associated
						(s-decl
							(p-ident (raw "nested"))
							(e-int (raw "42")))))))
		(s-decl
			(p-ident (raw "depth2_6"))
			(e-ident (raw "Outer3.first")))
		(s-decl
			(p-ident (raw "depth2_7"))
			(e-ident (raw "Outer3.Inner3.nested")))
		(s-type-decl
			(header (name "Outer4")
				(args))
			(ty-tag-union
				(tags
					(ty (name "J"))))
			(associated
				(s-decl
					(p-ident (raw "val"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "InnerA")
						(args))
					(ty-tag-union
						(tags
							(ty (name "K"))))
					(associated
						(s-decl
							(p-ident (raw "valA"))
							(e-int (raw "2")))))
				(s-type-decl
					(header (name "InnerB")
						(args))
					(ty-tag-union
						(tags
							(ty (name "L"))))
					(associated
						(s-decl
							(p-ident (raw "valB"))
							(e-binop (op "+")
								(e-ident (raw "Outer4.InnerA.valA"))
								(e-int (raw "1"))))))))
		(s-decl
			(p-ident (raw "depth2_8"))
			(e-ident (raw "Outer4.val")))
		(s-decl
			(p-ident (raw "depth2_9"))
			(e-ident (raw "Outer4.InnerA.valA")))
		(s-decl
			(p-ident (raw "depth2_10"))
			(e-ident (raw "Outer4.InnerB.valB")))
		(s-type-decl
			(header (name "Level1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "M"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "10")))
				(s-type-decl
					(header (name "Level2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "N"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "20")))
						(s-type-decl
							(header (name "Level3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "O"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-int (raw "30")))))))))
		(s-decl
			(p-ident (raw "depth3_1"))
			(e-ident (raw "Level1.val1")))
		(s-decl
			(p-ident (raw "depth3_2"))
			(e-ident (raw "Level1.Level2.val2")))
		(s-decl
			(p-ident (raw "depth3_3"))
			(e-ident (raw "Level1.Level2.Level3.val3")))
		(s-type-decl
			(header (name "CrossRef")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P"))))
			(associated
				(s-decl
					(p-ident (raw "top"))
					(e-int (raw "5")))
				(s-type-decl
					(header (name "Mid")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Q"))))
					(associated
						(s-decl
							(p-ident (raw "middle"))
							(e-binop (op "*")
								(e-ident (raw "CrossRef.top"))
								(e-int (raw "2"))))
						(s-type-decl
							(header (name "Deep")
								(args))
							(ty-tag-union
								(tags
									(ty (name "R"))))
							(associated
								(s-decl
									(p-ident (raw "deep"))
									(e-binop (op "+")
										(e-ident (raw "CrossRef.Mid.middle"))
										(e-ident (raw "CrossRef.top"))))))))))
		(s-decl
			(p-ident (raw "depth3_4"))
			(e-ident (raw "CrossRef.top")))
		(s-decl
			(p-ident (raw "depth3_5"))
			(e-ident (raw "CrossRef.Mid.middle")))
		(s-decl
			(p-ident (raw "depth3_6"))
			(e-ident (raw "CrossRef.Mid.Deep.deep")))
		(s-type-decl
			(header (name "ForwardDeep")
				(args))
			(ty-tag-union
				(tags
					(ty (name "S"))))
			(associated
				(s-decl
					(p-ident (raw "usesDeepNested"))
					(e-ident (raw "ForwardDeep.M1.M2.deepVal")))
				(s-type-decl
					(header (name "M1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "T"))))
					(associated
						(s-type-decl
							(header (name "M2")
								(args))
							(ty-tag-union
								(tags
									(ty (name "U"))))
							(associated
								(s-decl
									(p-ident (raw "deepVal"))
									(e-int (raw "99")))))))))
		(s-decl
			(p-ident (raw "depth3_7"))
			(e-ident (raw "ForwardDeep.usesDeepNested")))
		(s-decl
			(p-ident (raw "depth3_8"))
			(e-ident (raw "ForwardDeep.M1.M2.deepVal")))
		(s-type-decl
			(header (name "D1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "V"))))
			(associated
				(s-decl
					(p-ident (raw "v1"))
					(e-int (raw "1000")))
				(s-type-decl
					(header (name "D2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "W"))))
					(associated
						(s-decl
							(p-ident (raw "v2"))
							(e-int (raw "2000")))
						(s-type-decl
							(header (name "D3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "X"))))
							(associated
								(s-decl
									(p-ident (raw "v3"))
									(e-int (raw "3000")))
								(s-type-decl
									(header (name "D4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "Y"))))
									(associated
										(s-decl
											(p-ident (raw "v4"))
											(e-int (raw "4000")))))))))))
		(s-decl
			(p-ident (raw "depth4_1"))
			(e-ident (raw "D1.v1")))
		(s-decl
			(p-ident (raw "depth4_2"))
			(e-ident (raw "D1.D2.v2")))
		(s-decl
			(p-ident (raw "depth4_3"))
			(e-ident (raw "D1.D2.D3.v3")))
		(s-decl
			(p-ident (raw "depth4_4"))
			(e-ident (raw "D1.D2.D3.D4.v4")))
		(s-type-decl
			(header (name "DeepForward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Z"))))
			(associated
				(s-decl
					(p-ident (raw "usesDeeply"))
					(e-ident (raw "DeepForward.A.B.C.deepest")))
				(s-type-decl
					(header (name "A")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AA"))))
					(associated
						(s-type-decl
							(header (name "B")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BB"))))
							(associated
								(s-type-decl
									(header (name "C")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CC"))))
									(associated
										(s-decl
											(p-ident (raw "deepest"))
											(e-int (raw "777")))))))))))
		(s-decl
			(p-ident (raw "depth4_5"))
			(e-ident (raw "DeepForward.usesDeeply")))
		(s-type-decl
			(header (name "Max1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DD"))))
			(associated
				(s-type-decl
					(header (name "Max2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "EE"))))
					(associated
						(s-type-decl
							(header (name "Max3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "FF"))))
							(associated
								(s-type-decl
									(header (name "Max4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "GG"))))
									(associated
										(s-type-decl
											(header (name "Max5")
												(args))
											(ty-tag-union
												(tags
													(ty (name "HH"))))
											(associated
												(s-decl
													(p-ident (raw "deepestValue"))
													(e-int (raw "5555")))))))))))))
		(s-decl
			(p-ident (raw "depth5_1"))
			(e-ident (raw "Max1.Max2.Max3.Max4.Max5.deepestValue")))
		(s-type-decl
			(header (name "Full")
				(args))
			(ty-tag-union
				(tags
					(ty (name "II"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "JJ"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "+")
								(e-ident (raw "Full.val1"))
								(e-int (raw "1"))))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "KK"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "Full.L2.val2"))
										(e-int (raw "1"))))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "LL"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "+")
												(e-ident (raw "Full.L2.L3.val3"))
												(e-int (raw "1"))))
										(s-type-decl
											(header (name "L5")
												(args))
											(ty-tag-union
												(tags
													(ty (name "MM"))))
											(associated
												(s-decl
													(p-ident (raw "val5"))
													(e-binop (op "+")
														(e-ident (raw "Full.L2.L3.L4.val4"))
														(e-int (raw "1"))))))))))))))
		(s-decl
			(p-ident (raw "depth5_2"))
			(e-ident (raw "Full.val1")))
		(s-decl
			(p-ident (raw "depth5_3"))
			(e-ident (raw "Full.L2.val2")))
		(s-decl
			(p-ident (raw "depth5_4"))
			(e-ident (raw "Full.L2.L3.val3")))
		(s-decl
			(p-ident (raw "depth5_5"))
			(e-ident (raw "Full.L2.L3.L4.val4")))
		(s-decl
			(p-ident (raw "depth5_6"))
			(e-ident (raw "Full.L2.L3.L4.L5.val5")))
		(s-type-decl
			(header (name "ScopeTest")
				(args))
			(ty-tag-union
				(tags
					(ty (name "NN"))))
			(associated
				(s-decl
					(p-ident (raw "innerOnly"))
					(e-int (raw "888")))
				(s-decl
					(p-ident (raw "canUse"))
					(e-ident (raw "innerOnly")))))
		(s-decl
			(p-ident (raw "scopeOuter"))
			(e-ident (raw "ScopeTest.innerOnly")))
		(s-decl
			(p-ident (raw "scopeAlias"))
			(e-ident (raw "ScopeTest.canUse")))
		(s-type-decl
			(header (name "ScopeNested")
				(args))
			(ty-tag-union
				(tags
					(ty (name "OO"))))
			(associated
				(s-decl
					(p-ident (raw "outer"))
					(e-int (raw "111")))
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "PP"))))
					(associated
						(s-decl
							(p-ident (raw "usesOuter"))
							(e-ident (raw "outer")))
						(s-decl
							(p-ident (raw "inner"))
							(e-int (raw "222")))))
				(s-decl
					(p-ident (raw "usesNested"))
					(e-ident (raw "ScopeNested.Nested.inner")))))
		(s-type-decl
			(header (name "Annotated")
				(args))
			(ty-tag-union
				(tags
					(ty (name "QQ"))))
			(associated
				(s-type-anno (name "typed")
					(ty (name "U64")))
				(s-decl
					(p-ident (raw "typed"))
					(e-int (raw "999")))
				(s-type-decl
					(header (name "NestedAnnotated")
						(args))
					(ty-tag-union
						(tags
							(ty (name "RR"))))
					(associated
						(s-type-anno (name "alsoTyped")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "alsoTyped"))
							(e-int (raw "111")))))))
		(s-decl
			(p-ident (raw "annoTest1"))
			(e-ident (raw "Annotated.typed")))
		(s-decl
			(p-ident (raw "annoTest2"))
			(e-ident (raw "Annotated.NestedAnnotated.alsoTyped")))
		(s-type-decl
			(header (name "Chain")
				(args))
			(ty-tag-union
				(tags
					(ty (name "SS"))))
			(associated
				(s-decl
					(p-ident (raw "start"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "Mid")
						(args))
					(ty-tag-union
						(tags
							(ty (name "TT"))))
					(associated
						(s-decl
							(p-ident (raw "step1"))
							(e-binop (op "*")
								(e-ident (raw "Chain.start"))
								(e-int (raw "2"))))
						(s-type-decl
							(header (name "Deep")
								(args))
							(ty-tag-union
								(tags
									(ty (name "UU"))))
							(associated
								(s-decl
									(p-ident (raw "step2"))
									(e-binop (op "*")
										(e-ident (raw "Chain.Mid.step1"))
										(e-int (raw "3"))))
								(s-type-decl
									(header (name "Deeper")
										(args))
									(ty-tag-union
										(tags
											(ty (name "VV"))))
									(associated
										(s-decl
											(p-ident (raw "step3"))
											(e-binop (op "*")
												(e-ident (raw "Chain.Mid.Deep.step2"))
												(e-int (raw "4"))))
										(s-type-decl
											(header (name "Deepest")
												(args))
											(ty-tag-union
												(tags
													(ty (name "WW"))))
											(associated
												(s-decl
													(p-ident (raw "final"))
													(e-binop (op "*")
														(e-ident (raw "Chain.Mid.Deep.Deeper.step3"))
														(e-int (raw "5"))))))))))))))
		(s-decl
			(p-ident (raw "chain1"))
			(e-ident (raw "Chain.start")))
		(s-decl
			(p-ident (raw "chain2"))
			(e-ident (raw "Chain.Mid.step1")))
		(s-decl
			(p-ident (raw "chain3"))
			(e-ident (raw "Chain.Mid.Deep.step2")))
		(s-decl
			(p-ident (raw "chain4"))
			(e-ident (raw "Chain.Mid.Deep.Deeper.step3")))
		(s-decl
			(p-ident (raw "chain5"))
			(e-ident (raw "Chain.Mid.Deep.Deeper.Deepest.final")))
		(s-type-decl
			(header (name "Circular")
				(args))
			(ty-tag-union
				(tags
					(ty (name "XX"))))
			(associated
				(s-decl
					(p-ident (raw "sum"))
					(e-binop (op "+")
						(e-binop (op "+")
							(e-ident (raw "Circular.a"))
							(e-ident (raw "Circular.b")))
						(e-ident (raw "Circular.c"))))
				(s-decl
					(p-ident (raw "a"))
					(e-int (raw "10")))
				(s-decl
					(p-ident (raw "b"))
					(e-int (raw "20")))
				(s-decl
					(p-ident (raw "c"))
					(e-int (raw "30")))))
		(s-decl
			(p-ident (raw "circTest"))
			(e-ident (raw "Circular.sum")))
		(s-type-decl
			(header (name "Ultimate")
				(args))
			(ty-tag-union
				(tags
					(ty (name "YY"))))
			(associated
				(s-type-anno (name "base")
					(ty (name "U64")))
				(s-decl
					(p-ident (raw "base"))
					(e-int (raw "100")))
				(s-type-decl
					(header (name "Branch1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ZZ"))))
					(associated
						(s-decl
							(p-ident (raw "b1val"))
							(e-binop (op "+")
								(e-ident (raw "Ultimate.base"))
								(e-ident (raw "Ultimate.Branch2.b2forward"))))
						(s-type-decl
							(header (name "Branch1Inner")
								(args))
							(ty-tag-union
								(tags
									(ty (name "AAA"))))
							(associated
								(s-type-anno (name "innerSum")
									(ty (name "U64")))
								(s-decl
									(p-ident (raw "innerSum"))
									(e-binop (op "+")
										(e-ident (raw "Ultimate.base"))
										(e-ident (raw "Ultimate.Branch1.b1val"))))))))
				(s-type-decl
					(header (name "Branch2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BBB"))))
					(associated
						(s-type-anno (name "b2forward")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "b2forward"))
							(e-int (raw "50")))
						(s-type-decl
							(header (name "Branch2Inner")
								(args))
							(ty-tag-union
								(tags
									(ty (name "CCC"))))
							(associated
								(s-decl
									(p-ident (raw "usesEverything"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-binop (op "+")
												(e-ident (raw "Ultimate.base"))
												(e-ident (raw "Ultimate.Branch1.b1val")))
											(e-ident (raw "Ultimate.Branch2.b2forward")))
										(e-ident (raw "Ultimate.Branch1.Branch1Inner.innerSum"))))))))))
		(s-decl
			(p-ident (raw "ultimate1"))
			(e-ident (raw "Ultimate.base")))
		(s-decl
			(p-ident (raw "ultimate2"))
			(e-ident (raw "Ultimate.Branch1.b1val")))
		(s-decl
			(p-ident (raw "ultimate3"))
			(e-ident (raw "Ultimate.Branch2.b2forward")))
		(s-decl
			(p-ident (raw "ultimate4"))
			(e-ident (raw "Ultimate.Branch1.Branch1Inner.innerSum")))
		(s-decl
			(p-ident (raw "ultimate5"))
			(e-ident (raw "Ultimate.Branch2.Branch2Inner.usesEverything")))
		(s-decl
			(p-ident (raw "errModuleUnqualified"))
			(e-ident (raw "value")))
		(s-type-decl
			(header (name "ErrOuterAccessInner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ERR1"))))
			(associated
				(s-decl
					(p-ident (raw "outerItem"))
					(e-int (raw "10")))
				(s-type-decl
					(header (name "InnerScope")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR2"))))
					(associated
						(s-decl
							(p-ident (raw "innerItem"))
							(e-int (raw "20")))))
				(s-decl
					(p-ident (raw "badAccess"))
					(e-ident (raw "innerItem")))))
		(s-type-decl
			(header (name "ErrSiblingAccess")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ERR3"))))
			(associated
				(s-type-decl
					(header (name "SiblingA")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR4"))))
					(associated
						(s-decl
							(p-ident (raw "sibAVal"))
							(e-int (raw "100")))))
				(s-type-decl
					(header (name "SiblingB")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR5"))))
					(associated
						(s-decl
							(p-ident (raw "badSiblingAccess"))
							(e-ident (raw "sibAVal")))))))
		(s-type-decl
			(header (name "ErrCousinAccess")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ERR6"))))
			(associated
				(s-type-decl
					(header (name "Branch1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR7"))))
					(associated
						(s-type-decl
							(header (name "Leaf1")
								(args))
							(ty-tag-union
								(tags
									(ty (name "ERR8"))))
							(associated
								(s-decl
									(p-ident (raw "leaf1Val"))
									(e-int (raw "1")))))))
				(s-type-decl
					(header (name "Branch2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR9"))))
					(associated
						(s-type-decl
							(header (name "Leaf2")
								(args))
							(ty-tag-union
								(tags
									(ty (name "ERR10"))))
							(associated
								(s-decl
									(p-ident (raw "badCousinAccess"))
									(e-ident (raw "leaf1Val")))))))))
		(s-type-decl
			(header (name "ErrGrandchildAccess")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ERR11"))))
			(associated
				(s-type-decl
					(header (name "Child")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR12"))))
					(associated
						(s-type-decl
							(header (name "Grandchild")
								(args))
							(ty-tag-union
								(tags
									(ty (name "ERR13"))))
							(associated
								(s-decl
									(p-ident (raw "grandchildVal"))
									(e-int (raw "999")))))))
				(s-decl
					(p-ident (raw "badGrandchildAccess"))
					(e-ident (raw "grandchildVal")))))
		(s-type-decl
			(header (name "ErrDeepSiblingAccess")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ERR14"))))
			(associated
				(s-decl
					(p-ident (raw "outerSibling"))
					(e-int (raw "50")))
				(s-type-decl
					(header (name "Level1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR15"))))
					(associated
						(s-type-decl
							(header (name "Level2")
								(args))
							(ty-tag-union
								(tags
									(ty (name "ERR16"))))
							(associated
								(s-type-decl
									(header (name "Level3")
										(args))
									(ty-tag-union
										(tags
											(ty (name "ERR17"))))
									(associated
										(s-decl
											(p-ident (raw "goodAccess"))
											(e-ident (raw "outerSibling")))))))
						(s-type-decl
							(header (name "OtherBranch")
								(args))
							(ty-tag-union
								(tags
									(ty (name "ERR18"))))
							(associated
								(s-decl
									(p-ident (raw "otherVal"))
									(e-int (raw "77")))))))
				(s-type-decl
					(header (name "Level1Alt")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ERR19"))))
					(associated
						(s-decl
							(p-ident (raw "badDeepAccess"))
							(e-ident (raw "otherVal")))))))
		(s-decl
			(p-ident (raw "errTryOuter"))
			(e-ident (raw "outerItem")))
		(s-decl
			(p-ident (raw "errTrySibA"))
			(e-ident (raw "sibAVal")))
		(s-decl
			(p-ident (raw "errTryLeaf"))
			(e-ident (raw "leaf1Val")))
		(s-decl
			(p-ident (raw "errTryGrand"))
			(e-ident (raw "grandchildVal")))))
~~~
# FORMATTED
~~~roc
# ============================================================================
# DEPTH 1: Basic associated items
# ============================================================================

# Test 1.1: Simple associated value
Simple := [A].{
	value = 1
}
simple1 = Simple.value # Should work

# Test 1.2: Forward reference within associated block
Forward := [B].{
	first = second
	second = 2
}
forward1 = Forward.first # Should be 2
forward2 = Forward.second # Should be 2

# Test 1.3: Multiple items referencing each other
Multi := [C].{
	a = 10
	b = a + 5
	c = b * 2
}
multi1 = Multi.a # 10
multi2 = Multi.b # 15
multi3 = Multi.c # 30

# Test 1.4: External references cannot see unqualified names
# (We test this by ensuring "value" alone doesn't resolve to Simple.value)
externalTest1 = Simple.value # Must use qualified name

# ============================================================================
# DEPTH 2: One level of nesting
# ============================================================================

# Test 2.1: Nested type with associated items
Outer1 := [D].{
	outerVal = 100
	Inner1 := [E].{
		innerVal = 200
	}
}
depth2_1 = Outer1.outerVal # 100
depth2_2 = Outer1.Inner1.innerVal # 200

# Test 2.2: Nested items referencing outer items
Outer2 := [F].{
	shared = 50
	Inner2 := [G].{
		usesOuter = Outer2.shared
		doubled = usesOuter * 2
	}
}
depth2_3 = Outer2.shared # 50
depth2_4 = Outer2.Inner2.usesOuter # 50
depth2_5 = Outer2.Inner2.doubled # 100

# Test 2.3: Forward references at depth 2
Outer3 := [H].{
	first = Outer3.Inner3.nested
	Inner3 := [I].{
		nested = 42
	}
}
depth2_6 = Outer3.first # 42
depth2_7 = Outer3.Inner3.nested # 42

# Test 2.4: Multiple nested types at same level
Outer4 := [J].{
	val = 1
	InnerA := [K].{
		valA = 2
	}
	InnerB := [L].{
		valB = Outer4.InnerA.valA + 1
	}
}
depth2_8 = Outer4.val # 1
depth2_9 = Outer4.InnerA.valA # 2
depth2_10 = Outer4.InnerB.valB # 3

# ============================================================================
# DEPTH 3: Two levels of nesting
# ============================================================================

# Test 3.1: Three-level hierarchy
Level1 := [M].{
	val1 = 10
	Level2 := [N].{
		val2 = 20
		Level3 := [O].{
			val3 = 30
		}
	}
}
depth3_1 = Level1.val1 # 10
depth3_2 = Level1.Level2.val2 # 20
depth3_3 = Level1.Level2.Level3.val3 # 30

# Test 3.2: Cross-level references
CrossRef := [P].{
	top = 5
	Mid := [Q].{
		middle = CrossRef.top * 2
		Deep := [R].{
			deep = CrossRef.Mid.middle + CrossRef.top
		}
	}
}
depth3_4 = CrossRef.top # 5
depth3_5 = CrossRef.Mid.middle # 10
depth3_6 = CrossRef.Mid.Deep.deep # 15

# Test 3.3: Forward references spanning multiple levels
ForwardDeep := [S].{
	usesDeepNested = ForwardDeep.M1.M2.deepVal
	M1 := [T].{
		M2 := [U].{
			deepVal = 99
		}
	}
}
depth3_7 = ForwardDeep.usesDeepNested # 99
depth3_8 = ForwardDeep.M1.M2.deepVal # 99

# ============================================================================
# DEPTH 4: Three levels of nesting
# ============================================================================

# Test 4.1: Four-level hierarchy with values at each level
D1 := [V].{
	v1 = 1000
	D2 := [W].{
		v2 = 2000
		D3 := [X].{
			v3 = 3000
			D4 := [Y].{
				v4 = 4000
			}
		}
	}
}
depth4_1 = D1.v1 # 1000
depth4_2 = D1.D2.v2 # 2000
depth4_3 = D1.D2.D3.v3 # 3000
depth4_4 = D1.D2.D3.D4.v4 # 4000

# Test 4.2: Deep forward reference
DeepForward := [Z].{
	usesDeeply = DeepForward.A.B.C.deepest
	A := [AA].{
		B := [BB].{
			C := [CC].{
				deepest = 777
			}
		}
	}
}
depth4_5 = DeepForward.usesDeeply # 777

# ============================================================================
# DEPTH 5: Four levels of nesting (maximum test depth)
# ============================================================================

# Test 5.1: Five-level hierarchy
Max1 := [DD].{
	Max2 := [EE].{
		Max3 := [FF].{
			Max4 := [GG].{
				Max5 := [HH].{
					deepestValue = 5555
				}
			}
		}
	}
}
depth5_1 = Max1.Max2.Max3.Max4.Max5.deepestValue # 5555

# Test 5.2: Values at each level of 5-deep hierarchy
Full := [II].{
	val1 = 1
	L2 := [JJ].{
		val2 = Full.val1 + 1
		L3 := [KK].{
			val3 = Full.L2.val2 + 1
			L4 := [LL].{
				val4 = Full.L2.L3.val3 + 1
				L5 := [MM].{
					val5 = Full.L2.L3.L4.val4 + 1
				}
			}
		}
	}
}
depth5_2 = Full.val1 # 1
depth5_3 = Full.L2.val2 # 2
depth5_4 = Full.L2.L3.val3 # 3
depth5_5 = Full.L2.L3.L4.val4 # 4
depth5_6 = Full.L2.L3.L4.L5.val5 # 5

# ============================================================================
# SCOPING EDGE CASES
# ============================================================================

# Test: Unqualified names only work within associated block
ScopeTest := [NN].{
	innerOnly = 888
	canUse = innerOnly
}
# Outside the block, must use qualified name
scopeOuter = ScopeTest.innerOnly # Must qualify
scopeAlias = ScopeTest.canUse # 888

# Test: Nested block can access outer unqualified, but outer cannot access inner
ScopeNested := [OO].{
	outer = 111
	Nested := [PP].{
		usesOuter = outer
		inner = 222
	}
	usesNested = ScopeNested.Nested.inner
}

# ============================================================================
# TYPE ANNOTATIONS WITH ASSOCIATED ITEMS
# ============================================================================

# Test: Type annotations on associated items
Annotated := [QQ].{
	typed : U64
	typed = 999
	NestedAnnotated := [RR].{
		alsoTyped : U64
		alsoTyped = 111
	}
}
annoTest1 = Annotated.typed # 999
annoTest2 = Annotated.NestedAnnotated.alsoTyped # 111

# ============================================================================
# COMPLEX DEPENDENCY CHAINS
# ============================================================================

# Test: Complex dependency chains across multiple nesting levels
Chain := [SS].{
	start = 1
	Mid := [TT].{
		step1 = Chain.start * 2
		Deep := [UU].{
			step2 = Chain.Mid.step1 * 3
			Deeper := [VV].{
				step3 = Chain.Mid.Deep.step2 * 4
				Deepest := [WW].{
					final = Chain.Mid.Deep.Deeper.step3 * 5
				}
			}
		}
	}
}
chain1 = Chain.start # 1
chain2 = Chain.Mid.step1 # 2
chain3 = Chain.Mid.Deep.step2 # 6
chain4 = Chain.Mid.Deep.Deeper.step3 # 24
chain5 = Chain.Mid.Deep.Deeper.Deepest.final # 120

# ============================================================================
# MULTIPLE FORWARD REFERENCES
# ============================================================================

# Test: Multiple forward references in circular dependency
Circular := [XX].{
	sum = Circular.a + Circular.b + Circular.c
	a = 10
	b = 20
	c = 30
}
circTest = Circular.sum # 60

# ============================================================================
# MIXED SCENARIOS
# ============================================================================

# Test: Everything at once - deep nesting, forward refs, cross-level refs, annotations
Ultimate := [YY].{
	base : U64
	base = 100
	Branch1 := [ZZ].{
		b1val = Ultimate.base + Ultimate.Branch2.b2forward
		Branch1Inner := [AAA].{
			innerSum : U64
			innerSum = Ultimate.base + Ultimate.Branch1.b1val
		}
	}
	Branch2 := [BBB].{
		b2forward : U64
		b2forward = 50
		Branch2Inner := [CCC].{
			usesEverything = Ultimate.base + Ultimate.Branch1.b1val + Ultimate.Branch2.b2forward + Ultimate.Branch1.Branch1Inner.innerSum
		}
	}
}
ultimate1 = Ultimate.base # 100
ultimate2 = Ultimate.Branch1.b1val # 150
ultimate3 = Ultimate.Branch2.b2forward # 50
ultimate4 = Ultimate.Branch1.Branch1Inner.innerSum # 250
ultimate5 = Ultimate.Branch2.Branch2Inner.usesEverything # 550

# ============================================================================
# INVALID LOOKUPS - These MUST produce errors
# ============================================================================

# Error 1: Module-level trying to access associated item unqualified
# "value" is only defined inside Simple's associated block
errModuleUnqualified = value # ERROR: 'value' not in scope at module level

# Error 2: Outer scope trying to access inner scope item unqualified
ErrOuterAccessInner := [ERR1].{
	outerItem = 10
	InnerScope := [ERR2].{
		innerItem = 20
	}
	badAccess = innerItem
}

# Error 3: Sibling nested types cannot access each other's items unqualified
ErrSiblingAccess := [ERR3].{
	SiblingA := [ERR4].{
		sibAVal = 100
	}
	SiblingB := [ERR5].{
		badSiblingAccess = sibAVal
	}
}

# Error 4: Deeply nested trying to access cousin's items unqualified
ErrCousinAccess := [ERR6].{
	Branch1 := [ERR7].{
		Leaf1 := [ERR8].{
			leaf1Val = 1
		}
	}
	Branch2 := [ERR9].{
		Leaf2 := [ERR10].{
			badCousinAccess = leaf1Val
		}
	}
}

# Error 5: Parent trying to access grandchild's items unqualified
ErrGrandchildAccess := [ERR11].{
	Child := [ERR12].{
		Grandchild := [ERR13].{
			grandchildVal = 999
		}
	}
	badGrandchildAccess = grandchildVal
}

# Error 6: Three levels deep - inner trying to access outer's sibling
ErrDeepSiblingAccess := [ERR14].{
	outerSibling = 50
	Level1 := [ERR15].{
		Level2 := [ERR16].{
			Level3 := [ERR17].{
				goodAccess = outerSibling
			}
		}
		OtherBranch := [ERR18].{
			otherVal = 77
		}
	}
	Level1Alt := [ERR19].{
		badDeepAccess = otherVal
	}
}

# Error 7: Module level trying various unqualified accesses
errTryOuter = outerItem # ERROR: not in scope
errTrySibA = sibAVal # ERROR: not in scope
errTryLeaf = leaf1Val # ERROR: not in scope
errTryGrand = grandchildVal # ERROR: not in scope
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "associated_items_comprehensive.Simple.value"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Forward.first"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Forward.second"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Forward.second"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Multi.a"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Multi.b"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Multi.a")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Multi.c"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Multi.b")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer1.Inner1.innerVal"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer1.outerVal"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer2.Inner2.usesOuter"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer2.shared"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer2.Inner2.doubled"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Outer2.Inner2.usesOuter")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer2.shared"))
		(e-num (value "50")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer3.Inner3.nested"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer3.first"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer3.Inner3.nested"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer4.InnerA.valA"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer4.InnerB.valB"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Outer4.InnerA.valA")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Outer4.val"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Level1.Level2.Level3.val3"))
		(e-num (value "30")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Level1.Level2.val2"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Level1.val1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.CrossRef.Mid.Deep.deep"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.CrossRef.Mid.middle")))
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.CrossRef.top")))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.CrossRef.Mid.middle"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.CrossRef.top")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.CrossRef.top"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ForwardDeep.M1.M2.deepVal"))
		(e-num (value "99")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ForwardDeep.usesDeepNested"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ForwardDeep.M1.M2.deepVal"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.D1.D2.D3.D4.v4"))
		(e-num (value "4000")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.D1.D2.D3.v3"))
		(e-num (value "3000")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.D1.D2.v2"))
		(e-num (value "2000")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.D1.v1"))
		(e-num (value "1000")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.DeepForward.A.B.C.deepest"))
		(e-num (value "777")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.DeepForward.usesDeeply"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.DeepForward.A.B.C.deepest"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Max1.Max2.Max3.Max4.Max5.deepestValue"))
		(e-num (value "5555")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Full.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Full.L2.L3.L4.val4")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Full.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Full.L2.L3.val3")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Full.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Full.L2.val2")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Full.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Full.val1")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Full.val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeTest.innerOnly"))
		(e-num (value "888")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeTest.canUse"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ScopeTest.innerOnly"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeNested.Nested.usesOuter"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ScopeNested.outer"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeNested.Nested.inner"))
		(e-num (value "222")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeNested.outer"))
		(e-num (value "111")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ScopeNested.usesNested"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ScopeNested.Nested.inner"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Annotated.NestedAnnotated.alsoTyped"))
		(e-num (value "111"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Annotated.typed"))
		(e-num (value "999"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.Deeper.Deepest.final"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.Deeper.step3")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.Deeper.step3"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.step2")))
			(e-num (value "4"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.step2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Chain.Mid.step1")))
			(e-num (value "3"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Chain.Mid.step1"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Chain.start")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Chain.start"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Circular.sum"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "associated_items_comprehensive.Circular.a")))
				(e-lookup-local
					(p-assign (ident "associated_items_comprehensive.Circular.b"))))
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Circular.c")))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Circular.a"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Circular.b"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Circular.c"))
		(e-num (value "30")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner.innerSum"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Ultimate.base")))
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.b1val"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.b1val"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Ultimate.base")))
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.b2forward")))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.Branch2Inner.usesEverything"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "associated_items_comprehensive.Ultimate.base")))
					(e-lookup-local
						(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.b1val"))))
				(e-lookup-local
					(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.b2forward"))))
			(e-lookup-local
				(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner.innerSum")))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.b2forward"))
		(e-num (value "50"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.Ultimate.base"))
		(e-num (value "100"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrOuterAccessInner.InnerScope.innerItem"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrOuterAccessInner.outerItem"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrOuterAccessInner.badAccess"))
		(e-lookup-local
			(p-assign (ident "innerItem"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrSiblingAccess.SiblingA.sibAVal"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrSiblingAccess.SiblingB.badSiblingAccess"))
		(e-lookup-local
			(p-assign (ident "sibAVal"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrCousinAccess.Branch1.Leaf1.leaf1Val"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrCousinAccess.Branch2.Leaf2.badCousinAccess"))
		(e-lookup-local
			(p-assign (ident "leaf1Val"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrGrandchildAccess.Child.Grandchild.grandchildVal"))
		(e-num (value "999")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrGrandchildAccess.badGrandchildAccess"))
		(e-lookup-local
			(p-assign (ident "grandchildVal"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2.Level3.goodAccess"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ErrDeepSiblingAccess.outerSibling"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.OtherBranch.otherVal"))
		(e-num (value "77")))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrDeepSiblingAccess.Level1Alt.badDeepAccess"))
		(e-lookup-local
			(p-assign (ident "otherVal"))))
	(d-let
		(p-assign (ident "associated_items_comprehensive.ErrDeepSiblingAccess.outerSibling"))
		(e-num (value "50")))
	(d-let
		(p-assign (ident "simple1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Simple.value"))))
	(d-let
		(p-assign (ident "forward1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Forward.first"))))
	(d-let
		(p-assign (ident "forward2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Forward.second"))))
	(d-let
		(p-assign (ident "multi1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Multi.a"))))
	(d-let
		(p-assign (ident "multi2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Multi.b"))))
	(d-let
		(p-assign (ident "multi3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Multi.c"))))
	(d-let
		(p-assign (ident "externalTest1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Simple.value"))))
	(d-let
		(p-assign (ident "depth2_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer1.outerVal"))))
	(d-let
		(p-assign (ident "depth2_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer1.Inner1.innerVal"))))
	(d-let
		(p-assign (ident "depth2_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer2.shared"))))
	(d-let
		(p-assign (ident "depth2_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer2.Inner2.usesOuter"))))
	(d-let
		(p-assign (ident "depth2_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer2.Inner2.doubled"))))
	(d-let
		(p-assign (ident "depth2_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer3.first"))))
	(d-let
		(p-assign (ident "depth2_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer3.Inner3.nested"))))
	(d-let
		(p-assign (ident "depth2_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer4.val"))))
	(d-let
		(p-assign (ident "depth2_9"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer4.InnerA.valA"))))
	(d-let
		(p-assign (ident "depth2_10"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Outer4.InnerB.valB"))))
	(d-let
		(p-assign (ident "depth3_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Level1.val1"))))
	(d-let
		(p-assign (ident "depth3_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Level1.Level2.val2"))))
	(d-let
		(p-assign (ident "depth3_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Level1.Level2.Level3.val3"))))
	(d-let
		(p-assign (ident "depth3_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.CrossRef.top"))))
	(d-let
		(p-assign (ident "depth3_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.CrossRef.Mid.middle"))))
	(d-let
		(p-assign (ident "depth3_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.CrossRef.Mid.Deep.deep"))))
	(d-let
		(p-assign (ident "depth3_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ForwardDeep.usesDeepNested"))))
	(d-let
		(p-assign (ident "depth3_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ForwardDeep.M1.M2.deepVal"))))
	(d-let
		(p-assign (ident "depth4_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.D1.v1"))))
	(d-let
		(p-assign (ident "depth4_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.D1.D2.v2"))))
	(d-let
		(p-assign (ident "depth4_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.D1.D2.D3.v3"))))
	(d-let
		(p-assign (ident "depth4_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.D1.D2.D3.D4.v4"))))
	(d-let
		(p-assign (ident "depth4_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.DeepForward.usesDeeply"))))
	(d-let
		(p-assign (ident "depth5_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Max1.Max2.Max3.Max4.Max5.deepestValue"))))
	(d-let
		(p-assign (ident "depth5_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Full.val1"))))
	(d-let
		(p-assign (ident "depth5_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Full.L2.val2"))))
	(d-let
		(p-assign (ident "depth5_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Full.L2.L3.val3"))))
	(d-let
		(p-assign (ident "depth5_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Full.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "depth5_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Full.L2.L3.L4.L5.val5"))))
	(d-let
		(p-assign (ident "scopeOuter"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ScopeTest.innerOnly"))))
	(d-let
		(p-assign (ident "scopeAlias"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.ScopeTest.canUse"))))
	(d-let
		(p-assign (ident "annoTest1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Annotated.typed"))))
	(d-let
		(p-assign (ident "annoTest2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Annotated.NestedAnnotated.alsoTyped"))))
	(d-let
		(p-assign (ident "chain1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Chain.start"))))
	(d-let
		(p-assign (ident "chain2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Chain.Mid.step1"))))
	(d-let
		(p-assign (ident "chain3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.step2"))))
	(d-let
		(p-assign (ident "chain4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.Deeper.step3"))))
	(d-let
		(p-assign (ident "chain5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Chain.Mid.Deep.Deeper.Deepest.final"))))
	(d-let
		(p-assign (ident "circTest"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Circular.sum"))))
	(d-let
		(p-assign (ident "ultimate1"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Ultimate.base"))))
	(d-let
		(p-assign (ident "ultimate2"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.b1val"))))
	(d-let
		(p-assign (ident "ultimate3"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.b2forward"))))
	(d-let
		(p-assign (ident "ultimate4"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner.innerSum"))))
	(d-let
		(p-assign (ident "ultimate5"))
		(e-lookup-local
			(p-assign (ident "associated_items_comprehensive.Ultimate.Branch2.Branch2Inner.usesEverything"))))
	(d-let
		(p-assign (ident "errModuleUnqualified"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "errTryOuter"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "errTrySibA"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "errTryLeaf"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "errTryGrand"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-nominal-decl
		(ty-header (name "Simple"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "Forward"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "Multi"))
		(ty-tag-union
			(ty-tag-name (name "C"))))
	(s-nominal-decl
		(ty-header (name "Outer1"))
		(ty-tag-union
			(ty-tag-name (name "D"))))
	(s-nominal-decl
		(ty-header (name "Outer2"))
		(ty-tag-union
			(ty-tag-name (name "F"))))
	(s-nominal-decl
		(ty-header (name "Outer3"))
		(ty-tag-union
			(ty-tag-name (name "H"))))
	(s-nominal-decl
		(ty-header (name "Outer4"))
		(ty-tag-union
			(ty-tag-name (name "J"))))
	(s-nominal-decl
		(ty-header (name "Level1"))
		(ty-tag-union
			(ty-tag-name (name "M"))))
	(s-nominal-decl
		(ty-header (name "CrossRef"))
		(ty-tag-union
			(ty-tag-name (name "P"))))
	(s-nominal-decl
		(ty-header (name "ForwardDeep"))
		(ty-tag-union
			(ty-tag-name (name "S"))))
	(s-nominal-decl
		(ty-header (name "D1"))
		(ty-tag-union
			(ty-tag-name (name "V"))))
	(s-nominal-decl
		(ty-header (name "DeepForward"))
		(ty-tag-union
			(ty-tag-name (name "Z"))))
	(s-nominal-decl
		(ty-header (name "Max1"))
		(ty-tag-union
			(ty-tag-name (name "DD"))))
	(s-nominal-decl
		(ty-header (name "Full"))
		(ty-tag-union
			(ty-tag-name (name "II"))))
	(s-nominal-decl
		(ty-header (name "ScopeTest"))
		(ty-tag-union
			(ty-tag-name (name "NN"))))
	(s-nominal-decl
		(ty-header (name "ScopeNested"))
		(ty-tag-union
			(ty-tag-name (name "OO"))))
	(s-nominal-decl
		(ty-header (name "Annotated"))
		(ty-tag-union
			(ty-tag-name (name "QQ"))))
	(s-nominal-decl
		(ty-header (name "Chain"))
		(ty-tag-union
			(ty-tag-name (name "SS"))))
	(s-nominal-decl
		(ty-header (name "Circular"))
		(ty-tag-union
			(ty-tag-name (name "XX"))))
	(s-nominal-decl
		(ty-header (name "Ultimate"))
		(ty-tag-union
			(ty-tag-name (name "YY"))))
	(s-nominal-decl
		(ty-header (name "ErrOuterAccessInner"))
		(ty-tag-union
			(ty-tag-name (name "ERR1"))))
	(s-nominal-decl
		(ty-header (name "ErrSiblingAccess"))
		(ty-tag-union
			(ty-tag-name (name "ERR3"))))
	(s-nominal-decl
		(ty-header (name "ErrCousinAccess"))
		(ty-tag-union
			(ty-tag-name (name "ERR6"))))
	(s-nominal-decl
		(ty-header (name "ErrGrandchildAccess"))
		(ty-tag-union
			(ty-tag-name (name "ERR11"))))
	(s-nominal-decl
		(ty-header (name "ErrDeepSiblingAccess"))
		(ty-tag-union
			(ty-tag-name (name "ERR14"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Outer1.Inner1"))
		(ty-tag-union
			(ty-tag-name (name "E"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Outer2.Inner2"))
		(ty-tag-union
			(ty-tag-name (name "G"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Outer3.Inner3"))
		(ty-tag-union
			(ty-tag-name (name "I"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Outer4.InnerA"))
		(ty-tag-union
			(ty-tag-name (name "K"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Outer4.InnerB"))
		(ty-tag-union
			(ty-tag-name (name "L"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Level1.Level2"))
		(ty-tag-union
			(ty-tag-name (name "N"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Level1.Level2.Level3"))
		(ty-tag-union
			(ty-tag-name (name "O"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.CrossRef.Mid"))
		(ty-tag-union
			(ty-tag-name (name "Q"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.CrossRef.Mid.Deep"))
		(ty-tag-union
			(ty-tag-name (name "R"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ForwardDeep.M1"))
		(ty-tag-union
			(ty-tag-name (name "T"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ForwardDeep.M1.M2"))
		(ty-tag-union
			(ty-tag-name (name "U"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.D1.D2"))
		(ty-tag-union
			(ty-tag-name (name "W"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.D1.D2.D3"))
		(ty-tag-union
			(ty-tag-name (name "X"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.D1.D2.D3.D4"))
		(ty-tag-union
			(ty-tag-name (name "Y"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.DeepForward.A"))
		(ty-tag-union
			(ty-tag-name (name "AA"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.DeepForward.A.B"))
		(ty-tag-union
			(ty-tag-name (name "BB"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.DeepForward.A.B.C"))
		(ty-tag-union
			(ty-tag-name (name "CC"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Max1.Max2"))
		(ty-tag-union
			(ty-tag-name (name "EE"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3"))
		(ty-tag-union
			(ty-tag-name (name "FF"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3.Max4"))
		(ty-tag-union
			(ty-tag-name (name "GG"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3.Max4.Max5"))
		(ty-tag-union
			(ty-tag-name (name "HH"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Full.L2"))
		(ty-tag-union
			(ty-tag-name (name "JJ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Full.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "KK"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Full.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "LL"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Full.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "MM"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ScopeNested.Nested"))
		(ty-tag-union
			(ty-tag-name (name "PP"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Annotated.NestedAnnotated"))
		(ty-tag-union
			(ty-tag-name (name "RR"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Chain.Mid"))
		(ty-tag-union
			(ty-tag-name (name "TT"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep"))
		(ty-tag-union
			(ty-tag-name (name "UU"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep.Deeper"))
		(ty-tag-union
			(ty-tag-name (name "VV"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep.Deeper.Deepest"))
		(ty-tag-union
			(ty-tag-name (name "WW"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Ultimate.Branch1"))
		(ty-tag-union
			(ty-tag-name (name "ZZ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Ultimate.Branch2"))
		(ty-tag-union
			(ty-tag-name (name "BBB"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner"))
		(ty-tag-union
			(ty-tag-name (name "AAA"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.Ultimate.Branch2.Branch2Inner"))
		(ty-tag-union
			(ty-tag-name (name "CCC"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrOuterAccessInner.InnerScope"))
		(ty-tag-union
			(ty-tag-name (name "ERR2"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrSiblingAccess.SiblingA"))
		(ty-tag-union
			(ty-tag-name (name "ERR4"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrSiblingAccess.SiblingB"))
		(ty-tag-union
			(ty-tag-name (name "ERR5"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch1"))
		(ty-tag-union
			(ty-tag-name (name "ERR7"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch2"))
		(ty-tag-union
			(ty-tag-name (name "ERR9"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch1.Leaf1"))
		(ty-tag-union
			(ty-tag-name (name "ERR8"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch2.Leaf2"))
		(ty-tag-union
			(ty-tag-name (name "ERR10"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrGrandchildAccess.Child"))
		(ty-tag-union
			(ty-tag-name (name "ERR12"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrGrandchildAccess.Child.Grandchild"))
		(ty-tag-union
			(ty-tag-name (name "ERR13"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1"))
		(ty-tag-union
			(ty-tag-name (name "ERR15"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1Alt"))
		(ty-tag-union
			(ty-tag-name (name "ERR19"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2"))
		(ty-tag-union
			(ty-tag-name (name "ERR16"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.OtherBranch"))
		(ty-tag-union
			(ty-tag-name (name "ERR18"))))
	(s-nominal-decl
		(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2.Level3"))
		(ty-tag-union
			(ty-tag-name (name "ERR17")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "_d"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "_d"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "_d"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "_d"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "_d"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Simple")
			(ty-header (name "Simple")))
		(nominal (type "Forward")
			(ty-header (name "Forward")))
		(nominal (type "Multi")
			(ty-header (name "Multi")))
		(nominal (type "Outer1")
			(ty-header (name "Outer1")))
		(nominal (type "Outer2")
			(ty-header (name "Outer2")))
		(nominal (type "Outer3")
			(ty-header (name "Outer3")))
		(nominal (type "Outer4")
			(ty-header (name "Outer4")))
		(nominal (type "Level1")
			(ty-header (name "Level1")))
		(nominal (type "CrossRef")
			(ty-header (name "CrossRef")))
		(nominal (type "ForwardDeep")
			(ty-header (name "ForwardDeep")))
		(nominal (type "D1")
			(ty-header (name "D1")))
		(nominal (type "DeepForward")
			(ty-header (name "DeepForward")))
		(nominal (type "Max1")
			(ty-header (name "Max1")))
		(nominal (type "Full")
			(ty-header (name "Full")))
		(nominal (type "ScopeTest")
			(ty-header (name "ScopeTest")))
		(nominal (type "ScopeNested")
			(ty-header (name "ScopeNested")))
		(nominal (type "Annotated")
			(ty-header (name "Annotated")))
		(nominal (type "Chain")
			(ty-header (name "Chain")))
		(nominal (type "Circular")
			(ty-header (name "Circular")))
		(nominal (type "Ultimate")
			(ty-header (name "Ultimate")))
		(nominal (type "ErrOuterAccessInner")
			(ty-header (name "ErrOuterAccessInner")))
		(nominal (type "ErrSiblingAccess")
			(ty-header (name "ErrSiblingAccess")))
		(nominal (type "ErrCousinAccess")
			(ty-header (name "ErrCousinAccess")))
		(nominal (type "ErrGrandchildAccess")
			(ty-header (name "ErrGrandchildAccess")))
		(nominal (type "ErrDeepSiblingAccess")
			(ty-header (name "ErrDeepSiblingAccess")))
		(nominal (type "associated_items_comprehensive.Outer1.Inner1")
			(ty-header (name "associated_items_comprehensive.Outer1.Inner1")))
		(nominal (type "associated_items_comprehensive.Outer2.Inner2")
			(ty-header (name "associated_items_comprehensive.Outer2.Inner2")))
		(nominal (type "associated_items_comprehensive.Outer3.Inner3")
			(ty-header (name "associated_items_comprehensive.Outer3.Inner3")))
		(nominal (type "associated_items_comprehensive.Outer4.InnerA")
			(ty-header (name "associated_items_comprehensive.Outer4.InnerA")))
		(nominal (type "associated_items_comprehensive.Outer4.InnerB")
			(ty-header (name "associated_items_comprehensive.Outer4.InnerB")))
		(nominal (type "associated_items_comprehensive.Level1.Level2")
			(ty-header (name "associated_items_comprehensive.Level1.Level2")))
		(nominal (type "associated_items_comprehensive.Level1.Level2.Level3")
			(ty-header (name "associated_items_comprehensive.Level1.Level2.Level3")))
		(nominal (type "associated_items_comprehensive.CrossRef.Mid")
			(ty-header (name "associated_items_comprehensive.CrossRef.Mid")))
		(nominal (type "associated_items_comprehensive.CrossRef.Mid.Deep")
			(ty-header (name "associated_items_comprehensive.CrossRef.Mid.Deep")))
		(nominal (type "associated_items_comprehensive.ForwardDeep.M1")
			(ty-header (name "associated_items_comprehensive.ForwardDeep.M1")))
		(nominal (type "associated_items_comprehensive.ForwardDeep.M1.M2")
			(ty-header (name "associated_items_comprehensive.ForwardDeep.M1.M2")))
		(nominal (type "associated_items_comprehensive.D1.D2")
			(ty-header (name "associated_items_comprehensive.D1.D2")))
		(nominal (type "associated_items_comprehensive.D1.D2.D3")
			(ty-header (name "associated_items_comprehensive.D1.D2.D3")))
		(nominal (type "associated_items_comprehensive.D1.D2.D3.D4")
			(ty-header (name "associated_items_comprehensive.D1.D2.D3.D4")))
		(nominal (type "associated_items_comprehensive.DeepForward.A")
			(ty-header (name "associated_items_comprehensive.DeepForward.A")))
		(nominal (type "associated_items_comprehensive.DeepForward.A.B")
			(ty-header (name "associated_items_comprehensive.DeepForward.A.B")))
		(nominal (type "associated_items_comprehensive.DeepForward.A.B.C")
			(ty-header (name "associated_items_comprehensive.DeepForward.A.B.C")))
		(nominal (type "associated_items_comprehensive.Max1.Max2")
			(ty-header (name "associated_items_comprehensive.Max1.Max2")))
		(nominal (type "associated_items_comprehensive.Max1.Max2.Max3")
			(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3")))
		(nominal (type "associated_items_comprehensive.Max1.Max2.Max3.Max4")
			(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3.Max4")))
		(nominal (type "associated_items_comprehensive.Max1.Max2.Max3.Max4.Max5")
			(ty-header (name "associated_items_comprehensive.Max1.Max2.Max3.Max4.Max5")))
		(nominal (type "associated_items_comprehensive.Full.L2")
			(ty-header (name "associated_items_comprehensive.Full.L2")))
		(nominal (type "associated_items_comprehensive.Full.L2.L3")
			(ty-header (name "associated_items_comprehensive.Full.L2.L3")))
		(nominal (type "associated_items_comprehensive.Full.L2.L3.L4")
			(ty-header (name "associated_items_comprehensive.Full.L2.L3.L4")))
		(nominal (type "associated_items_comprehensive.Full.L2.L3.L4.L5")
			(ty-header (name "associated_items_comprehensive.Full.L2.L3.L4.L5")))
		(nominal (type "associated_items_comprehensive.ScopeNested.Nested")
			(ty-header (name "associated_items_comprehensive.ScopeNested.Nested")))
		(nominal (type "associated_items_comprehensive.Annotated.NestedAnnotated")
			(ty-header (name "associated_items_comprehensive.Annotated.NestedAnnotated")))
		(nominal (type "associated_items_comprehensive.Chain.Mid")
			(ty-header (name "associated_items_comprehensive.Chain.Mid")))
		(nominal (type "associated_items_comprehensive.Chain.Mid.Deep")
			(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep")))
		(nominal (type "associated_items_comprehensive.Chain.Mid.Deep.Deeper")
			(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep.Deeper")))
		(nominal (type "associated_items_comprehensive.Chain.Mid.Deep.Deeper.Deepest")
			(ty-header (name "associated_items_comprehensive.Chain.Mid.Deep.Deeper.Deepest")))
		(nominal (type "associated_items_comprehensive.Ultimate.Branch1")
			(ty-header (name "associated_items_comprehensive.Ultimate.Branch1")))
		(nominal (type "associated_items_comprehensive.Ultimate.Branch2")
			(ty-header (name "associated_items_comprehensive.Ultimate.Branch2")))
		(nominal (type "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner")
			(ty-header (name "associated_items_comprehensive.Ultimate.Branch1.Branch1Inner")))
		(nominal (type "associated_items_comprehensive.Ultimate.Branch2.Branch2Inner")
			(ty-header (name "associated_items_comprehensive.Ultimate.Branch2.Branch2Inner")))
		(nominal (type "associated_items_comprehensive.ErrOuterAccessInner.InnerScope")
			(ty-header (name "associated_items_comprehensive.ErrOuterAccessInner.InnerScope")))
		(nominal (type "associated_items_comprehensive.ErrSiblingAccess.SiblingA")
			(ty-header (name "associated_items_comprehensive.ErrSiblingAccess.SiblingA")))
		(nominal (type "associated_items_comprehensive.ErrSiblingAccess.SiblingB")
			(ty-header (name "associated_items_comprehensive.ErrSiblingAccess.SiblingB")))
		(nominal (type "associated_items_comprehensive.ErrCousinAccess.Branch1")
			(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch1")))
		(nominal (type "associated_items_comprehensive.ErrCousinAccess.Branch2")
			(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch2")))
		(nominal (type "associated_items_comprehensive.ErrCousinAccess.Branch1.Leaf1")
			(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch1.Leaf1")))
		(nominal (type "associated_items_comprehensive.ErrCousinAccess.Branch2.Leaf2")
			(ty-header (name "associated_items_comprehensive.ErrCousinAccess.Branch2.Leaf2")))
		(nominal (type "associated_items_comprehensive.ErrGrandchildAccess.Child")
			(ty-header (name "associated_items_comprehensive.ErrGrandchildAccess.Child")))
		(nominal (type "associated_items_comprehensive.ErrGrandchildAccess.Child.Grandchild")
			(ty-header (name "associated_items_comprehensive.ErrGrandchildAccess.Child.Grandchild")))
		(nominal (type "associated_items_comprehensive.ErrDeepSiblingAccess.Level1")
			(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1")))
		(nominal (type "associated_items_comprehensive.ErrDeepSiblingAccess.Level1Alt")
			(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1Alt")))
		(nominal (type "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2")
			(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2")))
		(nominal (type "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.OtherBranch")
			(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.OtherBranch")))
		(nominal (type "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2.Level3")
			(ty-header (name "associated_items_comprehensive.ErrDeepSiblingAccess.Level1.Level2.Level3"))))
	(expressions
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "_d"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "_d"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "_d"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "_d"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "_d"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
