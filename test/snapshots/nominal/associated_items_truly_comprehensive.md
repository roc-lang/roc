# META
~~~ini
description=Truly comprehensive test of associated items - every combination of ordering and access patterns
type=snippet
~~~
# SOURCE
~~~roc
# ============================================================================
# DEPTH 1: All combinations at single level
# ============================================================================

# Test 1.1: Simple forward reference
D1_Forward := [A].{
    first = second      # Forward ref
    second = 100
}
d1_1 = D1_Forward.first   # 100
d1_2 = D1_Forward.second  # 100

# Test 1.2: Simple backward reference
D1_Backward := [B].{
    first = 200
    second = first      # Backward ref
}
d1_3 = D1_Backward.first   # 200
d1_4 = D1_Backward.second  # 200

# Test 1.3: Multiple items - all orderings
D1_Multi := [C].{
    a = b + c           # Forward refs to both
    b = 10
    c = d + 5           # Forward ref to d
    d = 20
}
d1_5 = D1_Multi.a      # 35
d1_6 = D1_Multi.b      # 10
d1_7 = D1_Multi.c      # 25
d1_8 = D1_Multi.d      # 20

# ============================================================================
# DEPTH 2: Nested types - all access pattern combinations
# ============================================================================

# Test 2.1: Inner defined BEFORE outer value, inner refs outer (qualified)
D2_InnerFirst_Qual := [D].{
    Inner := [E].{
        innerVal = D2_InnerFirst_Qual.outerVal    # Forward ref to outer's val (qualified)
    }

    outerVal = 42
}
d2_1 = D2_InnerFirst_Qual.outerVal              # 42
d2_2 = D2_InnerFirst_Qual.Inner.innerVal        # 42

# Test 2.2: Inner defined BEFORE outer value, inner refs outer (unqualified)
D2_InnerFirst_Unqual := [F].{
    Inner := [G].{
        innerVal = outerVal    # Forward ref to outer's val (unqualified) - should work, we're in outer's block
    }

    outerVal = 43
}
d2_3 = D2_InnerFirst_Unqual.outerVal            # 43
d2_4 = D2_InnerFirst_Unqual.Inner.innerVal      # 43

# Test 2.3: Inner defined AFTER outer value, inner refs outer (qualified)
D2_InnerAfter_Qual := [H].{
    outerVal = 44

    Inner := [I].{
        innerVal = D2_InnerAfter_Qual.outerVal    # Backward ref to outer's val (qualified)
    }
}
d2_5 = D2_InnerAfter_Qual.outerVal              # 44
d2_6 = D2_InnerAfter_Qual.Inner.innerVal        # 44

# Test 2.4: Inner defined AFTER outer value, inner refs outer (unqualified)
D2_InnerAfter_Unqual := [J].{
    outerVal = 45

    Inner := [K].{
        innerVal = outerVal    # Backward ref to outer's val (unqualified)
    }
}
d2_7 = D2_InnerAfter_Unqual.outerVal            # 45
d2_8 = D2_InnerAfter_Unqual.Inner.innerVal      # 45

# Test 2.5: Outer refs inner (must be qualified, must be forward ref)
D2_OuterRefsInner := [L].{
    outerVal = D2_OuterRefsInner.Inner.innerVal    # Forward ref to inner's val

    Inner := [M].{
        innerVal = 46
    }
}
d2_9 = D2_OuterRefsInner.outerVal               # 46
d2_10 = D2_OuterRefsInner.Inner.innerVal        # 46

# Test 2.6: Outer refs inner (backward ref)
D2_OuterRefsInner_Back := [N].{
    Inner := [O].{
        innerVal = 47
    }

    outerVal = D2_OuterRefsInner_Back.Inner.innerVal    # Backward ref to inner's val
}
d2_11 = D2_OuterRefsInner_Back.outerVal         # 47
d2_12 = D2_OuterRefsInner_Back.Inner.innerVal   # 47

# Test 2.7: Sibling nested types referencing each other - A refs B forward
D2_Siblings_Forward := [P].{
    InnerA := [Q].{
        valA = D2_Siblings_Forward.InnerB.valB + 1    # Forward ref to sibling
    }

    InnerB := [R].{
        valB = 48
    }
}
d2_13 = D2_Siblings_Forward.InnerA.valA         # 49
d2_14 = D2_Siblings_Forward.InnerB.valB         # 48

# Test 2.8: Sibling nested types referencing each other - A refs B backward
D2_Siblings_Backward := [S].{
    InnerA := [T].{
        valA = 50
    }

    InnerB := [U].{
        valB = D2_Siblings_Backward.InnerA.valA + 1    # Backward ref to sibling
    }
}
d2_15 = D2_Siblings_Backward.InnerA.valA        # 50
d2_16 = D2_Siblings_Backward.InnerB.valB        # 51

# Test 2.9: Outer, Inner, outer val - complex interleaving
D2_Interleaved := [V].{
    val1 = D2_Interleaved.Inner.innerVal + 10   # Forward ref to inner

    Inner := [W].{
        innerVal = val2 + 5                      # Forward ref to outer val (unqualified)
    }

    val2 = 20
}
d2_17 = D2_Interleaved.val1                     # 35 (25 + 10)
d2_18 = D2_Interleaved.Inner.innerVal           # 25 (20 + 5)
d2_19 = D2_Interleaved.val2                     # 20

# ============================================================================
# DEPTH 3: Three levels - every ordering combination
# ============================================================================

# Test 3.1: L1 val, L2, L2 val, L3, L3 val - L3 refs L1 (unqualified)
D3_Pattern1 := [X].{
    val1 = 100

    L2 := [Y].{
        val2 = 200

        L3 := [Z].{
            val3 = val1 + val2    # Unqualified refs to L1 and L2 vals
        }
    }
}
d3_1 = D3_Pattern1.val1                         # 100
d3_2 = D3_Pattern1.L2.val2                      # 200
d3_3 = D3_Pattern1.L2.L3.val3                   # 300

# Test 3.2: L2, L3, L3 val, L2 val, L1 val - all forward refs
D3_Pattern2 := [AA].{
    L2 := [BB].{
        L3 := [CC].{
            val3 = val2 + val1                  # Forward refs to L2 and L1 vals (unqualified)
        }

        val2 = D3_Pattern2.L2.L3.val3 + 10      # Forward ref to L3 val (qualified)
    }

    val1 = D3_Pattern2.L2.val2 + 5              # Forward ref to L2 val (qualified)
}
# This creates circular dependency: val3 = val2 + val1, val2 = val3 + 10, val1 = val2 + 5
# Should either resolve or error gracefully

# Test 3.3: L3 val defined, then L2 val, then L1 val - L2 refs L3, L1 refs L2
D3_Pattern3 := [DD].{
    L2 := [EE].{
        L3 := [FF].{
            val3 = 1000
        }

        val2 = D3_Pattern3.L2.L3.val3 * 2       # Backward ref to L3 (qualified)
    }

    val1 = D3_Pattern3.L2.val2 * 2              # Backward ref to L2 (qualified)
}
d3_4 = D3_Pattern3.L2.L3.val3                   # 1000
d3_5 = D3_Pattern3.L2.val2                      # 2000
d3_6 = D3_Pattern3.val1                         # 4000

# Test 3.4: L1 val, L3, L2 (out of order), L3 val, L2 val
D3_Pattern4 := [GG].{
    val1 = 5

    L2 := [HH].{
        L3 := [II].{
            val3 = val1 * 10                    # Unqualified ref to L1
        }

        val2 = D3_Pattern4.L2.L3.val3 + val1    # Backward ref to L3 (qualified), unqualified to L1
    }
}
d3_7 = D3_Pattern4.val1                         # 5
d3_8 = D3_Pattern4.L2.L3.val3                   # 50
d3_9 = D3_Pattern4.L2.val2                      # 55

# Test 3.5: All vals before all types
D3_Pattern5 := [JJ].{
    val1 = 1

    L2 := [KK].{
        val2 = 2

        L3 := [LL].{
            val3 = val1 + val2                  # Backward refs (unqualified)
        }
    }
}
d3_10 = D3_Pattern5.L2.L3.val3                  # 3

# Test 3.6: All types before all vals
D3_Pattern6 := [MM].{
    L2 := [NN].{
        L3 := [OO].{
            val3 = val2 + val1                  # Forward refs (unqualified)
        }

        val2 = val1 * 2                         # Forward ref (unqualified)
    }

    val1 = 7
}
d3_11 = D3_Pattern6.val1                        # 7
d3_12 = D3_Pattern6.L2.val2                     # 14
d3_13 = D3_Pattern6.L2.L3.val3                  # 21

# Test 3.7: Sibling access at L2 - L2a and L2b both exist
D3_Siblings := [PP].{
    L2a := [QQ].{
        val2a = 10

        L3a := [RR].{
            val3a = D3_Siblings.L2b.val2b       # Cross-sibling at parent level (qualified)
        }
    }

    L2b := [SS].{
        val2b = 20

        L3b := [TT].{
            val3b = D3_Siblings.L2a.L3a.val3a   # Access cousin (qualified)
        }
    }
}
d3_14 = D3_Siblings.L2a.L3a.val3a               # 20
d3_15 = D3_Siblings.L2b.L3b.val3b               # 20

# ============================================================================
# DEPTH 4: Four levels - selected critical patterns
# ============================================================================

# Test 4.1: Deepest level refs all ancestors (unqualified)
D4_Pattern1 := [UU].{
    val1 = 1

    L2 := [VV].{
        val2 = 2

        L3 := [WW].{
            val3 = 3

            L4 := [XX].{
                val4 = val1 + val2 + val3       # Unqualified refs to all ancestors
            }
        }
    }
}
d4_1 = D4_Pattern1.L2.L3.L4.val4                # 6

# Test 4.2: L4 val defined first, rest reference it
D4_Pattern2 := [YY].{
    L2 := [ZZ].{
        L3 := [AAA].{
            L4 := [BBB].{
                val4 = 100
            }

            val3 = D4_Pattern2.L2.L3.L4.val4 * 2    # Backward ref (qualified)
        }

        val2 = D4_Pattern2.L2.L3.val3 * 2           # Backward ref (qualified)
    }

    val1 = D4_Pattern2.L2.val2 * 2                  # Backward ref (qualified)
}
d4_2 = D4_Pattern2.L2.L3.L4.val4                # 100
d4_3 = D4_Pattern2.L2.L3.val3                   # 200
d4_4 = D4_Pattern2.L2.val2                      # 400
d4_5 = D4_Pattern2.val1                         # 800

# Test 4.3: Alternating defined/undefined as we go deeper
D4_Pattern3 := [CCC].{
    val1 = D4_Pattern3.L2.val2 + 1              # Forward ref

    L2 := [DDD].{
        val2 = 10

        L3 := [EEE].{
            val3 = val1 + val2                  # Unqualified refs

            L4 := [FFF].{
                val4 = val3 * 2                 # Unqualified ref
            }
        }
    }
}
d4_6 = D4_Pattern3.val1                         # 11
d4_7 = D4_Pattern3.L2.val2                      # 10
d4_8 = D4_Pattern3.L2.L3.val3                   # 21
d4_9 = D4_Pattern3.L2.L3.L4.val4                # 42

# Test 4.4: Middle level defined last
D4_Pattern4 := [GGG].{
    val1 = 1

    L2 := [HHH].{
        L3 := [III].{
            val3 = val1 + val2                  # Forward ref to val2 (unqualified)

            L4 := [JJJ].{
                val4 = val3 + val2              # Unqualified refs
            }
        }

        val2 = 5                                # Defined after L3 and L4 but they ref it
    }
}
d4_10 = D4_Pattern4.L2.L3.val3                  # 6
d4_11 = D4_Pattern4.L2.L3.L4.val4               # 11

# ============================================================================
# DEPTH 5: Five levels - ultimate nesting test
# ============================================================================

# Test 5.1: Deepest refs all ancestors unqualified
D5_Pattern1 := [KKK].{
    val1 = 1

    L2 := [LLL].{
        val2 = 2

        L3 := [MMM].{
            val3 = 3

            L4 := [NNN].{
                val4 = 4

                L5 := [OOO].{
                    val5 = val1 + val2 + val3 + val4    # All unqualified
                }
            }
        }
    }
}
d5_1 = D5_Pattern1.L2.L3.L4.L5.val5             # 10

# Test 5.2: L5 val defined first, everyone refs it
D5_Pattern2 := [PPP].{
    L2 := [QQQ].{
        L3 := [RRR].{
            L4 := [SSS].{
                L5 := [TTT].{
                    val5 = 999
                }

                val4 = D5_Pattern2.L2.L3.L4.L5.val5 + 1    # Qualified
            }

            val3 = val4 + 1                                 # Unqualified
        }

        val2 = D5_Pattern2.L2.L3.val3 + 1                  # Qualified
    }

    val1 = val2 + 1                                         # Unqualified
}
d5_2 = D5_Pattern2.L2.L3.L4.L5.val5             # 999
d5_3 = D5_Pattern2.L2.L3.L4.val4                # 1000
d5_4 = D5_Pattern2.L2.L3.val3                   # 1001
d5_5 = D5_Pattern2.L2.val2                      # 1002
d5_6 = D5_Pattern2.val1                         # 1003

# Test 5.3: Random interleaving at 5 levels
D5_Pattern3 := [UUU].{
    val1 = D5_Pattern3.L2.L3.val3 + 10          # Forward ref

    L2 := [VVV].{
        L3 := [WWW].{
            val3 = 5

            L4 := [XXX].{
                L5 := [YYY].{
                    val5 = val1 + val2 + val3 + val4        # Mixed refs
                }

                val4 = val3 * 2                             # Unqualified
            }
        }

        val2 = D5_Pattern3.L2.L3.L4.val4 + 1               # Forward ref (qualified)
    }
}
d5_7 = D5_Pattern3.val1                         # 15 (5 + 10)
d5_8 = D5_Pattern3.L2.L3.val3                   # 5
d5_9 = D5_Pattern3.L2.L3.L4.val4                # 10 (5 * 2)
d5_10 = D5_Pattern3.L2.val2                     # 11 (10 + 1)
d5_11 = D5_Pattern3.L2.L3.L4.L5.val5            # 41 (15 + 11 + 5 + 10)

# ============================================================================
# EDGE CASES: Name shadowing and scoping boundaries
# ============================================================================

# Test: Same name at different levels - each should be distinct
Shadowing := [ZZZ].{
    val = 1

    L2 := [AAAA].{
        val = 2                                 # Shadows outer val in this scope

        useOuter = Shadowing.val                # Must qualify to get outer
        useLocal = val                          # Unqualified gets local

        L3 := [BBBB].{
            val = 3                             # Shadows both

            useL1 = Shadowing.val               # L1's val
            useL2 = Shadowing.L2.val            # L2's val
            useL3 = val                         # L3's val (unqualified)
        }
    }
}
shadow1 = Shadowing.val                         # 1
shadow2 = Shadowing.L2.val                      # 2
shadow3 = Shadowing.L2.useOuter                 # 1
shadow4 = Shadowing.L2.useLocal                 # 2
shadow5 = Shadowing.L2.L3.val                   # 3
shadow6 = Shadowing.L2.L3.useL1                 # 1
shadow7 = Shadowing.L2.L3.useL2                 # 2
shadow8 = Shadowing.L2.L3.useL3                 # 3

# Test: External code cannot use unqualified names from associated blocks
External1 := [CCCC].{
    hidden = 777
}
# Cannot use "hidden" here - must use External1.hidden
external1 = External1.hidden                    # 777

# Test: Type annotations with all patterns
Annotated := [DDDD].{
    typed : U64
    typed = 888

    L2 := [EEEE].{
        alsoTyped : U64
        alsoTyped = typed + 1                   # Unqualified ref to outer
    }
}
anno1 = Annotated.typed                         # 888
anno2 = Annotated.L2.alsoTyped                  # 889
~~~
# EXPECTED
UNDEFINED VARIABLE - associated_items_truly_comprehensive.md:382:20:382:24
UNUSED VARIABLE - associated_items_truly_comprehensive.md:382:20:382:24
UNDEFINED VARIABLE - associated_items_truly_comprehensive.md:388:12:388:16
UNUSED VARIABLE - associated_items_truly_comprehensive.md:388:12:388:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `val4` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_truly_comprehensive.md:382:20:382:24:**
```roc
            val3 = val4 + 1                                 # Unqualified
```
                   ^^^^


**UNUSED VARIABLE**
Variable `val4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_val4` to suppress this warning.
The unused variable is declared here:
**associated_items_truly_comprehensive.md:382:20:382:24:**
```roc
            val3 = val4 + 1                                 # Unqualified
```
                   ^^^^


**UNDEFINED VARIABLE**
Nothing is named `val2` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_truly_comprehensive.md:388:12:388:16:**
```roc
    val1 = val2 + 1                                         # Unqualified
```
           ^^^^


**UNUSED VARIABLE**
Variable `val2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_val2` to suppress this warning.
The unused variable is declared here:
**associated_items_truly_comprehensive.md:388:12:388:16:**
```roc
    val1 = val2 + 1                                         # Unqualified
```
           ^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "D1_Forward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-decl
					(p-ident (raw "first"))
					(e-ident (raw "second")))
				(s-decl
					(p-ident (raw "second"))
					(e-int (raw "100")))))
		(s-decl
			(p-ident (raw "d1_1"))
			(e-ident (raw "D1_Forward.first")))
		(s-decl
			(p-ident (raw "d1_2"))
			(e-ident (raw "D1_Forward.second")))
		(s-type-decl
			(header (name "D1_Backward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "B"))))
			(associated
				(s-decl
					(p-ident (raw "first"))
					(e-int (raw "200")))
				(s-decl
					(p-ident (raw "second"))
					(e-ident (raw "first")))))
		(s-decl
			(p-ident (raw "d1_3"))
			(e-ident (raw "D1_Backward.first")))
		(s-decl
			(p-ident (raw "d1_4"))
			(e-ident (raw "D1_Backward.second")))
		(s-type-decl
			(header (name "D1_Multi")
				(args))
			(ty-tag-union
				(tags
					(ty (name "C"))))
			(associated
				(s-decl
					(p-ident (raw "a"))
					(e-binop (op "+")
						(e-ident (raw "b"))
						(e-ident (raw "c"))))
				(s-decl
					(p-ident (raw "b"))
					(e-int (raw "10")))
				(s-decl
					(p-ident (raw "c"))
					(e-binop (op "+")
						(e-ident (raw "d"))
						(e-int (raw "5"))))
				(s-decl
					(p-ident (raw "d"))
					(e-int (raw "20")))))
		(s-decl
			(p-ident (raw "d1_5"))
			(e-ident (raw "D1_Multi.a")))
		(s-decl
			(p-ident (raw "d1_6"))
			(e-ident (raw "D1_Multi.b")))
		(s-decl
			(p-ident (raw "d1_7"))
			(e-ident (raw "D1_Multi.c")))
		(s-decl
			(p-ident (raw "d1_8"))
			(e-ident (raw "D1_Multi.d")))
		(s-type-decl
			(header (name "D2_InnerFirst_Qual")
				(args))
			(ty-tag-union
				(tags
					(ty (name "D"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "E"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-ident (raw "D2_InnerFirst_Qual.outerVal")))))
				(s-decl
					(p-ident (raw "outerVal"))
					(e-int (raw "42")))))
		(s-decl
			(p-ident (raw "d2_1"))
			(e-ident (raw "D2_InnerFirst_Qual.outerVal")))
		(s-decl
			(p-ident (raw "d2_2"))
			(e-ident (raw "D2_InnerFirst_Qual.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_InnerFirst_Unqual")
				(args))
			(ty-tag-union
				(tags
					(ty (name "F"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "G"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-ident (raw "outerVal")))))
				(s-decl
					(p-ident (raw "outerVal"))
					(e-int (raw "43")))))
		(s-decl
			(p-ident (raw "d2_3"))
			(e-ident (raw "D2_InnerFirst_Unqual.outerVal")))
		(s-decl
			(p-ident (raw "d2_4"))
			(e-ident (raw "D2_InnerFirst_Unqual.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_InnerAfter_Qual")
				(args))
			(ty-tag-union
				(tags
					(ty (name "H"))))
			(associated
				(s-decl
					(p-ident (raw "outerVal"))
					(e-int (raw "44")))
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "I"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-ident (raw "D2_InnerAfter_Qual.outerVal")))))))
		(s-decl
			(p-ident (raw "d2_5"))
			(e-ident (raw "D2_InnerAfter_Qual.outerVal")))
		(s-decl
			(p-ident (raw "d2_6"))
			(e-ident (raw "D2_InnerAfter_Qual.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_InnerAfter_Unqual")
				(args))
			(ty-tag-union
				(tags
					(ty (name "J"))))
			(associated
				(s-decl
					(p-ident (raw "outerVal"))
					(e-int (raw "45")))
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "K"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-ident (raw "outerVal")))))))
		(s-decl
			(p-ident (raw "d2_7"))
			(e-ident (raw "D2_InnerAfter_Unqual.outerVal")))
		(s-decl
			(p-ident (raw "d2_8"))
			(e-ident (raw "D2_InnerAfter_Unqual.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_OuterRefsInner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "L"))))
			(associated
				(s-decl
					(p-ident (raw "outerVal"))
					(e-ident (raw "D2_OuterRefsInner.Inner.innerVal")))
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "M"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-int (raw "46")))))))
		(s-decl
			(p-ident (raw "d2_9"))
			(e-ident (raw "D2_OuterRefsInner.outerVal")))
		(s-decl
			(p-ident (raw "d2_10"))
			(e-ident (raw "D2_OuterRefsInner.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_OuterRefsInner_Back")
				(args))
			(ty-tag-union
				(tags
					(ty (name "N"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "O"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-int (raw "47")))))
				(s-decl
					(p-ident (raw "outerVal"))
					(e-ident (raw "D2_OuterRefsInner_Back.Inner.innerVal")))))
		(s-decl
			(p-ident (raw "d2_11"))
			(e-ident (raw "D2_OuterRefsInner_Back.outerVal")))
		(s-decl
			(p-ident (raw "d2_12"))
			(e-ident (raw "D2_OuterRefsInner_Back.Inner.innerVal")))
		(s-type-decl
			(header (name "D2_Siblings_Forward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P"))))
			(associated
				(s-type-decl
					(header (name "InnerA")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Q"))))
					(associated
						(s-decl
							(p-ident (raw "valA"))
							(e-binop (op "+")
								(e-ident (raw "D2_Siblings_Forward.InnerB.valB"))
								(e-int (raw "1"))))))
				(s-type-decl
					(header (name "InnerB")
						(args))
					(ty-tag-union
						(tags
							(ty (name "R"))))
					(associated
						(s-decl
							(p-ident (raw "valB"))
							(e-int (raw "48")))))))
		(s-decl
			(p-ident (raw "d2_13"))
			(e-ident (raw "D2_Siblings_Forward.InnerA.valA")))
		(s-decl
			(p-ident (raw "d2_14"))
			(e-ident (raw "D2_Siblings_Forward.InnerB.valB")))
		(s-type-decl
			(header (name "D2_Siblings_Backward")
				(args))
			(ty-tag-union
				(tags
					(ty (name "S"))))
			(associated
				(s-type-decl
					(header (name "InnerA")
						(args))
					(ty-tag-union
						(tags
							(ty (name "T"))))
					(associated
						(s-decl
							(p-ident (raw "valA"))
							(e-int (raw "50")))))
				(s-type-decl
					(header (name "InnerB")
						(args))
					(ty-tag-union
						(tags
							(ty (name "U"))))
					(associated
						(s-decl
							(p-ident (raw "valB"))
							(e-binop (op "+")
								(e-ident (raw "D2_Siblings_Backward.InnerA.valA"))
								(e-int (raw "1"))))))))
		(s-decl
			(p-ident (raw "d2_15"))
			(e-ident (raw "D2_Siblings_Backward.InnerA.valA")))
		(s-decl
			(p-ident (raw "d2_16"))
			(e-ident (raw "D2_Siblings_Backward.InnerB.valB")))
		(s-type-decl
			(header (name "D2_Interleaved")
				(args))
			(ty-tag-union
				(tags
					(ty (name "V"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "+")
						(e-ident (raw "D2_Interleaved.Inner.innerVal"))
						(e-int (raw "10"))))
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "W"))))
					(associated
						(s-decl
							(p-ident (raw "innerVal"))
							(e-binop (op "+")
								(e-ident (raw "val2"))
								(e-int (raw "5"))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-int (raw "20")))))
		(s-decl
			(p-ident (raw "d2_17"))
			(e-ident (raw "D2_Interleaved.val1")))
		(s-decl
			(p-ident (raw "d2_18"))
			(e-ident (raw "D2_Interleaved.Inner.innerVal")))
		(s-decl
			(p-ident (raw "d2_19"))
			(e-ident (raw "D2_Interleaved.val2")))
		(s-type-decl
			(header (name "D3_Pattern1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "X"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "100")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Y"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "200")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "Z"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val1"))
										(e-ident (raw "val2"))))))))))
		(s-decl
			(p-ident (raw "d3_1"))
			(e-ident (raw "D3_Pattern1.val1")))
		(s-decl
			(p-ident (raw "d3_2"))
			(e-ident (raw "D3_Pattern1.L2.val2")))
		(s-decl
			(p-ident (raw "d3_3"))
			(e-ident (raw "D3_Pattern1.L2.L3.val3")))
		(s-type-decl
			(header (name "D3_Pattern2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AA"))))
			(associated
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BB"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "CC"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val2"))
										(e-ident (raw "val1"))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "+")
								(e-ident (raw "D3_Pattern2.L2.L3.val3"))
								(e-int (raw "10"))))))
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "+")
						(e-ident (raw "D3_Pattern2.L2.val2"))
						(e-int (raw "5"))))))
		(s-type-decl
			(header (name "D3_Pattern3")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DD"))))
			(associated
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "EE"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "FF"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-int (raw "1000")))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "*")
								(e-ident (raw "D3_Pattern3.L2.L3.val3"))
								(e-int (raw "2"))))))
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "*")
						(e-ident (raw "D3_Pattern3.L2.val2"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "d3_4"))
			(e-ident (raw "D3_Pattern3.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d3_5"))
			(e-ident (raw "D3_Pattern3.L2.val2")))
		(s-decl
			(p-ident (raw "d3_6"))
			(e-ident (raw "D3_Pattern3.val1")))
		(s-type-decl
			(header (name "D3_Pattern4")
				(args))
			(ty-tag-union
				(tags
					(ty (name "GG"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "5")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "HH"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "II"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "*")
										(e-ident (raw "val1"))
										(e-int (raw "10"))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "+")
								(e-ident (raw "D3_Pattern4.L2.L3.val3"))
								(e-ident (raw "val1"))))))))
		(s-decl
			(p-ident (raw "d3_7"))
			(e-ident (raw "D3_Pattern4.val1")))
		(s-decl
			(p-ident (raw "d3_8"))
			(e-ident (raw "D3_Pattern4.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d3_9"))
			(e-ident (raw "D3_Pattern4.L2.val2")))
		(s-type-decl
			(header (name "D3_Pattern5")
				(args))
			(ty-tag-union
				(tags
					(ty (name "JJ"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "KK"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "2")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "LL"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val1"))
										(e-ident (raw "val2"))))))))))
		(s-decl
			(p-ident (raw "d3_10"))
			(e-ident (raw "D3_Pattern5.L2.L3.val3")))
		(s-type-decl
			(header (name "D3_Pattern6")
				(args))
			(ty-tag-union
				(tags
					(ty (name "MM"))))
			(associated
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "NN"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "OO"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val2"))
										(e-ident (raw "val1"))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "*")
								(e-ident (raw "val1"))
								(e-int (raw "2"))))))
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "7")))))
		(s-decl
			(p-ident (raw "d3_11"))
			(e-ident (raw "D3_Pattern6.val1")))
		(s-decl
			(p-ident (raw "d3_12"))
			(e-ident (raw "D3_Pattern6.L2.val2")))
		(s-decl
			(p-ident (raw "d3_13"))
			(e-ident (raw "D3_Pattern6.L2.L3.val3")))
		(s-type-decl
			(header (name "D3_Siblings")
				(args))
			(ty-tag-union
				(tags
					(ty (name "PP"))))
			(associated
				(s-type-decl
					(header (name "L2a")
						(args))
					(ty-tag-union
						(tags
							(ty (name "QQ"))))
					(associated
						(s-decl
							(p-ident (raw "val2a"))
							(e-int (raw "10")))
						(s-type-decl
							(header (name "L3a")
								(args))
							(ty-tag-union
								(tags
									(ty (name "RR"))))
							(associated
								(s-decl
									(p-ident (raw "val3a"))
									(e-ident (raw "D3_Siblings.L2b.val2b")))))))
				(s-type-decl
					(header (name "L2b")
						(args))
					(ty-tag-union
						(tags
							(ty (name "SS"))))
					(associated
						(s-decl
							(p-ident (raw "val2b"))
							(e-int (raw "20")))
						(s-type-decl
							(header (name "L3b")
								(args))
							(ty-tag-union
								(tags
									(ty (name "TT"))))
							(associated
								(s-decl
									(p-ident (raw "val3b"))
									(e-ident (raw "D3_Siblings.L2a.L3a.val3a")))))))))
		(s-decl
			(p-ident (raw "d3_14"))
			(e-ident (raw "D3_Siblings.L2a.L3a.val3a")))
		(s-decl
			(p-ident (raw "d3_15"))
			(e-ident (raw "D3_Siblings.L2b.L3b.val3b")))
		(s-type-decl
			(header (name "D4_Pattern1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "UU"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "VV"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "2")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "WW"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-int (raw "3")))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "XX"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "+")
												(e-binop (op "+")
													(e-ident (raw "val1"))
													(e-ident (raw "val2")))
												(e-ident (raw "val3"))))))))))))
		(s-decl
			(p-ident (raw "d4_1"))
			(e-ident (raw "D4_Pattern1.L2.L3.L4.val4")))
		(s-type-decl
			(header (name "D4_Pattern2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "YY"))))
			(associated
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "ZZ"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "AAA"))))
							(associated
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "BBB"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-int (raw "100")))))
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "*")
										(e-ident (raw "D4_Pattern2.L2.L3.L4.val4"))
										(e-int (raw "2"))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "*")
								(e-ident (raw "D4_Pattern2.L2.L3.val3"))
								(e-int (raw "2"))))))
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "*")
						(e-ident (raw "D4_Pattern2.L2.val2"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "d4_2"))
			(e-ident (raw "D4_Pattern2.L2.L3.L4.val4")))
		(s-decl
			(p-ident (raw "d4_3"))
			(e-ident (raw "D4_Pattern2.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d4_4"))
			(e-ident (raw "D4_Pattern2.L2.val2")))
		(s-decl
			(p-ident (raw "d4_5"))
			(e-ident (raw "D4_Pattern2.val1")))
		(s-type-decl
			(header (name "D4_Pattern3")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CCC"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "+")
						(e-ident (raw "D4_Pattern3.L2.val2"))
						(e-int (raw "1"))))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "DDD"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "10")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "EEE"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val1"))
										(e-ident (raw "val2"))))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "FFF"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "*")
												(e-ident (raw "val3"))
												(e-int (raw "2"))))))))))))
		(s-decl
			(p-ident (raw "d4_6"))
			(e-ident (raw "D4_Pattern3.val1")))
		(s-decl
			(p-ident (raw "d4_7"))
			(e-ident (raw "D4_Pattern3.L2.val2")))
		(s-decl
			(p-ident (raw "d4_8"))
			(e-ident (raw "D4_Pattern3.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d4_9"))
			(e-ident (raw "D4_Pattern3.L2.L3.L4.val4")))
		(s-type-decl
			(header (name "D4_Pattern4")
				(args))
			(ty-tag-union
				(tags
					(ty (name "GGG"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "HHH"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "III"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val1"))
										(e-ident (raw "val2"))))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "JJJ"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "+")
												(e-ident (raw "val3"))
												(e-ident (raw "val2"))))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "5")))))))
		(s-decl
			(p-ident (raw "d4_10"))
			(e-ident (raw "D4_Pattern4.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d4_11"))
			(e-ident (raw "D4_Pattern4.L2.L3.L4.val4")))
		(s-type-decl
			(header (name "D5_Pattern1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "KKK"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "LLL"))))
					(associated
						(s-decl
							(p-ident (raw "val2"))
							(e-int (raw "2")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "MMM"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-int (raw "3")))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "NNN"))))
									(associated
										(s-decl
											(p-ident (raw "val4"))
											(e-int (raw "4")))
										(s-type-decl
											(header (name "L5")
												(args))
											(ty-tag-union
												(tags
													(ty (name "OOO"))))
											(associated
												(s-decl
													(p-ident (raw "val5"))
													(e-binop (op "+")
														(e-binop (op "+")
															(e-binop (op "+")
																(e-ident (raw "val1"))
																(e-ident (raw "val2")))
															(e-ident (raw "val3")))
														(e-ident (raw "val4"))))))))))))))
		(s-decl
			(p-ident (raw "d5_1"))
			(e-ident (raw "D5_Pattern1.L2.L3.L4.L5.val5")))
		(s-type-decl
			(header (name "D5_Pattern2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "PPP"))))
			(associated
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "QQQ"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "RRR"))))
							(associated
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "SSS"))))
									(associated
										(s-type-decl
											(header (name "L5")
												(args))
											(ty-tag-union
												(tags
													(ty (name "TTT"))))
											(associated
												(s-decl
													(p-ident (raw "val5"))
													(e-int (raw "999")))))
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "+")
												(e-ident (raw "D5_Pattern2.L2.L3.L4.L5.val5"))
												(e-int (raw "1"))))))
								(s-decl
									(p-ident (raw "val3"))
									(e-binop (op "+")
										(e-ident (raw "val4"))
										(e-int (raw "1"))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "+")
								(e-ident (raw "D5_Pattern2.L2.L3.val3"))
								(e-int (raw "1"))))))
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "+")
						(e-ident (raw "val2"))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "d5_2"))
			(e-ident (raw "D5_Pattern2.L2.L3.L4.L5.val5")))
		(s-decl
			(p-ident (raw "d5_3"))
			(e-ident (raw "D5_Pattern2.L2.L3.L4.val4")))
		(s-decl
			(p-ident (raw "d5_4"))
			(e-ident (raw "D5_Pattern2.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d5_5"))
			(e-ident (raw "D5_Pattern2.L2.val2")))
		(s-decl
			(p-ident (raw "d5_6"))
			(e-ident (raw "D5_Pattern2.val1")))
		(s-type-decl
			(header (name "D5_Pattern3")
				(args))
			(ty-tag-union
				(tags
					(ty (name "UUU"))))
			(associated
				(s-decl
					(p-ident (raw "val1"))
					(e-binop (op "+")
						(e-ident (raw "D5_Pattern3.L2.L3.val3"))
						(e-int (raw "10"))))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "VVV"))))
					(associated
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "WWW"))))
							(associated
								(s-decl
									(p-ident (raw "val3"))
									(e-int (raw "5")))
								(s-type-decl
									(header (name "L4")
										(args))
									(ty-tag-union
										(tags
											(ty (name "XXX"))))
									(associated
										(s-type-decl
											(header (name "L5")
												(args))
											(ty-tag-union
												(tags
													(ty (name "YYY"))))
											(associated
												(s-decl
													(p-ident (raw "val5"))
													(e-binop (op "+")
														(e-binop (op "+")
															(e-binop (op "+")
																(e-ident (raw "val1"))
																(e-ident (raw "val2")))
															(e-ident (raw "val3")))
														(e-ident (raw "val4"))))))
										(s-decl
											(p-ident (raw "val4"))
											(e-binop (op "*")
												(e-ident (raw "val3"))
												(e-int (raw "2"))))))))
						(s-decl
							(p-ident (raw "val2"))
							(e-binop (op "+")
								(e-ident (raw "D5_Pattern3.L2.L3.L4.val4"))
								(e-int (raw "1"))))))))
		(s-decl
			(p-ident (raw "d5_7"))
			(e-ident (raw "D5_Pattern3.val1")))
		(s-decl
			(p-ident (raw "d5_8"))
			(e-ident (raw "D5_Pattern3.L2.L3.val3")))
		(s-decl
			(p-ident (raw "d5_9"))
			(e-ident (raw "D5_Pattern3.L2.L3.L4.val4")))
		(s-decl
			(p-ident (raw "d5_10"))
			(e-ident (raw "D5_Pattern3.L2.val2")))
		(s-decl
			(p-ident (raw "d5_11"))
			(e-ident (raw "D5_Pattern3.L2.L3.L4.L5.val5")))
		(s-type-decl
			(header (name "Shadowing")
				(args))
			(ty-tag-union
				(tags
					(ty (name "ZZZ"))))
			(associated
				(s-decl
					(p-ident (raw "val"))
					(e-int (raw "1")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AAAA"))))
					(associated
						(s-decl
							(p-ident (raw "val"))
							(e-int (raw "2")))
						(s-decl
							(p-ident (raw "useOuter"))
							(e-ident (raw "Shadowing.val")))
						(s-decl
							(p-ident (raw "useLocal"))
							(e-ident (raw "val")))
						(s-type-decl
							(header (name "L3")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BBBB"))))
							(associated
								(s-decl
									(p-ident (raw "val"))
									(e-int (raw "3")))
								(s-decl
									(p-ident (raw "useL1"))
									(e-ident (raw "Shadowing.val")))
								(s-decl
									(p-ident (raw "useL2"))
									(e-ident (raw "Shadowing.L2.val")))
								(s-decl
									(p-ident (raw "useL3"))
									(e-ident (raw "val")))))))))
		(s-decl
			(p-ident (raw "shadow1"))
			(e-ident (raw "Shadowing.val")))
		(s-decl
			(p-ident (raw "shadow2"))
			(e-ident (raw "Shadowing.L2.val")))
		(s-decl
			(p-ident (raw "shadow3"))
			(e-ident (raw "Shadowing.L2.useOuter")))
		(s-decl
			(p-ident (raw "shadow4"))
			(e-ident (raw "Shadowing.L2.useLocal")))
		(s-decl
			(p-ident (raw "shadow5"))
			(e-ident (raw "Shadowing.L2.L3.val")))
		(s-decl
			(p-ident (raw "shadow6"))
			(e-ident (raw "Shadowing.L2.L3.useL1")))
		(s-decl
			(p-ident (raw "shadow7"))
			(e-ident (raw "Shadowing.L2.L3.useL2")))
		(s-decl
			(p-ident (raw "shadow8"))
			(e-ident (raw "Shadowing.L2.L3.useL3")))
		(s-type-decl
			(header (name "External1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CCCC"))))
			(associated
				(s-decl
					(p-ident (raw "hidden"))
					(e-int (raw "777")))))
		(s-decl
			(p-ident (raw "external1"))
			(e-ident (raw "External1.hidden")))
		(s-type-decl
			(header (name "Annotated")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DDDD"))))
			(associated
				(s-type-anno (name "typed")
					(ty (name "U64")))
				(s-decl
					(p-ident (raw "typed"))
					(e-int (raw "888")))
				(s-type-decl
					(header (name "L2")
						(args))
					(ty-tag-union
						(tags
							(ty (name "EEEE"))))
					(associated
						(s-type-anno (name "alsoTyped")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "alsoTyped"))
							(e-binop (op "+")
								(e-ident (raw "typed"))
								(e-int (raw "1"))))))))
		(s-decl
			(p-ident (raw "anno1"))
			(e-ident (raw "Annotated.typed")))
		(s-decl
			(p-ident (raw "anno2"))
			(e-ident (raw "Annotated.L2.alsoTyped")))))
~~~
# FORMATTED
~~~roc
# ============================================================================
# DEPTH 1: All combinations at single level
# ============================================================================

# Test 1.1: Simple forward reference
D1_Forward := [A].{
	first = second
	second = 100
}
d1_1 = D1_Forward.first # 100
d1_2 = D1_Forward.second # 100

# Test 1.2: Simple backward reference
D1_Backward := [B].{
	first = 200
	second = first
}
d1_3 = D1_Backward.first # 200
d1_4 = D1_Backward.second # 200

# Test 1.3: Multiple items - all orderings
D1_Multi := [C].{
	a = b + c
	b = 10
	c = d + 5
	d = 20
}
d1_5 = D1_Multi.a # 35
d1_6 = D1_Multi.b # 10
d1_7 = D1_Multi.c # 25
d1_8 = D1_Multi.d # 20

# ============================================================================
# DEPTH 2: Nested types - all access pattern combinations
# ============================================================================

# Test 2.1: Inner defined BEFORE outer value, inner refs outer (qualified)
D2_InnerFirst_Qual := [D].{
	Inner := [E].{
		innerVal = D2_InnerFirst_Qual.outerVal
	}
	outerVal = 42
}
d2_1 = D2_InnerFirst_Qual.outerVal # 42
d2_2 = D2_InnerFirst_Qual.Inner.innerVal # 42

# Test 2.2: Inner defined BEFORE outer value, inner refs outer (unqualified)
D2_InnerFirst_Unqual := [F].{
	Inner := [G].{
		innerVal = outerVal
	}
	outerVal = 43
}
d2_3 = D2_InnerFirst_Unqual.outerVal # 43
d2_4 = D2_InnerFirst_Unqual.Inner.innerVal # 43

# Test 2.3: Inner defined AFTER outer value, inner refs outer (qualified)
D2_InnerAfter_Qual := [H].{
	outerVal = 44
	Inner := [I].{
		innerVal = D2_InnerAfter_Qual.outerVal
	}
}
d2_5 = D2_InnerAfter_Qual.outerVal # 44
d2_6 = D2_InnerAfter_Qual.Inner.innerVal # 44

# Test 2.4: Inner defined AFTER outer value, inner refs outer (unqualified)
D2_InnerAfter_Unqual := [J].{
	outerVal = 45
	Inner := [K].{
		innerVal = outerVal
	}
}
d2_7 = D2_InnerAfter_Unqual.outerVal # 45
d2_8 = D2_InnerAfter_Unqual.Inner.innerVal # 45

# Test 2.5: Outer refs inner (must be qualified, must be forward ref)
D2_OuterRefsInner := [L].{
	outerVal = D2_OuterRefsInner.Inner.innerVal
	Inner := [M].{
		innerVal = 46
	}
}
d2_9 = D2_OuterRefsInner.outerVal # 46
d2_10 = D2_OuterRefsInner.Inner.innerVal # 46

# Test 2.6: Outer refs inner (backward ref)
D2_OuterRefsInner_Back := [N].{
	Inner := [O].{
		innerVal = 47
	}
	outerVal = D2_OuterRefsInner_Back.Inner.innerVal
}
d2_11 = D2_OuterRefsInner_Back.outerVal # 47
d2_12 = D2_OuterRefsInner_Back.Inner.innerVal # 47

# Test 2.7: Sibling nested types referencing each other - A refs B forward
D2_Siblings_Forward := [P].{
	InnerA := [Q].{
		valA = D2_Siblings_Forward.InnerB.valB + 1
	}
	InnerB := [R].{
		valB = 48
	}
}
d2_13 = D2_Siblings_Forward.InnerA.valA # 49
d2_14 = D2_Siblings_Forward.InnerB.valB # 48

# Test 2.8: Sibling nested types referencing each other - A refs B backward
D2_Siblings_Backward := [S].{
	InnerA := [T].{
		valA = 50
	}
	InnerB := [U].{
		valB = D2_Siblings_Backward.InnerA.valA + 1
	}
}
d2_15 = D2_Siblings_Backward.InnerA.valA # 50
d2_16 = D2_Siblings_Backward.InnerB.valB # 51

# Test 2.9: Outer, Inner, outer val - complex interleaving
D2_Interleaved := [V].{
	val1 = D2_Interleaved.Inner.innerVal + 10
	Inner := [W].{
		innerVal = val2 + 5
	}
	val2 = 20
}
d2_17 = D2_Interleaved.val1 # 35 (25 + 10)
d2_18 = D2_Interleaved.Inner.innerVal # 25 (20 + 5)
d2_19 = D2_Interleaved.val2 # 20

# ============================================================================
# DEPTH 3: Three levels - every ordering combination
# ============================================================================

# Test 3.1: L1 val, L2, L2 val, L3, L3 val - L3 refs L1 (unqualified)
D3_Pattern1 := [X].{
	val1 = 100
	L2 := [Y].{
		val2 = 200
		L3 := [Z].{
			val3 = val1 + val2
		}
	}
}
d3_1 = D3_Pattern1.val1 # 100
d3_2 = D3_Pattern1.L2.val2 # 200
d3_3 = D3_Pattern1.L2.L3.val3 # 300

# Test 3.2: L2, L3, L3 val, L2 val, L1 val - all forward refs
D3_Pattern2 := [AA].{
	L2 := [BB].{
		L3 := [CC].{
			val3 = val2 + val1
		}
		val2 = D3_Pattern2.L2.L3.val3 + 10
	}
	val1 = D3_Pattern2.L2.val2 + 5
}
# This creates circular dependency: val3 = val2 + val1, val2 = val3 + 10, val1 = val2 + 5
# Should either resolve or error gracefully

# Test 3.3: L3 val defined, then L2 val, then L1 val - L2 refs L3, L1 refs L2
D3_Pattern3 := [DD].{
	L2 := [EE].{
		L3 := [FF].{
			val3 = 1000
		}
		val2 = D3_Pattern3.L2.L3.val3 * 2
	}
	val1 = D3_Pattern3.L2.val2 * 2
}
d3_4 = D3_Pattern3.L2.L3.val3 # 1000
d3_5 = D3_Pattern3.L2.val2 # 2000
d3_6 = D3_Pattern3.val1 # 4000

# Test 3.4: L1 val, L3, L2 (out of order), L3 val, L2 val
D3_Pattern4 := [GG].{
	val1 = 5
	L2 := [HH].{
		L3 := [II].{
			val3 = val1 * 10
		}
		val2 = D3_Pattern4.L2.L3.val3 + val1
	}
}
d3_7 = D3_Pattern4.val1 # 5
d3_8 = D3_Pattern4.L2.L3.val3 # 50
d3_9 = D3_Pattern4.L2.val2 # 55

# Test 3.5: All vals before all types
D3_Pattern5 := [JJ].{
	val1 = 1
	L2 := [KK].{
		val2 = 2
		L3 := [LL].{
			val3 = val1 + val2
		}
	}
}
d3_10 = D3_Pattern5.L2.L3.val3 # 3

# Test 3.6: All types before all vals
D3_Pattern6 := [MM].{
	L2 := [NN].{
		L3 := [OO].{
			val3 = val2 + val1
		}
		val2 = val1 * 2
	}
	val1 = 7
}
d3_11 = D3_Pattern6.val1 # 7
d3_12 = D3_Pattern6.L2.val2 # 14
d3_13 = D3_Pattern6.L2.L3.val3 # 21

# Test 3.7: Sibling access at L2 - L2a and L2b both exist
D3_Siblings := [PP].{
	L2a := [QQ].{
		val2a = 10
		L3a := [RR].{
			val3a = D3_Siblings.L2b.val2b
		}
	}
	L2b := [SS].{
		val2b = 20
		L3b := [TT].{
			val3b = D3_Siblings.L2a.L3a.val3a
		}
	}
}
d3_14 = D3_Siblings.L2a.L3a.val3a # 20
d3_15 = D3_Siblings.L2b.L3b.val3b # 20

# ============================================================================
# DEPTH 4: Four levels - selected critical patterns
# ============================================================================

# Test 4.1: Deepest level refs all ancestors (unqualified)
D4_Pattern1 := [UU].{
	val1 = 1
	L2 := [VV].{
		val2 = 2
		L3 := [WW].{
			val3 = 3
			L4 := [XX].{
				val4 = val1 + val2 + val3
			}
		}
	}
}
d4_1 = D4_Pattern1.L2.L3.L4.val4 # 6

# Test 4.2: L4 val defined first, rest reference it
D4_Pattern2 := [YY].{
	L2 := [ZZ].{
		L3 := [AAA].{
			L4 := [BBB].{
				val4 = 100
			}
			val3 = D4_Pattern2.L2.L3.L4.val4 * 2
		}
		val2 = D4_Pattern2.L2.L3.val3 * 2
	}
	val1 = D4_Pattern2.L2.val2 * 2
}
d4_2 = D4_Pattern2.L2.L3.L4.val4 # 100
d4_3 = D4_Pattern2.L2.L3.val3 # 200
d4_4 = D4_Pattern2.L2.val2 # 400
d4_5 = D4_Pattern2.val1 # 800

# Test 4.3: Alternating defined/undefined as we go deeper
D4_Pattern3 := [CCC].{
	val1 = D4_Pattern3.L2.val2 + 1
	L2 := [DDD].{
		val2 = 10
		L3 := [EEE].{
			val3 = val1 + val2
			L4 := [FFF].{
				val4 = val3 * 2
			}
		}
	}
}
d4_6 = D4_Pattern3.val1 # 11
d4_7 = D4_Pattern3.L2.val2 # 10
d4_8 = D4_Pattern3.L2.L3.val3 # 21
d4_9 = D4_Pattern3.L2.L3.L4.val4 # 42

# Test 4.4: Middle level defined last
D4_Pattern4 := [GGG].{
	val1 = 1
	L2 := [HHH].{
		L3 := [III].{
			val3 = val1 + val2
			L4 := [JJJ].{
				val4 = val3 + val2
			}
		}
		val2 = 5
	}
}
d4_10 = D4_Pattern4.L2.L3.val3 # 6
d4_11 = D4_Pattern4.L2.L3.L4.val4 # 11

# ============================================================================
# DEPTH 5: Five levels - ultimate nesting test
# ============================================================================

# Test 5.1: Deepest refs all ancestors unqualified
D5_Pattern1 := [KKK].{
	val1 = 1
	L2 := [LLL].{
		val2 = 2
		L3 := [MMM].{
			val3 = 3
			L4 := [NNN].{
				val4 = 4
				L5 := [OOO].{
					val5 = val1 + val2 + val3 + val4
				}
			}
		}
	}
}
d5_1 = D5_Pattern1.L2.L3.L4.L5.val5 # 10

# Test 5.2: L5 val defined first, everyone refs it
D5_Pattern2 := [PPP].{
	L2 := [QQQ].{
		L3 := [RRR].{
			L4 := [SSS].{
				L5 := [TTT].{
					val5 = 999
				}
				val4 = D5_Pattern2.L2.L3.L4.L5.val5 + 1
			}
			val3 = val4 + 1
		}
		val2 = D5_Pattern2.L2.L3.val3 + 1
	}
	val1 = val2 + 1
}
d5_2 = D5_Pattern2.L2.L3.L4.L5.val5 # 999
d5_3 = D5_Pattern2.L2.L3.L4.val4 # 1000
d5_4 = D5_Pattern2.L2.L3.val3 # 1001
d5_5 = D5_Pattern2.L2.val2 # 1002
d5_6 = D5_Pattern2.val1 # 1003

# Test 5.3: Random interleaving at 5 levels
D5_Pattern3 := [UUU].{
	val1 = D5_Pattern3.L2.L3.val3 + 10
	L2 := [VVV].{
		L3 := [WWW].{
			val3 = 5
			L4 := [XXX].{
				L5 := [YYY].{
					val5 = val1 + val2 + val3 + val4
				}
				val4 = val3 * 2
			}
		}
		val2 = D5_Pattern3.L2.L3.L4.val4 + 1
	}
}
d5_7 = D5_Pattern3.val1 # 15 (5 + 10)
d5_8 = D5_Pattern3.L2.L3.val3 # 5
d5_9 = D5_Pattern3.L2.L3.L4.val4 # 10 (5 * 2)
d5_10 = D5_Pattern3.L2.val2 # 11 (10 + 1)
d5_11 = D5_Pattern3.L2.L3.L4.L5.val5 # 41 (15 + 11 + 5 + 10)

# ============================================================================
# EDGE CASES: Name shadowing and scoping boundaries
# ============================================================================

# Test: Same name at different levels - each should be distinct
Shadowing := [ZZZ].{
	val = 1
	L2 := [AAAA].{
		val = 2
		useOuter = Shadowing.val
		useLocal = val
		L3 := [BBBB].{
			val = 3
			useL1 = Shadowing.val
			useL2 = Shadowing.L2.val
			useL3 = val
		}
	}
}
shadow1 = Shadowing.val # 1
shadow2 = Shadowing.L2.val # 2
shadow3 = Shadowing.L2.useOuter # 1
shadow4 = Shadowing.L2.useLocal # 2
shadow5 = Shadowing.L2.L3.val # 3
shadow6 = Shadowing.L2.L3.useL1 # 1
shadow7 = Shadowing.L2.L3.useL2 # 2
shadow8 = Shadowing.L2.L3.useL3 # 3

# Test: External code cannot use unqualified names from associated blocks
External1 := [CCCC].{
	hidden = 777
}
# Cannot use "hidden" here - must use External1.hidden
external1 = External1.hidden # 777

# Test: Type annotations with all patterns
Annotated := [DDDD].{
	typed : U64
	typed = 888
	L2 := [EEEE].{
		alsoTyped : U64
		alsoTyped = typed + 1
	}
}
anno1 = Annotated.typed # 888
anno2 = Annotated.L2.alsoTyped # 889
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Forward.first"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Forward.second"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Forward.second"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Backward.first"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Backward.second"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Backward.first"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.a"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.b")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.c")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.b"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.c"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.d")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.d"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Qual.Inner.innerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Qual.outerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Qual.outerVal"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.Inner.innerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.outerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.outerVal"))
		(e-num (value "43")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Qual.Inner.innerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Qual.outerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Qual.outerVal"))
		(e-num (value "44")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.Inner.innerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.outerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.outerVal"))
		(e-num (value "45")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner.innerVal"))
		(e-num (value "46")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner.outerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner.innerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner.innerVal"))
		(e-num (value "47")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.outerVal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner.innerVal"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerA.valA"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB.valB")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB.valB"))
		(e-num (value "48")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA.valA"))
		(e-num (value "50")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerB.valB"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA.valA")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.Inner.innerVal"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.val2")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.val1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.Inner.innerVal")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.val2"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.val1")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.L2.val2")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.L2.val2"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.val1"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.L2.val2")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.val1")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.L2.L3.val3")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.val1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern2.L2.val2")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.L3.val3"))
		(e-num (value "1000")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.val2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.L3.val3")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.val1"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.val2")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.L2.L3.val3"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.val1")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.L2.L3.val3")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.val1")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.val1"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.val1")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.L2.val2")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.L2.val2"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.L2.val2")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.val1")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.L2.val2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.val1")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.val1"))
		(e-num (value "7")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a.val3a"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2b.val2b"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2a.val2a"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2b.L3b.val3b"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a.val3a"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2b.val2b"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.val1")))
				(e-lookup-local
					(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.val3")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.val3"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.val2"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4.val4"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.val3"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4.val4")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.val2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.val3")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.val1"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.val2")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.L4.val4"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.val3")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.val1")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.val2")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.val2"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.val1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.val2")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.val3")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.val2")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.val1")))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.val2")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.val2"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.val1")))
					(e-lookup-local
						(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.val4"))
		(e-num (value "4")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.val3"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.val2"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5.val5"))
		(e-num (value "999")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5.val5")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val4")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.val3")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.val1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val2")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.val1")))
					(e-lookup-local
						(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.val4"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.val3")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.val3"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.val4")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.val1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.val3")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.val"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.val"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.val"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.val"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.val"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.useOuter"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.val"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.useLocal"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.val"))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Shadowing.val"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.External1.hidden"))
		(e-num (value "777")))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Annotated.L2.alsoTyped"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "associated_items_truly_comprehensive.Annotated.typed")))
			(e-num (value "1")))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "associated_items_truly_comprehensive.Annotated.typed"))
		(e-num (value "888"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "d1_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Forward.first"))))
	(d-let
		(p-assign (ident "d1_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Forward.second"))))
	(d-let
		(p-assign (ident "d1_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Backward.first"))))
	(d-let
		(p-assign (ident "d1_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Backward.second"))))
	(d-let
		(p-assign (ident "d1_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.a"))))
	(d-let
		(p-assign (ident "d1_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.b"))))
	(d-let
		(p-assign (ident "d1_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.c"))))
	(d-let
		(p-assign (ident "d1_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D1_Multi.d"))))
	(d-let
		(p-assign (ident "d2_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Qual.outerVal"))))
	(d-let
		(p-assign (ident "d2_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Qual.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.outerVal"))))
	(d-let
		(p-assign (ident "d2_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Qual.outerVal"))))
	(d-let
		(p-assign (ident "d2_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Qual.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.outerVal"))))
	(d-let
		(p-assign (ident "d2_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_9"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner.outerVal"))))
	(d-let
		(p-assign (ident "d2_10"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_11"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.outerVal"))))
	(d-let
		(p-assign (ident "d2_12"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_13"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerA.valA"))))
	(d-let
		(p-assign (ident "d2_14"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB.valB"))))
	(d-let
		(p-assign (ident "d2_15"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA.valA"))))
	(d-let
		(p-assign (ident "d2_16"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerB.valB"))))
	(d-let
		(p-assign (ident "d2_17"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.val1"))))
	(d-let
		(p-assign (ident "d2_18"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.Inner.innerVal"))))
	(d-let
		(p-assign (ident "d2_19"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D2_Interleaved.val2"))))
	(d-let
		(p-assign (ident "d3_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.val1"))))
	(d-let
		(p-assign (ident "d3_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.L2.val2"))))
	(d-let
		(p-assign (ident "d3_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern1.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d3_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d3_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.L2.val2"))))
	(d-let
		(p-assign (ident "d3_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern3.val1"))))
	(d-let
		(p-assign (ident "d3_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.val1"))))
	(d-let
		(p-assign (ident "d3_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d3_9"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern4.L2.val2"))))
	(d-let
		(p-assign (ident "d3_10"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern5.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d3_11"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.val1"))))
	(d-let
		(p-assign (ident "d3_12"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.L2.val2"))))
	(d-let
		(p-assign (ident "d3_13"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Pattern6.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d3_14"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a.val3a"))))
	(d-let
		(p-assign (ident "d3_15"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D3_Siblings.L2b.L3b.val3b"))))
	(d-let
		(p-assign (ident "d4_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d4_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d4_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d4_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.L2.val2"))))
	(d-let
		(p-assign (ident "d4_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern2.val1"))))
	(d-let
		(p-assign (ident "d4_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.val1"))))
	(d-let
		(p-assign (ident "d4_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.val2"))))
	(d-let
		(p-assign (ident "d4_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d4_9"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d4_10"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d4_11"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d5_1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.L5.val5"))))
	(d-let
		(p-assign (ident "d5_2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5.val5"))))
	(d-let
		(p-assign (ident "d5_3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d5_4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d5_5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.L2.val2"))))
	(d-let
		(p-assign (ident "d5_6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern2.val1"))))
	(d-let
		(p-assign (ident "d5_7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.val1"))))
	(d-let
		(p-assign (ident "d5_8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.val3"))))
	(d-let
		(p-assign (ident "d5_9"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.val4"))))
	(d-let
		(p-assign (ident "d5_10"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.val2"))))
	(d-let
		(p-assign (ident "d5_11"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.L5.val5"))))
	(d-let
		(p-assign (ident "shadow1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.val"))))
	(d-let
		(p-assign (ident "shadow2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.val"))))
	(d-let
		(p-assign (ident "shadow3"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.useOuter"))))
	(d-let
		(p-assign (ident "shadow4"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.useLocal"))))
	(d-let
		(p-assign (ident "shadow5"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.val"))))
	(d-let
		(p-assign (ident "shadow6"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL1"))))
	(d-let
		(p-assign (ident "shadow7"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL2"))))
	(d-let
		(p-assign (ident "shadow8"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Shadowing.L2.L3.useL3"))))
	(d-let
		(p-assign (ident "external1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.External1.hidden"))))
	(d-let
		(p-assign (ident "anno1"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Annotated.typed"))))
	(d-let
		(p-assign (ident "anno2"))
		(e-lookup-local
			(p-assign (ident "associated_items_truly_comprehensive.Annotated.L2.alsoTyped"))))
	(s-nominal-decl
		(ty-header (name "D1_Forward"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "D1_Backward"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "D1_Multi"))
		(ty-tag-union
			(ty-tag-name (name "C"))))
	(s-nominal-decl
		(ty-header (name "D2_InnerFirst_Qual"))
		(ty-tag-union
			(ty-tag-name (name "D"))))
	(s-nominal-decl
		(ty-header (name "D2_InnerFirst_Unqual"))
		(ty-tag-union
			(ty-tag-name (name "F"))))
	(s-nominal-decl
		(ty-header (name "D2_InnerAfter_Qual"))
		(ty-tag-union
			(ty-tag-name (name "H"))))
	(s-nominal-decl
		(ty-header (name "D2_InnerAfter_Unqual"))
		(ty-tag-union
			(ty-tag-name (name "J"))))
	(s-nominal-decl
		(ty-header (name "D2_OuterRefsInner"))
		(ty-tag-union
			(ty-tag-name (name "L"))))
	(s-nominal-decl
		(ty-header (name "D2_OuterRefsInner_Back"))
		(ty-tag-union
			(ty-tag-name (name "N"))))
	(s-nominal-decl
		(ty-header (name "D2_Siblings_Forward"))
		(ty-tag-union
			(ty-tag-name (name "P"))))
	(s-nominal-decl
		(ty-header (name "D2_Siblings_Backward"))
		(ty-tag-union
			(ty-tag-name (name "S"))))
	(s-nominal-decl
		(ty-header (name "D2_Interleaved"))
		(ty-tag-union
			(ty-tag-name (name "V"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern1"))
		(ty-tag-union
			(ty-tag-name (name "X"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern2"))
		(ty-tag-union
			(ty-tag-name (name "AA"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern3"))
		(ty-tag-union
			(ty-tag-name (name "DD"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern4"))
		(ty-tag-union
			(ty-tag-name (name "GG"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern5"))
		(ty-tag-union
			(ty-tag-name (name "JJ"))))
	(s-nominal-decl
		(ty-header (name "D3_Pattern6"))
		(ty-tag-union
			(ty-tag-name (name "MM"))))
	(s-nominal-decl
		(ty-header (name "D3_Siblings"))
		(ty-tag-union
			(ty-tag-name (name "PP"))))
	(s-nominal-decl
		(ty-header (name "D4_Pattern1"))
		(ty-tag-union
			(ty-tag-name (name "UU"))))
	(s-nominal-decl
		(ty-header (name "D4_Pattern2"))
		(ty-tag-union
			(ty-tag-name (name "YY"))))
	(s-nominal-decl
		(ty-header (name "D4_Pattern3"))
		(ty-tag-union
			(ty-tag-name (name "CCC"))))
	(s-nominal-decl
		(ty-header (name "D4_Pattern4"))
		(ty-tag-union
			(ty-tag-name (name "GGG"))))
	(s-nominal-decl
		(ty-header (name "D5_Pattern1"))
		(ty-tag-union
			(ty-tag-name (name "KKK"))))
	(s-nominal-decl
		(ty-header (name "D5_Pattern2"))
		(ty-tag-union
			(ty-tag-name (name "PPP"))))
	(s-nominal-decl
		(ty-header (name "D5_Pattern3"))
		(ty-tag-union
			(ty-tag-name (name "UUU"))))
	(s-nominal-decl
		(ty-header (name "Shadowing"))
		(ty-tag-union
			(ty-tag-name (name "ZZZ"))))
	(s-nominal-decl
		(ty-header (name "External1"))
		(ty-tag-union
			(ty-tag-name (name "CCCC"))))
	(s-nominal-decl
		(ty-header (name "Annotated"))
		(ty-tag-union
			(ty-tag-name (name "DDDD"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_InnerFirst_Qual.Inner"))
		(ty-tag-union
			(ty-tag-name (name "E"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.Inner"))
		(ty-tag-union
			(ty-tag-name (name "G"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_InnerAfter_Qual.Inner"))
		(ty-tag-union
			(ty-tag-name (name "I"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.Inner"))
		(ty-tag-union
			(ty-tag-name (name "K"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner"))
		(ty-tag-union
			(ty-tag-name (name "M"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner"))
		(ty-tag-union
			(ty-tag-name (name "O"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerA"))
		(ty-tag-union
			(ty-tag-name (name "Q"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB"))
		(ty-tag-union
			(ty-tag-name (name "R"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA"))
		(ty-tag-union
			(ty-tag-name (name "T"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerB"))
		(ty-tag-union
			(ty-tag-name (name "U"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D2_Interleaved.Inner"))
		(ty-tag-union
			(ty-tag-name (name "W"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern1.L2"))
		(ty-tag-union
			(ty-tag-name (name "Y"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern1.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "Z"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern2.L2"))
		(ty-tag-union
			(ty-tag-name (name "BB"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern2.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CC"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern3.L2"))
		(ty-tag-union
			(ty-tag-name (name "EE"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern3.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "FF"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern4.L2"))
		(ty-tag-union
			(ty-tag-name (name "HH"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern4.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "II"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern5.L2"))
		(ty-tag-union
			(ty-tag-name (name "KK"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern5.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "LL"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern6.L2"))
		(ty-tag-union
			(ty-tag-name (name "NN"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Pattern6.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "OO"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2a"))
		(ty-tag-union
			(ty-tag-name (name "QQ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2b"))
		(ty-tag-union
			(ty-tag-name (name "SS"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a"))
		(ty-tag-union
			(ty-tag-name (name "RR"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2b.L3b"))
		(ty-tag-union
			(ty-tag-name (name "TT"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2"))
		(ty-tag-union
			(ty-tag-name (name "VV"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "WW"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "XX"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2"))
		(ty-tag-union
			(ty-tag-name (name "ZZ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AAA"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BBB"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2"))
		(ty-tag-union
			(ty-tag-name (name "DDD"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "EEE"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "FFF"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2"))
		(ty-tag-union
			(ty-tag-name (name "HHH"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "III"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "JJJ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2"))
		(ty-tag-union
			(ty-tag-name (name "LLL"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "MMM"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "NNN"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "OOO"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2"))
		(ty-tag-union
			(ty-tag-name (name "QQQ"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "RRR"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "SSS"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "TTT"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2"))
		(ty-tag-union
			(ty-tag-name (name "VVV"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "WWW"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "XXX"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "YYY"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.Shadowing.L2"))
		(ty-tag-union
			(ty-tag-name (name "AAAA"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.Shadowing.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BBBB"))))
	(s-nominal-decl
		(ty-header (name "associated_items_truly_comprehensive.Annotated.L2"))
		(ty-tag-union
			(ty-tag-name (name "EEEE")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "U64"))
		(patt (type "U64")))
	(type_decls
		(nominal (type "D1_Forward")
			(ty-header (name "D1_Forward")))
		(nominal (type "D1_Backward")
			(ty-header (name "D1_Backward")))
		(nominal (type "D1_Multi")
			(ty-header (name "D1_Multi")))
		(nominal (type "D2_InnerFirst_Qual")
			(ty-header (name "D2_InnerFirst_Qual")))
		(nominal (type "D2_InnerFirst_Unqual")
			(ty-header (name "D2_InnerFirst_Unqual")))
		(nominal (type "D2_InnerAfter_Qual")
			(ty-header (name "D2_InnerAfter_Qual")))
		(nominal (type "D2_InnerAfter_Unqual")
			(ty-header (name "D2_InnerAfter_Unqual")))
		(nominal (type "D2_OuterRefsInner")
			(ty-header (name "D2_OuterRefsInner")))
		(nominal (type "D2_OuterRefsInner_Back")
			(ty-header (name "D2_OuterRefsInner_Back")))
		(nominal (type "D2_Siblings_Forward")
			(ty-header (name "D2_Siblings_Forward")))
		(nominal (type "D2_Siblings_Backward")
			(ty-header (name "D2_Siblings_Backward")))
		(nominal (type "D2_Interleaved")
			(ty-header (name "D2_Interleaved")))
		(nominal (type "D3_Pattern1")
			(ty-header (name "D3_Pattern1")))
		(nominal (type "D3_Pattern2")
			(ty-header (name "D3_Pattern2")))
		(nominal (type "D3_Pattern3")
			(ty-header (name "D3_Pattern3")))
		(nominal (type "D3_Pattern4")
			(ty-header (name "D3_Pattern4")))
		(nominal (type "D3_Pattern5")
			(ty-header (name "D3_Pattern5")))
		(nominal (type "D3_Pattern6")
			(ty-header (name "D3_Pattern6")))
		(nominal (type "D3_Siblings")
			(ty-header (name "D3_Siblings")))
		(nominal (type "D4_Pattern1")
			(ty-header (name "D4_Pattern1")))
		(nominal (type "D4_Pattern2")
			(ty-header (name "D4_Pattern2")))
		(nominal (type "D4_Pattern3")
			(ty-header (name "D4_Pattern3")))
		(nominal (type "D4_Pattern4")
			(ty-header (name "D4_Pattern4")))
		(nominal (type "D5_Pattern1")
			(ty-header (name "D5_Pattern1")))
		(nominal (type "D5_Pattern2")
			(ty-header (name "D5_Pattern2")))
		(nominal (type "D5_Pattern3")
			(ty-header (name "D5_Pattern3")))
		(nominal (type "Shadowing")
			(ty-header (name "Shadowing")))
		(nominal (type "External1")
			(ty-header (name "External1")))
		(nominal (type "Annotated")
			(ty-header (name "Annotated")))
		(nominal (type "associated_items_truly_comprehensive.D2_InnerFirst_Qual.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_InnerFirst_Qual.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_InnerFirst_Unqual.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_InnerAfter_Qual.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_InnerAfter_Qual.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_InnerAfter_Unqual.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_OuterRefsInner.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_OuterRefsInner_Back.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerA")
			(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerA")))
		(nominal (type "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB")
			(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Forward.InnerB")))
		(nominal (type "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA")
			(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerA")))
		(nominal (type "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerB")
			(ty-header (name "associated_items_truly_comprehensive.D2_Siblings_Backward.InnerB")))
		(nominal (type "associated_items_truly_comprehensive.D2_Interleaved.Inner")
			(ty-header (name "associated_items_truly_comprehensive.D2_Interleaved.Inner")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern1.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern1.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern1.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern1.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern2.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern2.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern2.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern2.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern3.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern3.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern3.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern3.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern4.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern4.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern4.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern4.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern5.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern5.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern5.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern5.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern6.L2")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern6.L2")))
		(nominal (type "associated_items_truly_comprehensive.D3_Pattern6.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D3_Pattern6.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D3_Siblings.L2a")
			(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2a")))
		(nominal (type "associated_items_truly_comprehensive.D3_Siblings.L2b")
			(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2b")))
		(nominal (type "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a")
			(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2a.L3a")))
		(nominal (type "associated_items_truly_comprehensive.D3_Siblings.L2b.L3b")
			(ty-header (name "associated_items_truly_comprehensive.D3_Siblings.L2b.L3b")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern1.L2")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern1.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern1.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern2.L2")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern2.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern2.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern3.L2")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern3.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern3.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern4.L2")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern4.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D4_Pattern4.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern1.L2")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern1.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.L5")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern1.L2.L3.L4.L5")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern2.L2")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern2.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern2.L2.L3.L4.L5")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern3.L2")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern3.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4")))
		(nominal (type "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.L5")
			(ty-header (name "associated_items_truly_comprehensive.D5_Pattern3.L2.L3.L4.L5")))
		(nominal (type "associated_items_truly_comprehensive.Shadowing.L2")
			(ty-header (name "associated_items_truly_comprehensive.Shadowing.L2")))
		(nominal (type "associated_items_truly_comprehensive.Shadowing.L2.L3")
			(ty-header (name "associated_items_truly_comprehensive.Shadowing.L2.L3")))
		(nominal (type "associated_items_truly_comprehensive.Annotated.L2")
			(ty-header (name "associated_items_truly_comprehensive.Annotated.L2"))))
	(expressions
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "e where [e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "U64"))
		(expr (type "U64"))))
~~~
