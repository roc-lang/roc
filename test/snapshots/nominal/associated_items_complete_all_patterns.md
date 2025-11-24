# META
~~~ini
description=Complete test - all ordering patterns at all depths, plus scoping violations
type=repl
~~~
# SOURCE
~~~roc

d1_forward := [A].{
    first = second
    second = 100
}
d1_1 = d1_forward.first

d1_scope := [B].{
    inner = 200
}
d1_2 = d1_scope.inner

d2_inner_first := [C].{
    Inner := [D].{
        inner_val = outer_val
    }

    outer_val = 300
}
d2_1 = d2_inner_first.outer_val
d2_2 = d2_inner_first.Inner.inner_val

d2_outer_val_middle := [G].{
    Inner := [H].{
        inner_val = outer_val
    }

    outer_val = 500
}
d2_3 = d2_outer_val_middle.Inner.inner_val

d2_outer_refs_inner := [I].{
    outer_val = d2_outer_refs_inner.Inner.inner_val

    Inner := [J].{
        inner_val = 600
    }
}
d2_4 = d2_outer_refs_inner.outer_val

d2_scope_violation := [K].{
    Inner := [L].{
        inner_private = 700
    }

    outer_trying_inner = inner_private
}

d2_siblings := [M].{
    InnerA := [N].{
        valA = d2_siblings.InnerB.valB + 1
    }

    InnerB := [O].{
        valB = 800
    }
}
d2_5 = d2_siblings.InnerA.valA

d3_types_then_vals := [P].{
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }

    val1 = 10
}
d3_1 = d3_types_then_vals.val1
d3_2 = d3_types_then_vals.L2.val2
d3_3 = d3_types_then_vals.L2.L3.val3

d3_vals_then_types := [S].{
    val1 = 30

    L2 := [T].{
        val2 = val1 + 5

        L3 := [U].{
            val3 = val1 + val2
        }
    }
}
d3_4 = d3_vals_then_types.val1
d3_5 = d3_vals_then_types.L2.val2
d3_6 = d3_vals_then_types.L2.L3.val3

d3_l1_scope_violation := [V].{
    L2 := [W].{
        L3 := [X].{
            l3_private = 999
        }
    }

    bad_l1 = l3_private
}

d3_l2_scope_violation := [Y].{
    L2 := [Z].{
        L3 := [AA].{
            l3_secret = 888
        }

        bad_l2 = l3_secret
    }
}

d3_val_after_nested := [AB].{
    L2 := [AC].{
        L3 := [AD].{
            val3 = val2 * 2
        }

        val2 = val1 * 3
    }

    val1 = 5
}
d3_7 = d3_val_after_nested.val1
d3_8 = d3_val_after_nested.L2.val2
d3_9 = d3_val_after_nested.L2.L3.val3

d4_all_types_then_vals := [AE].{
    L2 := [AF].{
        L3 := [AG].{
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }

            val3 = 3
        }

        val2 = 2
    }

    val1 = 1
}
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4

d4_all_vals_then_types := [AI].{
    val1 = 10

    L2 := [AJ].{
        val2 = val1 + 1

        L3 := [AK].{
            val3 = val1 + val2

            L4 := [AL].{
                val4 = val1 + val2 + val3
            }
        }
    }
}
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4

d4_reverse_types := [AM].{
    L2 := [AN].{
        L3 := [AO].{
            L4 := [AP].{
                val4 = val3 + 1
            }

            val3 = val2 + 1
        }

        val2 = val1 + 1
    }

    val1 = 7
}
d4_3 = d4_reverse_types.L2.L3.L4.val4

d4_interleaved := [AQ].{
    val1 = 15

    L2 := [AR].{
        L3 := [AS].{
            val3 = val1 + val2

            L4 := [AT].{
                val4 = val1 + val2 + val3
            }
        }

        val2 = val1 + 5
    }
}
d4_4 = d4_interleaved.L2.L3.L4.val4

d4_l3_val_after_l4 := [BA].{
    L2 := [BB].{
        L3 := [BC].{
            L4 := [BD].{
                val4 = val3 * 3
            }
            val3 = 12
        }
    }
}
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4

d4_l2_val_after_l3 := [BE].{
    L2 := [BF].{
        L3 := [BG].{
            L4 := [BH].{
                val4 = val2 + val3
            }

            val3 = 8
        }

        val2 = 4
    }
}
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4

d4_l1_val_after_l2 := [BI].{
    L2 := [BJ].{
        L3 := [BK].{
            L4 := [BL].{
                val4 = val1 + 100
            }

            val3 = val1 + 50
        }

        val2 = val1 + 10
    }

    val1 = 3
}
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4

d4_l1_scope_violation := [BM].{
    L2 := [BN].{
        L3 := [BO].{
            L4 := [BP].{
                l4_val = 444
            }
        }
    }

    bad = l4_val
}

d4_l2_scope_violation := [BQ].{
    L2 := [BR].{
        L3 := [BS].{
            L4 := [BT].{
                l4_secret = 333
            }
        }

        bad = l4_secret
    }
}

d4_l3_scope_violation := [BU].{
    L2 := [BV].{
        L3 := [BW].{
            L4 := [BX].{
                l4_private = 555
            }

            attempt = l4_private
        }
    }
}

d5_all_types_then_vals := [BY].{
    L2 := [BZ].{
        L3 := [CA].{
            L4 := [CB].{
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = 4
            }

            val3 = 3
        }

        val2 = 2
    }

    val1 = 1
}
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5

d5_all_vals_then_types := [CD].{
    val1 = 100

    L2 := [CE].{
        val2 = val1 + 10

        L3 := [CF].{
            val3 = val1 + val2

            L4 := [CG].{
                val4 = val1 + val2 + val3

                L5 := [CH].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
        }
    }
}
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5

d5_deep_interleave := [CI].{
    val1 = 2

    L2 := [CJ].{
        L3 := [CK].{
            val3 = val1 + val2

            L4 := [CL].{
                L5 := [CM].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = val1 + val2 + val3
            }
        }

        val2 = val1 + 1
    }
}
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5

d5_l4_val_after_l5 := [CN].{
    L2 := [CO].{
        L3 := [CP].{
            L4 := [CQ].{
                L5 := [CR].{
                    val5 = val4 * 5
                }

                val4 = 6
            }
        }
    }
}
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5

d5_l3_val_after_l4 := [CS].{
    L2 := [CT].{
        L3 := [CU].{
            L4 := [CV].{
                L5 := [CW].{
                    val5 = val3 + val4
                }

                val4 = 7
            }

            val3 = 3
        }
    }
}
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5

d5_l1_val_last := [DC].{
    L2 := [DD].{
        val2 = val1 + 10

        L3 := [DE].{
            val3 = val1 + val2

            L4 := [DF].{
                val4 = val1 + val2 + val3

                L5 := [DG].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
        }
    }

    val1 = 5
}
d5_6 = d5_l1_val_last.val1
d5_7 = d5_l1_val_last.L2.val2
d5_8 = d5_l1_val_last.L2.L3.val3
d5_9 = d5_l1_val_last.L2.L3.L4.val4
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5

d5_l1_to_l5_violation := [DH].{
    L2 := [DI].{
        L3 := [DJ].{
            L4 := [DK].{
                L5 := [DL].{
                    deep_secret = 12345
                }
            }
        }
    }

    bad = deep_secret
}

d5_l3_to_l5_violation := [DM].{
    L2 := [DN].{
        L3 := [DO].{
            L4 := [DP].{
                L5 := [DQ].{
                    l5_secret = 9999
                }
            }

            bad = l5_secret
        }
    }
}

d5_l4_to_l5_violation := [DR].{
    L2 := [DS].{
        L3 := [DT].{
            L4 := [DU].{
                L5 := [DV].{
                    l5_only = 8888
                }

                bad = l5_only
            }
        }
    }
}
~~~
# OUTPUT
# PROBLEMS
NIL
