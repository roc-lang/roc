# META
~~~ini
description=Complete test - all ordering patterns at all depths, plus scoping violations
type=file:Test.roc
mod_validation_diagnostics=true
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
# EXPECTED
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:2:1:2:11
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:2:12:2:14
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:2:15:2:16
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:2:17:2:18
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:2:18:2:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:2:19:2:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:5:1:5:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:8:1:8:9
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:8:10:8:12
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:8:13:8:14
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:8:15:8:16
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:8:16:8:17
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:8:17:8:18
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:10:1:10:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:13:1:13:15
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:13:16:13:18
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:13:19:13:20
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:13:21:13:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:13:22:13:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:13:23:13:24
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:19:1:19:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:21:22:21:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:23:1:23:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:23:21:23:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:23:24:23:25
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:23:26:23:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:23:27:23:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:23:28:23:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:29:1:29:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:30:27:30:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:32:1:32:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:32:21:32:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:32:24:32:25
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:32:26:32:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:32:27:32:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:32:28:32:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:33:36:33:42
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:38:1:38:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:41:1:41:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:41:20:41:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:41:23:41:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:41:25:41:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:41:26:41:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:41:27:41:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:47:1:47:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:49:1:49:12
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:49:13:49:15
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:49:16:49:17
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:49:18:49:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:49:19:49:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:49:20:49:21
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:51:27:51:34
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:57:1:57:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:58:19:58:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:60:1:60:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:60:20:60:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:60:23:60:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:60:25:60:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:60:26:60:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:60:27:60:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:70:1:70:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:72:26:72:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:73:26:73:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:73:29:73:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:75:1:75:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:75:20:75:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:75:23:75:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:75:25:75:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:75:26:75:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:75:27:75:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:85:1:85:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:87:26:87:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:88:26:88:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:88:29:88:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:90:1:90:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:90:23:90:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:90:26:90:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:90:28:90:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:90:29:90:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:90:30:90:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:98:1:98:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:100:1:100:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:100:23:100:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:100:26:100:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:100:28:100:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:100:29:100:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:100:30:100:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:108:1:108:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:110:1:110:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:110:21:110:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:110:24:110:25
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:110:27:110:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:110:28:110:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:110:29:110:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:120:1:120:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:122:27:122:30
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:123:27:123:30
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:123:30:123:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:125:1:125:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:125:24:125:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:125:27:125:28
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:125:30:125:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:125:31:125:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:125:32:125:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:139:1:139:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:140:30:140:33
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:140:33:140:36
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:140:36:140:39
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:142:1:142:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:142:24:142:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:142:27:142:28
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:142:30:142:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:142:31:142:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:142:32:142:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:156:1:156:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:157:30:157:33
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:157:33:157:36
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:157:36:157:39
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:159:1:159:17
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:159:18:159:20
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:159:21:159:22
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:159:24:159:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:159:25:159:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:159:26:159:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:173:1:173:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:174:24:174:27
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:174:27:174:30
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:174:30:174:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:176:1:176:15
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:176:16:176:18
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:176:19:176:20
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:176:22:176:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:176:23:176:24
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:176:24:176:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:190:1:190:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:191:22:191:25
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:191:25:191:28
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:191:28:191:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:193:1:193:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:193:20:193:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:193:23:193:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:193:26:193:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:193:27:193:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:193:28:193:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:202:1:202:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:203:26:203:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:203:29:203:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:203:32:203:35
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:205:1:205:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:205:20:205:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:205:23:205:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:205:26:205:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:205:27:205:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:205:28:205:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:217:1:217:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:218:26:218:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:218:29:218:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:218:32:218:35
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:220:1:220:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:220:20:220:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:220:23:220:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:220:26:220:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:220:27:220:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:220:28:220:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:234:1:234:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:235:26:235:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:235:29:235:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:235:32:235:35
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:237:1:237:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:237:23:237:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:237:26:237:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:237:29:237:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:237:30:237:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:237:31:237:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:247:1:247:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:249:1:249:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:249:23:249:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:249:26:249:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:249:29:249:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:249:30:249:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:249:31:249:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:259:1:259:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:261:1:261:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:261:23:261:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:261:26:261:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:261:29:261:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:261:30:261:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:261:31:261:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:271:1:271:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:273:1:273:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:273:24:273:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:273:27:273:28
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:273:30:273:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:273:31:273:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:273:32:273:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:291:1:291:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:292:30:292:33
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:292:33:292:36
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:292:36:292:39
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:292:39:292:42
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:294:1:294:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:294:24:294:26
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:294:27:294:28
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:294:30:294:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:294:31:294:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:294:32:294:33
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:312:1:312:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:313:30:313:33
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:313:33:313:36
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:313:36:313:39
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:313:39:313:42
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:315:1:315:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:315:20:315:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:315:23:315:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:315:26:315:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:315:27:315:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:315:28:315:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:333:1:333:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:334:26:334:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:334:29:334:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:334:32:334:35
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:334:35:334:38
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:336:1:336:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:336:20:336:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:336:23:336:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:336:26:336:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:336:27:336:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:336:28:336:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:348:1:348:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:349:26:349:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:349:29:349:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:349:32:349:35
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:349:35:349:38
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:351:1:351:19
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:351:20:351:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:351:23:351:24
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:351:26:351:27
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:351:27:351:28
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:351:28:351:29
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:365:1:365:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:366:26:366:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:366:29:366:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:366:32:366:35
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:366:35:366:38
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:368:1:368:15
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:368:16:368:18
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:368:19:368:20
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:368:22:368:23
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:368:23:368:24
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:368:24:368:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:386:1:386:2
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:388:22:388:25
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:389:22:389:25
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:389:25:389:28
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:390:22:390:25
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:390:25:390:28
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:390:28:390:31
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:391:23:391:26
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:391:26:391:29
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:391:29:391:32
EXPECTED RECORD ACCESSOR - associated_items_complete_all_patterns.md:391:32:391:35
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:393:1:393:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:393:23:393:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:393:26:393:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:393:29:393:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:393:30:393:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:393:31:393:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:405:1:405:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:407:1:407:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:407:23:407:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:407:26:407:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:407:29:407:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:407:30:407:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:407:31:407:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:419:1:419:2
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:421:1:421:22
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:421:23:421:25
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:421:26:421:27
TYPE APPLICATION NEEDS PARENTHESES - associated_items_complete_all_patterns.md:421:29:421:30
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:421:30:421:31
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:421:31:421:32
UNEXPECTED STATEMENT - associated_items_complete_all_patterns.md:433:1:433:2
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:6:8:6:18
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:11:8:11:16
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:20:8:20:22
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:21:8:21:38
TYPE REDECLARED - associated_items_complete_all_patterns.md:24:5:26:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:28:5:28:14
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:30:8:30:43
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:33:5:33:14
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:33:17:33:52
TYPE REDECLARED - associated_items_complete_all_patterns.md:35:5:37:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:39:8:39:27
TYPE REDECLARED - associated_items_complete_all_patterns.md:42:5:44:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:46:26:46:39
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:51:16:51:39
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:58:8:58:31
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:71:8:71:26
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:72:8:72:34
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:73:8:73:37
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:76:5:76:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:78:5:84:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:86:8:86:26
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:87:8:87:34
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:88:8:88:37
TYPE REDECLARED - associated_items_complete_all_patterns.md:91:5:95:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:97:14:97:24
TYPE REDECLARED - associated_items_complete_all_patterns.md:101:5:107:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:111:5:117:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:119:5:119:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:121:8:121:27
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:122:8:122:35
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:123:8:123:38
TYPE REDECLARED - associated_items_complete_all_patterns.md:126:5:136:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:138:5:138:9
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:140:8:140:44
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:143:5:143:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:145:5:155:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:157:8:157:44
TYPE REDECLARED - associated_items_complete_all_patterns.md:160:5:170:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:172:5:172:9
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:174:8:174:38
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:177:5:177:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:179:5:189:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:191:8:191:36
TYPE REDECLARED - associated_items_complete_all_patterns.md:194:5:201:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:203:8:203:40
TYPE REDECLARED - associated_items_complete_all_patterns.md:206:5:216:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:218:8:218:40
TYPE REDECLARED - associated_items_complete_all_patterns.md:221:5:231:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:233:5:233:9
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:235:8:235:40
TYPE REDECLARED - associated_items_complete_all_patterns.md:238:5:244:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:246:11:246:17
TYPE REDECLARED - associated_items_complete_all_patterns.md:250:5:258:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:262:5:270:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:274:5:288:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:290:5:290:9
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:292:8:292:47
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:295:5:295:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:297:5:311:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:313:8:313:47
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:316:5:316:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:318:5:332:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:334:8:334:43
TYPE REDECLARED - associated_items_complete_all_patterns.md:337:5:347:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:349:8:349:43
TYPE REDECLARED - associated_items_complete_all_patterns.md:352:5:364:6
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:366:8:366:43
TYPE REDECLARED - associated_items_complete_all_patterns.md:369:5:383:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:385:5:385:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:387:8:387:22
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:388:8:388:30
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:389:8:389:33
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:390:8:390:36
UNRECOGNIZED SYNTAX - associated_items_complete_all_patterns.md:391:9:391:40
TYPE REDECLARED - associated_items_complete_all_patterns.md:394:5:402:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:404:5:404:8
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:404:11:404:22
TYPE REDECLARED - associated_items_complete_all_patterns.md:408:5:418:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:422:5:432:6
TYPE MOD MISSING MATCHING TYPE - associated_items_complete_all_patterns.md:2:1:433:2
POLYMORPHIC VALUE - associated_items_complete_all_patterns.md:51:9:51:13
MISSING METHOD - associated_items_complete_all_patterns.md:51:16:51:39
# PROBLEMS
── ✗ unexpected statement ──────── associated_items_complete_all_patterns.md:2:1

I was parsing a statement, and this token cannot start a statement here.

d1_forward := [A].{
^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d1_forward here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:2:12

I was parsing a statement, and this token cannot start a statement here.

d1_forward := [A].{
           ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:2:15

I was parsing a statement, and this token cannot start a statement here.

d1_forward := [A].{
              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:2:17

I was parsing a type annotation, and I found a type argument without
parentheses.

d1_forward := [A].{
                ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:2:18

I was parsing a statement, and this token cannot start a statement here.

d1_forward := [A].{
                 ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:2:19

I was parsing a statement, and this token cannot start a statement here.

d1_forward := [A].{
                  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ──────── associated_items_complete_all_patterns.md:5:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ──────── associated_items_complete_all_patterns.md:8:1

I was parsing a statement, and this token cannot start a statement here.

d1_scope := [B].{
^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d1_scope here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:8:10

I was parsing a statement, and this token cannot start a statement here.

d1_scope := [B].{
         ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:8:13

I was parsing a statement, and this token cannot start a statement here.

d1_scope := [B].{
            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:8:15

I was parsing a type annotation, and I found a type argument without
parentheses.

d1_scope := [B].{
              ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:8:16

I was parsing a statement, and this token cannot start a statement here.

d1_scope := [B].{
               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:8:17

I was parsing a statement, and this token cannot start a statement here.

d1_scope := [B].{
                ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:10:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:13:1

I was parsing a statement, and this token cannot start a statement here.

d2_inner_first := [C].{
^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d2_inner_first here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:13:16

I was parsing a statement, and this token cannot start a statement here.

d2_inner_first := [C].{
               ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:13:19

I was parsing a statement, and this token cannot start a statement here.

d2_inner_first := [C].{
                  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:13:21

I was parsing a type annotation, and I found a type argument without
parentheses.

d2_inner_first := [C].{
                    ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:13:22

I was parsing a statement, and this token cannot start a statement here.

d2_inner_first := [C].{
                     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:13:23

I was parsing a statement, and this token cannot start a statement here.

d2_inner_first := [C].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:19:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:21:22

I was parsing access after `.`, and I expected a field name or tuple index.

d2_2 = d2_inner_first.Inner.inner_val
                     ^^^^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .Inner here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:23:1

I was parsing a statement, and this token cannot start a statement here.

d2_outer_val_middle := [G].{
^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d2_outer_val_middle here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:23:21

I was parsing a statement, and this token cannot start a statement here.

d2_outer_val_middle := [G].{
                    ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:23:24

I was parsing a statement, and this token cannot start a statement here.

d2_outer_val_middle := [G].{
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:23:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d2_outer_val_middle := [G].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:23:27

I was parsing a statement, and this token cannot start a statement here.

d2_outer_val_middle := [G].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:23:28

I was parsing a statement, and this token cannot start a statement here.

d2_outer_val_middle := [G].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:29:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:30:27

I was parsing access after `.`, and I expected a field name or tuple index.

d2_3 = d2_outer_val_middle.Inner.inner_val
                          ^^^^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .Inner here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:32:1

I was parsing a statement, and this token cannot start a statement here.

d2_outer_refs_inner := [I].{
^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d2_outer_refs_inner here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:32:21

I was parsing a statement, and this token cannot start a statement here.

d2_outer_refs_inner := [I].{
                    ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:32:24

I was parsing a statement, and this token cannot start a statement here.

d2_outer_refs_inner := [I].{
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:32:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d2_outer_refs_inner := [I].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:32:27

I was parsing a statement, and this token cannot start a statement here.

d2_outer_refs_inner := [I].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:32:28

I was parsing a statement, and this token cannot start a statement here.

d2_outer_refs_inner := [I].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:33:36

I was parsing access after `.`, and I expected a field name or tuple index.

outer_val = d2_outer_refs_inner.Inner.inner_val
                               ^^^^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .Inner here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:38:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:41:1

I was parsing a statement, and this token cannot start a statement here.

d2_scope_violation := [K].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d2_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:41:20

I was parsing a statement, and this token cannot start a statement here.

d2_scope_violation := [K].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:41:23

I was parsing a statement, and this token cannot start a statement here.

d2_scope_violation := [K].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:41:25

I was parsing a type annotation, and I found a type argument without
parentheses.

d2_scope_violation := [K].{
                        ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:41:26

I was parsing a statement, and this token cannot start a statement here.

d2_scope_violation := [K].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:41:27

I was parsing a statement, and this token cannot start a statement here.

d2_scope_violation := [K].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:47:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:49:1

I was parsing a statement, and this token cannot start a statement here.

d2_siblings := [M].{
^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d2_siblings here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:49:13

I was parsing a statement, and this token cannot start a statement here.

d2_siblings := [M].{
            ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:49:16

I was parsing a statement, and this token cannot start a statement here.

d2_siblings := [M].{
               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:49:18

I was parsing a type annotation, and I found a type argument without
parentheses.

d2_siblings := [M].{
                 ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:49:19

I was parsing a statement, and this token cannot start a statement here.

d2_siblings := [M].{
                  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:49:20

I was parsing a statement, and this token cannot start a statement here.

d2_siblings := [M].{
                   ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:51:27

I was parsing access after `.`, and I expected a field name or tuple index.

valA = d2_siblings.InnerB.valB + 1
                  ^^^^^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .InnerB here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:57:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:58:19

I was parsing access after `.`, and I expected a field name or tuple index.

d2_5 = d2_siblings.InnerA.valA
                  ^^^^^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .InnerA here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:60:1

I was parsing a statement, and this token cannot start a statement here.

d3_types_then_vals := [P].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d3_types_then_vals here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:60:20

I was parsing a statement, and this token cannot start a statement here.

d3_types_then_vals := [P].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:60:23

I was parsing a statement, and this token cannot start a statement here.

d3_types_then_vals := [P].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:60:25

I was parsing a type annotation, and I found a type argument without
parentheses.

d3_types_then_vals := [P].{
                        ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:60:26

I was parsing a statement, and this token cannot start a statement here.

d3_types_then_vals := [P].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:60:27

I was parsing a statement, and this token cannot start a statement here.

d3_types_then_vals := [P].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:70:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:72:26

I was parsing access after `.`, and I expected a field name or tuple index.

d3_2 = d3_types_then_vals.L2.val2
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:73:26

I was parsing access after `.`, and I expected a field name or tuple index.

d3_3 = d3_types_then_vals.L2.L3.val3
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:73:29

I was parsing access after `.`, and I expected a field name or tuple index.

d3_3 = d3_types_then_vals.L2.L3.val3
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:75:1

I was parsing a statement, and this token cannot start a statement here.

d3_vals_then_types := [S].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d3_vals_then_types here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:75:20

I was parsing a statement, and this token cannot start a statement here.

d3_vals_then_types := [S].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:75:23

I was parsing a statement, and this token cannot start a statement here.

d3_vals_then_types := [S].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:75:25

I was parsing a type annotation, and I found a type argument without
parentheses.

d3_vals_then_types := [S].{
                        ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:75:26

I was parsing a statement, and this token cannot start a statement here.

d3_vals_then_types := [S].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:75:27

I was parsing a statement, and this token cannot start a statement here.

d3_vals_then_types := [S].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:85:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:87:26

I was parsing access after `.`, and I expected a field name or tuple index.

d3_5 = d3_vals_then_types.L2.val2
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:88:26

I was parsing access after `.`, and I expected a field name or tuple index.

d3_6 = d3_vals_then_types.L2.L3.val3
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ── associated_items_complete_all_patterns.md:88:29

I was parsing access after `.`, and I expected a field name or tuple index.

d3_6 = d3_vals_then_types.L2.L3.val3
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:90:1

I was parsing a statement, and this token cannot start a statement here.

d3_l1_scope_violation := [V].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d3_l1_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:90:23

I was parsing a statement, and this token cannot start a statement here.

d3_l1_scope_violation := [V].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:90:26

I was parsing a statement, and this token cannot start a statement here.

d3_l1_scope_violation := [V].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:90:28

I was parsing a type annotation, and I found a type argument without
parentheses.

d3_l1_scope_violation := [V].{
                           ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:90:29

I was parsing a statement, and this token cannot start a statement here.

d3_l1_scope_violation := [V].{
                            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:90:30

I was parsing a statement, and this token cannot start a statement here.

d3_l1_scope_violation := [V].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────── associated_items_complete_all_patterns.md:98:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:100:1

I was parsing a statement, and this token cannot start a statement here.

d3_l2_scope_violation := [Y].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d3_l2_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:100:23

I was parsing a statement, and this token cannot start a statement here.

d3_l2_scope_violation := [Y].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:100:26

I was parsing a statement, and this token cannot start a statement here.

d3_l2_scope_violation := [Y].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:100:28

I was parsing a type annotation, and I found a type argument without
parentheses.

d3_l2_scope_violation := [Y].{
                           ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:100:29

I was parsing a statement, and this token cannot start a statement here.

d3_l2_scope_violation := [Y].{
                            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:100:30

I was parsing a statement, and this token cannot start a statement here.

d3_l2_scope_violation := [Y].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:108:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:110:1

I was parsing a statement, and this token cannot start a statement here.

d3_val_after_nested := [AB].{
^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d3_val_after_nested here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:110:21

I was parsing a statement, and this token cannot start a statement here.

d3_val_after_nested := [AB].{
                    ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:110:24

I was parsing a statement, and this token cannot start a statement here.

d3_val_after_nested := [AB].{
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:110:27

I was parsing a type annotation, and I found a type argument without
parentheses.

d3_val_after_nested := [AB].{
                          ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:110:28

I was parsing a statement, and this token cannot start a statement here.

d3_val_after_nested := [AB].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:110:29

I was parsing a statement, and this token cannot start a statement here.

d3_val_after_nested := [AB].{
                            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:120:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:122:27

I was parsing access after `.`, and I expected a field name or tuple index.

d3_8 = d3_val_after_nested.L2.val2
                          ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:123:27

I was parsing access after `.`, and I expected a field name or tuple index.

d3_9 = d3_val_after_nested.L2.L3.val3
                          ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:123:30

I was parsing access after `.`, and I expected a field name or tuple index.

d3_9 = d3_val_after_nested.L2.L3.val3
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:125:1

I was parsing a statement, and this token cannot start a statement here.

d4_all_types_then_vals := [AE].{
^^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_all_types_then_vals here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:125:24

I was parsing a statement, and this token cannot start a statement here.

d4_all_types_then_vals := [AE].{
                       ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:125:27

I was parsing a statement, and this token cannot start a statement here.

d4_all_types_then_vals := [AE].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:125:30

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_all_types_then_vals := [AE].{
                             ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:125:31

I was parsing a statement, and this token cannot start a statement here.

d4_all_types_then_vals := [AE].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:125:32

I was parsing a statement, and this token cannot start a statement here.

d4_all_types_then_vals := [AE].{
                               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:139:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:140:30

I was parsing access after `.`, and I expected a field name or tuple index.

d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:140:33

I was parsing access after `.`, and I expected a field name or tuple index.

d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
                                ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:140:36

I was parsing access after `.`, and I expected a field name or tuple index.

d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
                                   ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:142:1

I was parsing a statement, and this token cannot start a statement here.

d4_all_vals_then_types := [AI].{
^^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_all_vals_then_types here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:142:24

I was parsing a statement, and this token cannot start a statement here.

d4_all_vals_then_types := [AI].{
                       ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:142:27

I was parsing a statement, and this token cannot start a statement here.

d4_all_vals_then_types := [AI].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:142:30

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_all_vals_then_types := [AI].{
                             ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:142:31

I was parsing a statement, and this token cannot start a statement here.

d4_all_vals_then_types := [AI].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:142:32

I was parsing a statement, and this token cannot start a statement here.

d4_all_vals_then_types := [AI].{
                               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:156:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:157:30

I was parsing access after `.`, and I expected a field name or tuple index.

d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:157:33

I was parsing access after `.`, and I expected a field name or tuple index.

d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
                                ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:157:36

I was parsing access after `.`, and I expected a field name or tuple index.

d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
                                   ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:159:1

I was parsing a statement, and this token cannot start a statement here.

d4_reverse_types := [AM].{
^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_reverse_types here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:159:18

I was parsing a statement, and this token cannot start a statement here.

d4_reverse_types := [AM].{
                 ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:159:21

I was parsing a statement, and this token cannot start a statement here.

d4_reverse_types := [AM].{
                    ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:159:24

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_reverse_types := [AM].{
                       ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:159:25

I was parsing a statement, and this token cannot start a statement here.

d4_reverse_types := [AM].{
                        ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:159:26

I was parsing a statement, and this token cannot start a statement here.

d4_reverse_types := [AM].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:173:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:174:24

I was parsing access after `.`, and I expected a field name or tuple index.

d4_3 = d4_reverse_types.L2.L3.L4.val4
                       ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:174:27

I was parsing access after `.`, and I expected a field name or tuple index.

d4_3 = d4_reverse_types.L2.L3.L4.val4
                          ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:174:30

I was parsing access after `.`, and I expected a field name or tuple index.

d4_3 = d4_reverse_types.L2.L3.L4.val4
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:176:1

I was parsing a statement, and this token cannot start a statement here.

d4_interleaved := [AQ].{
^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_interleaved here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:176:16

I was parsing a statement, and this token cannot start a statement here.

d4_interleaved := [AQ].{
               ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:176:19

I was parsing a statement, and this token cannot start a statement here.

d4_interleaved := [AQ].{
                  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:176:22

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_interleaved := [AQ].{
                     ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:176:23

I was parsing a statement, and this token cannot start a statement here.

d4_interleaved := [AQ].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:176:24

I was parsing a statement, and this token cannot start a statement here.

d4_interleaved := [AQ].{
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:190:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:191:22

I was parsing access after `.`, and I expected a field name or tuple index.

d4_4 = d4_interleaved.L2.L3.L4.val4
                     ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:191:25

I was parsing access after `.`, and I expected a field name or tuple index.

d4_4 = d4_interleaved.L2.L3.L4.val4
                        ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:191:28

I was parsing access after `.`, and I expected a field name or tuple index.

d4_4 = d4_interleaved.L2.L3.L4.val4
                           ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:193:1

I was parsing a statement, and this token cannot start a statement here.

d4_l3_val_after_l4 := [BA].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l3_val_after_l4 here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:193:20

I was parsing a statement, and this token cannot start a statement here.

d4_l3_val_after_l4 := [BA].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:193:23

I was parsing a statement, and this token cannot start a statement here.

d4_l3_val_after_l4 := [BA].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:193:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l3_val_after_l4 := [BA].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:193:27

I was parsing a statement, and this token cannot start a statement here.

d4_l3_val_after_l4 := [BA].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:193:28

I was parsing a statement, and this token cannot start a statement here.

d4_l3_val_after_l4 := [BA].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:202:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:203:26

I was parsing access after `.`, and I expected a field name or tuple index.

d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:203:29

I was parsing access after `.`, and I expected a field name or tuple index.

d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:203:32

I was parsing access after `.`, and I expected a field name or tuple index.

d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:205:1

I was parsing a statement, and this token cannot start a statement here.

d4_l2_val_after_l3 := [BE].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l2_val_after_l3 here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:205:20

I was parsing a statement, and this token cannot start a statement here.

d4_l2_val_after_l3 := [BE].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:205:23

I was parsing a statement, and this token cannot start a statement here.

d4_l2_val_after_l3 := [BE].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:205:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l2_val_after_l3 := [BE].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:205:27

I was parsing a statement, and this token cannot start a statement here.

d4_l2_val_after_l3 := [BE].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:205:28

I was parsing a statement, and this token cannot start a statement here.

d4_l2_val_after_l3 := [BE].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:217:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:218:26

I was parsing access after `.`, and I expected a field name or tuple index.

d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:218:29

I was parsing access after `.`, and I expected a field name or tuple index.

d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:218:32

I was parsing access after `.`, and I expected a field name or tuple index.

d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:220:1

I was parsing a statement, and this token cannot start a statement here.

d4_l1_val_after_l2 := [BI].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l1_val_after_l2 here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:220:20

I was parsing a statement, and this token cannot start a statement here.

d4_l1_val_after_l2 := [BI].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:220:23

I was parsing a statement, and this token cannot start a statement here.

d4_l1_val_after_l2 := [BI].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:220:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l1_val_after_l2 := [BI].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:220:27

I was parsing a statement, and this token cannot start a statement here.

d4_l1_val_after_l2 := [BI].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:220:28

I was parsing a statement, and this token cannot start a statement here.

d4_l1_val_after_l2 := [BI].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:234:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:235:26

I was parsing access after `.`, and I expected a field name or tuple index.

d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:235:29

I was parsing access after `.`, and I expected a field name or tuple index.

d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:235:32

I was parsing access after `.`, and I expected a field name or tuple index.

d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:237:1

I was parsing a statement, and this token cannot start a statement here.

d4_l1_scope_violation := [BM].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l1_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:237:23

I was parsing a statement, and this token cannot start a statement here.

d4_l1_scope_violation := [BM].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:237:26

I was parsing a statement, and this token cannot start a statement here.

d4_l1_scope_violation := [BM].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:237:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l1_scope_violation := [BM].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:237:30

I was parsing a statement, and this token cannot start a statement here.

d4_l1_scope_violation := [BM].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:237:31

I was parsing a statement, and this token cannot start a statement here.

d4_l1_scope_violation := [BM].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:247:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:249:1

I was parsing a statement, and this token cannot start a statement here.

d4_l2_scope_violation := [BQ].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l2_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:249:23

I was parsing a statement, and this token cannot start a statement here.

d4_l2_scope_violation := [BQ].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:249:26

I was parsing a statement, and this token cannot start a statement here.

d4_l2_scope_violation := [BQ].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:249:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l2_scope_violation := [BQ].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:249:30

I was parsing a statement, and this token cannot start a statement here.

d4_l2_scope_violation := [BQ].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:249:31

I was parsing a statement, and this token cannot start a statement here.

d4_l2_scope_violation := [BQ].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:259:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:261:1

I was parsing a statement, and this token cannot start a statement here.

d4_l3_scope_violation := [BU].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d4_l3_scope_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:261:23

I was parsing a statement, and this token cannot start a statement here.

d4_l3_scope_violation := [BU].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:261:26

I was parsing a statement, and this token cannot start a statement here.

d4_l3_scope_violation := [BU].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:261:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d4_l3_scope_violation := [BU].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:261:30

I was parsing a statement, and this token cannot start a statement here.

d4_l3_scope_violation := [BU].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:261:31

I was parsing a statement, and this token cannot start a statement here.

d4_l3_scope_violation := [BU].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:271:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:273:1

I was parsing a statement, and this token cannot start a statement here.

d5_all_types_then_vals := [BY].{
^^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_all_types_then_vals here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:273:24

I was parsing a statement, and this token cannot start a statement here.

d5_all_types_then_vals := [BY].{
                       ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:273:27

I was parsing a statement, and this token cannot start a statement here.

d5_all_types_then_vals := [BY].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:273:30

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_all_types_then_vals := [BY].{
                             ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:273:31

I was parsing a statement, and this token cannot start a statement here.

d5_all_types_then_vals := [BY].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:273:32

I was parsing a statement, and this token cannot start a statement here.

d5_all_types_then_vals := [BY].{
                               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:291:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:292:30

I was parsing access after `.`, and I expected a field name or tuple index.

d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:292:33

I was parsing access after `.`, and I expected a field name or tuple index.

d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
                                ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:292:36

I was parsing access after `.`, and I expected a field name or tuple index.

d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
                                   ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:292:39

I was parsing access after `.`, and I expected a field name or tuple index.

d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
                                      ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:294:1

I was parsing a statement, and this token cannot start a statement here.

d5_all_vals_then_types := [CD].{
^^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_all_vals_then_types here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:294:24

I was parsing a statement, and this token cannot start a statement here.

d5_all_vals_then_types := [CD].{
                       ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:294:27

I was parsing a statement, and this token cannot start a statement here.

d5_all_vals_then_types := [CD].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:294:30

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_all_vals_then_types := [CD].{
                             ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:294:31

I was parsing a statement, and this token cannot start a statement here.

d5_all_vals_then_types := [CD].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:294:32

I was parsing a statement, and this token cannot start a statement here.

d5_all_vals_then_types := [CD].{
                               ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:312:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:313:30

I was parsing access after `.`, and I expected a field name or tuple index.

d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
                             ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:313:33

I was parsing access after `.`, and I expected a field name or tuple index.

d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
                                ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:313:36

I was parsing access after `.`, and I expected a field name or tuple index.

d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
                                   ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:313:39

I was parsing access after `.`, and I expected a field name or tuple index.

d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
                                      ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:315:1

I was parsing a statement, and this token cannot start a statement here.

d5_deep_interleave := [CI].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_deep_interleave here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:315:20

I was parsing a statement, and this token cannot start a statement here.

d5_deep_interleave := [CI].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:315:23

I was parsing a statement, and this token cannot start a statement here.

d5_deep_interleave := [CI].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:315:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_deep_interleave := [CI].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:315:27

I was parsing a statement, and this token cannot start a statement here.

d5_deep_interleave := [CI].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:315:28

I was parsing a statement, and this token cannot start a statement here.

d5_deep_interleave := [CI].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:333:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:334:26

I was parsing access after `.`, and I expected a field name or tuple index.

d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:334:29

I was parsing access after `.`, and I expected a field name or tuple index.

d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:334:32

I was parsing access after `.`, and I expected a field name or tuple index.

d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:334:35

I was parsing access after `.`, and I expected a field name or tuple index.

d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
                                  ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:336:1

I was parsing a statement, and this token cannot start a statement here.

d5_l4_val_after_l5 := [CN].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l4_val_after_l5 here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:336:20

I was parsing a statement, and this token cannot start a statement here.

d5_l4_val_after_l5 := [CN].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:336:23

I was parsing a statement, and this token cannot start a statement here.

d5_l4_val_after_l5 := [CN].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:336:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l4_val_after_l5 := [CN].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:336:27

I was parsing a statement, and this token cannot start a statement here.

d5_l4_val_after_l5 := [CN].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:336:28

I was parsing a statement, and this token cannot start a statement here.

d5_l4_val_after_l5 := [CN].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:348:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:349:26

I was parsing access after `.`, and I expected a field name or tuple index.

d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:349:29

I was parsing access after `.`, and I expected a field name or tuple index.

d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:349:32

I was parsing access after `.`, and I expected a field name or tuple index.

d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:349:35

I was parsing access after `.`, and I expected a field name or tuple index.

d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
                                  ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:351:1

I was parsing a statement, and this token cannot start a statement here.

d5_l3_val_after_l4 := [CS].{
^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l3_val_after_l4 here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:351:20

I was parsing a statement, and this token cannot start a statement here.

d5_l3_val_after_l4 := [CS].{
                   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:351:23

I was parsing a statement, and this token cannot start a statement here.

d5_l3_val_after_l4 := [CS].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:351:26

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l3_val_after_l4 := [CS].{
                         ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:351:27

I was parsing a statement, and this token cannot start a statement here.

d5_l3_val_after_l4 := [CS].{
                          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:351:28

I was parsing a statement, and this token cannot start a statement here.

d5_l3_val_after_l4 := [CS].{
                           ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:365:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:366:26

I was parsing access after `.`, and I expected a field name or tuple index.

d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:366:29

I was parsing access after `.`, and I expected a field name or tuple index.

d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:366:32

I was parsing access after `.`, and I expected a field name or tuple index.

d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:366:35

I was parsing access after `.`, and I expected a field name or tuple index.

d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
                                  ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:368:1

I was parsing a statement, and this token cannot start a statement here.

d5_l1_val_last := [DC].{
^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l1_val_last here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:368:16

I was parsing a statement, and this token cannot start a statement here.

d5_l1_val_last := [DC].{
               ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:368:19

I was parsing a statement, and this token cannot start a statement here.

d5_l1_val_last := [DC].{
                  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:368:22

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l1_val_last := [DC].{
                     ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:368:23

I was parsing a statement, and this token cannot start a statement here.

d5_l1_val_last := [DC].{
                      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:368:24

I was parsing a statement, and this token cannot start a statement here.

d5_l1_val_last := [DC].{
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:386:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:388:22

I was parsing access after `.`, and I expected a field name or tuple index.

d5_7 = d5_l1_val_last.L2.val2
                     ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:389:22

I was parsing access after `.`, and I expected a field name or tuple index.

d5_8 = d5_l1_val_last.L2.L3.val3
                     ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:389:25

I was parsing access after `.`, and I expected a field name or tuple index.

d5_8 = d5_l1_val_last.L2.L3.val3
                        ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:390:22

I was parsing access after `.`, and I expected a field name or tuple index.

d5_9 = d5_l1_val_last.L2.L3.L4.val4
                     ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:390:25

I was parsing access after `.`, and I expected a field name or tuple index.

d5_9 = d5_l1_val_last.L2.L3.L4.val4
                        ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:390:28

I was parsing access after `.`, and I expected a field name or tuple index.

d5_9 = d5_l1_val_last.L2.L3.L4.val4
                           ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:391:23

I was parsing access after `.`, and I expected a field name or tuple index.

d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
                      ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L2 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:391:26

I was parsing access after `.`, and I expected a field name or tuple index.

d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
                         ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L3 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:391:29

I was parsing access after `.`, and I expected a field name or tuple index.

d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
                            ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L4 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ expected record accessor ─ associated_items_complete_all_patterns.md:391:32

I was parsing access after `.`, and I expected a field name or tuple index.

d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
                               ^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found .L5 here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:393:1

I was parsing a statement, and this token cannot start a statement here.

d5_l1_to_l5_violation := [DH].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l1_to_l5_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:393:23

I was parsing a statement, and this token cannot start a statement here.

d5_l1_to_l5_violation := [DH].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:393:26

I was parsing a statement, and this token cannot start a statement here.

d5_l1_to_l5_violation := [DH].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:393:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l1_to_l5_violation := [DH].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:393:30

I was parsing a statement, and this token cannot start a statement here.

d5_l1_to_l5_violation := [DH].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:393:31

I was parsing a statement, and this token cannot start a statement here.

d5_l1_to_l5_violation := [DH].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:405:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:407:1

I was parsing a statement, and this token cannot start a statement here.

d5_l3_to_l5_violation := [DM].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l3_to_l5_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:407:23

I was parsing a statement, and this token cannot start a statement here.

d5_l3_to_l5_violation := [DM].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:407:26

I was parsing a statement, and this token cannot start a statement here.

d5_l3_to_l5_violation := [DM].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:407:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l3_to_l5_violation := [DM].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:407:30

I was parsing a statement, and this token cannot start a statement here.

d5_l3_to_l5_violation := [DM].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:407:31

I was parsing a statement, and this token cannot start a statement here.

d5_l3_to_l5_violation := [DM].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:419:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:421:1

I was parsing a statement, and this token cannot start a statement here.

d5_l4_to_l5_violation := [DR].{
^^^^^^^^^^^^^^^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found d5_l4_to_l5_violation here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:421:23

I was parsing a statement, and this token cannot start a statement here.

d5_l4_to_l5_violation := [DR].{
                      ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found := here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:421:26

I was parsing a statement, and this token cannot start a statement here.

d5_l4_to_l5_violation := [DR].{
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ─ associated_items_complete_all_patterns.md:421:29

I was parsing a type annotation, and I found a type argument without
parentheses.

d5_l4_to_l5_violation := [DR].{
                            ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:421:30

I was parsing a statement, and this token cannot start a statement here.

d5_l4_to_l5_violation := [DR].{
                             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found . here.

── ✗ unexpected statement ───── associated_items_complete_all_patterns.md:421:31

I was parsing a statement, and this token cannot start a statement here.

d5_l4_to_l5_violation := [DR].{
                              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ────── associated_items_complete_all_patterns.md:433:1

I was parsing a statement, and this token cannot start a statement here.

}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ name not in scope ─────────── associated_items_complete_all_patterns.md:6:8

Nothing is named d1_forward in this scope.

d1_1 = d1_forward.first
       ^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────── associated_items_complete_all_patterns.md:11:8

Nothing is named d1_scope in this scope.

d1_2 = d1_scope.inner
       ^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────── associated_items_complete_all_patterns.md:20:8

Nothing is named d2_inner_first in this scope.

d2_1 = d2_inner_first.outer_val
       ^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:21:8

I don't recognize this syntax.

d2_2 = d2_inner_first.Inner.inner_val
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ──────────── associated_items_complete_all_patterns.md:24:5

The type Inner is being redeclared.

Inner := [H].{
    inner_val = outer_val
}

But Inner was already declared in associated_items_complete_all_patterns.md:14:5:

Inner := [D].{
    inner_val = outer_val
}

── ● duplicate definition ─────── associated_items_complete_all_patterns.md:28:5

The name outer_val is being redeclared here:

outer_val = 500
^^^^^^^^^

In this scope, outer_val was already defined in associated_items_complete_all_patterns.md:15:21:

inner_val = outer_val
            ^^^^^^^^^

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:30:8

I don't recognize this syntax.

d2_3 = d2_outer_val_middle.Inner.inner_val
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ─────── associated_items_complete_all_patterns.md:33:5

The name outer_val is being redeclared here:

outer_val = d2_outer_refs_inner.Inner.inner_val
^^^^^^^^^

In this scope, outer_val was already defined in associated_items_complete_all_patterns.md:28:5:

outer_val = 500
^^^^^^^^^

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:33:17

I don't recognize this syntax.

outer_val = d2_outer_refs_inner.Inner.inner_val
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ──────────── associated_items_complete_all_patterns.md:35:5

The type Inner is being redeclared.

Inner := [J].{
    inner_val = 600
}

But Inner was already declared in associated_items_complete_all_patterns.md:14:5:

Inner := [D].{
    inner_val = outer_val
}

── ✗ name not in scope ────────── associated_items_complete_all_patterns.md:39:8

Nothing is named d2_outer_refs_inner in this scope.

d2_4 = d2_outer_refs_inner.outer_val
       ^^^^^^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ type redeclared ──────────── associated_items_complete_all_patterns.md:42:5

The type Inner is being redeclared.

Inner := [L].{
    inner_private = 700
}

But Inner was already declared in associated_items_complete_all_patterns.md:14:5:

Inner := [D].{
    inner_val = outer_val
}

── ✗ name not in scope ───────── associated_items_complete_all_patterns.md:46:26

Nothing is named inner_private in this scope.

outer_trying_inner = inner_private
                     ^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:51:16

I don't recognize this syntax.

valA = d2_siblings.InnerB.valB + 1
       ^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:58:8

I don't recognize this syntax.

d2_5 = d2_siblings.InnerA.valA
       ^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ name not in scope ────────── associated_items_complete_all_patterns.md:71:8

Nothing is named d3_types_then_vals in this scope.

d3_1 = d3_types_then_vals.val1
       ^^^^^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:72:8

I don't recognize this syntax.

d3_2 = d3_types_then_vals.L2.val2
       ^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:73:8

I don't recognize this syntax.

d3_3 = d3_types_then_vals.L2.L3.val3
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ─────── associated_items_complete_all_patterns.md:76:5

The name val1 is being redeclared here:

val1 = 30
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:63:20:

val3 = val1 + val2
       ^^^^

── ✗ type redeclared ──────────── associated_items_complete_all_patterns.md:78:5

The type L2 is being redeclared.

L2 := [T].{
    val2 = val1 + 5

    L3 := [U].{
        val3 = val1 + val2
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ name not in scope ────────── associated_items_complete_all_patterns.md:86:8

Nothing is named d3_vals_then_types in this scope.

d3_4 = d3_vals_then_types.val1
       ^^^^^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:87:8

I don't recognize this syntax.

d3_5 = d3_vals_then_types.L2.val2
       ^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ──────── associated_items_complete_all_patterns.md:88:8

I don't recognize this syntax.

d3_6 = d3_vals_then_types.L2.L3.val3
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ──────────── associated_items_complete_all_patterns.md:91:5

The type L2 is being redeclared.

L2 := [W].{
    L3 := [X].{
        l3_private = 999
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ name not in scope ───────── associated_items_complete_all_patterns.md:97:14

Nothing is named l3_private in this scope.

bad_l1 = l3_private
         ^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:101:5

The type L2 is being redeclared.

L2 := [Z].{
    L3 := [AA].{
        l3_secret = 888
    }

    bad_l2 = l3_secret
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:111:5

The type L2 is being redeclared.

L2 := [AC].{
    L3 := [AD].{
        val3 = val2 * 2
    }

    val2 = val1 * 3
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:119:5

The name val1 is being redeclared here:

val1 = 5
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:76:5:

val1 = 30
^^^^

── ✗ name not in scope ───────── associated_items_complete_all_patterns.md:121:8

Nothing is named d3_val_after_nested in this scope.

d3_7 = d3_val_after_nested.val1
       ^^^^^^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:122:8

I don't recognize this syntax.

d3_8 = d3_val_after_nested.L2.val2
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:123:8

I don't recognize this syntax.

d3_9 = d3_val_after_nested.L2.L3.val3
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:126:5

The type L2 is being redeclared.

L2 := [AF].{
    L3 := [AG].{
        L4 := [AH].{
            val4 = val1 + val2 + val3
        }

        val3 = 3
    }

    val2 = 2
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:138:5

The name val1 is being redeclared here:

val1 = 1
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:119:5:

val1 = 5
^^^^

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:140:8

I don't recognize this syntax.

d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ────── associated_items_complete_all_patterns.md:143:5

The name val1 is being redeclared here:

val1 = 10
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:138:5:

val1 = 1
^^^^

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:145:5

The type L2 is being redeclared.

L2 := [AJ].{
    val2 = val1 + 1

    L3 := [AK].{
        val3 = val1 + val2

        L4 := [AL].{
            val4 = val1 + val2 + val3
        }
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:157:8

I don't recognize this syntax.

d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:160:5

The type L2 is being redeclared.

L2 := [AN].{
    L3 := [AO].{
        L4 := [AP].{
            val4 = val3 + 1
        }

        val3 = val2 + 1
    }

    val2 = val1 + 1
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:172:5

The name val1 is being redeclared here:

val1 = 7
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:143:5:

val1 = 10
^^^^

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:174:8

I don't recognize this syntax.

d4_3 = d4_reverse_types.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ────── associated_items_complete_all_patterns.md:177:5

The name val1 is being redeclared here:

val1 = 15
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:172:5:

val1 = 7
^^^^

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:179:5

The type L2 is being redeclared.

L2 := [AR].{
    L3 := [AS].{
        val3 = val1 + val2

        L4 := [AT].{
            val4 = val1 + val2 + val3
        }
    }

    val2 = val1 + 5
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:191:8

I don't recognize this syntax.

d4_4 = d4_interleaved.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:194:5

The type L2 is being redeclared.

L2 := [BB].{
    L3 := [BC].{
        L4 := [BD].{
            val4 = val3 * 3
        }
        val3 = 12
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:203:8

I don't recognize this syntax.

d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:206:5

The type L2 is being redeclared.

L2 := [BF].{
    L3 := [BG].{
        L4 := [BH].{
            val4 = val2 + val3
        }

        val3 = 8
    }

    val2 = 4
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:218:8

I don't recognize this syntax.

d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:221:5

The type L2 is being redeclared.

L2 := [BJ].{
    L3 := [BK].{
        L4 := [BL].{
            val4 = val1 + 100
        }

        val3 = val1 + 50
    }

    val2 = val1 + 10
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:233:5

The name val1 is being redeclared here:

val1 = 3
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:177:5:

val1 = 15
^^^^

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:235:8

I don't recognize this syntax.

d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:238:5

The type L2 is being redeclared.

L2 := [BN].{
    L3 := [BO].{
        L4 := [BP].{
            l4_val = 444
        }
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ name not in scope ──────── associated_items_complete_all_patterns.md:246:11

Nothing is named l4_val in this scope.

bad = l4_val
      ^^^^^^

Is it misspelled, or is there an import missing?

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:250:5

The type L2 is being redeclared.

L2 := [BR].{
    L3 := [BS].{
        L4 := [BT].{
            l4_secret = 333
        }
    }

    bad = l4_secret
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:262:5

The type L2 is being redeclared.

L2 := [BV].{
    L3 := [BW].{
        L4 := [BX].{
            l4_private = 555
        }

        attempt = l4_private
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:274:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:290:5

The name val1 is being redeclared here:

val1 = 1
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:233:5:

val1 = 3
^^^^

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:292:8

I don't recognize this syntax.

d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ────── associated_items_complete_all_patterns.md:295:5

The name val1 is being redeclared here:

val1 = 100
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:290:5:

val1 = 1
^^^^

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:297:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:313:8

I don't recognize this syntax.

d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ● duplicate definition ────── associated_items_complete_all_patterns.md:316:5

The name val1 is being redeclared here:

val1 = 2
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:295:5:

val1 = 100
^^^^

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:318:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:334:8

I don't recognize this syntax.

d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:337:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:349:8

I don't recognize this syntax.

d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:352:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:366:8

I don't recognize this syntax.

d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:369:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:385:5

The name val1 is being redeclared here:

val1 = 5
^^^^

In this scope, val1 was already defined in associated_items_complete_all_patterns.md:316:5:

val1 = 2
^^^^

── ✗ name not in scope ───────── associated_items_complete_all_patterns.md:387:8

Nothing is named d5_l1_val_last in this scope.

d5_6 = d5_l1_val_last.val1
       ^^^^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:388:8

I don't recognize this syntax.

d5_7 = d5_l1_val_last.L2.val2
       ^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:389:8

I don't recognize this syntax.

d5_8 = d5_l1_val_last.L2.L3.val3
       ^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:390:8

I don't recognize this syntax.

d5_9 = d5_l1_val_last.L2.L3.L4.val4
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ unrecognized syntax ─────── associated_items_complete_all_patterns.md:391:9

I don't recognize this syntax.

d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:394:5

The type L2 is being redeclared.

L2 := [DI].{
    L3 := [DJ].{
        L4 := [DK].{
            L5 := [DL].{
                deep_secret = 12345
            }
        }
    }
}

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ● duplicate definition ────── associated_items_complete_all_patterns.md:404:5

The name bad is being redeclared here:

bad = deep_secret
^^^

In this scope, bad was already defined in associated_items_complete_all_patterns.md:246:5:

bad = l4_val
^^^

── ✗ name not in scope ──────── associated_items_complete_all_patterns.md:404:11

Nothing is named deep_secret in this scope.

bad = deep_secret
      ^^^^^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:408:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ type redeclared ─────────── associated_items_complete_all_patterns.md:422:5

The type L2 is being redeclared.

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

But L2 was already declared in associated_items_complete_all_patterns.md:61:5:

L2 := [Q].{
    L3 := [R].{
        val3 = val1 + val2
    }

    val2 = 20
}

── ✗ type mod missing matching type ─ associated_items_complete_all_patterns.md:2:1

Type mods must have a nominal type declaration matching the mod name.

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

This file is named Test.roc, but no top-level nominal type named Test was found.

Add a nominal type like:
Test := ...
or:
Test :: ... (opaque nominal type)

── ✗ polymorphic value ────────── associated_items_complete_all_patterns.md:51:9

This top-level value still has an unresolved polymorphic type.

valA = d2_siblings.InnerB.valB + 1
^^^^

Its type is:
a where [a.plus : a, Dec -> a]
Add an annotation or use this value in a way that fixes its concrete type.

── ✗ missing method ──────────── associated_items_complete_all_patterns.md:51:16

This is trying to use the + operator on a value whose type is an unresolved
type variable, which has no methods.

valA = d2_siblings.InnerB.valB + 1
       ^^^^^^^^^^^^^^^^^^^^^^^

Hint: You can replace this static dispatch call with an ordinary function call,
or force the type variable to become more concrete—for example, by adding a
type annotation that narrows its type to something that actually has methods.

# TOKENS
~~~zig
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "first"))
			(e-ident (raw "second")))
		(s-decl
			(p-ident (raw "second"))
			(e-int (raw "100")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d1_1"))
			(e-field-access
				(receiver
					(e-ident (raw "d1_forward")))
				(segment (mode "required") (field "first"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "inner"))
			(e-int (raw "200")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d1_2"))
			(e-field-access
				(receiver
					(e-ident (raw "d1_scope")))
				(segment (mode "required") (field "inner"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "Inner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "D"))))
			(associated
				(s-decl
					(p-ident (raw "inner_val"))
					(e-ident (raw "outer_val")))))
		(s-decl
			(p-ident (raw "outer_val"))
			(e-int (raw "300")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d2_1"))
			(e-field-access
				(receiver
					(e-ident (raw "d2_inner_first")))
				(segment (mode "required") (field "outer_val"))))
		(s-decl
			(p-ident (raw "d2_2"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "inner_val"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "Inner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "H"))))
			(associated
				(s-decl
					(p-ident (raw "inner_val"))
					(e-ident (raw "outer_val")))))
		(s-decl
			(p-ident (raw "outer_val"))
			(e-int (raw "500")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d2_3"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "inner_val"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "outer_val"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "inner_val"))))
		(s-type-decl
			(header (name "Inner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "J"))))
			(associated
				(s-decl
					(p-ident (raw "inner_val"))
					(e-int (raw "600")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d2_4"))
			(e-field-access
				(receiver
					(e-ident (raw "d2_outer_refs_inner")))
				(segment (mode "required") (field "outer_val"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "Inner")
				(args))
			(ty-tag-union
				(tags
					(ty (name "L"))))
			(associated
				(s-decl
					(p-ident (raw "inner_private"))
					(e-int (raw "700")))))
		(s-decl
			(p-ident (raw "outer_trying_inner"))
			(e-ident (raw "inner_private")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "InnerA")
				(args))
			(ty-tag-union
				(tags
					(ty (name "N"))))
			(associated
				(s-decl
					(p-ident (raw "valA"))
					(e-binop (op "+")
						(e-field-access
							(receiver
								(e-malformed (reason "expr_dot_suffix_not_allowed")))
							(segment (mode "required") (field "valB")))
						(e-int (raw "1"))))))
		(s-type-decl
			(header (name "InnerB")
				(args))
			(ty-tag-union
				(tags
					(ty (name "O"))))
			(associated
				(s-decl
					(p-ident (raw "valB"))
					(e-int (raw "800")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d2_5"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "valA"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Q"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "R"))))
					(associated
						(s-decl
							(p-ident (raw "val3"))
							(e-binop (op "+")
								(e-ident (raw "val1"))
								(e-ident (raw "val2"))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-int (raw "20")))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "10")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_1"))
			(e-field-access
				(receiver
					(e-ident (raw "d3_types_then_vals")))
				(segment (mode "required") (field "val1"))))
		(s-decl
			(p-ident (raw "d3_2"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val2"))))
		(s-decl
			(p-ident (raw "d3_3"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val3"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "30")))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "T"))))
			(associated
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "5"))))
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "U"))))
					(associated
						(s-decl
							(p-ident (raw "val3"))
							(e-binop (op "+")
								(e-ident (raw "val1"))
								(e-ident (raw "val2"))))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_4"))
			(e-field-access
				(receiver
					(e-ident (raw "d3_vals_then_types")))
				(segment (mode "required") (field "val1"))))
		(s-decl
			(p-ident (raw "d3_5"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val2"))))
		(s-decl
			(p-ident (raw "d3_6"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val3"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "W"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "X"))))
					(associated
						(s-decl
							(p-ident (raw "l3_private"))
							(e-int (raw "999")))))))
		(s-decl
			(p-ident (raw "bad_l1"))
			(e-ident (raw "l3_private")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Z"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AA"))))
					(associated
						(s-decl
							(p-ident (raw "l3_secret"))
							(e-int (raw "888")))))
				(s-decl
					(p-ident (raw "bad_l2"))
					(e-ident (raw "l3_secret")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AC"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AD"))))
					(associated
						(s-decl
							(p-ident (raw "val3"))
							(e-binop (op "*")
								(e-ident (raw "val2"))
								(e-int (raw "2"))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "*")
						(e-ident (raw "val1"))
						(e-int (raw "3"))))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "5")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_7"))
			(e-field-access
				(receiver
					(e-ident (raw "d3_val_after_nested")))
				(segment (mode "required") (field "val1"))))
		(s-decl
			(p-ident (raw "d3_8"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val2"))))
		(s-decl
			(p-ident (raw "d3_9"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val3"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AF"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AG"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "AH"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))))
						(s-decl
							(p-ident (raw "val3"))
							(e-int (raw "3")))))
				(s-decl
					(p-ident (raw "val2"))
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "1")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_1"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "10")))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AJ"))))
			(associated
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "1"))))
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AK"))))
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
									(ty (name "AL"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_2"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AN"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AO"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "AP"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-ident (raw "val3"))
										(e-int (raw "1"))))))
						(s-decl
							(p-ident (raw "val3"))
							(e-binop (op "+")
								(e-ident (raw "val2"))
								(e-int (raw "1"))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "7")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_3"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "15")))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "AR"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "AS"))))
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
									(ty (name "AT"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "5"))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_4"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
							(ty (name "BC"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BD"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "*")
										(e-ident (raw "val3"))
										(e-int (raw "3"))))))
						(s-decl
							(p-ident (raw "val3"))
							(e-int (raw "12")))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_5"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BF"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BG"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BH"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-ident (raw "val2"))
										(e-ident (raw "val3"))))))
						(s-decl
							(p-ident (raw "val3"))
							(e-int (raw "8")))))
				(s-decl
					(p-ident (raw "val2"))
					(e-int (raw "4")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_6"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BJ"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BK"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BL"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-ident (raw "val1"))
										(e-int (raw "100"))))))
						(s-decl
							(p-ident (raw "val3"))
							(e-binop (op "+")
								(e-ident (raw "val1"))
								(e-int (raw "50"))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "10"))))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "3")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d4_7"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BN"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BO"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BP"))))
							(associated
								(s-decl
									(p-ident (raw "l4_val"))
									(e-int (raw "444")))))))))
		(s-decl
			(p-ident (raw "bad"))
			(e-ident (raw "l4_val")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BR"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BS"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BT"))))
							(associated
								(s-decl
									(p-ident (raw "l4_secret"))
									(e-int (raw "333")))))))
				(s-decl
					(p-ident (raw "bad"))
					(e-ident (raw "l4_secret")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BV"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "BW"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "BX"))))
							(associated
								(s-decl
									(p-ident (raw "l4_private"))
									(e-int (raw "555")))))
						(s-decl
							(p-ident (raw "attempt"))
							(e-ident (raw "l4_private")))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "BZ"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "CA"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "CB"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CC"))))
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
									(e-int (raw "4")))))
						(s-decl
							(p-ident (raw "val3"))
							(e-int (raw "3")))))
				(s-decl
					(p-ident (raw "val2"))
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "1")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_1"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "100")))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CE"))))
			(associated
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "10"))))
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "CF"))))
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
									(ty (name "CG"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CH"))))
									(associated
										(s-decl
											(p-ident (raw "val5"))
											(e-binop (op "+")
												(e-binop (op "+")
													(e-binop (op "+")
														(e-ident (raw "val1"))
														(e-ident (raw "val2")))
													(e-ident (raw "val3")))
												(e-ident (raw "val4"))))))))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_2"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "2")))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CJ"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "CK"))))
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
									(ty (name "CL"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CM"))))
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
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))))))
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "1"))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_3"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CO"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "CP"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "CQ"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CR"))))
									(associated
										(s-decl
											(p-ident (raw "val5"))
											(e-binop (op "*")
												(e-ident (raw "val4"))
												(e-int (raw "5"))))))
								(s-decl
									(p-ident (raw "val4"))
									(e-int (raw "6")))))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_4"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "CT"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "CU"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "CV"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "CW"))))
									(associated
										(s-decl
											(p-ident (raw "val5"))
											(e-binop (op "+")
												(e-ident (raw "val3"))
												(e-ident (raw "val4"))))))
								(s-decl
									(p-ident (raw "val4"))
									(e-int (raw "7")))))
						(s-decl
							(p-ident (raw "val3"))
							(e-int (raw "3")))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_5"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DD"))))
			(associated
				(s-decl
					(p-ident (raw "val2"))
					(e-binop (op "+")
						(e-ident (raw "val1"))
						(e-int (raw "10"))))
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "DE"))))
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
									(ty (name "DF"))))
							(associated
								(s-decl
									(p-ident (raw "val4"))
									(e-binop (op "+")
										(e-binop (op "+")
											(e-ident (raw "val1"))
											(e-ident (raw "val2")))
										(e-ident (raw "val3"))))
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "DG"))))
									(associated
										(s-decl
											(p-ident (raw "val5"))
											(e-binop (op "+")
												(e-binop (op "+")
													(e-binop (op "+")
														(e-ident (raw "val1"))
														(e-ident (raw "val2")))
													(e-ident (raw "val3")))
												(e-ident (raw "val4"))))))))))))
		(s-decl
			(p-ident (raw "val1"))
			(e-int (raw "5")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_6"))
			(e-field-access
				(receiver
					(e-ident (raw "d5_l1_val_last")))
				(segment (mode "required") (field "val1"))))
		(s-decl
			(p-ident (raw "d5_7"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val2"))))
		(s-decl
			(p-ident (raw "d5_8"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val3"))))
		(s-decl
			(p-ident (raw "d5_9"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val4"))))
		(s-decl
			(p-ident (raw "d5_10"))
			(e-field-access
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(segment (mode "required") (field "val5"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DI"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "DJ"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "DK"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "DL"))))
									(associated
										(s-decl
											(p-ident (raw "deep_secret"))
											(e-int (raw "12345")))))))))))
		(s-decl
			(p-ident (raw "bad"))
			(e-ident (raw "deep_secret")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DN"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "DO"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "DP"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "DQ"))))
									(associated
										(s-decl
											(p-ident (raw "l5_secret"))
											(e-int (raw "9999")))))))
						(s-decl
							(p-ident (raw "bad"))
							(e-ident (raw "l5_secret")))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "L2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "DS"))))
			(associated
				(s-type-decl
					(header (name "L3")
						(args))
					(ty-tag-union
						(tags
							(ty (name "DT"))))
					(associated
						(s-type-decl
							(header (name "L4")
								(args))
							(ty-tag-union
								(tags
									(ty (name "DU"))))
							(associated
								(s-type-decl
									(header (name "L5")
										(args))
									(ty-tag-union
										(tags
											(ty (name "DV"))))
									(associated
										(s-decl
											(p-ident (raw "l5_only"))
											(e-int (raw "8888")))))
								(s-decl
									(p-ident (raw "bad"))
									(e-ident (raw "l5_only")))))))))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc


first = second

second = 100

d1_1 = d1_forward.first


inner = 200

d1_2 = d1_scope.inner


Inner := [D].{
	inner_val = outer_val
}

outer_val = 300

d2_1 = d2_inner_first.outer_val

d2_2 = .inner_val


Inner := [H].{
	inner_val = outer_val
}

outer_val = 500

d2_3 = .inner_val


outer_val = .inner_val

Inner := [J].{
	inner_val = 600
}

d2_4 = d2_outer_refs_inner.outer_val


Inner := [L].{
	inner_private = 700
}

outer_trying_inner = inner_private



InnerA := [N].{
	valA = .valB + 1
}

InnerB := [O].{
	valB = 800
}

d2_5 = .valA


L2 := [Q].{
	L3 := [R].{
		val3 = val1 + val2
	}

	val2 = 20
}

val1 = 10

d3_1 = d3_types_then_vals.val1

d3_2 = .val2

d3_3 = .val3


val1 = 30

L2 := [T].{
	val2 = val1 + 5

	L3 := [U].{
		val3 = val1 + val2
	}
}

d3_4 = d3_vals_then_types.val1

d3_5 = .val2

d3_6 = .val3


L2 := [W].{
	L3 := [X].{
		l3_private = 999
	}
}

bad_l1 = l3_private



L2 := [Z].{
	L3 := [AA].{
		l3_secret = 888
	}

	bad_l2 = l3_secret
}



L2 := [AC].{
	L3 := [AD].{
		val3 = val2 * 2
	}

	val2 = val1 * 3
}

val1 = 5

d3_7 = d3_val_after_nested.val1

d3_8 = .val2

d3_9 = .val3


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

d4_1 = .val4


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

d4_2 = .val4


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

d4_3 = .val4


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

d4_4 = .val4


L2 := [BB].{
	L3 := [BC].{
		L4 := [BD].{
			val4 = val3 * 3
		}
		val3 = 12
	}
}

d4_5 = .val4


L2 := [BF].{
	L3 := [BG].{
		L4 := [BH].{
			val4 = val2 + val3
		}

		val3 = 8
	}

	val2 = 4
}

d4_6 = .val4


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

d4_7 = .val4


L2 := [BN].{
	L3 := [BO].{
		L4 := [BP].{
			l4_val = 444
		}
	}
}

bad = l4_val



L2 := [BR].{
	L3 := [BS].{
		L4 := [BT].{
			l4_secret = 333
		}
	}

	bad = l4_secret
}



L2 := [BV].{
	L3 := [BW].{
		L4 := [BX].{
			l4_private = 555
		}

		attempt = l4_private
	}
}



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

d5_1 = .val5


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

d5_2 = .val5


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

d5_3 = .val5


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

d5_4 = .val5


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

d5_5 = .val5


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

d5_6 = d5_l1_val_last.val1

d5_7 = .val2

d5_8 = .val3

d5_9 = .val4

d5_10 = .val5


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
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "first"))
		(e-lookup-local
			(p-assign (ident "second"))))
	(d-let
		(p-assign (ident "second"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "d1_1"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "first") (mode "required")))))
	(d-let
		(p-assign (ident "inner"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "d1_2"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "inner") (mode "required")))))
	(d-let
		(p-assign (ident "Test.Inner.inner_val"))
		(e-lookup-local
			(p-assign (ident "outer_val"))))
	(d-let
		(p-assign (ident "outer_val"))
		(e-num (value "300")))
	(d-let
		(p-assign (ident "d2_1"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "outer_val") (mode "required")))))
	(d-let
		(p-assign (ident "d2_2"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "inner_val") (mode "required")))))
	(d-let
		(p-assign (ident "outer_val"))
		(e-num (value "500")))
	(d-let
		(p-assign (ident "d2_3"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "inner_val") (mode "required")))))
	(d-let
		(p-assign (ident "outer_val"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "inner_val") (mode "required")))))
	(d-let
		(p-assign (ident "d2_4"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "outer_val") (mode "required")))))
	(d-let
		(p-assign (ident "outer_trying_inner"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "Test.InnerA.valA"))
		(e-dispatch-call (method "plus") (constraint-fn-var 741)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "Test.InnerB.valB"))
		(e-num (value "800")))
	(d-let
		(p-assign (ident "d2_5"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "valA") (mode "required")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-dispatch-call (method "plus") (constraint-fn-var 766)
			(receiver
				(e-lookup-local
					(p-assign (ident "val1"))))
			(args
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "d3_1"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "val1") (mode "required")))))
	(d-let
		(p-assign (ident "d3_2"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val2") (mode "required")))))
	(d-let
		(p-assign (ident "d3_3"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val3") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "30")))
	(d-let
		(p-assign (ident "d3_4"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "val1") (mode "required")))))
	(d-let
		(p-assign (ident "d3_5"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val2") (mode "required")))))
	(d-let
		(p-assign (ident "d3_6"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val3") (mode "required")))))
	(d-let
		(p-assign (ident "bad_l1"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "d3_7"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "val1") (mode "required")))))
	(d-let
		(p-assign (ident "d3_8"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val2") (mode "required")))))
	(d-let
		(p-assign (ident "d3_9"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val3") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "d4_1"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "d4_2"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "7")))
	(d-let
		(p-assign (ident "d4_3"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "15")))
	(d-let
		(p-assign (ident "d4_4"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "d4_5"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "d4_6"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "d4_7"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "d5_1"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "d5_2"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "d5_3"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "d5_4"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "d5_5"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "d5_6"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(segments
				(segment (name "val1") (mode "required")))))
	(d-let
		(p-assign (ident "d5_7"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val2") (mode "required")))))
	(d-let
		(p-assign (ident "d5_8"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val3") (mode "required")))))
	(d-let
		(p-assign (ident "d5_9"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val4") (mode "required")))))
	(d-let
		(p-assign (ident "d5_10"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "expr_not_canonicalized")))
			(segments
				(segment (name "val5") (mode "required")))))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-nominal-decl
		(ty-header (name "Inner"))
		(ty-tag-union
			(ty-tag-name (name "D"))))
	(s-nominal-decl
		(ty-header (name "Inner"))
		(ty-tag-union
			(ty-tag-name (name "H"))))
	(s-nominal-decl
		(ty-header (name "Inner"))
		(ty-tag-union
			(ty-tag-name (name "J"))))
	(s-nominal-decl
		(ty-header (name "Inner"))
		(ty-tag-union
			(ty-tag-name (name "L"))))
	(s-nominal-decl
		(ty-header (name "InnerA"))
		(ty-tag-union
			(ty-tag-name (name "N"))))
	(s-nominal-decl
		(ty-header (name "InnerB"))
		(ty-tag-union
			(ty-tag-name (name "O"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "Q"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "R"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "T"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "W"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "Z"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "AC"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "AF"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "AJ"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "AN"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "AR"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BB"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BF"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BJ"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BN"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BR"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BV"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "BZ"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "CE"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "CJ"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "CO"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "CT"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "DD"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "DI"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "DN"))))
	(s-nominal-decl
		(ty-header (name "L2"))
		(ty-tag-union
			(ty-tag-name (name "DS")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "a where [a.plus : a, Dec -> a]"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Inner")
			(ty-header (name "Inner")))
		(nominal (type "Inner")
			(ty-header (name "Inner")))
		(nominal (type "Inner")
			(ty-header (name "Inner")))
		(nominal (type "Inner")
			(ty-header (name "Inner")))
		(nominal (type "InnerA")
			(ty-header (name "InnerA")))
		(nominal (type "InnerB")
			(ty-header (name "InnerB")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2")))
		(nominal (type "L2")
			(ty-header (name "L2"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "a where [a.plus : a, Dec -> a]"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))))
~~~
