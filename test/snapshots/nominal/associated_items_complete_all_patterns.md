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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 1) (end 2 11))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d1_forward")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 1) (end 2 11) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 12) (end 2 14))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 12) (end 2 14) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 15) (end 2 16))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 15) (end 2 16) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 2 17) (end 2 18))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 17) (end 2 18) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 18) (end 2 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 18) (end 2 19) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 19) (end 2 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 19) (end 2 20) (annotation error) (line-text "d1_forward := [A].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 5 1) (end 5 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 5 1) (end 5 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 8 1) (end 8 9))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d1_scope")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 1) (end 8 9) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 8 10) (end 8 12))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 10) (end 8 12) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 8 13) (end 8 14))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 13) (end 8 14) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 8 15) (end 8 16))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 15) (end 8 16) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 8 16) (end 8 17))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 16) (end 8 17) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 8 17) (end 8 18))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 8 17) (end 8 18) (annotation error) (line-text "d1_scope := [B].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 10 1) (end 10 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 10 1) (end 10 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 1) (end 13 15))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d2_inner_first")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 1) (end 13 15) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 16) (end 13 18))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 16) (end 13 18) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 19) (end 13 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 19) (end 13 20) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 13 21) (end 13 22))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 21) (end 13 22) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 22) (end 13 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 22) (end 13 23) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 23) (end 13 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 13 23) (end 13 24) (annotation error) (line-text "d2_inner_first := [C].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 19 1) (end 19 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 19 1) (end 19 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 21 22) (end 21 28))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".Inner")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 21 22) (end 21 28) (annotation error) (line-text "d2_2 = d2_inner_first.Inner.inner_val"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 1) (end 23 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d2_outer_val_middle")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 1) (end 23 20) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 21) (end 23 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 21) (end 23 23) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 24) (end 23 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 24) (end 23 25) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 23 26) (end 23 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 26) (end 23 27) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 27) (end 23 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 27) (end 23 28) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 28) (end 23 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 23 28) (end 23 29) (annotation error) (line-text "d2_outer_val_middle := [G].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 29 1) (end 29 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 29 1) (end 29 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 30 27) (end 30 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".Inner")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 30 27) (end 30 33) (annotation error) (line-text "d2_3 = d2_outer_val_middle.Inner.inner_val"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 32 1) (end 32 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d2_outer_refs_inner")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 1) (end 32 20) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 32 21) (end 32 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 21) (end 32 23) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 32 24) (end 32 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 24) (end 32 25) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 32 26) (end 32 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 26) (end 32 27) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 32 27) (end 32 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 27) (end 32 28) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 32 28) (end 32 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 32 28) (end 32 29) (annotation error) (line-text "d2_outer_refs_inner := [I].{"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 33 36) (end 33 42))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".Inner")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 33 36) (end 33 42) (annotation error) (line-text "    outer_val = d2_outer_refs_inner.Inner.inner_val"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 38 1) (end 38 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 38 1) (end 38 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 1) (end 41 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d2_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 1) (end 41 19) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 20) (end 41 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 20) (end 41 22) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 23) (end 41 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 23) (end 41 24) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 41 25) (end 41 26))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 25) (end 41 26) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 26) (end 41 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 26) (end 41 27) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 27) (end 41 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 41 27) (end 41 28) (annotation error) (line-text "d2_scope_violation := [K].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 47 1) (end 47 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 47 1) (end 47 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 49 1) (end 49 12))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d2_siblings")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 1) (end 49 12) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 49 13) (end 49 15))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 13) (end 49 15) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 49 16) (end 49 17))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 16) (end 49 17) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 49 18) (end 49 19))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 18) (end 49 19) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 49 19) (end 49 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 19) (end 49 20) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 49 20) (end 49 21))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 49 20) (end 49 21) (annotation error) (line-text "d2_siblings := [M].{"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 51 27) (end 51 34))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".InnerB")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 51 27) (end 51 34) (annotation error) (line-text "        valA = d2_siblings.InnerB.valB + 1"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 57 1) (end 57 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 57 1) (end 57 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 58 19) (end 58 26))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".InnerA")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 58 19) (end 58 26) (annotation error) (line-text "d2_5 = d2_siblings.InnerA.valA"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 60 1) (end 60 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d3_types_then_vals")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 1) (end 60 19) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 60 20) (end 60 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 20) (end 60 22) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 60 23) (end 60 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 23) (end 60 24) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 60 25) (end 60 26))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 25) (end 60 26) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 60 26) (end 60 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 26) (end 60 27) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 60 27) (end 60 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 60 27) (end 60 28) (annotation error) (line-text "d3_types_then_vals := [P].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 70 1) (end 70 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 70 1) (end 70 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 72 26) (end 72 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 72 26) (end 72 29) (annotation error) (line-text "d3_2 = d3_types_then_vals.L2.val2"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 73 26) (end 73 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 73 26) (end 73 29) (annotation error) (line-text "d3_3 = d3_types_then_vals.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 73 29) (end 73 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 73 29) (end 73 32) (annotation error) (line-text "d3_3 = d3_types_then_vals.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 75 1) (end 75 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d3_vals_then_types")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 1) (end 75 19) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 75 20) (end 75 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 20) (end 75 22) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 75 23) (end 75 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 23) (end 75 24) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 75 25) (end 75 26))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 25) (end 75 26) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 75 26) (end 75 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 26) (end 75 27) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 75 27) (end 75 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 75 27) (end 75 28) (annotation error) (line-text "d3_vals_then_types := [S].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 85 1) (end 85 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 85 1) (end 85 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 87 26) (end 87 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 87 26) (end 87 29) (annotation error) (line-text "d3_5 = d3_vals_then_types.L2.val2"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 88 26) (end 88 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 88 26) (end 88 29) (annotation error) (line-text "d3_6 = d3_vals_then_types.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 88 29) (end 88 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 88 29) (end 88 32) (annotation error) (line-text "d3_6 = d3_vals_then_types.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 90 1) (end 90 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d3_l1_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 1) (end 90 22) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 90 23) (end 90 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 23) (end 90 25) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 90 26) (end 90 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 26) (end 90 27) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 90 28) (end 90 29))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 28) (end 90 29) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 90 29) (end 90 30))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 29) (end 90 30) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 90 30) (end 90 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 90 30) (end 90 31) (annotation error) (line-text "d3_l1_scope_violation := [V].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 98 1) (end 98 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 98 1) (end 98 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 100 1) (end 100 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d3_l2_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 1) (end 100 22) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 100 23) (end 100 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 23) (end 100 25) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 100 26) (end 100 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 26) (end 100 27) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 100 28) (end 100 29))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 28) (end 100 29) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 100 29) (end 100 30))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 29) (end 100 30) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 100 30) (end 100 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 100 30) (end 100 31) (annotation error) (line-text "d3_l2_scope_violation := [Y].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 108 1) (end 108 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 108 1) (end 108 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 110 1) (end 110 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d3_val_after_nested")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 1) (end 110 20) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 110 21) (end 110 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 21) (end 110 23) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 110 24) (end 110 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 24) (end 110 25) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 110 27) (end 110 28))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 27) (end 110 28) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 110 28) (end 110 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 28) (end 110 29) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 110 29) (end 110 30))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 110 29) (end 110 30) (annotation error) (line-text "d3_val_after_nested := [AB].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 120 1) (end 120 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 120 1) (end 120 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 122 27) (end 122 30))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 122 27) (end 122 30) (annotation error) (line-text "d3_8 = d3_val_after_nested.L2.val2"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 123 27) (end 123 30))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 123 27) (end 123 30) (annotation error) (line-text "d3_9 = d3_val_after_nested.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 123 30) (end 123 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 123 30) (end 123 33) (annotation error) (line-text "d3_9 = d3_val_after_nested.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 125 1) (end 125 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_all_types_then_vals")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 1) (end 125 23) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 125 24) (end 125 26))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 24) (end 125 26) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 125 27) (end 125 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 27) (end 125 28) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 125 30) (end 125 31))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 30) (end 125 31) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 125 31) (end 125 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 31) (end 125 32) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 125 32) (end 125 33))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 125 32) (end 125 33) (annotation error) (line-text "d4_all_types_then_vals := [AE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 139 1) (end 139 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 139 1) (end 139 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 140 30) (end 140 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 140 30) (end 140 33) (annotation error) (line-text "d4_1 = d4_all_types_then_vals.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 140 33) (end 140 36))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 140 33) (end 140 36) (annotation error) (line-text "d4_1 = d4_all_types_then_vals.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 140 36) (end 140 39))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 140 36) (end 140 39) (annotation error) (line-text "d4_1 = d4_all_types_then_vals.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 142 1) (end 142 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_all_vals_then_types")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 1) (end 142 23) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 142 24) (end 142 26))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 24) (end 142 26) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 142 27) (end 142 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 27) (end 142 28) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 142 30) (end 142 31))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 30) (end 142 31) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 142 31) (end 142 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 31) (end 142 32) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 142 32) (end 142 33))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 142 32) (end 142 33) (annotation error) (line-text "d4_all_vals_then_types := [AI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 156 1) (end 156 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 156 1) (end 156 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 157 30) (end 157 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 157 30) (end 157 33) (annotation error) (line-text "d4_2 = d4_all_vals_then_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 157 33) (end 157 36))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 157 33) (end 157 36) (annotation error) (line-text "d4_2 = d4_all_vals_then_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 157 36) (end 157 39))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 157 36) (end 157 39) (annotation error) (line-text "d4_2 = d4_all_vals_then_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 159 1) (end 159 17))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_reverse_types")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 1) (end 159 17) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 159 18) (end 159 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 18) (end 159 20) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 159 21) (end 159 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 21) (end 159 22) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 159 24) (end 159 25))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 24) (end 159 25) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 159 25) (end 159 26))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 25) (end 159 26) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 159 26) (end 159 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 159 26) (end 159 27) (annotation error) (line-text "d4_reverse_types := [AM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 173 1) (end 173 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 173 1) (end 173 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 174 24) (end 174 27))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 174 24) (end 174 27) (annotation error) (line-text "d4_3 = d4_reverse_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 174 27) (end 174 30))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 174 27) (end 174 30) (annotation error) (line-text "d4_3 = d4_reverse_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 174 30) (end 174 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 174 30) (end 174 33) (annotation error) (line-text "d4_3 = d4_reverse_types.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 176 1) (end 176 15))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_interleaved")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 1) (end 176 15) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 176 16) (end 176 18))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 16) (end 176 18) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 176 19) (end 176 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 19) (end 176 20) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 176 22) (end 176 23))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 22) (end 176 23) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 176 23) (end 176 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 23) (end 176 24) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 176 24) (end 176 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 176 24) (end 176 25) (annotation error) (line-text "d4_interleaved := [AQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 190 1) (end 190 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 190 1) (end 190 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 191 22) (end 191 25))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 191 22) (end 191 25) (annotation error) (line-text "d4_4 = d4_interleaved.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 191 25) (end 191 28))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 191 25) (end 191 28) (annotation error) (line-text "d4_4 = d4_interleaved.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 191 28) (end 191 31))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 191 28) (end 191 31) (annotation error) (line-text "d4_4 = d4_interleaved.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 193 1) (end 193 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l3_val_after_l4")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 1) (end 193 19) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 193 20) (end 193 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 20) (end 193 22) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 193 23) (end 193 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 23) (end 193 24) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 193 26) (end 193 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 26) (end 193 27) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 193 27) (end 193 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 27) (end 193 28) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 193 28) (end 193 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 193 28) (end 193 29) (annotation error) (line-text "d4_l3_val_after_l4 := [BA].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 202 1) (end 202 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 202 1) (end 202 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 203 26) (end 203 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 203 26) (end 203 29) (annotation error) (line-text "d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 203 29) (end 203 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 203 29) (end 203 32) (annotation error) (line-text "d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 203 32) (end 203 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 203 32) (end 203 35) (annotation error) (line-text "d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 205 1) (end 205 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l2_val_after_l3")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 1) (end 205 19) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 205 20) (end 205 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 20) (end 205 22) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 205 23) (end 205 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 23) (end 205 24) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 205 26) (end 205 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 26) (end 205 27) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 205 27) (end 205 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 27) (end 205 28) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 205 28) (end 205 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 205 28) (end 205 29) (annotation error) (line-text "d4_l2_val_after_l3 := [BE].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 217 1) (end 217 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 217 1) (end 217 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 218 26) (end 218 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 218 26) (end 218 29) (annotation error) (line-text "d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 218 29) (end 218 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 218 29) (end 218 32) (annotation error) (line-text "d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 218 32) (end 218 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 218 32) (end 218 35) (annotation error) (line-text "d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 220 1) (end 220 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l1_val_after_l2")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 1) (end 220 19) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 220 20) (end 220 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 20) (end 220 22) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 220 23) (end 220 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 23) (end 220 24) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 220 26) (end 220 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 26) (end 220 27) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 220 27) (end 220 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 27) (end 220 28) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 220 28) (end 220 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 220 28) (end 220 29) (annotation error) (line-text "d4_l1_val_after_l2 := [BI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 234 1) (end 234 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 234 1) (end 234 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 235 26) (end 235 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 235 26) (end 235 29) (annotation error) (line-text "d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 235 29) (end 235 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 235 29) (end 235 32) (annotation error) (line-text "d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 235 32) (end 235 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 235 32) (end 235 35) (annotation error) (line-text "d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 237 1) (end 237 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l1_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 1) (end 237 22) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 237 23) (end 237 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 23) (end 237 25) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 237 26) (end 237 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 26) (end 237 27) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 237 29) (end 237 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 29) (end 237 30) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 237 30) (end 237 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 30) (end 237 31) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 237 31) (end 237 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 237 31) (end 237 32) (annotation error) (line-text "d4_l1_scope_violation := [BM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 247 1) (end 247 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 247 1) (end 247 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 249 1) (end 249 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l2_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 1) (end 249 22) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 249 23) (end 249 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 23) (end 249 25) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 249 26) (end 249 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 26) (end 249 27) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 249 29) (end 249 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 29) (end 249 30) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 249 30) (end 249 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 30) (end 249 31) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 249 31) (end 249 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 249 31) (end 249 32) (annotation error) (line-text "d4_l2_scope_violation := [BQ].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 259 1) (end 259 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 259 1) (end 259 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 261 1) (end 261 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d4_l3_scope_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 1) (end 261 22) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 261 23) (end 261 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 23) (end 261 25) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 261 26) (end 261 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 26) (end 261 27) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 261 29) (end 261 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 29) (end 261 30) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 261 30) (end 261 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 30) (end 261 31) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 261 31) (end 261 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 261 31) (end 261 32) (annotation error) (line-text "d4_l3_scope_violation := [BU].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 271 1) (end 271 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 271 1) (end 271 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 273 1) (end 273 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_all_types_then_vals")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 1) (end 273 23) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 273 24) (end 273 26))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 24) (end 273 26) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 273 27) (end 273 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 27) (end 273 28) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 273 30) (end 273 31))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 30) (end 273 31) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 273 31) (end 273 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 31) (end 273 32) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 273 32) (end 273 33))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 273 32) (end 273 33) (annotation error) (line-text "d5_all_types_then_vals := [BY].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 291 1) (end 291 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 291 1) (end 291 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 292 30) (end 292 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 292 30) (end 292 33) (annotation error) (line-text "d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 292 33) (end 292 36))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 292 33) (end 292 36) (annotation error) (line-text "d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 292 36) (end 292 39))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 292 36) (end 292 39) (annotation error) (line-text "d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 292 39) (end 292 42))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 292 39) (end 292 42) (annotation error) (line-text "d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 294 1) (end 294 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_all_vals_then_types")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 1) (end 294 23) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 294 24) (end 294 26))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 24) (end 294 26) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 294 27) (end 294 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 27) (end 294 28) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 294 30) (end 294 31))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 30) (end 294 31) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 294 31) (end 294 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 31) (end 294 32) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 294 32) (end 294 33))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 294 32) (end 294 33) (annotation error) (line-text "d5_all_vals_then_types := [CD].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 312 1) (end 312 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 312 1) (end 312 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 313 30) (end 313 33))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 313 30) (end 313 33) (annotation error) (line-text "d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 313 33) (end 313 36))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 313 33) (end 313 36) (annotation error) (line-text "d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 313 36) (end 313 39))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 313 36) (end 313 39) (annotation error) (line-text "d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 313 39) (end 313 42))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 313 39) (end 313 42) (annotation error) (line-text "d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 315 1) (end 315 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_deep_interleave")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 1) (end 315 19) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 315 20) (end 315 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 20) (end 315 22) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 315 23) (end 315 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 23) (end 315 24) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 315 26) (end 315 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 26) (end 315 27) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 315 27) (end 315 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 27) (end 315 28) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 315 28) (end 315 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 315 28) (end 315 29) (annotation error) (line-text "d5_deep_interleave := [CI].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 333 1) (end 333 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 333 1) (end 333 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 334 26) (end 334 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 334 26) (end 334 29) (annotation error) (line-text "d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 334 29) (end 334 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 334 29) (end 334 32) (annotation error) (line-text "d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 334 32) (end 334 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 334 32) (end 334 35) (annotation error) (line-text "d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 334 35) (end 334 38))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 334 35) (end 334 38) (annotation error) (line-text "d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 336 1) (end 336 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l4_val_after_l5")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 1) (end 336 19) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 336 20) (end 336 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 20) (end 336 22) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 336 23) (end 336 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 23) (end 336 24) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 336 26) (end 336 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 26) (end 336 27) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 336 27) (end 336 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 27) (end 336 28) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 336 28) (end 336 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 336 28) (end 336 29) (annotation error) (line-text "d5_l4_val_after_l5 := [CN].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 348 1) (end 348 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 348 1) (end 348 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 349 26) (end 349 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 349 26) (end 349 29) (annotation error) (line-text "d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 349 29) (end 349 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 349 29) (end 349 32) (annotation error) (line-text "d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 349 32) (end 349 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 349 32) (end 349 35) (annotation error) (line-text "d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 349 35) (end 349 38))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 349 35) (end 349 38) (annotation error) (line-text "d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 351 1) (end 351 19))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l3_val_after_l4")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 1) (end 351 19) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 351 20) (end 351 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 20) (end 351 22) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 351 23) (end 351 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 23) (end 351 24) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 351 26) (end 351 27))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 26) (end 351 27) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 351 27) (end 351 28))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 27) (end 351 28) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 351 28) (end 351 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 351 28) (end 351 29) (annotation error) (line-text "d5_l3_val_after_l4 := [CS].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 365 1) (end 365 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 365 1) (end 365 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 366 26) (end 366 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 366 26) (end 366 29) (annotation error) (line-text "d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 366 29) (end 366 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 366 29) (end 366 32) (annotation error) (line-text "d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 366 32) (end 366 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 366 32) (end 366 35) (annotation error) (line-text "d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 366 35) (end 366 38))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 366 35) (end 366 38) (annotation error) (line-text "d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 368 1) (end 368 15))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l1_val_last")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 1) (end 368 15) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 368 16) (end 368 18))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 16) (end 368 18) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 368 19) (end 368 20))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 19) (end 368 20) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 368 22) (end 368 23))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 22) (end 368 23) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 368 23) (end 368 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 23) (end 368 24) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 368 24) (end 368 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 368 24) (end 368 25) (annotation error) (line-text "d5_l1_val_last := [DC].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 386 1) (end 386 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 386 1) (end 386 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 388 22) (end 388 25))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 388 22) (end 388 25) (annotation error) (line-text "d5_7 = d5_l1_val_last.L2.val2"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 389 22) (end 389 25))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 389 22) (end 389 25) (annotation error) (line-text "d5_8 = d5_l1_val_last.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 389 25) (end 389 28))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 389 25) (end 389 28) (annotation error) (line-text "d5_8 = d5_l1_val_last.L2.L3.val3"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 390 22) (end 390 25))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 390 22) (end 390 25) (annotation error) (line-text "d5_9 = d5_l1_val_last.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 390 25) (end 390 28))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 390 25) (end 390 28) (annotation error) (line-text "d5_9 = d5_l1_val_last.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 390 28) (end 390 31))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 390 28) (end 390 31) (annotation error) (line-text "d5_9 = d5_l1_val_last.L2.L3.L4.val4"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 391 23) (end 391 26))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L2")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 391 23) (end 391 26) (annotation error) (line-text "d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 391 26) (end 391 29))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L3")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 391 26) (end 391 29) (annotation error) (line-text "d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 391 29) (end 391 32))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L4")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 391 29) (end 391 32) (annotation error) (line-text "d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 391 32) (end 391 35))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".L5")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 391 32) (end 391 35) (annotation error) (line-text "d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 393 1) (end 393 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l1_to_l5_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 1) (end 393 22) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 393 23) (end 393 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 23) (end 393 25) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 393 26) (end 393 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 26) (end 393 27) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 393 29) (end 393 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 29) (end 393 30) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 393 30) (end 393 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 30) (end 393 31) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 393 31) (end 393 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 393 31) (end 393 32) (annotation error) (line-text "d5_l1_to_l5_violation := [DH].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 405 1) (end 405 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 405 1) (end 405 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 407 1) (end 407 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l3_to_l5_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 1) (end 407 22) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 407 23) (end 407 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 23) (end 407 25) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 407 26) (end 407 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 26) (end 407 27) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 407 29) (end 407 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 29) (end 407 30) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 407 30) (end 407 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 30) (end 407 31) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 407 31) (end 407 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 407 31) (end 407 32) (annotation error) (line-text "d5_l3_to_l5_violation := [DM].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 419 1) (end 419 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 419 1) (end 419 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 421 1) (end 421 22))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d5_l4_to_l5_violation")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 1) (end 421 22) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 421 23) (end 421 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 23) (end 421 25) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 421 26) (end 421 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 26) (end 421 27) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 421 29) (end 421 30))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 29) (end 421 30) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 421 30) (end 421 31))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 30) (end 421 31) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 421 31) (end 421 32))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 421 31) (end 421 32) (annotation error) (line-text "d5_l4_to_l5_violation := [DR].{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 433 1) (end 433 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 433 1) (end 433 2) (annotation error) (line-text "}"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 6 8) (end 6 18))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d1_forward")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 6 8) (end 6 18) (annotation error) (line-text "d1_1 = d1_forward.first"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 11 8) (end 11 16))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d1_scope")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 11 8) (end 11 16) (annotation error) (line-text "d1_2 = d1_scope.inner"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 20 8) (end 20 22))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d2_inner_first")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 20 8) (end 20 22) (annotation error) (line-text "d2_1 = d2_inner_first.outer_val"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 21 8) (end 21 38))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 21 8) (end 21 38) (annotation error) (line-text "d2_2 = d2_inner_first.Inner.inner_val"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 24 5) (end 26 6))
		(headline
			(reflow "The type ")
			(annotated code "Inner")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 24 5) (end 26 6) (annotation error) (line-text "    Inner := [H].{\n        inner_val = outer_val\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "Inner")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 14)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 14 5) (end 16 6) (annotation dim) (line-text "    Inner := [D].{\n        inner_val = outer_val\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 28 5) (end 28 14))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "outer_val")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 28 5) (end 28 14) (annotation error) (line-text "    outer_val = 500"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "outer_val")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 15)
				(column 21))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 15 21) (end 15 30) (annotation dim) (line-text "        inner_val = outer_val"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 30 8) (end 30 43))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 30 8) (end 30 43) (annotation error) (line-text "d2_3 = d2_outer_val_middle.Inner.inner_val"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 33 5) (end 33 14))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "outer_val")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 33 5) (end 33 14) (annotation error) (line-text "    outer_val = d2_outer_refs_inner.Inner.inner_val"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "outer_val")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 28)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 28 5) (end 28 14) (annotation dim) (line-text "    outer_val = 500"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 33 17) (end 33 52))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 33 17) (end 33 52) (annotation error) (line-text "    outer_val = d2_outer_refs_inner.Inner.inner_val"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 35 5) (end 37 6))
		(headline
			(reflow "The type ")
			(annotated code "Inner")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 35 5) (end 37 6) (annotation error) (line-text "    Inner := [J].{\n        inner_val = 600\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "Inner")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 14)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 14 5) (end 16 6) (annotation dim) (line-text "    Inner := [D].{\n        inner_val = outer_val\n    }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 39 8) (end 39 27))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d2_outer_refs_inner")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 39 8) (end 39 27) (annotation error) (line-text "d2_4 = d2_outer_refs_inner.outer_val"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 42 5) (end 44 6))
		(headline
			(reflow "The type ")
			(annotated code "Inner")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 42 5) (end 44 6) (annotation error) (line-text "    Inner := [L].{\n        inner_private = 700\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "Inner")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 14)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 14 5) (end 16 6) (annotation dim) (line-text "    Inner := [D].{\n        inner_val = outer_val\n    }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 46 26) (end 46 39))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "inner_private")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 46 26) (end 46 39) (annotation error) (line-text "    outer_trying_inner = inner_private"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 51 16) (end 51 39))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 51 16) (end 51 39) (annotation error) (line-text "        valA = d2_siblings.InnerB.valB + 1"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 58 8) (end 58 31))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 58 8) (end 58 31) (annotation error) (line-text "d2_5 = d2_siblings.InnerA.valA"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 71 8) (end 71 26))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d3_types_then_vals")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 71 8) (end 71 26) (annotation error) (line-text "d3_1 = d3_types_then_vals.val1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 72 8) (end 72 34))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 72 8) (end 72 34) (annotation error) (line-text "d3_2 = d3_types_then_vals.L2.val2"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 73 8) (end 73 37))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 73 8) (end 73 37) (annotation error) (line-text "d3_3 = d3_types_then_vals.L2.L3.val3"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 76 5) (end 76 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 76 5) (end 76 9) (annotation error) (line-text "    val1 = 30"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 63)
				(column 20))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 63 20) (end 63 24) (annotation dim) (line-text "            val3 = val1 + val2"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 78 5) (end 84 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 78 5) (end 84 6) (annotation error) (line-text "    L2 := [T].{\n        val2 = val1 + 5\n\n        L3 := [U].{\n            val3 = val1 + val2\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 86 8) (end 86 26))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d3_vals_then_types")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 86 8) (end 86 26) (annotation error) (line-text "d3_4 = d3_vals_then_types.val1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 87 8) (end 87 34))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 87 8) (end 87 34) (annotation error) (line-text "d3_5 = d3_vals_then_types.L2.val2"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 88 8) (end 88 37))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 88 8) (end 88 37) (annotation error) (line-text "d3_6 = d3_vals_then_types.L2.L3.val3"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 91 5) (end 95 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 91 5) (end 95 6) (annotation error) (line-text "    L2 := [W].{\n        L3 := [X].{\n            l3_private = 999\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 97 14) (end 97 24))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "l3_private")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 97 14) (end 97 24) (annotation error) (line-text "    bad_l1 = l3_private"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 101 5) (end 107 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 101 5) (end 107 6) (annotation error) (line-text "    L2 := [Z].{\n        L3 := [AA].{\n            l3_secret = 888\n        }\n\n        bad_l2 = l3_secret\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 111 5) (end 117 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 111 5) (end 117 6) (annotation error) (line-text "    L2 := [AC].{\n        L3 := [AD].{\n            val3 = val2 * 2\n        }\n\n        val2 = val1 * 3\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 119 5) (end 119 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 119 5) (end 119 9) (annotation error) (line-text "    val1 = 5"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 76)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 76 5) (end 76 9) (annotation dim) (line-text "    val1 = 30"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 121 8) (end 121 27))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d3_val_after_nested")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 121 8) (end 121 27) (annotation error) (line-text "d3_7 = d3_val_after_nested.val1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 122 8) (end 122 35))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 122 8) (end 122 35) (annotation error) (line-text "d3_8 = d3_val_after_nested.L2.val2"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 123 8) (end 123 38))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 123 8) (end 123 38) (annotation error) (line-text "d3_9 = d3_val_after_nested.L2.L3.val3"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 126 5) (end 136 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 126 5) (end 136 6) (annotation error) (line-text "    L2 := [AF].{\n        L3 := [AG].{\n            L4 := [AH].{\n                val4 = val1 + val2 + val3\n            }\n\n            val3 = 3\n        }\n\n        val2 = 2\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 138 5) (end 138 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 138 5) (end 138 9) (annotation error) (line-text "    val1 = 1"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 119)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 119 5) (end 119 9) (annotation dim) (line-text "    val1 = 5"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 140 8) (end 140 44))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 140 8) (end 140 44) (annotation error) (line-text "d4_1 = d4_all_types_then_vals.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 143 5) (end 143 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 143 5) (end 143 9) (annotation error) (line-text "    val1 = 10"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 138)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 138 5) (end 138 9) (annotation dim) (line-text "    val1 = 1"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 145 5) (end 155 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 145 5) (end 155 6) (annotation error) (line-text "    L2 := [AJ].{\n        val2 = val1 + 1\n\n        L3 := [AK].{\n            val3 = val1 + val2\n\n            L4 := [AL].{\n                val4 = val1 + val2 + val3\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 157 8) (end 157 44))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 157 8) (end 157 44) (annotation error) (line-text "d4_2 = d4_all_vals_then_types.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 160 5) (end 170 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 160 5) (end 170 6) (annotation error) (line-text "    L2 := [AN].{\n        L3 := [AO].{\n            L4 := [AP].{\n                val4 = val3 + 1\n            }\n\n            val3 = val2 + 1\n        }\n\n        val2 = val1 + 1\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 172 5) (end 172 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 172 5) (end 172 9) (annotation error) (line-text "    val1 = 7"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 143)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 143 5) (end 143 9) (annotation dim) (line-text "    val1 = 10"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 174 8) (end 174 38))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 174 8) (end 174 38) (annotation error) (line-text "d4_3 = d4_reverse_types.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 177 5) (end 177 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 177 5) (end 177 9) (annotation error) (line-text "    val1 = 15"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 172)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 172 5) (end 172 9) (annotation dim) (line-text "    val1 = 7"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 179 5) (end 189 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 179 5) (end 189 6) (annotation error) (line-text "    L2 := [AR].{\n        L3 := [AS].{\n            val3 = val1 + val2\n\n            L4 := [AT].{\n                val4 = val1 + val2 + val3\n            }\n        }\n\n        val2 = val1 + 5\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 191 8) (end 191 36))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 191 8) (end 191 36) (annotation error) (line-text "d4_4 = d4_interleaved.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 194 5) (end 201 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 194 5) (end 201 6) (annotation error) (line-text "    L2 := [BB].{\n        L3 := [BC].{\n            L4 := [BD].{\n                val4 = val3 * 3\n            }\n            val3 = 12\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 203 8) (end 203 40))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 203 8) (end 203 40) (annotation error) (line-text "d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 206 5) (end 216 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 206 5) (end 216 6) (annotation error) (line-text "    L2 := [BF].{\n        L3 := [BG].{\n            L4 := [BH].{\n                val4 = val2 + val3\n            }\n\n            val3 = 8\n        }\n\n        val2 = 4\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 218 8) (end 218 40))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 218 8) (end 218 40) (annotation error) (line-text "d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 221 5) (end 231 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 221 5) (end 231 6) (annotation error) (line-text "    L2 := [BJ].{\n        L3 := [BK].{\n            L4 := [BL].{\n                val4 = val1 + 100\n            }\n\n            val3 = val1 + 50\n        }\n\n        val2 = val1 + 10\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 233 5) (end 233 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 233 5) (end 233 9) (annotation error) (line-text "    val1 = 3"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 177)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 177 5) (end 177 9) (annotation dim) (line-text "    val1 = 15"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 235 8) (end 235 40))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 235 8) (end 235 40) (annotation error) (line-text "d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 238 5) (end 244 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 238 5) (end 244 6) (annotation error) (line-text "    L2 := [BN].{\n        L3 := [BO].{\n            L4 := [BP].{\n                l4_val = 444\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 246 11) (end 246 17))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "l4_val")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 246 11) (end 246 17) (annotation error) (line-text "    bad = l4_val"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 250 5) (end 258 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 250 5) (end 258 6) (annotation error) (line-text "    L2 := [BR].{\n        L3 := [BS].{\n            L4 := [BT].{\n                l4_secret = 333\n            }\n        }\n\n        bad = l4_secret\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 262 5) (end 270 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 262 5) (end 270 6) (annotation error) (line-text "    L2 := [BV].{\n        L3 := [BW].{\n            L4 := [BX].{\n                l4_private = 555\n            }\n\n            attempt = l4_private\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 274 5) (end 288 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 274 5) (end 288 6) (annotation error) (line-text "    L2 := [BZ].{\n        L3 := [CA].{\n            L4 := [CB].{\n                L5 := [CC].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n\n                val4 = 4\n            }\n\n            val3 = 3\n        }\n\n        val2 = 2\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 290 5) (end 290 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 290 5) (end 290 9) (annotation error) (line-text "    val1 = 1"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 233)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 233 5) (end 233 9) (annotation dim) (line-text "    val1 = 3"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 292 8) (end 292 47))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 292 8) (end 292 47) (annotation error) (line-text "d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 295 5) (end 295 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 295 5) (end 295 9) (annotation error) (line-text "    val1 = 100"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 290)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 290 5) (end 290 9) (annotation dim) (line-text "    val1 = 1"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 297 5) (end 311 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 297 5) (end 311 6) (annotation error) (line-text "    L2 := [CE].{\n        val2 = val1 + 10\n\n        L3 := [CF].{\n            val3 = val1 + val2\n\n            L4 := [CG].{\n                val4 = val1 + val2 + val3\n\n                L5 := [CH].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 313 8) (end 313 47))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 313 8) (end 313 47) (annotation error) (line-text "d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 316 5) (end 316 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 316 5) (end 316 9) (annotation error) (line-text "    val1 = 2"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 295)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 295 5) (end 295 9) (annotation dim) (line-text "    val1 = 100"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 318 5) (end 332 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 318 5) (end 332 6) (annotation error) (line-text "    L2 := [CJ].{\n        L3 := [CK].{\n            val3 = val1 + val2\n\n            L4 := [CL].{\n                L5 := [CM].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n\n                val4 = val1 + val2 + val3\n            }\n        }\n\n        val2 = val1 + 1\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 334 8) (end 334 43))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 334 8) (end 334 43) (annotation error) (line-text "d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 337 5) (end 347 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 337 5) (end 347 6) (annotation error) (line-text "    L2 := [CO].{\n        L3 := [CP].{\n            L4 := [CQ].{\n                L5 := [CR].{\n                    val5 = val4 * 5\n                }\n\n                val4 = 6\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 349 8) (end 349 43))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 349 8) (end 349 43) (annotation error) (line-text "d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 352 5) (end 364 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 352 5) (end 364 6) (annotation error) (line-text "    L2 := [CT].{\n        L3 := [CU].{\n            L4 := [CV].{\n                L5 := [CW].{\n                    val5 = val3 + val4\n                }\n\n                val4 = 7\n            }\n\n            val3 = 3\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 366 8) (end 366 43))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 366 8) (end 366 43) (annotation error) (line-text "d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 369 5) (end 383 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 369 5) (end 383 6) (annotation error) (line-text "    L2 := [DD].{\n        val2 = val1 + 10\n\n        L3 := [DE].{\n            val3 = val1 + val2\n\n            L4 := [DF].{\n                val4 = val1 + val2 + val3\n\n                L5 := [DG].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 385 5) (end 385 9))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "val1")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 385 5) (end 385 9) (annotation error) (line-text "    val1 = 5"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "val1")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 316)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 316 5) (end 316 9) (annotation dim) (line-text "    val1 = 2"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 387 8) (end 387 22))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d5_l1_val_last")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 387 8) (end 387 22) (annotation error) (line-text "d5_6 = d5_l1_val_last.val1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 388 8) (end 388 30))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 388 8) (end 388 30) (annotation error) (line-text "d5_7 = d5_l1_val_last.L2.val2"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 389 8) (end 389 33))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 389 8) (end 389 33) (annotation error) (line-text "d5_8 = d5_l1_val_last.L2.L3.val3"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 390 8) (end 390 36))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 390 8) (end 390 36) (annotation error) (line-text "d5_9 = d5_l1_val_last.L2.L3.L4.val4"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 391 9) (end 391 40))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 391 9) (end 391 40) (annotation error) (line-text "d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 394 5) (end 402 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 394 5) (end 402 6) (annotation error) (line-text "    L2 := [DI].{\n        L3 := [DJ].{\n            L4 := [DK].{\n                L5 := [DL].{\n                    deep_secret = 12345\n                }\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 404 5) (end 404 8))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "bad")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 404 5) (end 404 8) (annotation error) (line-text "    bad = deep_secret"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "bad")
			(reflow " was already defined in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 246)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 246 5) (end 246 8) (annotation dim) (line-text "    bad = l4_val"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 404 11) (end 404 22))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "deep_secret")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 404 11) (end 404 22) (annotation error) (line-text "    bad = deep_secret"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 408 5) (end 418 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 408 5) (end 418 6) (annotation error) (line-text "    L2 := [DN].{\n        L3 := [DO].{\n            L4 := [DP].{\n                L5 := [DQ].{\n                    l5_secret = 9999\n                }\n            }\n\n            bad = l5_secret\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 422 5) (end 432 6))
		(headline
			(reflow "The type ")
			(annotated code "L2")
			(reflow " is being redeclared."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 422 5) (end 432 6) (annotation error) (line-text "    L2 := [DS].{\n        L3 := [DT].{\n            L4 := [DU].{\n                L5 := [DV].{\n                    l5_only = 8888\n                }\n\n                bad = l5_only\n            }\n        }\n    }"))
			(line-break)
			(reflow "But ")
			(annotated type "L2")
			(reflow " was already declared in ")
			(source-location
				(file "associated_items_complete_all_patterns.md")
				(line 61)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 61 5) (end 67 6) (annotation dim) (line-text "    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }"))))
	(report
		(severity runtime_error)
		(title "Type Mod Missing Matching Type")
		(region (start 2 1) (end 433 2))
		(headline
			(reflow "Type mods must have a nominal type declaration matching the mod name."))
		(document
			(text "This file is named ")
			(annotated code "Test")
			(reflow ".roc, but no top-level nominal type named ")
			(annotated code "Test")
			(reflow " was found.")
			(line-break)
			(line-break)
			(reflow "Add a nominal type like:")
			(line-break)
			(annotated code "Test := ...")
			(line-break)
			(reflow "or:")
			(line-break)
			(annotated code "Test :: ...")
			(reflow " (opaque nominal type)")
			(line-break)
			(source-region (file "associated_items_complete_all_patterns.md") (start 2 1) (end 433 2) (annotation error) (line-text "d1_forward := [A].{\n    first = second\n    second = 100\n}\nd1_1 = d1_forward.first\n\nd1_scope := [B].{\n    inner = 200\n}\nd1_2 = d1_scope.inner\n\nd2_inner_first := [C].{\n    Inner := [D].{\n        inner_val = outer_val\n    }\n\n    outer_val = 300\n}\nd2_1 = d2_inner_first.outer_val\nd2_2 = d2_inner_first.Inner.inner_val\n\nd2_outer_val_middle := [G].{\n    Inner := [H].{\n        inner_val = outer_val\n    }\n\n    outer_val = 500\n}\nd2_3 = d2_outer_val_middle.Inner.inner_val\n\nd2_outer_refs_inner := [I].{\n    outer_val = d2_outer_refs_inner.Inner.inner_val\n\n    Inner := [J].{\n        inner_val = 600\n    }\n}\nd2_4 = d2_outer_refs_inner.outer_val\n\nd2_scope_violation := [K].{\n    Inner := [L].{\n        inner_private = 700\n    }\n\n    outer_trying_inner = inner_private\n}\n\nd2_siblings := [M].{\n    InnerA := [N].{\n        valA = d2_siblings.InnerB.valB + 1\n    }\n\n    InnerB := [O].{\n        valB = 800\n    }\n}\nd2_5 = d2_siblings.InnerA.valA\n\nd3_types_then_vals := [P].{\n    L2 := [Q].{\n        L3 := [R].{\n            val3 = val1 + val2\n        }\n\n        val2 = 20\n    }\n\n    val1 = 10\n}\nd3_1 = d3_types_then_vals.val1\nd3_2 = d3_types_then_vals.L2.val2\nd3_3 = d3_types_then_vals.L2.L3.val3\n\nd3_vals_then_types := [S].{\n    val1 = 30\n\n    L2 := [T].{\n        val2 = val1 + 5\n\n        L3 := [U].{\n            val3 = val1 + val2\n        }\n    }\n}\nd3_4 = d3_vals_then_types.val1\nd3_5 = d3_vals_then_types.L2.val2\nd3_6 = d3_vals_then_types.L2.L3.val3\n\nd3_l1_scope_violation := [V].{\n    L2 := [W].{\n        L3 := [X].{\n            l3_private = 999\n        }\n    }\n\n    bad_l1 = l3_private\n}\n\nd3_l2_scope_violation := [Y].{\n    L2 := [Z].{\n        L3 := [AA].{\n            l3_secret = 888\n        }\n\n        bad_l2 = l3_secret\n    }\n}\n\nd3_val_after_nested := [AB].{\n    L2 := [AC].{\n        L3 := [AD].{\n            val3 = val2 * 2\n        }\n\n        val2 = val1 * 3\n    }\n\n    val1 = 5\n}\nd3_7 = d3_val_after_nested.val1\nd3_8 = d3_val_after_nested.L2.val2\nd3_9 = d3_val_after_nested.L2.L3.val3\n\nd4_all_types_then_vals := [AE].{\n    L2 := [AF].{\n        L3 := [AG].{\n            L4 := [AH].{\n                val4 = val1 + val2 + val3\n            }\n\n            val3 = 3\n        }\n\n        val2 = 2\n    }\n\n    val1 = 1\n}\nd4_1 = d4_all_types_then_vals.L2.L3.L4.val4\n\nd4_all_vals_then_types := [AI].{\n    val1 = 10\n\n    L2 := [AJ].{\n        val2 = val1 + 1\n\n        L3 := [AK].{\n            val3 = val1 + val2\n\n            L4 := [AL].{\n                val4 = val1 + val2 + val3\n            }\n        }\n    }\n}\nd4_2 = d4_all_vals_then_types.L2.L3.L4.val4\n\nd4_reverse_types := [AM].{\n    L2 := [AN].{\n        L3 := [AO].{\n            L4 := [AP].{\n                val4 = val3 + 1\n            }\n\n            val3 = val2 + 1\n        }\n\n        val2 = val1 + 1\n    }\n\n    val1 = 7\n}\nd4_3 = d4_reverse_types.L2.L3.L4.val4\n\nd4_interleaved := [AQ].{\n    val1 = 15\n\n    L2 := [AR].{\n        L3 := [AS].{\n            val3 = val1 + val2\n\n            L4 := [AT].{\n                val4 = val1 + val2 + val3\n            }\n        }\n\n        val2 = val1 + 5\n    }\n}\nd4_4 = d4_interleaved.L2.L3.L4.val4\n\nd4_l3_val_after_l4 := [BA].{\n    L2 := [BB].{\n        L3 := [BC].{\n            L4 := [BD].{\n                val4 = val3 * 3\n            }\n            val3 = 12\n        }\n    }\n}\nd4_5 = d4_l3_val_after_l4.L2.L3.L4.val4\n\nd4_l2_val_after_l3 := [BE].{\n    L2 := [BF].{\n        L3 := [BG].{\n            L4 := [BH].{\n                val4 = val2 + val3\n            }\n\n            val3 = 8\n        }\n\n        val2 = 4\n    }\n}\nd4_6 = d4_l2_val_after_l3.L2.L3.L4.val4\n\nd4_l1_val_after_l2 := [BI].{\n    L2 := [BJ].{\n        L3 := [BK].{\n            L4 := [BL].{\n                val4 = val1 + 100\n            }\n\n            val3 = val1 + 50\n        }\n\n        val2 = val1 + 10\n    }\n\n    val1 = 3\n}\nd4_7 = d4_l1_val_after_l2.L2.L3.L4.val4\n\nd4_l1_scope_violation := [BM].{\n    L2 := [BN].{\n        L3 := [BO].{\n            L4 := [BP].{\n                l4_val = 444\n            }\n        }\n    }\n\n    bad = l4_val\n}\n\nd4_l2_scope_violation := [BQ].{\n    L2 := [BR].{\n        L3 := [BS].{\n            L4 := [BT].{\n                l4_secret = 333\n            }\n        }\n\n        bad = l4_secret\n    }\n}\n\nd4_l3_scope_violation := [BU].{\n    L2 := [BV].{\n        L3 := [BW].{\n            L4 := [BX].{\n                l4_private = 555\n            }\n\n            attempt = l4_private\n        }\n    }\n}\n\nd5_all_types_then_vals := [BY].{\n    L2 := [BZ].{\n        L3 := [CA].{\n            L4 := [CB].{\n                L5 := [CC].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n\n                val4 = 4\n            }\n\n            val3 = 3\n        }\n\n        val2 = 2\n    }\n\n    val1 = 1\n}\nd5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5\n\nd5_all_vals_then_types := [CD].{\n    val1 = 100\n\n    L2 := [CE].{\n        val2 = val1 + 10\n\n        L3 := [CF].{\n            val3 = val1 + val2\n\n            L4 := [CG].{\n                val4 = val1 + val2 + val3\n\n                L5 := [CH].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n            }\n        }\n    }\n}\nd5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5\n\nd5_deep_interleave := [CI].{\n    val1 = 2\n\n    L2 := [CJ].{\n        L3 := [CK].{\n            val3 = val1 + val2\n\n            L4 := [CL].{\n                L5 := [CM].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n\n                val4 = val1 + val2 + val3\n            }\n        }\n\n        val2 = val1 + 1\n    }\n}\nd5_3 = d5_deep_interleave.L2.L3.L4.L5.val5\n\nd5_l4_val_after_l5 := [CN].{\n    L2 := [CO].{\n        L3 := [CP].{\n            L4 := [CQ].{\n                L5 := [CR].{\n                    val5 = val4 * 5\n                }\n\n                val4 = 6\n            }\n        }\n    }\n}\nd5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5\n\nd5_l3_val_after_l4 := [CS].{\n    L2 := [CT].{\n        L3 := [CU].{\n            L4 := [CV].{\n                L5 := [CW].{\n                    val5 = val3 + val4\n                }\n\n                val4 = 7\n            }\n\n            val3 = 3\n        }\n    }\n}\nd5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5\n\nd5_l1_val_last := [DC].{\n    L2 := [DD].{\n        val2 = val1 + 10\n\n        L3 := [DE].{\n            val3 = val1 + val2\n\n            L4 := [DF].{\n                val4 = val1 + val2 + val3\n\n                L5 := [DG].{\n                    val5 = val1 + val2 + val3 + val4\n                }\n            }\n        }\n    }\n\n    val1 = 5\n}\nd5_6 = d5_l1_val_last.val1\nd5_7 = d5_l1_val_last.L2.val2\nd5_8 = d5_l1_val_last.L2.L3.val3\nd5_9 = d5_l1_val_last.L2.L3.L4.val4\nd5_10 = d5_l1_val_last.L2.L3.L4.L5.val5\n\nd5_l1_to_l5_violation := [DH].{\n    L2 := [DI].{\n        L3 := [DJ].{\n            L4 := [DK].{\n                L5 := [DL].{\n                    deep_secret = 12345\n                }\n            }\n        }\n    }\n\n    bad = deep_secret\n}\n\nd5_l3_to_l5_violation := [DM].{\n    L2 := [DN].{\n        L3 := [DO].{\n            L4 := [DP].{\n                L5 := [DQ].{\n                    l5_secret = 9999\n                }\n            }\n\n            bad = l5_secret\n        }\n    }\n}\n\nd5_l4_to_l5_violation := [DR].{\n    L2 := [DS].{\n        L3 := [DT].{\n            L4 := [DU].{\n                L5 := [DV].{\n                    l5_only = 8888\n                }\n\n                bad = l5_only\n            }\n        }\n    }\n}"))))
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 51 9) (end 51 13))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 51 9) (end 51 13) (annotation error) (line-text "        valA = d2_siblings.InnerB.valB + 1"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a where [a.plus : a, Dec -> a]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 51 16) (end 51 39))
		(headline
			(reflow "This is trying to use the")
			(reflow " ")
			(annotated code "+")
			(reflow " ")
			(reflow "operator on a value whose type is an unresolved type variable, which has no methods."))
		(document
			(source-region (file "associated_items_complete_all_patterns.md") (start 51 16) (end 51 39) (annotation error) (line-text "        valA = d2_siblings.InnerB.valB + 1"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods."))))
~~~
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
