# META
~~~ini
description=Complete test - all ordering patterns at all depths, plus scoping violations
type=file:Test.roc
module_validation_diagnostics=true
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
PARSE ERROR - associated_items_complete_all_patterns.md:2:1:2:11
PARSE ERROR - associated_items_complete_all_patterns.md:2:12:2:14
PARSE ERROR - associated_items_complete_all_patterns.md:2:15:2:16
PARSE ERROR - associated_items_complete_all_patterns.md:2:17:2:18
PARSE ERROR - associated_items_complete_all_patterns.md:2:18:2:19
PARSE ERROR - associated_items_complete_all_patterns.md:2:19:2:20
PARSE ERROR - associated_items_complete_all_patterns.md:5:1:5:2
PARSE ERROR - associated_items_complete_all_patterns.md:8:1:8:9
PARSE ERROR - associated_items_complete_all_patterns.md:8:10:8:12
PARSE ERROR - associated_items_complete_all_patterns.md:8:13:8:14
PARSE ERROR - associated_items_complete_all_patterns.md:8:15:8:16
PARSE ERROR - associated_items_complete_all_patterns.md:8:16:8:17
PARSE ERROR - associated_items_complete_all_patterns.md:8:17:8:18
PARSE ERROR - associated_items_complete_all_patterns.md:10:1:10:2
PARSE ERROR - associated_items_complete_all_patterns.md:13:1:13:15
PARSE ERROR - associated_items_complete_all_patterns.md:13:16:13:18
PARSE ERROR - associated_items_complete_all_patterns.md:13:19:13:20
PARSE ERROR - associated_items_complete_all_patterns.md:13:21:13:22
PARSE ERROR - associated_items_complete_all_patterns.md:13:22:13:23
PARSE ERROR - associated_items_complete_all_patterns.md:13:23:13:24
PARSE ERROR - associated_items_complete_all_patterns.md:19:1:19:2
PARSE ERROR - associated_items_complete_all_patterns.md:21:22:21:28
PARSE ERROR - associated_items_complete_all_patterns.md:21:28:21:38
PARSE ERROR - associated_items_complete_all_patterns.md:23:1:23:20
PARSE ERROR - associated_items_complete_all_patterns.md:23:21:23:23
PARSE ERROR - associated_items_complete_all_patterns.md:23:24:23:25
PARSE ERROR - associated_items_complete_all_patterns.md:23:26:23:27
PARSE ERROR - associated_items_complete_all_patterns.md:23:27:23:28
PARSE ERROR - associated_items_complete_all_patterns.md:23:28:23:29
PARSE ERROR - associated_items_complete_all_patterns.md:29:1:29:2
PARSE ERROR - associated_items_complete_all_patterns.md:30:27:30:33
PARSE ERROR - associated_items_complete_all_patterns.md:30:33:30:43
PARSE ERROR - associated_items_complete_all_patterns.md:32:1:32:20
PARSE ERROR - associated_items_complete_all_patterns.md:32:21:32:23
PARSE ERROR - associated_items_complete_all_patterns.md:32:24:32:25
PARSE ERROR - associated_items_complete_all_patterns.md:32:26:32:27
PARSE ERROR - associated_items_complete_all_patterns.md:32:27:32:28
PARSE ERROR - associated_items_complete_all_patterns.md:32:28:32:29
PARSE ERROR - associated_items_complete_all_patterns.md:33:36:33:42
PARSE ERROR - associated_items_complete_all_patterns.md:33:42:33:52
PARSE ERROR - associated_items_complete_all_patterns.md:38:1:38:2
PARSE ERROR - associated_items_complete_all_patterns.md:41:1:41:19
PARSE ERROR - associated_items_complete_all_patterns.md:41:20:41:22
PARSE ERROR - associated_items_complete_all_patterns.md:41:23:41:24
PARSE ERROR - associated_items_complete_all_patterns.md:41:25:41:26
PARSE ERROR - associated_items_complete_all_patterns.md:41:26:41:27
PARSE ERROR - associated_items_complete_all_patterns.md:41:27:41:28
PARSE ERROR - associated_items_complete_all_patterns.md:47:1:47:2
PARSE ERROR - associated_items_complete_all_patterns.md:49:1:49:12
PARSE ERROR - associated_items_complete_all_patterns.md:49:13:49:15
PARSE ERROR - associated_items_complete_all_patterns.md:49:16:49:17
PARSE ERROR - associated_items_complete_all_patterns.md:49:18:49:19
PARSE ERROR - associated_items_complete_all_patterns.md:49:19:49:20
PARSE ERROR - associated_items_complete_all_patterns.md:49:20:49:21
UNEXPECTED TOKEN IN EXPRESSION - associated_items_complete_all_patterns.md:51:27:51:34
EXPRESSION IN ASSOCIATED ITEMS - associated_items_complete_all_patterns.md:51:27:51:43
PARSE ERROR - associated_items_complete_all_patterns.md:57:1:57:2
PARSE ERROR - associated_items_complete_all_patterns.md:58:19:58:26
PARSE ERROR - associated_items_complete_all_patterns.md:58:26:58:31
PARSE ERROR - associated_items_complete_all_patterns.md:60:1:60:19
PARSE ERROR - associated_items_complete_all_patterns.md:60:20:60:22
PARSE ERROR - associated_items_complete_all_patterns.md:60:23:60:24
PARSE ERROR - associated_items_complete_all_patterns.md:60:25:60:26
PARSE ERROR - associated_items_complete_all_patterns.md:60:26:60:27
PARSE ERROR - associated_items_complete_all_patterns.md:60:27:60:28
PARSE ERROR - associated_items_complete_all_patterns.md:70:1:70:2
PARSE ERROR - associated_items_complete_all_patterns.md:72:26:72:29
PARSE ERROR - associated_items_complete_all_patterns.md:72:29:72:34
PARSE ERROR - associated_items_complete_all_patterns.md:73:26:73:29
PARSE ERROR - associated_items_complete_all_patterns.md:73:29:73:32
PARSE ERROR - associated_items_complete_all_patterns.md:73:32:73:37
PARSE ERROR - associated_items_complete_all_patterns.md:75:1:75:19
PARSE ERROR - associated_items_complete_all_patterns.md:75:20:75:22
PARSE ERROR - associated_items_complete_all_patterns.md:75:23:75:24
PARSE ERROR - associated_items_complete_all_patterns.md:75:25:75:26
PARSE ERROR - associated_items_complete_all_patterns.md:75:26:75:27
PARSE ERROR - associated_items_complete_all_patterns.md:75:27:75:28
PARSE ERROR - associated_items_complete_all_patterns.md:85:1:85:2
PARSE ERROR - associated_items_complete_all_patterns.md:87:26:87:29
PARSE ERROR - associated_items_complete_all_patterns.md:87:29:87:34
PARSE ERROR - associated_items_complete_all_patterns.md:88:26:88:29
PARSE ERROR - associated_items_complete_all_patterns.md:88:29:88:32
PARSE ERROR - associated_items_complete_all_patterns.md:88:32:88:37
PARSE ERROR - associated_items_complete_all_patterns.md:90:1:90:22
PARSE ERROR - associated_items_complete_all_patterns.md:90:23:90:25
PARSE ERROR - associated_items_complete_all_patterns.md:90:26:90:27
PARSE ERROR - associated_items_complete_all_patterns.md:90:28:90:29
PARSE ERROR - associated_items_complete_all_patterns.md:90:29:90:30
PARSE ERROR - associated_items_complete_all_patterns.md:90:30:90:31
PARSE ERROR - associated_items_complete_all_patterns.md:98:1:98:2
PARSE ERROR - associated_items_complete_all_patterns.md:100:1:100:22
PARSE ERROR - associated_items_complete_all_patterns.md:100:23:100:25
PARSE ERROR - associated_items_complete_all_patterns.md:100:26:100:27
PARSE ERROR - associated_items_complete_all_patterns.md:100:28:100:29
PARSE ERROR - associated_items_complete_all_patterns.md:100:29:100:30
PARSE ERROR - associated_items_complete_all_patterns.md:100:30:100:31
PARSE ERROR - associated_items_complete_all_patterns.md:108:1:108:2
PARSE ERROR - associated_items_complete_all_patterns.md:110:1:110:20
PARSE ERROR - associated_items_complete_all_patterns.md:110:21:110:23
PARSE ERROR - associated_items_complete_all_patterns.md:110:24:110:25
PARSE ERROR - associated_items_complete_all_patterns.md:110:27:110:28
PARSE ERROR - associated_items_complete_all_patterns.md:110:28:110:29
PARSE ERROR - associated_items_complete_all_patterns.md:110:29:110:30
PARSE ERROR - associated_items_complete_all_patterns.md:120:1:120:2
PARSE ERROR - associated_items_complete_all_patterns.md:122:27:122:30
PARSE ERROR - associated_items_complete_all_patterns.md:122:30:122:35
PARSE ERROR - associated_items_complete_all_patterns.md:123:27:123:30
PARSE ERROR - associated_items_complete_all_patterns.md:123:30:123:33
PARSE ERROR - associated_items_complete_all_patterns.md:123:33:123:38
PARSE ERROR - associated_items_complete_all_patterns.md:125:1:125:23
PARSE ERROR - associated_items_complete_all_patterns.md:125:24:125:26
PARSE ERROR - associated_items_complete_all_patterns.md:125:27:125:28
PARSE ERROR - associated_items_complete_all_patterns.md:125:30:125:31
PARSE ERROR - associated_items_complete_all_patterns.md:125:31:125:32
PARSE ERROR - associated_items_complete_all_patterns.md:125:32:125:33
PARSE ERROR - associated_items_complete_all_patterns.md:139:1:139:2
PARSE ERROR - associated_items_complete_all_patterns.md:140:30:140:33
PARSE ERROR - associated_items_complete_all_patterns.md:140:33:140:36
PARSE ERROR - associated_items_complete_all_patterns.md:140:36:140:39
PARSE ERROR - associated_items_complete_all_patterns.md:140:39:140:44
PARSE ERROR - associated_items_complete_all_patterns.md:142:1:142:23
PARSE ERROR - associated_items_complete_all_patterns.md:142:24:142:26
PARSE ERROR - associated_items_complete_all_patterns.md:142:27:142:28
PARSE ERROR - associated_items_complete_all_patterns.md:142:30:142:31
PARSE ERROR - associated_items_complete_all_patterns.md:142:31:142:32
PARSE ERROR - associated_items_complete_all_patterns.md:142:32:142:33
PARSE ERROR - associated_items_complete_all_patterns.md:156:1:156:2
PARSE ERROR - associated_items_complete_all_patterns.md:157:30:157:33
PARSE ERROR - associated_items_complete_all_patterns.md:157:33:157:36
PARSE ERROR - associated_items_complete_all_patterns.md:157:36:157:39
PARSE ERROR - associated_items_complete_all_patterns.md:157:39:157:44
PARSE ERROR - associated_items_complete_all_patterns.md:159:1:159:17
PARSE ERROR - associated_items_complete_all_patterns.md:159:18:159:20
PARSE ERROR - associated_items_complete_all_patterns.md:159:21:159:22
PARSE ERROR - associated_items_complete_all_patterns.md:159:24:159:25
PARSE ERROR - associated_items_complete_all_patterns.md:159:25:159:26
PARSE ERROR - associated_items_complete_all_patterns.md:159:26:159:27
PARSE ERROR - associated_items_complete_all_patterns.md:173:1:173:2
PARSE ERROR - associated_items_complete_all_patterns.md:174:24:174:27
PARSE ERROR - associated_items_complete_all_patterns.md:174:27:174:30
PARSE ERROR - associated_items_complete_all_patterns.md:174:30:174:33
PARSE ERROR - associated_items_complete_all_patterns.md:174:33:174:38
PARSE ERROR - associated_items_complete_all_patterns.md:176:1:176:15
PARSE ERROR - associated_items_complete_all_patterns.md:176:16:176:18
PARSE ERROR - associated_items_complete_all_patterns.md:176:19:176:20
PARSE ERROR - associated_items_complete_all_patterns.md:176:22:176:23
PARSE ERROR - associated_items_complete_all_patterns.md:176:23:176:24
PARSE ERROR - associated_items_complete_all_patterns.md:176:24:176:25
PARSE ERROR - associated_items_complete_all_patterns.md:190:1:190:2
PARSE ERROR - associated_items_complete_all_patterns.md:191:22:191:25
PARSE ERROR - associated_items_complete_all_patterns.md:191:25:191:28
PARSE ERROR - associated_items_complete_all_patterns.md:191:28:191:31
PARSE ERROR - associated_items_complete_all_patterns.md:191:31:191:36
PARSE ERROR - associated_items_complete_all_patterns.md:193:1:193:19
PARSE ERROR - associated_items_complete_all_patterns.md:193:20:193:22
PARSE ERROR - associated_items_complete_all_patterns.md:193:23:193:24
PARSE ERROR - associated_items_complete_all_patterns.md:193:26:193:27
PARSE ERROR - associated_items_complete_all_patterns.md:193:27:193:28
PARSE ERROR - associated_items_complete_all_patterns.md:193:28:193:29
PARSE ERROR - associated_items_complete_all_patterns.md:202:1:202:2
PARSE ERROR - associated_items_complete_all_patterns.md:203:26:203:29
PARSE ERROR - associated_items_complete_all_patterns.md:203:29:203:32
PARSE ERROR - associated_items_complete_all_patterns.md:203:32:203:35
PARSE ERROR - associated_items_complete_all_patterns.md:203:35:203:40
PARSE ERROR - associated_items_complete_all_patterns.md:205:1:205:19
PARSE ERROR - associated_items_complete_all_patterns.md:205:20:205:22
PARSE ERROR - associated_items_complete_all_patterns.md:205:23:205:24
PARSE ERROR - associated_items_complete_all_patterns.md:205:26:205:27
PARSE ERROR - associated_items_complete_all_patterns.md:205:27:205:28
PARSE ERROR - associated_items_complete_all_patterns.md:205:28:205:29
PARSE ERROR - associated_items_complete_all_patterns.md:217:1:217:2
PARSE ERROR - associated_items_complete_all_patterns.md:218:26:218:29
PARSE ERROR - associated_items_complete_all_patterns.md:218:29:218:32
PARSE ERROR - associated_items_complete_all_patterns.md:218:32:218:35
PARSE ERROR - associated_items_complete_all_patterns.md:218:35:218:40
PARSE ERROR - associated_items_complete_all_patterns.md:220:1:220:19
PARSE ERROR - associated_items_complete_all_patterns.md:220:20:220:22
PARSE ERROR - associated_items_complete_all_patterns.md:220:23:220:24
PARSE ERROR - associated_items_complete_all_patterns.md:220:26:220:27
PARSE ERROR - associated_items_complete_all_patterns.md:220:27:220:28
PARSE ERROR - associated_items_complete_all_patterns.md:220:28:220:29
PARSE ERROR - associated_items_complete_all_patterns.md:234:1:234:2
PARSE ERROR - associated_items_complete_all_patterns.md:235:26:235:29
PARSE ERROR - associated_items_complete_all_patterns.md:235:29:235:32
PARSE ERROR - associated_items_complete_all_patterns.md:235:32:235:35
PARSE ERROR - associated_items_complete_all_patterns.md:235:35:235:40
PARSE ERROR - associated_items_complete_all_patterns.md:237:1:237:22
PARSE ERROR - associated_items_complete_all_patterns.md:237:23:237:25
PARSE ERROR - associated_items_complete_all_patterns.md:237:26:237:27
PARSE ERROR - associated_items_complete_all_patterns.md:237:29:237:30
PARSE ERROR - associated_items_complete_all_patterns.md:237:30:237:31
PARSE ERROR - associated_items_complete_all_patterns.md:237:31:237:32
PARSE ERROR - associated_items_complete_all_patterns.md:247:1:247:2
PARSE ERROR - associated_items_complete_all_patterns.md:249:1:249:22
PARSE ERROR - associated_items_complete_all_patterns.md:249:23:249:25
PARSE ERROR - associated_items_complete_all_patterns.md:249:26:249:27
PARSE ERROR - associated_items_complete_all_patterns.md:249:29:249:30
PARSE ERROR - associated_items_complete_all_patterns.md:249:30:249:31
PARSE ERROR - associated_items_complete_all_patterns.md:249:31:249:32
PARSE ERROR - associated_items_complete_all_patterns.md:259:1:259:2
PARSE ERROR - associated_items_complete_all_patterns.md:261:1:261:22
PARSE ERROR - associated_items_complete_all_patterns.md:261:23:261:25
PARSE ERROR - associated_items_complete_all_patterns.md:261:26:261:27
PARSE ERROR - associated_items_complete_all_patterns.md:261:29:261:30
PARSE ERROR - associated_items_complete_all_patterns.md:261:30:261:31
PARSE ERROR - associated_items_complete_all_patterns.md:261:31:261:32
PARSE ERROR - associated_items_complete_all_patterns.md:271:1:271:2
PARSE ERROR - associated_items_complete_all_patterns.md:273:1:273:23
PARSE ERROR - associated_items_complete_all_patterns.md:273:24:273:26
PARSE ERROR - associated_items_complete_all_patterns.md:273:27:273:28
PARSE ERROR - associated_items_complete_all_patterns.md:273:30:273:31
PARSE ERROR - associated_items_complete_all_patterns.md:273:31:273:32
PARSE ERROR - associated_items_complete_all_patterns.md:273:32:273:33
PARSE ERROR - associated_items_complete_all_patterns.md:291:1:291:2
PARSE ERROR - associated_items_complete_all_patterns.md:292:30:292:33
PARSE ERROR - associated_items_complete_all_patterns.md:292:33:292:36
PARSE ERROR - associated_items_complete_all_patterns.md:292:36:292:39
PARSE ERROR - associated_items_complete_all_patterns.md:292:39:292:42
PARSE ERROR - associated_items_complete_all_patterns.md:292:42:292:47
PARSE ERROR - associated_items_complete_all_patterns.md:294:1:294:23
PARSE ERROR - associated_items_complete_all_patterns.md:294:24:294:26
PARSE ERROR - associated_items_complete_all_patterns.md:294:27:294:28
PARSE ERROR - associated_items_complete_all_patterns.md:294:30:294:31
PARSE ERROR - associated_items_complete_all_patterns.md:294:31:294:32
PARSE ERROR - associated_items_complete_all_patterns.md:294:32:294:33
PARSE ERROR - associated_items_complete_all_patterns.md:312:1:312:2
PARSE ERROR - associated_items_complete_all_patterns.md:313:30:313:33
PARSE ERROR - associated_items_complete_all_patterns.md:313:33:313:36
PARSE ERROR - associated_items_complete_all_patterns.md:313:36:313:39
PARSE ERROR - associated_items_complete_all_patterns.md:313:39:313:42
PARSE ERROR - associated_items_complete_all_patterns.md:313:42:313:47
PARSE ERROR - associated_items_complete_all_patterns.md:315:1:315:19
PARSE ERROR - associated_items_complete_all_patterns.md:315:20:315:22
PARSE ERROR - associated_items_complete_all_patterns.md:315:23:315:24
PARSE ERROR - associated_items_complete_all_patterns.md:315:26:315:27
PARSE ERROR - associated_items_complete_all_patterns.md:315:27:315:28
PARSE ERROR - associated_items_complete_all_patterns.md:315:28:315:29
PARSE ERROR - associated_items_complete_all_patterns.md:333:1:333:2
PARSE ERROR - associated_items_complete_all_patterns.md:334:26:334:29
PARSE ERROR - associated_items_complete_all_patterns.md:334:29:334:32
PARSE ERROR - associated_items_complete_all_patterns.md:334:32:334:35
PARSE ERROR - associated_items_complete_all_patterns.md:334:35:334:38
PARSE ERROR - associated_items_complete_all_patterns.md:334:38:334:43
PARSE ERROR - associated_items_complete_all_patterns.md:336:1:336:19
PARSE ERROR - associated_items_complete_all_patterns.md:336:20:336:22
PARSE ERROR - associated_items_complete_all_patterns.md:336:23:336:24
PARSE ERROR - associated_items_complete_all_patterns.md:336:26:336:27
PARSE ERROR - associated_items_complete_all_patterns.md:336:27:336:28
PARSE ERROR - associated_items_complete_all_patterns.md:336:28:336:29
PARSE ERROR - associated_items_complete_all_patterns.md:348:1:348:2
PARSE ERROR - associated_items_complete_all_patterns.md:349:26:349:29
PARSE ERROR - associated_items_complete_all_patterns.md:349:29:349:32
PARSE ERROR - associated_items_complete_all_patterns.md:349:32:349:35
PARSE ERROR - associated_items_complete_all_patterns.md:349:35:349:38
PARSE ERROR - associated_items_complete_all_patterns.md:349:38:349:43
PARSE ERROR - associated_items_complete_all_patterns.md:351:1:351:19
PARSE ERROR - associated_items_complete_all_patterns.md:351:20:351:22
PARSE ERROR - associated_items_complete_all_patterns.md:351:23:351:24
PARSE ERROR - associated_items_complete_all_patterns.md:351:26:351:27
PARSE ERROR - associated_items_complete_all_patterns.md:351:27:351:28
PARSE ERROR - associated_items_complete_all_patterns.md:351:28:351:29
PARSE ERROR - associated_items_complete_all_patterns.md:365:1:365:2
PARSE ERROR - associated_items_complete_all_patterns.md:366:26:366:29
PARSE ERROR - associated_items_complete_all_patterns.md:366:29:366:32
PARSE ERROR - associated_items_complete_all_patterns.md:366:32:366:35
PARSE ERROR - associated_items_complete_all_patterns.md:366:35:366:38
PARSE ERROR - associated_items_complete_all_patterns.md:366:38:366:43
PARSE ERROR - associated_items_complete_all_patterns.md:368:1:368:15
PARSE ERROR - associated_items_complete_all_patterns.md:368:16:368:18
PARSE ERROR - associated_items_complete_all_patterns.md:368:19:368:20
PARSE ERROR - associated_items_complete_all_patterns.md:368:22:368:23
PARSE ERROR - associated_items_complete_all_patterns.md:368:23:368:24
PARSE ERROR - associated_items_complete_all_patterns.md:368:24:368:25
PARSE ERROR - associated_items_complete_all_patterns.md:386:1:386:2
PARSE ERROR - associated_items_complete_all_patterns.md:388:22:388:25
PARSE ERROR - associated_items_complete_all_patterns.md:388:25:388:30
PARSE ERROR - associated_items_complete_all_patterns.md:389:22:389:25
PARSE ERROR - associated_items_complete_all_patterns.md:389:25:389:28
PARSE ERROR - associated_items_complete_all_patterns.md:389:28:389:33
PARSE ERROR - associated_items_complete_all_patterns.md:390:22:390:25
PARSE ERROR - associated_items_complete_all_patterns.md:390:25:390:28
PARSE ERROR - associated_items_complete_all_patterns.md:390:28:390:31
PARSE ERROR - associated_items_complete_all_patterns.md:390:31:390:36
PARSE ERROR - associated_items_complete_all_patterns.md:391:23:391:26
PARSE ERROR - associated_items_complete_all_patterns.md:391:26:391:29
PARSE ERROR - associated_items_complete_all_patterns.md:391:29:391:32
PARSE ERROR - associated_items_complete_all_patterns.md:391:32:391:35
PARSE ERROR - associated_items_complete_all_patterns.md:391:35:391:40
PARSE ERROR - associated_items_complete_all_patterns.md:393:1:393:22
PARSE ERROR - associated_items_complete_all_patterns.md:393:23:393:25
PARSE ERROR - associated_items_complete_all_patterns.md:393:26:393:27
PARSE ERROR - associated_items_complete_all_patterns.md:393:29:393:30
PARSE ERROR - associated_items_complete_all_patterns.md:393:30:393:31
PARSE ERROR - associated_items_complete_all_patterns.md:393:31:393:32
PARSE ERROR - associated_items_complete_all_patterns.md:405:1:405:2
PARSE ERROR - associated_items_complete_all_patterns.md:407:1:407:22
PARSE ERROR - associated_items_complete_all_patterns.md:407:23:407:25
PARSE ERROR - associated_items_complete_all_patterns.md:407:26:407:27
PARSE ERROR - associated_items_complete_all_patterns.md:407:29:407:30
PARSE ERROR - associated_items_complete_all_patterns.md:407:30:407:31
PARSE ERROR - associated_items_complete_all_patterns.md:407:31:407:32
PARSE ERROR - associated_items_complete_all_patterns.md:419:1:419:2
PARSE ERROR - associated_items_complete_all_patterns.md:421:1:421:22
PARSE ERROR - associated_items_complete_all_patterns.md:421:23:421:25
PARSE ERROR - associated_items_complete_all_patterns.md:421:26:421:27
PARSE ERROR - associated_items_complete_all_patterns.md:421:29:421:30
PARSE ERROR - associated_items_complete_all_patterns.md:421:30:421:31
PARSE ERROR - associated_items_complete_all_patterns.md:421:31:421:32
PARSE ERROR - associated_items_complete_all_patterns.md:433:1:433:2
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:6:8:6:18
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:11:8:11:16
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:20:8:20:22
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:21:8:21:22
TYPE REDECLARED - associated_items_complete_all_patterns.md:24:5:26:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:28:5:28:14
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:30:8:30:27
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:33:5:33:14
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:33:17:33:36
TYPE REDECLARED - associated_items_complete_all_patterns.md:35:5:37:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:39:8:39:27
TYPE REDECLARED - associated_items_complete_all_patterns.md:42:5:44:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:46:26:46:39
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:51:16:51:27
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:58:8:58:19
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:71:8:71:26
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:72:8:72:26
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:73:8:73:26
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:76:5:76:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:78:5:84:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:86:8:86:26
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:87:8:87:26
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:88:8:88:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:91:5:95:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:97:14:97:24
TYPE REDECLARED - associated_items_complete_all_patterns.md:101:5:107:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:111:5:117:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:119:5:119:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:121:8:121:27
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:122:8:122:27
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:123:8:123:27
TYPE REDECLARED - associated_items_complete_all_patterns.md:126:5:136:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:138:5:138:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:140:8:140:30
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:143:5:143:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:145:5:155:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:157:8:157:30
TYPE REDECLARED - associated_items_complete_all_patterns.md:160:5:170:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:172:5:172:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:174:8:174:24
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:177:5:177:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:179:5:189:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:191:8:191:22
TYPE REDECLARED - associated_items_complete_all_patterns.md:194:5:201:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:203:8:203:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:206:5:216:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:218:8:218:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:221:5:231:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:233:5:233:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:235:8:235:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:238:5:244:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:246:11:246:17
TYPE REDECLARED - associated_items_complete_all_patterns.md:250:5:258:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:262:5:270:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:274:5:288:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:290:5:290:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:292:8:292:30
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:295:5:295:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:297:5:311:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:313:8:313:30
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:316:5:316:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:318:5:332:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:334:8:334:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:337:5:347:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:349:8:349:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:352:5:364:6
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:366:8:366:26
TYPE REDECLARED - associated_items_complete_all_patterns.md:369:5:383:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:385:5:385:9
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:387:8:387:22
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:388:8:388:22
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:389:8:389:22
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:390:8:390:22
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:391:9:391:23
TYPE REDECLARED - associated_items_complete_all_patterns.md:394:5:402:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:404:5:404:8
NAME NOT IN SCOPE - associated_items_complete_all_patterns.md:404:11:404:22
TYPE REDECLARED - associated_items_complete_all_patterns.md:408:5:418:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:422:5:432:6
TYPE MODULE MISSING MATCHING TYPE - associated_items_complete_all_patterns.md:2:1:433:2
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │  ‾‾‾‾‾‾‾‾‾‾                                                                │
 └───────────────────────────── associated_items_complete_all_patterns.md:2:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │             ‾‾                                                             │
 └──────────────────────────── associated_items_complete_all_patterns.md:2:12 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │                ‾                                                           │
 └──────────────────────────── associated_items_complete_all_patterns.md:2:15 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │                  ‾                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:2:17 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │                   ‾                                                        │
 └──────────────────────────── associated_items_complete_all_patterns.md:2:18 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │                    ‾                                                       │
 └──────────────────────────── associated_items_complete_all_patterns.md:2:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └───────────────────────────── associated_items_complete_all_patterns.md:5:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └───────────────────────────── associated_items_complete_all_patterns.md:8:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │           ‾‾                                                               │
 └──────────────────────────── associated_items_complete_all_patterns.md:8:10 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │              ‾                                                             │
 └──────────────────────────── associated_items_complete_all_patterns.md:8:13 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │                ‾                                                           │
 └──────────────────────────── associated_items_complete_all_patterns.md:8:15 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │                 ‾                                                          │
 └──────────────────────────── associated_items_complete_all_patterns.md:8:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │                  ‾                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:8:17 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:10:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:13:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │                 ‾‾                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:13:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │                    ‾                                                       │
 └─────────────────────────── associated_items_complete_all_patterns.md:13:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │                      ‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:13:21 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │                       ‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:13:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │                        ‾                                                   │
 └─────────────────────────── associated_items_complete_all_patterns.md:13:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:19:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_2 = d2_inner_first.Inner.inner_val                                     │
 │                       ‾‾‾‾‾‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:21:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_2 = d2_inner_first.Inner.inner_val                                     │
 │                             ‾‾‾‾‾‾‾‾‾‾                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:21:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────── associated_items_complete_all_patterns.md:23:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │                      ‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:23:21 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │                         ‾                                                  │
 └─────────────────────────── associated_items_complete_all_patterns.md:23:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:23:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │                            ‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:23:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │                             ‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:23:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:29:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_3 = d2_outer_val_middle.Inner.inner_val                                │
 │                            ‾‾‾‾‾‾                                          │
 └─────────────────────────── associated_items_complete_all_patterns.md:30:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_3 = d2_outer_val_middle.Inner.inner_val                                │
 │                                  ‾‾‾‾‾‾‾‾‾‾                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:30:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────── associated_items_complete_all_patterns.md:32:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │                      ‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:32:21 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │                         ‾                                                  │
 └─────────────────────────── associated_items_complete_all_patterns.md:32:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:32:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │                            ‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:32:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │                             ‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:32:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  outer_val = d2_outer_refs_inner.Inner.inner_val                           │
 │                                 ‾‾‾‾‾‾                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:33:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  outer_val = d2_outer_refs_inner.Inner.inner_val                           │
 │                                       ‾‾‾‾‾‾‾‾‾‾                           │
 └─────────────────────────── associated_items_complete_all_patterns.md:33:42 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:38:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────── associated_items_complete_all_patterns.md:41:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │                     ‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:41:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │                        ‾                                                   │
 └─────────────────────────── associated_items_complete_all_patterns.md:41:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │                          ‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:41:25 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:41:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │                            ‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:41:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:47:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │  ‾‾‾‾‾‾‾‾‾‾‾                                                               │
 └──────────────────────────── associated_items_complete_all_patterns.md:49:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │              ‾‾                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:49:13 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │                 ‾                                                          │
 └─────────────────────────── associated_items_complete_all_patterns.md:49:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │                   ‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:49:18 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │                    ‾                                                       │
 └─────────────────────────── associated_items_complete_all_patterns.md:49:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │                     ‾                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:49:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token .InnerB is not expected in an ──┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  valA = d2_siblings.InnerB.valB + 1                                        │
 │                    ‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:51:27 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ EXPRESSION IN ASSOCIATED ITEMS ├─ Associated items (such as types or ───────┐
└┬───────────────────────────────┘  methods) can only have associated types   │
 │                                  and values, not plain expressions.        │
 │                                                                            │
 │  valA = d2_siblings.InnerB.valB + 1                                        │
 │                    ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:51:27 ┘

    To fix this, remove the expression at the very end.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:57:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_5 = d2_siblings.InnerA.valA                                            │
 │                    ‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:58:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d2_5 = d2_siblings.InnerA.valA                                            │
 │                           ‾‾‾‾‾                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:58:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────── associated_items_complete_all_patterns.md:60:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │                     ‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:60:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │                        ‾                                                   │
 └─────────────────────────── associated_items_complete_all_patterns.md:60:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │                          ‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:60:25 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:60:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │                            ‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:60:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:70:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_2 = d3_types_then_vals.L2.val2                                         │
 │                           ‾‾‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:72:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_2 = d3_types_then_vals.L2.val2                                         │
 │                              ‾‾‾‾‾                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:72:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_3 = d3_types_then_vals.L2.L3.val3                                      │
 │                           ‾‾‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:73:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_3 = d3_types_then_vals.L2.L3.val3                                      │
 │                              ‾‾‾                                           │
 └─────────────────────────── associated_items_complete_all_patterns.md:73:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_3 = d3_types_then_vals.L2.L3.val3                                      │
 │                                 ‾‾‾‾‾                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:73:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────── associated_items_complete_all_patterns.md:75:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │                     ‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:75:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │                        ‾                                                   │
 └─────────────────────────── associated_items_complete_all_patterns.md:75:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │                          ‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:75:25 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:75:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │                            ‾                                               │
 └─────────────────────────── associated_items_complete_all_patterns.md:75:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:85:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_5 = d3_vals_then_types.L2.val2                                         │
 │                           ‾‾‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:87:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_5 = d3_vals_then_types.L2.val2                                         │
 │                              ‾‾‾‾‾                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:87:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_6 = d3_vals_then_types.L2.L3.val3                                      │
 │                           ‾‾‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:88:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_6 = d3_vals_then_types.L2.L3.val3                                      │
 │                              ‾‾‾                                           │
 └─────────────────────────── associated_items_complete_all_patterns.md:88:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_6 = d3_vals_then_types.L2.L3.val3                                      │
 │                                 ‾‾‾‾‾                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:88:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────── associated_items_complete_all_patterns.md:90:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │                        ‾‾                                                  │
 └─────────────────────────── associated_items_complete_all_patterns.md:90:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │                           ‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:90:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │                             ‾                                              │
 └─────────────────────────── associated_items_complete_all_patterns.md:90:28 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │                              ‾                                             │
 └─────────────────────────── associated_items_complete_all_patterns.md:90:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │                               ‾                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:90:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └──────────────────────────── associated_items_complete_all_patterns.md:98:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:100:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:100:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:100:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:100:28 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:100:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:100:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:108:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └─────────────────────────── associated_items_complete_all_patterns.md:110:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │                      ‾‾                                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:110:21 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │                         ‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:110:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:110:27 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:110:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:110:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:120:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_8 = d3_val_after_nested.L2.val2                                        │
 │                            ‾‾‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:122:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_8 = d3_val_after_nested.L2.val2                                        │
 │                               ‾‾‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:122:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_9 = d3_val_after_nested.L2.L3.val3                                     │
 │                            ‾‾‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:123:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_9 = d3_val_after_nested.L2.L3.val3                                     │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:123:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d3_9 = d3_val_after_nested.L2.L3.val3                                     │
 │                                  ‾‾‾‾‾                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:123:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:125:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │                         ‾‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:125:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:125:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:125:30 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:125:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │                                 ‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:125:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:139:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:140:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │                                  ‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:140:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │                                     ‾‾‾                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:140:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │                                        ‾‾‾‾‾                               │
 └────────────────────────── associated_items_complete_all_patterns.md:140:39 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:142:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │                         ‾‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:142:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:142:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:142:30 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:142:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │                                 ‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:142:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:156:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:157:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │                                  ‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:157:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │                                     ‾‾‾                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:157:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │                                        ‾‾‾‾‾                               │
 └────────────────────────── associated_items_complete_all_patterns.md:157:39 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └─────────────────────────── associated_items_complete_all_patterns.md:159:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │                   ‾‾                                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:159:18 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │                      ‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:159:21 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │                         ‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:159:24 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │                          ‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:159:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:159:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:173:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │                         ‾‾‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:174:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │                            ‾‾‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:174:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:174:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │                                  ‾‾‾‾‾                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:174:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:176:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │                 ‾‾                                                         │
 └────────────────────────── associated_items_complete_all_patterns.md:176:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │                    ‾                                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:176:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │                       ‾                                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:176:22 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:176:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │                         ‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:176:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:190:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │                       ‾‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:191:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │                          ‾‾‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:191:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │                             ‾‾‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:191:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │                                ‾‾‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:191:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:193:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:193:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:193:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:193:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:193:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:193:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:202:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:203:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:203:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:203:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │                                    ‾‾‾‾‾                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:203:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:205:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:205:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:205:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:205:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:205:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:205:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:217:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:218:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:218:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:218:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │                                    ‾‾‾‾‾                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:218:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:220:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:220:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:220:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:220:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:220:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:220:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:234:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:235:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:235:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:235:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │                                    ‾‾‾‾‾                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:235:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:237:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:237:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:237:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:237:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:237:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:237:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:247:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:249:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:249:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:249:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:249:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:249:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:249:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:259:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:261:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:261:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:261:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:261:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:261:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:261:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:271:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:273:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │                         ‾‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:273:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:273:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:273:30 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:273:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │                                 ‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:273:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:291:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:292:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                                  ‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:292:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                                     ‾‾‾                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:292:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                                        ‾‾‾                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:292:39 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                                           ‾‾‾‾‾                            │
 └────────────────────────── associated_items_complete_all_patterns.md:292:42 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:294:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │                         ‾‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:294:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:294:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:294:30 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:294:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │                                 ‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:294:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:312:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                               ‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:313:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                                  ‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:313:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                                     ‾‾‾                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:313:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                                        ‾‾‾                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:313:39 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                                           ‾‾‾‾‾                            │
 └────────────────────────── associated_items_complete_all_patterns.md:313:42 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:315:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:315:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:315:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:315:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:315:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:315:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:333:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:334:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:334:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:334:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                                    ‾‾‾                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:334:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                                       ‾‾‾‾‾                                │
 └────────────────────────── associated_items_complete_all_patterns.md:334:38 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:336:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:336:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:336:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:336:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:336:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:336:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:348:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:349:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:349:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:349:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                                    ‾‾‾                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:349:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                                       ‾‾‾‾‾                                │
 └────────────────────────── associated_items_complete_all_patterns.md:349:38 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:351:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │                     ‾‾                                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:351:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:351:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:351:26 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │                            ‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:351:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │                             ‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:351:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:365:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:366:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:366:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:366:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                                    ‾‾‾                                     │
 └────────────────────────── associated_items_complete_all_patterns.md:366:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                                       ‾‾‾‾‾                                │
 └────────────────────────── associated_items_complete_all_patterns.md:366:38 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:368:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │                 ‾‾                                                         │
 └────────────────────────── associated_items_complete_all_patterns.md:368:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │                    ‾                                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:368:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │                       ‾                                                    │
 └────────────────────────── associated_items_complete_all_patterns.md:368:22 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │                        ‾                                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:368:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │                         ‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:368:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:386:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_7 = d5_l1_val_last.L2.val2                                             │
 │                       ‾‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:388:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_7 = d5_l1_val_last.L2.val2                                             │
 │                          ‾‾‾‾‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:388:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_8 = d5_l1_val_last.L2.L3.val3                                          │
 │                       ‾‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:389:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_8 = d5_l1_val_last.L2.L3.val3                                          │
 │                          ‾‾‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:389:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_8 = d5_l1_val_last.L2.L3.val3                                          │
 │                             ‾‾‾‾‾                                          │
 └────────────────────────── associated_items_complete_all_patterns.md:389:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │                       ‾‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:390:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │                          ‾‾‾                                               │
 └────────────────────────── associated_items_complete_all_patterns.md:390:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │                             ‾‾‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:390:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │                                ‾‾‾‾‾                                       │
 └────────────────────────── associated_items_complete_all_patterns.md:390:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                        ‾‾‾                                                 │
 └────────────────────────── associated_items_complete_all_patterns.md:391:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                           ‾‾‾                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:391:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                              ‾‾‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:391:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                                 ‾‾‾                                        │
 └────────────────────────── associated_items_complete_all_patterns.md:391:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                                    ‾‾‾‾‾                                   │
 └────────────────────────── associated_items_complete_all_patterns.md:391:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:393:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:393:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:393:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:393:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:393:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:393:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:405:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:407:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:407:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:407:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:407:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:407:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:407:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:419:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:421:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │                        ‾‾                                                  │
 └────────────────────────── associated_items_complete_all_patterns.md:421:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │                           ‾                                                │
 └────────────────────────── associated_items_complete_all_patterns.md:421:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │                              ‾                                             │
 └────────────────────────── associated_items_complete_all_patterns.md:421:29 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │                               ‾                                            │
 └────────────────────────── associated_items_complete_all_patterns.md:421:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │                                ‾                                           │
 └────────────────────────── associated_items_complete_all_patterns.md:421:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └─────────────────────────── associated_items_complete_all_patterns.md:433:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d1_forward` in this scope. ──────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d1_1 = d1_forward.first                                                   │
 │         ‾‾‾‾‾‾‾‾‾‾                                                         │
 └───────────────────────────── associated_items_complete_all_patterns.md:6:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d1_scope` in this scope. ────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d1_2 = d1_scope.inner                                                     │
 │         ‾‾‾‾‾‾‾‾                                                           │
 └──────────────────────────── associated_items_complete_all_patterns.md:11:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_inner_first` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d2_1 = d2_inner_first.outer_val                                           │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────── associated_items_complete_all_patterns.md:20:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_inner_first` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d2_2 = d2_inner_first.Inner.inner_val                                     │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────── associated_items_complete_all_patterns.md:21:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `Inner` is being redeclared. ───────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Inner := [H].{                                                            │
 │      inner_val = outer_val                                                 │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:24:5 ┘

    The redeclaration is here:

    But Inner was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    14 │      Inner := [D].{                                                  │
    15 │          inner_val = outer_val                                       │
    16 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:14:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `outer_val` is being redeclared here. ─────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  outer_val = 500                                                           │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:28:5 ┘

    In this scope, `outer_val` was already defined here:
       ┌──────────────────────────────────────────────────────────────────────┐
    15 │          inner_val = outer_val                                       │
       │                      ‾‾‾‾‾‾‾‾‾                                       │
       └───────────────────── associated_items_complete_all_patterns.md:15:21 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_outer_val_middle` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d2_3 = d2_outer_val_middle.Inner.inner_val                                │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────── associated_items_complete_all_patterns.md:30:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `outer_val` is being redeclared here. ─────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  outer_val = d2_outer_refs_inner.Inner.inner_val                           │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:33:5 ┘

    In this scope, `outer_val` was already defined here:
       ┌──────────────────────────────────────────────────────────────────────┐
    28 │      outer_val = 500                                                 │
       │      ‾‾‾‾‾‾‾‾‾                                                       │
       └────────────────────── associated_items_complete_all_patterns.md:28:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_outer_refs_inner` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  outer_val = d2_outer_refs_inner.Inner.inner_val                           │
 │              ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                           │
 └─────────────────────────── associated_items_complete_all_patterns.md:33:17 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `Inner` is being redeclared. ───────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Inner := [J].{                                                            │
 │      inner_val = 600                                                       │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:35:5 ┘

    The redeclaration is here:

    But Inner was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    14 │      Inner := [D].{                                                  │
    15 │          inner_val = outer_val                                       │
    16 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:14:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_outer_refs_inner` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d2_4 = d2_outer_refs_inner.outer_val                                      │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────── associated_items_complete_all_patterns.md:39:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `Inner` is being redeclared. ───────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Inner := [L].{                                                            │
 │      inner_private = 700                                                   │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:42:5 ┘

    The redeclaration is here:

    But Inner was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    14 │      Inner := [D].{                                                  │
    15 │          inner_val = outer_val                                       │
    16 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:14:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `inner_private` in this scope. ───────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  outer_trying_inner = inner_private                                        │
 │                       ‾‾‾‾‾‾‾‾‾‾‾‾‾                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:46:26 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_siblings` in this scope. ─────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  valA = d2_siblings.InnerB.valB + 1                                        │
 │         ‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────── associated_items_complete_all_patterns.md:51:16 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d2_siblings` in this scope. ─────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d2_5 = d2_siblings.InnerA.valA                                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────── associated_items_complete_all_patterns.md:58:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_types_then_vals` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_1 = d3_types_then_vals.val1                                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:71:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_types_then_vals` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_2 = d3_types_then_vals.L2.val2                                         │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:72:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_types_then_vals` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_3 = d3_types_then_vals.L2.L3.val3                                      │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:73:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 30                                                                 │
 │  ‾‾‾‾                                                                      │
 └──────────────────────────── associated_items_complete_all_patterns.md:76:5 ┘

    In this scope, `val1` was already defined here:
       ┌──────────────────────────────────────────────────────────────────────┐
    63 │              val3 = val1 + val2                                      │
       │                     ‾‾‾‾                                             │
       └───────────────────── associated_items_complete_all_patterns.md:63:20 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [T].{                                                               │
 │      val2 = val1 + 5                                                       │
 │                                                                            │
 │      L3 := [U].{                                                           │
 │          val3 = val1 + val2                                                │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:78:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_vals_then_types` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_4 = d3_vals_then_types.val1                                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:86:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_vals_then_types` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_5 = d3_vals_then_types.L2.val2                                         │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:87:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_vals_then_types` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_6 = d3_vals_then_types.L2.L3.val3                                      │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────── associated_items_complete_all_patterns.md:88:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [W].{                                                               │
 │      L3 := [X].{                                                           │
 │          l3_private = 999                                                  │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────── associated_items_complete_all_patterns.md:91:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `l3_private` in this scope. ──────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  bad_l1 = l3_private                                                       │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └─────────────────────────── associated_items_complete_all_patterns.md:97:14 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [Z].{                                                               │
 │      L3 := [AA].{                                                          │
 │          l3_secret = 888                                                   │
 │      }                                                                     │
 │                                                                            │
 │      bad_l2 = l3_secret                                                    │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:101:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [AC].{                                                              │
 │      L3 := [AD].{                                                          │
 │          val3 = val2 * 2                                                   │
 │      }                                                                     │
 │                                                                            │
 │      val2 = val1 * 3                                                       │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:111:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 5                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:119:5 ┘

    In this scope, `val1` was already defined here:
       ┌──────────────────────────────────────────────────────────────────────┐
    76 │      val1 = 30                                                       │
       │      ‾‾‾‾                                                            │
       └────────────────────── associated_items_complete_all_patterns.md:76:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_val_after_nested` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_7 = d3_val_after_nested.val1                                           │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:121:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_val_after_nested` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_8 = d3_val_after_nested.L2.val2                                        │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:122:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d3_val_after_nested` in this scope. ─┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d3_9 = d3_val_after_nested.L2.L3.val3                                     │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────── associated_items_complete_all_patterns.md:123:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [AF].{                                                              │
 │      L3 := [AG].{                                                          │
 │          L4 := [AH].{                                                      │
 │              val4 = val1 + val2 + val3                                     │
 │          }                                                                 │
 │                                                                            │
 │          val3 = 3                                                          │
 │      }                                                                     │
 │                                                                            │
 │      val2 = 2                                                              │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:126:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 1                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:138:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    119 │      val1 = 5                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:119:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_all_types_then_vals` in this ─────┐
└┬──────────────────┘  scope.                                                 │
 │                                                                            │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └─────────────────────────── associated_items_complete_all_patterns.md:140:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 10                                                                 │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:143:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    138 │      val1 = 1                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:138:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [AJ].{                                                              │
 │      val2 = val1 + 1                                                       │
 │                                                                            │
 │      L3 := [AK].{                                                          │
 │          val3 = val1 + val2                                                │
 │                                                                            │
 │          L4 := [AL].{                                                      │
 │              val4 = val1 + val2 + val3                                     │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:145:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_all_vals_then_types` in this ─────┐
└┬──────────────────┘  scope.                                                 │
 │                                                                            │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └─────────────────────────── associated_items_complete_all_patterns.md:157:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [AN].{                                                              │
 │      L3 := [AO].{                                                          │
 │          L4 := [AP].{                                                      │
 │              val4 = val3 + 1                                               │
 │          }                                                                 │
 │                                                                            │
 │          val3 = val2 + 1                                                   │
 │      }                                                                     │
 │                                                                            │
 │      val2 = val1 + 1                                                       │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:160:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 7                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:172:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    143 │      val1 = 10                                                      │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:143:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_reverse_types` in this scope. ────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └─────────────────────────── associated_items_complete_all_patterns.md:174:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 15                                                                 │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:177:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    172 │      val1 = 7                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:172:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [AR].{                                                              │
 │      L3 := [AS].{                                                          │
 │          val3 = val1 + val2                                                │
 │                                                                            │
 │          L4 := [AT].{                                                      │
 │              val4 = val1 + val2 + val3                                     │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      val2 = val1 + 5                                                       │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:179:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_interleaved` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:191:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BB].{                                                              │
 │      L3 := [BC].{                                                          │
 │          L4 := [BD].{                                                      │
 │              val4 = val3 * 3                                               │
 │          }                                                                 │
 │          val3 = 12                                                         │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:194:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_l3_val_after_l4` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:203:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BF].{                                                              │
 │      L3 := [BG].{                                                          │
 │          L4 := [BH].{                                                      │
 │              val4 = val2 + val3                                            │
 │          }                                                                 │
 │                                                                            │
 │          val3 = 8                                                          │
 │      }                                                                     │
 │                                                                            │
 │      val2 = 4                                                              │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:206:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_l2_val_after_l3` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:218:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BJ].{                                                              │
 │      L3 := [BK].{                                                          │
 │          L4 := [BL].{                                                      │
 │              val4 = val1 + 100                                             │
 │          }                                                                 │
 │                                                                            │
 │          val3 = val1 + 50                                                  │
 │      }                                                                     │
 │                                                                            │
 │      val2 = val1 + 10                                                      │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:221:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 3                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:233:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    177 │      val1 = 15                                                      │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:177:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d4_l1_val_after_l2` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:235:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BN].{                                                              │
 │      L3 := [BO].{                                                          │
 │          L4 := [BP].{                                                      │
 │              l4_val = 444                                                  │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:238:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `l4_val` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  bad = l4_val                                                              │
 │        ‾‾‾‾‾‾                                                              │
 └────────────────────────── associated_items_complete_all_patterns.md:246:11 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BR].{                                                              │
 │      L3 := [BS].{                                                          │
 │          L4 := [BT].{                                                      │
 │              l4_secret = 333                                               │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      bad = l4_secret                                                       │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:250:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BV].{                                                              │
 │      L3 := [BW].{                                                          │
 │          L4 := [BX].{                                                      │
 │              l4_private = 555                                              │
 │          }                                                                 │
 │                                                                            │
 │          attempt = l4_private                                              │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:262:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [BZ].{                                                              │
 │      L3 := [CA].{                                                          │
 │          L4 := [CB].{                                                      │
 │              L5 := [CC].{                                                  │
 │                  val5 = val1 + val2 + val3 + val4                          │
 │              }                                                             │
 │                                                                            │
 │              val4 = 4                                                      │
 │          }                                                                 │
 │                                                                            │
 │          val3 = 3                                                          │
 │      }                                                                     │
 │                                                                            │
 │      val2 = 2                                                              │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:274:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 1                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:290:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    233 │      val1 = 3                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:233:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_all_types_then_vals` in this ─────┐
└┬──────────────────┘  scope.                                                 │
 │                                                                            │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └─────────────────────────── associated_items_complete_all_patterns.md:292:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 100                                                                │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:295:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    290 │      val1 = 1                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:290:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [CE].{                                                              │
 │      val2 = val1 + 10                                                      │
 │                                                                            │
 │      L3 := [CF].{                                                          │
 │          val3 = val1 + val2                                                │
 │                                                                            │
 │          L4 := [CG].{                                                      │
 │              val4 = val1 + val2 + val3                                     │
 │                                                                            │
 │              L5 := [CH].{                                                  │
 │                  val5 = val1 + val2 + val3 + val4                          │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:297:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_all_vals_then_types` in this ─────┐
└┬──────────────────┘  scope.                                                 │
 │                                                                            │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └─────────────────────────── associated_items_complete_all_patterns.md:313:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 2                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:316:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    295 │      val1 = 100                                                     │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:295:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [CJ].{                                                              │
 │      L3 := [CK].{                                                          │
 │          val3 = val1 + val2                                                │
 │                                                                            │
 │          L4 := [CL].{                                                      │
 │              L5 := [CM].{                                                  │
 │                  val5 = val1 + val2 + val3 + val4                          │
 │              }                                                             │
 │                                                                            │
 │              val4 = val1 + val2 + val3                                     │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      val2 = val1 + 1                                                       │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:318:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_deep_interleave` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:334:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [CO].{                                                              │
 │      L3 := [CP].{                                                          │
 │          L4 := [CQ].{                                                      │
 │              L5 := [CR].{                                                  │
 │                  val5 = val4 * 5                                           │
 │              }                                                             │
 │                                                                            │
 │              val4 = 6                                                      │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:337:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l4_val_after_l5` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:349:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [CT].{                                                              │
 │      L3 := [CU].{                                                          │
 │          L4 := [CV].{                                                      │
 │              L5 := [CW].{                                                  │
 │                  val5 = val3 + val4                                        │
 │              }                                                             │
 │                                                                            │
 │              val4 = 7                                                      │
 │          }                                                                 │
 │                                                                            │
 │          val3 = 3                                                          │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:352:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l3_val_after_l4` in this scope. ──┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └─────────────────────────── associated_items_complete_all_patterns.md:366:8 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [DD].{                                                              │
 │      val2 = val1 + 10                                                      │
 │                                                                            │
 │      L3 := [DE].{                                                          │
 │          val3 = val1 + val2                                                │
 │                                                                            │
 │          L4 := [DF].{                                                      │
 │              val4 = val1 + val2 + val3                                     │
 │                                                                            │
 │              L5 := [DG].{                                                  │
 │                  val5 = val1 + val2 + val3 + val4                          │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:369:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `val1` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  val1 = 5                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────── associated_items_complete_all_patterns.md:385:5 ┘

    In this scope, `val1` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    316 │      val1 = 2                                                       │
        │      ‾‾‾‾                                                           │
        └──────────────────── associated_items_complete_all_patterns.md:316:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l1_val_last` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_6 = d5_l1_val_last.val1                                                │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:387:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l1_val_last` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_7 = d5_l1_val_last.L2.val2                                             │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:388:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l1_val_last` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_8 = d5_l1_val_last.L2.L3.val3                                          │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:389:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l1_val_last` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └─────────────────────────── associated_items_complete_all_patterns.md:390:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d5_l1_val_last` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────── associated_items_complete_all_patterns.md:391:9 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [DI].{                                                              │
 │      L3 := [DJ].{                                                          │
 │          L4 := [DK].{                                                      │
 │              L5 := [DL].{                                                  │
 │                  deep_secret = 12345                                       │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:394:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `bad` is being redeclared here. ───────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  bad = deep_secret                                                         │
 │  ‾‾‾                                                                       │
 └─────────────────────────── associated_items_complete_all_patterns.md:404:5 ┘

    In this scope, `bad` was already defined here:
        ┌─────────────────────────────────────────────────────────────────────┐
    246 │      bad = l4_val                                                   │
        │      ‾‾‾                                                            │
        └──────────────────── associated_items_complete_all_patterns.md:246:5 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `deep_secret` in this scope. ─────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  bad = deep_secret                                                         │
 │        ‾‾‾‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────── associated_items_complete_all_patterns.md:404:11 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [DN].{                                                              │
 │      L3 := [DO].{                                                          │
 │          L4 := [DP].{                                                      │
 │              L5 := [DQ].{                                                  │
 │                  l5_secret = 9999                                          │
 │              }                                                             │
 │          }                                                                 │
 │                                                                            │
 │          bad = l5_secret                                                   │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:408:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌─────────────────┐
│ TYPE REDECLARED ├─ The type `L2` is being redeclared. ──────────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  L2 := [DS].{                                                              │
 │      L3 := [DT].{                                                          │
 │          L4 := [DU].{                                                      │
 │              L5 := [DV].{                                                  │
 │                  l5_only = 8888                                            │
 │              }                                                             │
 │                                                                            │
 │              bad = l5_only                                                 │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────── associated_items_complete_all_patterns.md:422:5 ┘

    The redeclaration is here:

    But L2 was already declared here:
       ┌──────────────────────────────────────────────────────────────────────┐
    61 │      L2 := [Q].{                                                     │
    62 │          L3 := [R].{                                                 │
    63 │              val3 = val1 + val2                                      │
    64 │          }                                                           │
    65 │                                                                      │
    66 │          val2 = 20                                                   │
    67 │      }                                                               │
       └────────────────────── associated_items_complete_all_patterns.md:61:5 ┘


┌───────────────────────────────────┐
│ TYPE MODULE MISSING MATCHING TYPE ├─ Type modules must have a nominal ──────┐
└┬──────────────────────────────────┘  type declaration matching the module   │
 │                                     name.                                  │
 │                                                                            │
 │  d1_forward := [A].{                                                       │
 │      first = second                                                        │
 │      second = 100                                                          │
 │  }                                                                         │
 │  d1_1 = d1_forward.first                                                   │
 │                                                                            │
 │  d1_scope := [B].{                                                         │
 │      inner = 200                                                           │
 │  }                                                                         │
 │  d1_2 = d1_scope.inner                                                     │
 │                                                                            │
 │  d2_inner_first := [C].{                                                   │
 │      Inner := [D].{                                                        │
 │          inner_val = outer_val                                             │
 │      }                                                                     │
 │                                                                            │
 │      outer_val = 300                                                       │
 │  }                                                                         │
 │  d2_1 = d2_inner_first.outer_val                                           │
 │  d2_2 = d2_inner_first.Inner.inner_val                                     │
 │                                                                            │
 │  d2_outer_val_middle := [G].{                                              │
 │      Inner := [H].{                                                        │
 │          inner_val = outer_val                                             │
 │      }                                                                     │
 │                                                                            │
 │      outer_val = 500                                                       │
 │  }                                                                         │
 │  d2_3 = d2_outer_val_middle.Inner.inner_val                                │
 │                                                                            │
 │  d2_outer_refs_inner := [I].{                                              │
 │      outer_val = d2_outer_refs_inner.Inner.inner_val                       │
 │                                                                            │
 │      Inner := [J].{                                                        │
 │          inner_val = 600                                                   │
 │      }                                                                     │
 │  }                                                                         │
 │  d2_4 = d2_outer_refs_inner.outer_val                                      │
 │                                                                            │
 │  d2_scope_violation := [K].{                                               │
 │      Inner := [L].{                                                        │
 │          inner_private = 700                                               │
 │      }                                                                     │
 │                                                                            │
 │      outer_trying_inner = inner_private                                    │
 │  }                                                                         │
 │                                                                            │
 │  d2_siblings := [M].{                                                      │
 │      InnerA := [N].{                                                       │
 │          valA = d2_siblings.InnerB.valB + 1                                │
 │      }                                                                     │
 │                                                                            │
 │      InnerB := [O].{                                                       │
 │          valB = 800                                                        │
 │      }                                                                     │
 │  }                                                                         │
 │  d2_5 = d2_siblings.InnerA.valA                                            │
 │                                                                            │
 │  d3_types_then_vals := [P].{                                               │
 │      L2 := [Q].{                                                           │
 │          L3 := [R].{                                                       │
 │              val3 = val1 + val2                                            │
 │          }                                                                 │
 │                                                                            │
 │          val2 = 20                                                         │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 10                                                             │
 │  }                                                                         │
 │  d3_1 = d3_types_then_vals.val1                                            │
 │  d3_2 = d3_types_then_vals.L2.val2                                         │
 │  d3_3 = d3_types_then_vals.L2.L3.val3                                      │
 │                                                                            │
 │  d3_vals_then_types := [S].{                                               │
 │      val1 = 30                                                             │
 │                                                                            │
 │      L2 := [T].{                                                           │
 │          val2 = val1 + 5                                                   │
 │                                                                            │
 │          L3 := [U].{                                                       │
 │              val3 = val1 + val2                                            │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d3_4 = d3_vals_then_types.val1                                            │
 │  d3_5 = d3_vals_then_types.L2.val2                                         │
 │  d3_6 = d3_vals_then_types.L2.L3.val3                                      │
 │                                                                            │
 │  d3_l1_scope_violation := [V].{                                            │
 │      L2 := [W].{                                                           │
 │          L3 := [X].{                                                       │
 │              l3_private = 999                                              │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      bad_l1 = l3_private                                                   │
 │  }                                                                         │
 │                                                                            │
 │  d3_l2_scope_violation := [Y].{                                            │
 │      L2 := [Z].{                                                           │
 │          L3 := [AA].{                                                      │
 │              l3_secret = 888                                               │
 │          }                                                                 │
 │                                                                            │
 │          bad_l2 = l3_secret                                                │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 │  d3_val_after_nested := [AB].{                                             │
 │      L2 := [AC].{                                                          │
 │          L3 := [AD].{                                                      │
 │              val3 = val2 * 2                                               │
 │          }                                                                 │
 │                                                                            │
 │          val2 = val1 * 3                                                   │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 5                                                              │
 │  }                                                                         │
 │  d3_7 = d3_val_after_nested.val1                                           │
 │  d3_8 = d3_val_after_nested.L2.val2                                        │
 │  d3_9 = d3_val_after_nested.L2.L3.val3                                     │
 │                                                                            │
 │  d4_all_types_then_vals := [AE].{                                          │
 │      L2 := [AF].{                                                          │
 │          L3 := [AG].{                                                      │
 │              L4 := [AH].{                                                  │
 │                  val4 = val1 + val2 + val3                                 │
 │              }                                                             │
 │                                                                            │
 │              val3 = 3                                                      │
 │          }                                                                 │
 │                                                                            │
 │          val2 = 2                                                          │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 1                                                              │
 │  }                                                                         │
 │  d4_1 = d4_all_types_then_vals.L2.L3.L4.val4                               │
 │                                                                            │
 │  d4_all_vals_then_types := [AI].{                                          │
 │      val1 = 10                                                             │
 │                                                                            │
 │      L2 := [AJ].{                                                          │
 │          val2 = val1 + 1                                                   │
 │                                                                            │
 │          L3 := [AK].{                                                      │
 │              val3 = val1 + val2                                            │
 │                                                                            │
 │              L4 := [AL].{                                                  │
 │                  val4 = val1 + val2 + val3                                 │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d4_2 = d4_all_vals_then_types.L2.L3.L4.val4                               │
 │                                                                            │
 │  d4_reverse_types := [AM].{                                                │
 │      L2 := [AN].{                                                          │
 │          L3 := [AO].{                                                      │
 │              L4 := [AP].{                                                  │
 │                  val4 = val3 + 1                                           │
 │              }                                                             │
 │                                                                            │
 │              val3 = val2 + 1                                               │
 │          }                                                                 │
 │                                                                            │
 │          val2 = val1 + 1                                                   │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 7                                                              │
 │  }                                                                         │
 │  d4_3 = d4_reverse_types.L2.L3.L4.val4                                     │
 │                                                                            │
 │  d4_interleaved := [AQ].{                                                  │
 │      val1 = 15                                                             │
 │                                                                            │
 │      L2 := [AR].{                                                          │
 │          L3 := [AS].{                                                      │
 │              val3 = val1 + val2                                            │
 │                                                                            │
 │              L4 := [AT].{                                                  │
 │                  val4 = val1 + val2 + val3                                 │
 │              }                                                             │
 │          }                                                                 │
 │                                                                            │
 │          val2 = val1 + 5                                                   │
 │      }                                                                     │
 │  }                                                                         │
 │  d4_4 = d4_interleaved.L2.L3.L4.val4                                       │
 │                                                                            │
 │  d4_l3_val_after_l4 := [BA].{                                              │
 │      L2 := [BB].{                                                          │
 │          L3 := [BC].{                                                      │
 │              L4 := [BD].{                                                  │
 │                  val4 = val3 * 3                                           │
 │              }                                                             │
 │              val3 = 12                                                     │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4                                   │
 │                                                                            │
 │  d4_l2_val_after_l3 := [BE].{                                              │
 │      L2 := [BF].{                                                          │
 │          L3 := [BG].{                                                      │
 │              L4 := [BH].{                                                  │
 │                  val4 = val2 + val3                                        │
 │              }                                                             │
 │                                                                            │
 │              val3 = 8                                                      │
 │          }                                                                 │
 │                                                                            │
 │          val2 = 4                                                          │
 │      }                                                                     │
 │  }                                                                         │
 │  d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4                                   │
 │                                                                            │
 │  d4_l1_val_after_l2 := [BI].{                                              │
 │      L2 := [BJ].{                                                          │
 │          L3 := [BK].{                                                      │
 │              L4 := [BL].{                                                  │
 │                  val4 = val1 + 100                                         │
 │              }                                                             │
 │                                                                            │
 │              val3 = val1 + 50                                              │
 │          }                                                                 │
 │                                                                            │
 │          val2 = val1 + 10                                                  │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 3                                                              │
 │  }                                                                         │
 │  d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4                                   │
 │                                                                            │
 │  d4_l1_scope_violation := [BM].{                                           │
 │      L2 := [BN].{                                                          │
 │          L3 := [BO].{                                                      │
 │              L4 := [BP].{                                                  │
 │                  l4_val = 444                                              │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      bad = l4_val                                                          │
 │  }                                                                         │
 │                                                                            │
 │  d4_l2_scope_violation := [BQ].{                                           │
 │      L2 := [BR].{                                                          │
 │          L3 := [BS].{                                                      │
 │              L4 := [BT].{                                                  │
 │                  l4_secret = 333                                           │
 │              }                                                             │
 │          }                                                                 │
 │                                                                            │
 │          bad = l4_secret                                                   │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 │  d4_l3_scope_violation := [BU].{                                           │
 │      L2 := [BV].{                                                          │
 │          L3 := [BW].{                                                      │
 │              L4 := [BX].{                                                  │
 │                  l4_private = 555                                          │
 │              }                                                             │
 │                                                                            │
 │              attempt = l4_private                                          │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 │  d5_all_types_then_vals := [BY].{                                          │
 │      L2 := [BZ].{                                                          │
 │          L3 := [CA].{                                                      │
 │              L4 := [CB].{                                                  │
 │                  L5 := [CC].{                                              │
 │                      val5 = val1 + val2 + val3 + val4                      │
 │                  }                                                         │
 │                                                                            │
 │                  val4 = 4                                                  │
 │              }                                                             │
 │                                                                            │
 │              val3 = 3                                                      │
 │          }                                                                 │
 │                                                                            │
 │          val2 = 2                                                          │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 1                                                              │
 │  }                                                                         │
 │  d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5                            │
 │                                                                            │
 │  d5_all_vals_then_types := [CD].{                                          │
 │      val1 = 100                                                            │
 │                                                                            │
 │      L2 := [CE].{                                                          │
 │          val2 = val1 + 10                                                  │
 │                                                                            │
 │          L3 := [CF].{                                                      │
 │              val3 = val1 + val2                                            │
 │                                                                            │
 │              L4 := [CG].{                                                  │
 │                  val4 = val1 + val2 + val3                                 │
 │                                                                            │
 │                  L5 := [CH].{                                              │
 │                      val5 = val1 + val2 + val3 + val4                      │
 │                  }                                                         │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5                            │
 │                                                                            │
 │  d5_deep_interleave := [CI].{                                              │
 │      val1 = 2                                                              │
 │                                                                            │
 │      L2 := [CJ].{                                                          │
 │          L3 := [CK].{                                                      │
 │              val3 = val1 + val2                                            │
 │                                                                            │
 │              L4 := [CL].{                                                  │
 │                  L5 := [CM].{                                              │
 │                      val5 = val1 + val2 + val3 + val4                      │
 │                  }                                                         │
 │                                                                            │
 │                  val4 = val1 + val2 + val3                                 │
 │              }                                                             │
 │          }                                                                 │
 │                                                                            │
 │          val2 = val1 + 1                                                   │
 │      }                                                                     │
 │  }                                                                         │
 │  d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5                                │
 │                                                                            │
 │  d5_l4_val_after_l5 := [CN].{                                              │
 │      L2 := [CO].{                                                          │
 │          L3 := [CP].{                                                      │
 │              L4 := [CQ].{                                                  │
 │                  L5 := [CR].{                                              │
 │                      val5 = val4 * 5                                       │
 │                  }                                                         │
 │                                                                            │
 │                  val4 = 6                                                  │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5                                │
 │                                                                            │
 │  d5_l3_val_after_l4 := [CS].{                                              │
 │      L2 := [CT].{                                                          │
 │          L3 := [CU].{                                                      │
 │              L4 := [CV].{                                                  │
 │                  L5 := [CW].{                                              │
 │                      val5 = val3 + val4                                    │
 │                  }                                                         │
 │                                                                            │
 │                  val4 = 7                                                  │
 │              }                                                             │
 │                                                                            │
 │              val3 = 3                                                      │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │  d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5                                │
 │                                                                            │
 │  d5_l1_val_last := [DC].{                                                  │
 │      L2 := [DD].{                                                          │
 │          val2 = val1 + 10                                                  │
 │                                                                            │
 │          L3 := [DE].{                                                      │
 │              val3 = val1 + val2                                            │
 │                                                                            │
 │              L4 := [DF].{                                                  │
 │                  val4 = val1 + val2 + val3                                 │
 │                                                                            │
 │                  L5 := [DG].{                                              │
 │                      val5 = val1 + val2 + val3 + val4                      │
 │                  }                                                         │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      val1 = 5                                                              │
 │  }                                                                         │
 │  d5_6 = d5_l1_val_last.val1                                                │
 │  d5_7 = d5_l1_val_last.L2.val2                                             │
 │  d5_8 = d5_l1_val_last.L2.L3.val3                                          │
 │  d5_9 = d5_l1_val_last.L2.L3.L4.val4                                       │
 │  d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5                                   │
 │                                                                            │
 │  d5_l1_to_l5_violation := [DH].{                                           │
 │      L2 := [DI].{                                                          │
 │          L3 := [DJ].{                                                      │
 │              L4 := [DK].{                                                  │
 │                  L5 := [DL].{                                              │
 │                      deep_secret = 12345                                   │
 │                  }                                                         │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │                                                                            │
 │      bad = deep_secret                                                     │
 │  }                                                                         │
 │                                                                            │
 │  d5_l3_to_l5_violation := [DM].{                                           │
 │      L2 := [DN].{                                                          │
 │          L3 := [DO].{                                                      │
 │              L4 := [DP].{                                                  │
 │                  L5 := [DQ].{                                              │
 │                      l5_secret = 9999                                      │
 │                  }                                                         │
 │              }                                                             │
 │                                                                            │
 │              bad = l5_secret                                               │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 │  d5_l4_to_l5_violation := [DR].{                                           │
 │      L2 := [DS].{                                                          │
 │          L3 := [DT].{                                                      │
 │              L4 := [DU].{                                                  │
 │                  L5 := [DV].{                                              │
 │                      l5_only = 8888                                        │
 │                  }                                                         │
 │                                                                            │
 │                  bad = l5_only                                             │
 │              }                                                             │
 │          }                                                                 │
 │      }                                                                     │
 │  }                                                                         │
 │                                                                            │
 └───────────────────────────── associated_items_complete_all_patterns.md:2:1 ┘

    This file is named `Test`.roc, but no top-level nominal type named `Test`
    was found.

    Add a nominal type like:
    `Test := ...`
    or:
    `Test :: ...` (opaque nominal type)

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
	(type-module)
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
				(e-ident (raw "d1_forward"))
				(e-ident (raw "first"))))
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
				(e-ident (raw "d1_scope"))
				(e-ident (raw "inner"))))
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
				(e-ident (raw "d2_inner_first"))
				(e-ident (raw "outer_val"))))
		(s-decl
			(p-ident (raw "d2_2"))
			(e-ident (raw "d2_inner_first")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d2_outer_val_middle")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "outer_val"))
			(e-ident (raw "d2_outer_refs_inner")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
				(e-ident (raw "d2_outer_refs_inner"))
				(e-ident (raw "outer_val"))))
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
					(e-ident (raw "d2_siblings")))
				(e-binop (op "+")
					(e-ident (raw "d2_siblings"))
					(e-int (raw "1")))))
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
			(e-ident (raw "d2_siblings")))
		(s-malformed (tag "statement_unexpected_token"))
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
				(e-ident (raw "d3_types_then_vals"))
				(e-ident (raw "val1"))))
		(s-decl
			(p-ident (raw "d3_2"))
			(e-ident (raw "d3_types_then_vals")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_3"))
			(e-ident (raw "d3_types_then_vals")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
				(e-ident (raw "d3_vals_then_types"))
				(e-ident (raw "val1"))))
		(s-decl
			(p-ident (raw "d3_5"))
			(e-ident (raw "d3_vals_then_types")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_6"))
			(e-ident (raw "d3_vals_then_types")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
				(e-ident (raw "d3_val_after_nested"))
				(e-ident (raw "val1"))))
		(s-decl
			(p-ident (raw "d3_8"))
			(e-ident (raw "d3_val_after_nested")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d3_9"))
			(e-ident (raw "d3_val_after_nested")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_all_types_then_vals")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_all_vals_then_types")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_reverse_types")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_interleaved")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_l3_val_after_l4")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_l2_val_after_l3")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d4_l1_val_after_l2")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d5_all_types_then_vals")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d5_all_vals_then_types")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d5_deep_interleave")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d5_l4_val_after_l5")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
			(e-ident (raw "d5_l3_val_after_l4")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
				(e-ident (raw "d5_l1_val_last"))
				(e-ident (raw "val1"))))
		(s-decl
			(p-ident (raw "d5_7"))
			(e-ident (raw "d5_l1_val_last")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_8"))
			(e-ident (raw "d5_l1_val_last")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_9"))
			(e-ident (raw "d5_l1_val_last")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "d5_10"))
			(e-ident (raw "d5_l1_val_last")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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

d2_2 = d2_inner_first



Inner := [H].{
	inner_val = outer_val
}

outer_val = 500

d2_3 = d2_outer_val_middle



outer_val = d2_outer_refs_inner


Inner := [J].{
	inner_val = 600
}

d2_4 = d2_outer_refs_inner.outer_val


Inner := [L].{
	inner_private = 700
}

outer_trying_inner = inner_private



InnerA := [N].{
	valA = d2_siblings
	d2_siblings + 1
}

InnerB := [O].{
	valB = 800
}

d2_5 = d2_siblings



L2 := [Q].{
	L3 := [R].{
		val3 = val1 + val2
	}

	val2 = 20
}

val1 = 10

d3_1 = d3_types_then_vals.val1

d3_2 = d3_types_then_vals

d3_3 = d3_types_then_vals



val1 = 30

L2 := [T].{
	val2 = val1 + 5

	L3 := [U].{
		val3 = val1 + val2
	}
}

d3_4 = d3_vals_then_types.val1

d3_5 = d3_vals_then_types

d3_6 = d3_vals_then_types



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

d3_8 = d3_val_after_nested

d3_9 = d3_val_after_nested



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

d4_1 = d4_all_types_then_vals



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

d4_2 = d4_all_vals_then_types



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

d4_3 = d4_reverse_types



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

d4_4 = d4_interleaved



L2 := [BB].{
	L3 := [BC].{
		L4 := [BD].{
			val4 = val3 * 3
		}
		val3 = 12
	}
}

d4_5 = d4_l3_val_after_l4



L2 := [BF].{
	L3 := [BG].{
		L4 := [BH].{
			val4 = val2 + val3
		}

		val3 = 8
	}

	val2 = 4
}

d4_6 = d4_l2_val_after_l3



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

d4_7 = d4_l1_val_after_l2



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

d5_1 = d5_all_types_then_vals



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

d5_2 = d5_all_vals_then_types



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

d5_3 = d5_deep_interleave



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

d5_4 = d5_l4_val_after_l5



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

d5_5 = d5_l3_val_after_l4



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

d5_7 = d5_l1_val_last

d5_8 = d5_l1_val_last

d5_9 = d5_l1_val_last

d5_10 = d5_l1_val_last



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
		(e-field-access (field "first")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "inner"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "d1_2"))
		(e-field-access (field "inner")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "Test.Inner.inner_val"))
		(e-lookup-local
			(p-assign (ident "outer_val"))))
	(d-let
		(p-assign (ident "outer_val"))
		(e-num (value "300")))
	(d-let
		(p-assign (ident "d2_1"))
		(e-field-access (field "outer_val")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "d2_2"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "outer_val"))
		(e-num (value "500")))
	(d-let
		(p-assign (ident "d2_3"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "outer_val"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d2_4"))
		(e-field-access (field "outer_val")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "outer_trying_inner"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "Test.InnerA.valA"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "Test.InnerB.valB"))
		(e-num (value "800")))
	(d-let
		(p-assign (ident "d2_5"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-dispatch-call (method "plus") (constraint-fn-var 675)
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
		(e-field-access (field "val1")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "d3_2"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d3_3"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "30")))
	(d-let
		(p-assign (ident "d3_4"))
		(e-field-access (field "val1")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "d3_5"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d3_6"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "bad_l1"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "d3_7"))
		(e-field-access (field "val1")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "d3_8"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d3_9"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "d4_1"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "d4_2"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "7")))
	(d-let
		(p-assign (ident "d4_3"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "15")))
	(d-let
		(p-assign (ident "d4_4"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d4_5"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d4_6"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "d4_7"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "d5_1"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "d5_2"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "d5_3"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d5_4"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d5_5"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "d5_6"))
		(e-field-access (field "val1")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "d5_7"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d5_8"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d5_9"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d5_10"))
		(e-runtime-error (tag "ident_not_in_scope")))
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
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
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
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
