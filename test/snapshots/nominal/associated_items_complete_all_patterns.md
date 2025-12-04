# META
~~~ini
description=Complete test - all ordering patterns at all depths, plus scoping violations
type=file:Test.roc
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
UNEXPECTED TOKEN IN EXPRESSION - associated_items_complete_all_patterns.md:51:34:51:39
UNEXPECTED TOKEN IN EXPRESSION - associated_items_complete_all_patterns.md:51:40:51:41
EXPRESSION IN ASSOCIATED ITEMS - associated_items_complete_all_patterns.md:51:42:51:43
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
TYPE REDECLARED - associated_items_complete_all_patterns.md:24:5:26:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:28:5:28:14
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:33:5:33:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:35:5:37:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:42:5:44:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:76:5:76:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:78:5:84:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:91:5:95:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:101:5:107:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:111:5:117:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:119:5:119:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:126:5:136:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:138:5:138:9
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:143:5:143:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:145:5:155:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:160:5:170:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:172:5:172:9
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:177:5:177:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:179:5:189:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:194:5:201:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:206:5:216:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:221:5:231:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:233:5:233:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:238:5:244:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:250:5:258:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:262:5:270:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:274:5:288:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:290:5:290:9
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:295:5:295:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:297:5:311:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:316:5:316:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:318:5:332:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:337:5:347:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:352:5:364:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:369:5:383:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:385:5:385:9
TYPE REDECLARED - associated_items_complete_all_patterns.md:394:5:402:6
DUPLICATE DEFINITION - associated_items_complete_all_patterns.md:404:5:404:8
TYPE REDECLARED - associated_items_complete_all_patterns.md:408:5:418:6
TYPE REDECLARED - associated_items_complete_all_patterns.md:422:5:432:6
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:51:16:51:27
UNUSED VARIABLE - associated_items_complete_all_patterns.md:51:16:51:27
TYPE REDECLARED - associated_items_complete_all_patterns.md:81:9:83:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:92:9:94:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:102:9:104:10
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:106:18:106:27
UNUSED VARIABLE - associated_items_complete_all_patterns.md:106:18:106:27
TYPE REDECLARED - associated_items_complete_all_patterns.md:112:9:114:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:127:9:133:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:148:9:154:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:151:13:153:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:161:9:167:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:162:13:164:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:180:9:186:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:183:13:185:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:195:9:200:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:196:13:198:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:207:9:213:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:208:13:210:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:222:9:228:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:223:13:225:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:239:9:243:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:240:13:242:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:251:9:255:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:252:13:254:14
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:257:15:257:24
UNUSED VARIABLE - associated_items_complete_all_patterns.md:257:15:257:24
TYPE REDECLARED - associated_items_complete_all_patterns.md:263:9:269:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:264:13:266:14
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:268:23:268:33
UNUSED VARIABLE - associated_items_complete_all_patterns.md:268:23:268:33
TYPE REDECLARED - associated_items_complete_all_patterns.md:275:9:285:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:276:13:282:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:300:9:310:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:303:13:309:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:306:17:308:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:319:9:329:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:322:13:328:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:323:17:325:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:338:9:346:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:339:13:345:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:340:17:342:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:353:9:363:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:354:13:360:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:355:17:357:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:372:9:382:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:375:13:381:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:378:17:380:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:395:9:401:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:396:13:400:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:397:17:399:18
TYPE REDECLARED - associated_items_complete_all_patterns.md:409:9:417:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:410:13:414:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:411:17:413:18
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:416:19:416:28
UNUSED VARIABLE - associated_items_complete_all_patterns.md:416:19:416:28
TYPE REDECLARED - associated_items_complete_all_patterns.md:423:9:431:10
TYPE REDECLARED - associated_items_complete_all_patterns.md:424:13:430:14
TYPE REDECLARED - associated_items_complete_all_patterns.md:425:17:427:18
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:429:23:429:30
UNUSED VARIABLE - associated_items_complete_all_patterns.md:429:23:429:30
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:6:8:6:18
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:11:8:11:16
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:20:8:20:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:21:8:21:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:30:8:30:27
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:33:17:33:36
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:39:8:39:27
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:46:26:46:39
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:58:8:58:19
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:71:8:71:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:72:8:72:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:73:8:73:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:86:8:86:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:87:8:87:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:88:8:88:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:97:14:97:24
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:121:8:121:27
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:122:8:122:27
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:123:8:123:27
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:140:8:140:30
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:157:8:157:30
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:174:8:174:24
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:191:8:191:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:203:8:203:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:218:8:218:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:235:8:235:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:246:11:246:17
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:292:8:292:30
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:313:8:313:30
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:334:8:334:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:349:8:349:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:366:8:366:26
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:387:8:387:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:388:8:388:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:389:8:389:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:390:8:390:22
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:391:9:391:23
UNDEFINED VARIABLE - associated_items_complete_all_patterns.md:404:11:404:22
TYPE MODULE MISSING MATCHING TYPE - associated_items_complete_all_patterns.md:2:1:433:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:2:1:2:11:**
```roc
d1_forward := [A].{
```
^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:2:12:2:14:**
```roc
d1_forward := [A].{
```
           ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:2:15:2:16:**
```roc
d1_forward := [A].{
```
              ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:2:17:2:18:**
```roc
d1_forward := [A].{
```
                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:2:18:2:19:**
```roc
d1_forward := [A].{
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:2:19:2:20:**
```roc
d1_forward := [A].{
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:5:1:5:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:8:1:8:9:**
```roc
d1_scope := [B].{
```
^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:8:10:8:12:**
```roc
d1_scope := [B].{
```
         ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:8:13:8:14:**
```roc
d1_scope := [B].{
```
            ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:8:15:8:16:**
```roc
d1_scope := [B].{
```
              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:8:16:8:17:**
```roc
d1_scope := [B].{
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:8:17:8:18:**
```roc
d1_scope := [B].{
```
                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:10:1:10:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:13:1:13:15:**
```roc
d2_inner_first := [C].{
```
^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:13:16:13:18:**
```roc
d2_inner_first := [C].{
```
               ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:13:19:13:20:**
```roc
d2_inner_first := [C].{
```
                  ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:13:21:13:22:**
```roc
d2_inner_first := [C].{
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:13:22:13:23:**
```roc
d2_inner_first := [C].{
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:13:23:13:24:**
```roc
d2_inner_first := [C].{
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:19:1:19:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:21:22:21:28:**
```roc
d2_2 = d2_inner_first.Inner.inner_val
```
                     ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:21:28:21:38:**
```roc
d2_2 = d2_inner_first.Inner.inner_val
```
                           ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:23:1:23:20:**
```roc
d2_outer_val_middle := [G].{
```
^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:23:21:23:23:**
```roc
d2_outer_val_middle := [G].{
```
                    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:23:24:23:25:**
```roc
d2_outer_val_middle := [G].{
```
                       ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:23:26:23:27:**
```roc
d2_outer_val_middle := [G].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:23:27:23:28:**
```roc
d2_outer_val_middle := [G].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:23:28:23:29:**
```roc
d2_outer_val_middle := [G].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:29:1:29:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:30:27:30:33:**
```roc
d2_3 = d2_outer_val_middle.Inner.inner_val
```
                          ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:30:33:30:43:**
```roc
d2_3 = d2_outer_val_middle.Inner.inner_val
```
                                ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:32:1:32:20:**
```roc
d2_outer_refs_inner := [I].{
```
^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:32:21:32:23:**
```roc
d2_outer_refs_inner := [I].{
```
                    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:32:24:32:25:**
```roc
d2_outer_refs_inner := [I].{
```
                       ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:32:26:32:27:**
```roc
d2_outer_refs_inner := [I].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:32:27:32:28:**
```roc
d2_outer_refs_inner := [I].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:32:28:32:29:**
```roc
d2_outer_refs_inner := [I].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:33:36:33:42:**
```roc
    outer_val = d2_outer_refs_inner.Inner.inner_val
```
                                   ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:33:42:33:52:**
```roc
    outer_val = d2_outer_refs_inner.Inner.inner_val
```
                                         ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:38:1:38:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:41:1:41:19:**
```roc
d2_scope_violation := [K].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:41:20:41:22:**
```roc
d2_scope_violation := [K].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:41:23:41:24:**
```roc
d2_scope_violation := [K].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:41:25:41:26:**
```roc
d2_scope_violation := [K].{
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:41:26:41:27:**
```roc
d2_scope_violation := [K].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:41:27:41:28:**
```roc
d2_scope_violation := [K].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:47:1:47:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:49:1:49:12:**
```roc
d2_siblings := [M].{
```
^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:49:13:49:15:**
```roc
d2_siblings := [M].{
```
            ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:49:16:49:17:**
```roc
d2_siblings := [M].{
```
               ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:49:18:49:19:**
```roc
d2_siblings := [M].{
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:49:19:49:20:**
```roc
d2_siblings := [M].{
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:49:20:49:21:**
```roc
d2_siblings := [M].{
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.InnerB** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**associated_items_complete_all_patterns.md:51:27:51:34:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
                          ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.valB** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**associated_items_complete_all_patterns.md:51:34:51:39:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
                                 ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**associated_items_complete_all_patterns.md:51:40:51:41:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
                                       ^


**EXPRESSION IN ASSOCIATED ITEMS**
Associated items (such as types or methods) can only have associated types and values, not plain expressions.

To fix this, remove the expression at the very end.

**associated_items_complete_all_patterns.md:51:42:51:43:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
                                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:57:1:57:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:58:19:58:26:**
```roc
d2_5 = d2_siblings.InnerA.valA
```
                  ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:58:26:58:31:**
```roc
d2_5 = d2_siblings.InnerA.valA
```
                         ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:60:1:60:19:**
```roc
d3_types_then_vals := [P].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:60:20:60:22:**
```roc
d3_types_then_vals := [P].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:60:23:60:24:**
```roc
d3_types_then_vals := [P].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:60:25:60:26:**
```roc
d3_types_then_vals := [P].{
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:60:26:60:27:**
```roc
d3_types_then_vals := [P].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:60:27:60:28:**
```roc
d3_types_then_vals := [P].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:70:1:70:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:72:26:72:29:**
```roc
d3_2 = d3_types_then_vals.L2.val2
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:72:29:72:34:**
```roc
d3_2 = d3_types_then_vals.L2.val2
```
                            ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:73:26:73:29:**
```roc
d3_3 = d3_types_then_vals.L2.L3.val3
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:73:29:73:32:**
```roc
d3_3 = d3_types_then_vals.L2.L3.val3
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:73:32:73:37:**
```roc
d3_3 = d3_types_then_vals.L2.L3.val3
```
                               ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:75:1:75:19:**
```roc
d3_vals_then_types := [S].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:75:20:75:22:**
```roc
d3_vals_then_types := [S].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:75:23:75:24:**
```roc
d3_vals_then_types := [S].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:75:25:75:26:**
```roc
d3_vals_then_types := [S].{
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:75:26:75:27:**
```roc
d3_vals_then_types := [S].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:75:27:75:28:**
```roc
d3_vals_then_types := [S].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:85:1:85:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:87:26:87:29:**
```roc
d3_5 = d3_vals_then_types.L2.val2
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:87:29:87:34:**
```roc
d3_5 = d3_vals_then_types.L2.val2
```
                            ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:88:26:88:29:**
```roc
d3_6 = d3_vals_then_types.L2.L3.val3
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:88:29:88:32:**
```roc
d3_6 = d3_vals_then_types.L2.L3.val3
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:88:32:88:37:**
```roc
d3_6 = d3_vals_then_types.L2.L3.val3
```
                               ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:90:1:90:22:**
```roc
d3_l1_scope_violation := [V].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:90:23:90:25:**
```roc
d3_l1_scope_violation := [V].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:90:26:90:27:**
```roc
d3_l1_scope_violation := [V].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:90:28:90:29:**
```roc
d3_l1_scope_violation := [V].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:90:29:90:30:**
```roc
d3_l1_scope_violation := [V].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:90:30:90:31:**
```roc
d3_l1_scope_violation := [V].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:98:1:98:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:100:1:100:22:**
```roc
d3_l2_scope_violation := [Y].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:100:23:100:25:**
```roc
d3_l2_scope_violation := [Y].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:100:26:100:27:**
```roc
d3_l2_scope_violation := [Y].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:100:28:100:29:**
```roc
d3_l2_scope_violation := [Y].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:100:29:100:30:**
```roc
d3_l2_scope_violation := [Y].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:100:30:100:31:**
```roc
d3_l2_scope_violation := [Y].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:108:1:108:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:110:1:110:20:**
```roc
d3_val_after_nested := [AB].{
```
^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:110:21:110:23:**
```roc
d3_val_after_nested := [AB].{
```
                    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:110:24:110:25:**
```roc
d3_val_after_nested := [AB].{
```
                       ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:110:27:110:28:**
```roc
d3_val_after_nested := [AB].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:110:28:110:29:**
```roc
d3_val_after_nested := [AB].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:110:29:110:30:**
```roc
d3_val_after_nested := [AB].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:120:1:120:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:122:27:122:30:**
```roc
d3_8 = d3_val_after_nested.L2.val2
```
                          ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:122:30:122:35:**
```roc
d3_8 = d3_val_after_nested.L2.val2
```
                             ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:123:27:123:30:**
```roc
d3_9 = d3_val_after_nested.L2.L3.val3
```
                          ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:123:30:123:33:**
```roc
d3_9 = d3_val_after_nested.L2.L3.val3
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:123:33:123:38:**
```roc
d3_9 = d3_val_after_nested.L2.L3.val3
```
                                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:125:1:125:23:**
```roc
d4_all_types_then_vals := [AE].{
```
^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:125:24:125:26:**
```roc
d4_all_types_then_vals := [AE].{
```
                       ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:125:27:125:28:**
```roc
d4_all_types_then_vals := [AE].{
```
                          ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:125:30:125:31:**
```roc
d4_all_types_then_vals := [AE].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:125:31:125:32:**
```roc
d4_all_types_then_vals := [AE].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:125:32:125:33:**
```roc
d4_all_types_then_vals := [AE].{
```
                               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:139:1:139:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:140:30:140:33:**
```roc
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:140:33:140:36:**
```roc
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
```
                                ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:140:36:140:39:**
```roc
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
```
                                   ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:140:39:140:44:**
```roc
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
```
                                      ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:142:1:142:23:**
```roc
d4_all_vals_then_types := [AI].{
```
^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:142:24:142:26:**
```roc
d4_all_vals_then_types := [AI].{
```
                       ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:142:27:142:28:**
```roc
d4_all_vals_then_types := [AI].{
```
                          ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:142:30:142:31:**
```roc
d4_all_vals_then_types := [AI].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:142:31:142:32:**
```roc
d4_all_vals_then_types := [AI].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:142:32:142:33:**
```roc
d4_all_vals_then_types := [AI].{
```
                               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:156:1:156:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:157:30:157:33:**
```roc
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:157:33:157:36:**
```roc
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
```
                                ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:157:36:157:39:**
```roc
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
```
                                   ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:157:39:157:44:**
```roc
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
```
                                      ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:159:1:159:17:**
```roc
d4_reverse_types := [AM].{
```
^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:159:18:159:20:**
```roc
d4_reverse_types := [AM].{
```
                 ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:159:21:159:22:**
```roc
d4_reverse_types := [AM].{
```
                    ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:159:24:159:25:**
```roc
d4_reverse_types := [AM].{
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:159:25:159:26:**
```roc
d4_reverse_types := [AM].{
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:159:26:159:27:**
```roc
d4_reverse_types := [AM].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:173:1:173:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:174:24:174:27:**
```roc
d4_3 = d4_reverse_types.L2.L3.L4.val4
```
                       ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:174:27:174:30:**
```roc
d4_3 = d4_reverse_types.L2.L3.L4.val4
```
                          ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:174:30:174:33:**
```roc
d4_3 = d4_reverse_types.L2.L3.L4.val4
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:174:33:174:38:**
```roc
d4_3 = d4_reverse_types.L2.L3.L4.val4
```
                                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:176:1:176:15:**
```roc
d4_interleaved := [AQ].{
```
^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:176:16:176:18:**
```roc
d4_interleaved := [AQ].{
```
               ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:176:19:176:20:**
```roc
d4_interleaved := [AQ].{
```
                  ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:176:22:176:23:**
```roc
d4_interleaved := [AQ].{
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:176:23:176:24:**
```roc
d4_interleaved := [AQ].{
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:176:24:176:25:**
```roc
d4_interleaved := [AQ].{
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:190:1:190:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:191:22:191:25:**
```roc
d4_4 = d4_interleaved.L2.L3.L4.val4
```
                     ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:191:25:191:28:**
```roc
d4_4 = d4_interleaved.L2.L3.L4.val4
```
                        ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:191:28:191:31:**
```roc
d4_4 = d4_interleaved.L2.L3.L4.val4
```
                           ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:191:31:191:36:**
```roc
d4_4 = d4_interleaved.L2.L3.L4.val4
```
                              ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:193:1:193:19:**
```roc
d4_l3_val_after_l4 := [BA].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:193:20:193:22:**
```roc
d4_l3_val_after_l4 := [BA].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:193:23:193:24:**
```roc
d4_l3_val_after_l4 := [BA].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:193:26:193:27:**
```roc
d4_l3_val_after_l4 := [BA].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:193:27:193:28:**
```roc
d4_l3_val_after_l4 := [BA].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:193:28:193:29:**
```roc
d4_l3_val_after_l4 := [BA].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:202:1:202:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:203:26:203:29:**
```roc
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:203:29:203:32:**
```roc
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:203:32:203:35:**
```roc
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:203:35:203:40:**
```roc
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
```
                                  ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:205:1:205:19:**
```roc
d4_l2_val_after_l3 := [BE].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:205:20:205:22:**
```roc
d4_l2_val_after_l3 := [BE].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:205:23:205:24:**
```roc
d4_l2_val_after_l3 := [BE].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:205:26:205:27:**
```roc
d4_l2_val_after_l3 := [BE].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:205:27:205:28:**
```roc
d4_l2_val_after_l3 := [BE].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:205:28:205:29:**
```roc
d4_l2_val_after_l3 := [BE].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:217:1:217:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:218:26:218:29:**
```roc
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:218:29:218:32:**
```roc
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:218:32:218:35:**
```roc
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:218:35:218:40:**
```roc
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
```
                                  ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:220:1:220:19:**
```roc
d4_l1_val_after_l2 := [BI].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:220:20:220:22:**
```roc
d4_l1_val_after_l2 := [BI].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:220:23:220:24:**
```roc
d4_l1_val_after_l2 := [BI].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:220:26:220:27:**
```roc
d4_l1_val_after_l2 := [BI].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:220:27:220:28:**
```roc
d4_l1_val_after_l2 := [BI].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:220:28:220:29:**
```roc
d4_l1_val_after_l2 := [BI].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:234:1:234:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:235:26:235:29:**
```roc
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:235:29:235:32:**
```roc
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:235:32:235:35:**
```roc
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:235:35:235:40:**
```roc
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
```
                                  ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:237:1:237:22:**
```roc
d4_l1_scope_violation := [BM].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:237:23:237:25:**
```roc
d4_l1_scope_violation := [BM].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:237:26:237:27:**
```roc
d4_l1_scope_violation := [BM].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:237:29:237:30:**
```roc
d4_l1_scope_violation := [BM].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:237:30:237:31:**
```roc
d4_l1_scope_violation := [BM].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:237:31:237:32:**
```roc
d4_l1_scope_violation := [BM].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:247:1:247:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:249:1:249:22:**
```roc
d4_l2_scope_violation := [BQ].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:249:23:249:25:**
```roc
d4_l2_scope_violation := [BQ].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:249:26:249:27:**
```roc
d4_l2_scope_violation := [BQ].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:249:29:249:30:**
```roc
d4_l2_scope_violation := [BQ].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:249:30:249:31:**
```roc
d4_l2_scope_violation := [BQ].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:249:31:249:32:**
```roc
d4_l2_scope_violation := [BQ].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:259:1:259:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:261:1:261:22:**
```roc
d4_l3_scope_violation := [BU].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:261:23:261:25:**
```roc
d4_l3_scope_violation := [BU].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:261:26:261:27:**
```roc
d4_l3_scope_violation := [BU].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:261:29:261:30:**
```roc
d4_l3_scope_violation := [BU].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:261:30:261:31:**
```roc
d4_l3_scope_violation := [BU].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:261:31:261:32:**
```roc
d4_l3_scope_violation := [BU].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:271:1:271:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:273:1:273:23:**
```roc
d5_all_types_then_vals := [BY].{
```
^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:273:24:273:26:**
```roc
d5_all_types_then_vals := [BY].{
```
                       ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:273:27:273:28:**
```roc
d5_all_types_then_vals := [BY].{
```
                          ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:273:30:273:31:**
```roc
d5_all_types_then_vals := [BY].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:273:31:273:32:**
```roc
d5_all_types_then_vals := [BY].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:273:32:273:33:**
```roc
d5_all_types_then_vals := [BY].{
```
                               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:291:1:291:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:292:30:292:33:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:292:33:292:36:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
                                ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:292:36:292:39:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
                                   ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:292:39:292:42:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
                                      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:292:42:292:47:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
                                         ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:294:1:294:23:**
```roc
d5_all_vals_then_types := [CD].{
```
^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:294:24:294:26:**
```roc
d5_all_vals_then_types := [CD].{
```
                       ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:294:27:294:28:**
```roc
d5_all_vals_then_types := [CD].{
```
                          ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:294:30:294:31:**
```roc
d5_all_vals_then_types := [CD].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:294:31:294:32:**
```roc
d5_all_vals_then_types := [CD].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:294:32:294:33:**
```roc
d5_all_vals_then_types := [CD].{
```
                               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:312:1:312:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:313:30:313:33:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
                             ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:313:33:313:36:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
                                ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:313:36:313:39:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
                                   ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:313:39:313:42:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
                                      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:313:42:313:47:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
                                         ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:315:1:315:19:**
```roc
d5_deep_interleave := [CI].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:315:20:315:22:**
```roc
d5_deep_interleave := [CI].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:315:23:315:24:**
```roc
d5_deep_interleave := [CI].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:315:26:315:27:**
```roc
d5_deep_interleave := [CI].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:315:27:315:28:**
```roc
d5_deep_interleave := [CI].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:315:28:315:29:**
```roc
d5_deep_interleave := [CI].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:333:1:333:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:334:26:334:29:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:334:29:334:32:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:334:32:334:35:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:334:35:334:38:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
                                  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:334:38:334:43:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
                                     ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:336:1:336:19:**
```roc
d5_l4_val_after_l5 := [CN].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:336:20:336:22:**
```roc
d5_l4_val_after_l5 := [CN].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:336:23:336:24:**
```roc
d5_l4_val_after_l5 := [CN].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:336:26:336:27:**
```roc
d5_l4_val_after_l5 := [CN].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:336:27:336:28:**
```roc
d5_l4_val_after_l5 := [CN].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:336:28:336:29:**
```roc
d5_l4_val_after_l5 := [CN].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:348:1:348:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:349:26:349:29:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:349:29:349:32:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:349:32:349:35:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:349:35:349:38:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
                                  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:349:38:349:43:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
                                     ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:351:1:351:19:**
```roc
d5_l3_val_after_l4 := [CS].{
```
^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:351:20:351:22:**
```roc
d5_l3_val_after_l4 := [CS].{
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:351:23:351:24:**
```roc
d5_l3_val_after_l4 := [CS].{
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:351:26:351:27:**
```roc
d5_l3_val_after_l4 := [CS].{
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:351:27:351:28:**
```roc
d5_l3_val_after_l4 := [CS].{
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:351:28:351:29:**
```roc
d5_l3_val_after_l4 := [CS].{
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:365:1:365:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:366:26:366:29:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:366:29:366:32:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:366:32:366:35:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:366:35:366:38:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
                                  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:366:38:366:43:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
                                     ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:368:1:368:15:**
```roc
d5_l1_val_last := [DC].{
```
^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:368:16:368:18:**
```roc
d5_l1_val_last := [DC].{
```
               ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:368:19:368:20:**
```roc
d5_l1_val_last := [DC].{
```
                  ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:368:22:368:23:**
```roc
d5_l1_val_last := [DC].{
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:368:23:368:24:**
```roc
d5_l1_val_last := [DC].{
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:368:24:368:25:**
```roc
d5_l1_val_last := [DC].{
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:386:1:386:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:388:22:388:25:**
```roc
d5_7 = d5_l1_val_last.L2.val2
```
                     ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:388:25:388:30:**
```roc
d5_7 = d5_l1_val_last.L2.val2
```
                        ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:389:22:389:25:**
```roc
d5_8 = d5_l1_val_last.L2.L3.val3
```
                     ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:389:25:389:28:**
```roc
d5_8 = d5_l1_val_last.L2.L3.val3
```
                        ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:389:28:389:33:**
```roc
d5_8 = d5_l1_val_last.L2.L3.val3
```
                           ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:390:22:390:25:**
```roc
d5_9 = d5_l1_val_last.L2.L3.L4.val4
```
                     ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:390:25:390:28:**
```roc
d5_9 = d5_l1_val_last.L2.L3.L4.val4
```
                        ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:390:28:390:31:**
```roc
d5_9 = d5_l1_val_last.L2.L3.L4.val4
```
                           ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:390:31:390:36:**
```roc
d5_9 = d5_l1_val_last.L2.L3.L4.val4
```
                              ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:391:23:391:26:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
                      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:391:26:391:29:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
                         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:391:29:391:32:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
                            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:391:32:391:35:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
                               ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:391:35:391:40:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
                                  ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:393:1:393:22:**
```roc
d5_l1_to_l5_violation := [DH].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:393:23:393:25:**
```roc
d5_l1_to_l5_violation := [DH].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:393:26:393:27:**
```roc
d5_l1_to_l5_violation := [DH].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:393:29:393:30:**
```roc
d5_l1_to_l5_violation := [DH].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:393:30:393:31:**
```roc
d5_l1_to_l5_violation := [DH].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:393:31:393:32:**
```roc
d5_l1_to_l5_violation := [DH].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:405:1:405:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:407:1:407:22:**
```roc
d5_l3_to_l5_violation := [DM].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:407:23:407:25:**
```roc
d5_l3_to_l5_violation := [DM].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:407:26:407:27:**
```roc
d5_l3_to_l5_violation := [DM].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:407:29:407:30:**
```roc
d5_l3_to_l5_violation := [DM].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:407:30:407:31:**
```roc
d5_l3_to_l5_violation := [DM].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:407:31:407:32:**
```roc
d5_l3_to_l5_violation := [DM].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:419:1:419:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:421:1:421:22:**
```roc
d5_l4_to_l5_violation := [DR].{
```
^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:421:23:421:25:**
```roc
d5_l4_to_l5_violation := [DR].{
```
                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:421:26:421:27:**
```roc
d5_l4_to_l5_violation := [DR].{
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**associated_items_complete_all_patterns.md:421:29:421:30:**
```roc
d5_l4_to_l5_violation := [DR].{
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:421:30:421:31:**
```roc
d5_l4_to_l5_violation := [DR].{
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:421:31:421:32:**
```roc
d5_l4_to_l5_violation := [DR].{
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**associated_items_complete_all_patterns.md:433:1:433:2:**
```roc
}
```
^


**TYPE REDECLARED**
The type _Inner_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:24:5:26:6:**
```roc
    Inner := [H].{
        inner_val = outer_val
    }
```

But _Inner_ was already declared here:
**associated_items_complete_all_patterns.md:14:5:16:6:**
```roc
    Inner := [D].{
        inner_val = outer_val
    }
```


**DUPLICATE DEFINITION**
The name `outer_val` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:28:5:28:14:**
```roc
    outer_val = 500
```
    ^^^^^^^^^

But `outer_val` was already defined here:
**associated_items_complete_all_patterns.md:18:5:18:14:**
```roc
    outer_val = 300
```
    ^^^^^^^^^


**DUPLICATE DEFINITION**
The name `outer_val` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:33:5:33:14:**
```roc
    outer_val = d2_outer_refs_inner.Inner.inner_val
```
    ^^^^^^^^^

But `outer_val` was already defined here:
**associated_items_complete_all_patterns.md:28:5:28:14:**
```roc
    outer_val = 500
```
    ^^^^^^^^^


**TYPE REDECLARED**
The type _Inner_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:35:5:37:6:**
```roc
    Inner := [J].{
        inner_val = 600
    }
```

But _Inner_ was already declared here:
**associated_items_complete_all_patterns.md:14:5:16:6:**
```roc
    Inner := [D].{
        inner_val = outer_val
    }
```


**TYPE REDECLARED**
The type _Inner_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:42:5:44:6:**
```roc
    Inner := [L].{
        inner_private = 700
    }
```

But _Inner_ was already declared here:
**associated_items_complete_all_patterns.md:14:5:16:6:**
```roc
    Inner := [D].{
        inner_val = outer_val
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:76:5:76:9:**
```roc
    val1 = 30
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:69:5:69:9:**
```roc
    val1 = 10
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:78:5:84:6:**
```roc
    L2 := [T].{
        val2 = val1 + 5

        L3 := [U].{
            val3 = val1 + val2
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:91:5:95:6:**
```roc
    L2 := [W].{
        L3 := [X].{
            l3_private = 999
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:101:5:107:6:**
```roc
    L2 := [Z].{
        L3 := [AA].{
            l3_secret = 888
        }

        bad_l2 = l3_secret
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:111:5:117:6:**
```roc
    L2 := [AC].{
        L3 := [AD].{
            val3 = val2 * 2
        }

        val2 = val1 * 3
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:119:5:119:9:**
```roc
    val1 = 5
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:76:5:76:9:**
```roc
    val1 = 30
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:126:5:136:6:**
```roc
    L2 := [AF].{
        L3 := [AG].{
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }

            val3 = 3
        }

        val2 = 2
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:138:5:138:9:**
```roc
    val1 = 1
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:119:5:119:9:**
```roc
    val1 = 5
```
    ^^^^


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:143:5:143:9:**
```roc
    val1 = 10
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:138:5:138:9:**
```roc
    val1 = 1
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:145:5:155:6:**
```roc
    L2 := [AJ].{
        val2 = val1 + 1

        L3 := [AK].{
            val3 = val1 + val2

            L4 := [AL].{
                val4 = val1 + val2 + val3
            }
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:160:5:170:6:**
```roc
    L2 := [AN].{
        L3 := [AO].{
            L4 := [AP].{
                val4 = val3 + 1
            }

            val3 = val2 + 1
        }

        val2 = val1 + 1
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:172:5:172:9:**
```roc
    val1 = 7
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:143:5:143:9:**
```roc
    val1 = 10
```
    ^^^^


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:177:5:177:9:**
```roc
    val1 = 15
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:172:5:172:9:**
```roc
    val1 = 7
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:179:5:189:6:**
```roc
    L2 := [AR].{
        L3 := [AS].{
            val3 = val1 + val2

            L4 := [AT].{
                val4 = val1 + val2 + val3
            }
        }

        val2 = val1 + 5
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:194:5:201:6:**
```roc
    L2 := [BB].{
        L3 := [BC].{
            L4 := [BD].{
                val4 = val3 * 3
            }
            val3 = 12
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:206:5:216:6:**
```roc
    L2 := [BF].{
        L3 := [BG].{
            L4 := [BH].{
                val4 = val2 + val3
            }

            val3 = 8
        }

        val2 = 4
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:221:5:231:6:**
```roc
    L2 := [BJ].{
        L3 := [BK].{
            L4 := [BL].{
                val4 = val1 + 100
            }

            val3 = val1 + 50
        }

        val2 = val1 + 10
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:233:5:233:9:**
```roc
    val1 = 3
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:177:5:177:9:**
```roc
    val1 = 15
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:238:5:244:6:**
```roc
    L2 := [BN].{
        L3 := [BO].{
            L4 := [BP].{
                l4_val = 444
            }
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:250:5:258:6:**
```roc
    L2 := [BR].{
        L3 := [BS].{
            L4 := [BT].{
                l4_secret = 333
            }
        }

        bad = l4_secret
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:262:5:270:6:**
```roc
    L2 := [BV].{
        L3 := [BW].{
            L4 := [BX].{
                l4_private = 555
            }

            attempt = l4_private
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:274:5:288:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:290:5:290:9:**
```roc
    val1 = 1
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:233:5:233:9:**
```roc
    val1 = 3
```
    ^^^^


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:295:5:295:9:**
```roc
    val1 = 100
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:290:5:290:9:**
```roc
    val1 = 1
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:297:5:311:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:316:5:316:9:**
```roc
    val1 = 2
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:295:5:295:9:**
```roc
    val1 = 100
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:318:5:332:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:337:5:347:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:352:5:364:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:369:5:383:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `val1` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:385:5:385:9:**
```roc
    val1 = 5
```
    ^^^^

But `val1` was already defined here:
**associated_items_complete_all_patterns.md:316:5:316:9:**
```roc
    val1 = 2
```
    ^^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:394:5:402:6:**
```roc
    L2 := [DI].{
        L3 := [DJ].{
            L4 := [DK].{
                L5 := [DL].{
                    deep_secret = 12345
                }
            }
        }
    }
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**DUPLICATE DEFINITION**
The name `bad` is being redeclared in this scope.

The redeclaration is here:
**associated_items_complete_all_patterns.md:404:5:404:8:**
```roc
    bad = deep_secret
```
    ^^^

But `bad` was already defined here:
**associated_items_complete_all_patterns.md:246:5:246:8:**
```roc
    bad = l4_val
```
    ^^^


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:408:5:418:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**TYPE REDECLARED**
The type _L2_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:422:5:432:6:**
```roc
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
```

But _L2_ was already declared here:
**associated_items_complete_all_patterns.md:61:5:67:6:**
```roc
    L2 := [Q].{
        L3 := [R].{
            val3 = val1 + val2
        }

        val2 = 20
    }
```


**UNDEFINED VARIABLE**
Nothing is named `d2_siblings` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:51:16:51:27:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
               ^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `d2_siblings` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_d2_siblings` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:51:16:51:27:**
```roc
        valA = d2_siblings.InnerB.valB + 1
```
               ^^^^^^^^^^^


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:81:9:83:10:**
```roc
        L3 := [U].{
            val3 = val1 + val2
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:92:9:94:10:**
```roc
        L3 := [X].{
            l3_private = 999
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:102:9:104:10:**
```roc
        L3 := [AA].{
            l3_secret = 888
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**UNDEFINED VARIABLE**
Nothing is named `l3_secret` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:106:18:106:27:**
```roc
        bad_l2 = l3_secret
```
                 ^^^^^^^^^


**UNUSED VARIABLE**
Variable `l3_secret` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_l3_secret` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:106:18:106:27:**
```roc
        bad_l2 = l3_secret
```
                 ^^^^^^^^^


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:112:9:114:10:**
```roc
        L3 := [AD].{
            val3 = val2 * 2
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:127:9:133:10:**
```roc
        L3 := [AG].{
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }

            val3 = 3
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:148:9:154:10:**
```roc
        L3 := [AK].{
            val3 = val1 + val2

            L4 := [AL].{
                val4 = val1 + val2 + val3
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:151:13:153:14:**
```roc
            L4 := [AL].{
                val4 = val1 + val2 + val3
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:161:9:167:10:**
```roc
        L3 := [AO].{
            L4 := [AP].{
                val4 = val3 + 1
            }

            val3 = val2 + 1
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:162:13:164:14:**
```roc
            L4 := [AP].{
                val4 = val3 + 1
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:180:9:186:10:**
```roc
        L3 := [AS].{
            val3 = val1 + val2

            L4 := [AT].{
                val4 = val1 + val2 + val3
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:183:13:185:14:**
```roc
            L4 := [AT].{
                val4 = val1 + val2 + val3
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:195:9:200:10:**
```roc
        L3 := [BC].{
            L4 := [BD].{
                val4 = val3 * 3
            }
            val3 = 12
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:196:13:198:14:**
```roc
            L4 := [BD].{
                val4 = val3 * 3
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:207:9:213:10:**
```roc
        L3 := [BG].{
            L4 := [BH].{
                val4 = val2 + val3
            }

            val3 = 8
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:208:13:210:14:**
```roc
            L4 := [BH].{
                val4 = val2 + val3
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:222:9:228:10:**
```roc
        L3 := [BK].{
            L4 := [BL].{
                val4 = val1 + 100
            }

            val3 = val1 + 50
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:223:13:225:14:**
```roc
            L4 := [BL].{
                val4 = val1 + 100
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:239:9:243:10:**
```roc
        L3 := [BO].{
            L4 := [BP].{
                l4_val = 444
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:240:13:242:14:**
```roc
            L4 := [BP].{
                l4_val = 444
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:251:9:255:10:**
```roc
        L3 := [BS].{
            L4 := [BT].{
                l4_secret = 333
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:252:13:254:14:**
```roc
            L4 := [BT].{
                l4_secret = 333
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**UNDEFINED VARIABLE**
Nothing is named `l4_secret` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:257:15:257:24:**
```roc
        bad = l4_secret
```
              ^^^^^^^^^


**UNUSED VARIABLE**
Variable `l4_secret` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_l4_secret` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:257:15:257:24:**
```roc
        bad = l4_secret
```
              ^^^^^^^^^


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:263:9:269:10:**
```roc
        L3 := [BW].{
            L4 := [BX].{
                l4_private = 555
            }

            attempt = l4_private
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:264:13:266:14:**
```roc
            L4 := [BX].{
                l4_private = 555
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**UNDEFINED VARIABLE**
Nothing is named `l4_private` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:268:23:268:33:**
```roc
            attempt = l4_private
```
                      ^^^^^^^^^^


**UNUSED VARIABLE**
Variable `l4_private` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_l4_private` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:268:23:268:33:**
```roc
            attempt = l4_private
```
                      ^^^^^^^^^^


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:275:9:285:10:**
```roc
        L3 := [CA].{
            L4 := [CB].{
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = 4
            }

            val3 = 3
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:276:13:282:14:**
```roc
            L4 := [CB].{
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = 4
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:300:9:310:10:**
```roc
        L3 := [CF].{
            val3 = val1 + val2

            L4 := [CG].{
                val4 = val1 + val2 + val3

                L5 := [CH].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:303:13:309:14:**
```roc
            L4 := [CG].{
                val4 = val1 + val2 + val3

                L5 := [CH].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:306:17:308:18:**
```roc
                L5 := [CH].{
                    val5 = val1 + val2 + val3 + val4
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:319:9:329:10:**
```roc
        L3 := [CK].{
            val3 = val1 + val2

            L4 := [CL].{
                L5 := [CM].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = val1 + val2 + val3
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:322:13:328:14:**
```roc
            L4 := [CL].{
                L5 := [CM].{
                    val5 = val1 + val2 + val3 + val4
                }

                val4 = val1 + val2 + val3
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:323:17:325:18:**
```roc
                L5 := [CM].{
                    val5 = val1 + val2 + val3 + val4
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:338:9:346:10:**
```roc
        L3 := [CP].{
            L4 := [CQ].{
                L5 := [CR].{
                    val5 = val4 * 5
                }

                val4 = 6
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:339:13:345:14:**
```roc
            L4 := [CQ].{
                L5 := [CR].{
                    val5 = val4 * 5
                }

                val4 = 6
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:340:17:342:18:**
```roc
                L5 := [CR].{
                    val5 = val4 * 5
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:353:9:363:10:**
```roc
        L3 := [CU].{
            L4 := [CV].{
                L5 := [CW].{
                    val5 = val3 + val4
                }

                val4 = 7
            }

            val3 = 3
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:354:13:360:14:**
```roc
            L4 := [CV].{
                L5 := [CW].{
                    val5 = val3 + val4
                }

                val4 = 7
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:355:17:357:18:**
```roc
                L5 := [CW].{
                    val5 = val3 + val4
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:372:9:382:10:**
```roc
        L3 := [DE].{
            val3 = val1 + val2

            L4 := [DF].{
                val4 = val1 + val2 + val3

                L5 := [DG].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:375:13:381:14:**
```roc
            L4 := [DF].{
                val4 = val1 + val2 + val3

                L5 := [DG].{
                    val5 = val1 + val2 + val3 + val4
                }
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:378:17:380:18:**
```roc
                L5 := [DG].{
                    val5 = val1 + val2 + val3 + val4
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:395:9:401:10:**
```roc
        L3 := [DJ].{
            L4 := [DK].{
                L5 := [DL].{
                    deep_secret = 12345
                }
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:396:13:400:14:**
```roc
            L4 := [DK].{
                L5 := [DL].{
                    deep_secret = 12345
                }
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:397:17:399:18:**
```roc
                L5 := [DL].{
                    deep_secret = 12345
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:409:9:417:10:**
```roc
        L3 := [DO].{
            L4 := [DP].{
                L5 := [DQ].{
                    l5_secret = 9999
                }
            }

            bad = l5_secret
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:410:13:414:14:**
```roc
            L4 := [DP].{
                L5 := [DQ].{
                    l5_secret = 9999
                }
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:411:17:413:18:**
```roc
                L5 := [DQ].{
                    l5_secret = 9999
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**UNDEFINED VARIABLE**
Nothing is named `l5_secret` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:416:19:416:28:**
```roc
            bad = l5_secret
```
                  ^^^^^^^^^


**UNUSED VARIABLE**
Variable `l5_secret` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_l5_secret` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:416:19:416:28:**
```roc
            bad = l5_secret
```
                  ^^^^^^^^^


**TYPE REDECLARED**
The type _Test.L2.L3_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:423:9:431:10:**
```roc
        L3 := [DT].{
            L4 := [DU].{
                L5 := [DV].{
                    l5_only = 8888
                }

                bad = l5_only
            }
        }
```

But _Test.L2.L3_ was already declared here:
**associated_items_complete_all_patterns.md:62:9:64:10:**
```roc
        L3 := [R].{
            val3 = val1 + val2
        }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:424:13:430:14:**
```roc
            L4 := [DU].{
                L5 := [DV].{
                    l5_only = 8888
                }

                bad = l5_only
            }
```

But _Test.L2.L3.L4_ was already declared here:
**associated_items_complete_all_patterns.md:128:13:130:14:**
```roc
            L4 := [AH].{
                val4 = val1 + val2 + val3
            }
```


**TYPE REDECLARED**
The type _Test.L2.L3.L4.L5_ is being redeclared.

The redeclaration is here:
**associated_items_complete_all_patterns.md:425:17:427:18:**
```roc
                L5 := [DV].{
                    l5_only = 8888
                }
```

But _Test.L2.L3.L4.L5_ was already declared here:
**associated_items_complete_all_patterns.md:277:17:279:18:**
```roc
                L5 := [CC].{
                    val5 = val1 + val2 + val3 + val4
                }
```


**UNDEFINED VARIABLE**
Nothing is named `l5_only` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:429:23:429:30:**
```roc
                bad = l5_only
```
                      ^^^^^^^


**UNUSED VARIABLE**
Variable `l5_only` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_l5_only` to suppress this warning.
The unused variable is declared here:
**associated_items_complete_all_patterns.md:429:23:429:30:**
```roc
                bad = l5_only
```
                      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d1_forward` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:6:8:6:18:**
```roc
d1_1 = d1_forward.first
```
       ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d1_scope` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:11:8:11:16:**
```roc
d1_2 = d1_scope.inner
```
       ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_inner_first` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:20:8:20:22:**
```roc
d2_1 = d2_inner_first.outer_val
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_inner_first` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:21:8:21:22:**
```roc
d2_2 = d2_inner_first.Inner.inner_val
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_outer_val_middle` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:30:8:30:27:**
```roc
d2_3 = d2_outer_val_middle.Inner.inner_val
```
       ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_outer_refs_inner` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:33:17:33:36:**
```roc
    outer_val = d2_outer_refs_inner.Inner.inner_val
```
                ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_outer_refs_inner` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:39:8:39:27:**
```roc
d2_4 = d2_outer_refs_inner.outer_val
```
       ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `inner_private` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:46:26:46:39:**
```roc
    outer_trying_inner = inner_private
```
                         ^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d2_siblings` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:58:8:58:19:**
```roc
d2_5 = d2_siblings.InnerA.valA
```
       ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_types_then_vals` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:71:8:71:26:**
```roc
d3_1 = d3_types_then_vals.val1
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_types_then_vals` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:72:8:72:26:**
```roc
d3_2 = d3_types_then_vals.L2.val2
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_types_then_vals` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:73:8:73:26:**
```roc
d3_3 = d3_types_then_vals.L2.L3.val3
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_vals_then_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:86:8:86:26:**
```roc
d3_4 = d3_vals_then_types.val1
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_vals_then_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:87:8:87:26:**
```roc
d3_5 = d3_vals_then_types.L2.val2
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_vals_then_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:88:8:88:26:**
```roc
d3_6 = d3_vals_then_types.L2.L3.val3
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `l3_private` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:97:14:97:24:**
```roc
    bad_l1 = l3_private
```
             ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_val_after_nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:121:8:121:27:**
```roc
d3_7 = d3_val_after_nested.val1
```
       ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_val_after_nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:122:8:122:27:**
```roc
d3_8 = d3_val_after_nested.L2.val2
```
       ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d3_val_after_nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:123:8:123:27:**
```roc
d3_9 = d3_val_after_nested.L2.L3.val3
```
       ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_all_types_then_vals` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:140:8:140:30:**
```roc
d4_1 = d4_all_types_then_vals.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_all_vals_then_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:157:8:157:30:**
```roc
d4_2 = d4_all_vals_then_types.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_reverse_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:174:8:174:24:**
```roc
d4_3 = d4_reverse_types.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_interleaved` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:191:8:191:22:**
```roc
d4_4 = d4_interleaved.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_l3_val_after_l4` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:203:8:203:26:**
```roc
d4_5 = d4_l3_val_after_l4.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_l2_val_after_l3` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:218:8:218:26:**
```roc
d4_6 = d4_l2_val_after_l3.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d4_l1_val_after_l2` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:235:8:235:26:**
```roc
d4_7 = d4_l1_val_after_l2.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `l4_val` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:246:11:246:17:**
```roc
    bad = l4_val
```
          ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_all_types_then_vals` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:292:8:292:30:**
```roc
d5_1 = d5_all_types_then_vals.L2.L3.L4.L5.val5
```
       ^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_all_vals_then_types` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:313:8:313:30:**
```roc
d5_2 = d5_all_vals_then_types.L2.L3.L4.L5.val5
```
       ^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_deep_interleave` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:334:8:334:26:**
```roc
d5_3 = d5_deep_interleave.L2.L3.L4.L5.val5
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l4_val_after_l5` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:349:8:349:26:**
```roc
d5_4 = d5_l4_val_after_l5.L2.L3.L4.L5.val5
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l3_val_after_l4` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:366:8:366:26:**
```roc
d5_5 = d5_l3_val_after_l4.L2.L3.L4.L5.val5
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l1_val_last` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:387:8:387:22:**
```roc
d5_6 = d5_l1_val_last.val1
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l1_val_last` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:388:8:388:22:**
```roc
d5_7 = d5_l1_val_last.L2.val2
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l1_val_last` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:389:8:389:22:**
```roc
d5_8 = d5_l1_val_last.L2.L3.val3
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l1_val_last` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:390:8:390:22:**
```roc
d5_9 = d5_l1_val_last.L2.L3.L4.val4
```
       ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d5_l1_val_last` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:391:9:391:23:**
```roc
d5_10 = d5_l1_val_last.L2.L3.L4.L5.val5
```
        ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `deep_secret` in this scope.
Is there an `import` or `exposing` missing up-top?

**associated_items_complete_all_patterns.md:404:11:404:22:**
```roc
    bad = deep_secret
```
          ^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `Test`.roc, but no top-level type declaration named `Test` was found.

Add either:
`Test := ...` (nominal type)
or:
`Test : ...` (type alias)
**associated_items_complete_all_patterns.md:2:1:433:2:**
```roc
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
```


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
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-int (raw "1"))))
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
				1
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
		(p-assign (ident "Test.Inner.inner_val"))
		(e-lookup-local
			(p-assign (ident "outer_val"))))
	(d-let
		(p-assign (ident "Test.Inner.inner_val"))
		(e-lookup-local
			(p-assign (ident "outer_val"))))
	(d-let
		(p-assign (ident "Test.Inner.inner_val"))
		(e-num (value "600")))
	(d-let
		(p-assign (ident "Test.Inner.inner_private"))
		(e-num (value "700")))
	(d-let
		(p-assign (ident "Test.InnerA.valA"))
		(e-lookup-local
			(p-assign (ident "d2_siblings"))))
	(d-let
		(p-assign (ident "Test.InnerB.valB"))
		(e-num (value "800")))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "Test.L2.L3.l3_private"))
		(e-num (value "999")))
	(d-let
		(p-assign (ident "Test.L2.L3.l3_secret"))
		(e-num (value "888")))
	(d-let
		(p-assign (ident "Test.L2.bad_l2"))
		(e-lookup-local
			(p-assign (ident "l3_secret"))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "3"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))
			(e-num (value "3"))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-num (value "12")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-num (value "8")))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-num (value "4")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "100"))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "50"))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.l4_val"))
		(e-num (value "444")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.l4_secret"))
		(e-num (value "333")))
	(d-let
		(p-assign (ident "Test.L2.bad"))
		(e-lookup-local
			(p-assign (ident "l4_secret"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.l4_private"))
		(e-num (value "555")))
	(d-let
		(p-assign (ident "Test.L2.L3.attempt"))
		(e-lookup-local
			(p-assign (ident "l4_private"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "val1")))
					(e-lookup-local
						(p-assign (ident "Test.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "Test.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-num (value "4")))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "val1")))
					(e-lookup-local
						(p-assign (ident "Test.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "Test.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "val1")))
					(e-lookup-local
						(p-assign (ident "Test.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "Test.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "1"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-num (value "6")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-num (value "7")))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.val5"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "val1")))
					(e-lookup-local
						(p-assign (ident "Test.L2.val2"))))
				(e-lookup-local
					(p-assign (ident "Test.L2.L3.val3"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.L4.val4")))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.val4"))
		(e-binop (op "add")
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "val1")))
				(e-lookup-local
					(p-assign (ident "Test.L2.val2"))))
			(e-lookup-local
				(p-assign (ident "Test.L2.L3.val3")))))
	(d-let
		(p-assign (ident "Test.L2.L3.val3"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-lookup-local
				(p-assign (ident "Test.L2.val2")))))
	(d-let
		(p-assign (ident "Test.L2.val2"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "val1")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.deep_secret"))
		(e-num (value "12345")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.l5_secret"))
		(e-num (value "9999")))
	(d-let
		(p-assign (ident "Test.L2.L3.bad"))
		(e-lookup-local
			(p-assign (ident "l5_secret"))))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.L5.l5_only"))
		(e-num (value "8888")))
	(d-let
		(p-assign (ident "Test.L2.L3.L4.bad"))
		(e-lookup-local
			(p-assign (ident "l5_only"))))
	(d-let
		(p-assign (ident "first"))
		(e-lookup-local
			(p-assign (ident "second"))))
	(d-let
		(p-assign (ident "second"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "d1_1"))
		(e-dot-access (field "first")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "inner"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "d1_2"))
		(e-dot-access (field "inner")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "outer_val"))
		(e-num (value "300")))
	(d-let
		(p-assign (ident "d2_1"))
		(e-dot-access (field "outer_val")
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
		(e-dot-access (field "outer_val")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(d-let
		(p-assign (ident "outer_trying_inner"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "d2_5"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "val1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "d3_1"))
		(e-dot-access (field "val1")
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
		(e-dot-access (field "val1")
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
		(e-dot-access (field "val1")
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
		(e-dot-access (field "val1")
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
			(ty-tag-name (name "DS"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "R"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "U"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "X"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AA"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AD"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AG"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "AH"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AK"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "AL"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AO"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "AP"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "AS"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "AT"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BC"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BD"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BG"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BH"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BK"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BL"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BO"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BP"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BS"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BT"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "BW"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "BX"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CA"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "CB"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "CC"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CF"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "CG"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "CH"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CK"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "CL"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "CM"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CP"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "CQ"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "CR"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "CU"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "CV"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "CW"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "DE"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "DF"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "DG"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "DJ"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "DK"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "DL"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "DO"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "DP"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "DQ"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3"))
		(ty-tag-union
			(ty-tag-name (name "DT"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4"))
		(ty-tag-union
			(ty-tag-name (name "DU"))))
	(s-nominal-decl
		(ty-header (name "Test.L2.L3.L4.L5"))
		(ty-tag-union
			(ty-tag-name (name "DV")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
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
		(patt (type "_a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
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
		(patt (type "_a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
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
			(ty-header (name "L2")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5")))
		(nominal (type "L2.L3")
			(ty-header (name "Test.L2.L3")))
		(nominal (type "L2.L3.L4")
			(ty-header (name "Test.L2.L3.L4")))
		(nominal (type "L2.L3.L4.L5")
			(ty-header (name "Test.L2.L3.L4.L5"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
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
		(expr (type "_a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
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
		(expr (type "_a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_a"))))
~~~
