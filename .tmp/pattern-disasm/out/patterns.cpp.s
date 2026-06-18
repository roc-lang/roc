.tmp/pattern-disasm/out/patterns.o:
(__TEXT,__text) section
_discard_only:
       0:	cmp	x1, #0x4
       4:	b.ls	0xa8
       8:	ldr	w8, [x0]
       c:	mov	w9, #0x4547
      10:	movk	w9, #0x2054, lsl #16
      14:	cmp	w8, w9
      18:	b.ne	0x188
      1c:	ldrb	w8, [x0, #0x4]
      20:	cmp	w8, #0x2f
      24:	b.ne	0x188
      28:	add	x9, x0, #0x5
      2c:	add	x8, x0, x1
      30:	sub	x10, x1, #0x15
      34:	cmn	x10, #0x11
      38:	b.hi	0xb0
      3c:	mov	x11, #-0x101010101010102
      40:	movk	x11, #0xfeff
      44:	mov	x12, #0xe0e0e0e0e0e0e0e
      48:	orr	x12, x12, #0x2222222222222222
      4c:	add	x10, x0, #0xd
      50:	sub	x13, x1, #0x5
      54:	ldur	x14, [x10, #-0x8]
      58:	eor	x15, x14, x12
      5c:	add	x15, x15, x11
      60:	bic	x14, x15, x14
      64:	ands	x14, x14, #0x8080808080808080
      68:	b.ne	0x140
      6c:	ldr	x14, [x10]
      70:	eor	x15, x14, x12
      74:	add	x15, x15, x11
      78:	bic	x14, x15, x14
      7c:	ands	x14, x14, #0x8080808080808080
      80:	b.ne	0x154
      84:	add	x10, x10, #0x10
      88:	add	x9, x9, #0x10
      8c:	sub	x13, x13, #0x10
      90:	cmp	x13, #0xf
      94:	b.hi	0x54
      98:	sub	x10, x10, #0x8
      9c:	cmp	x10, x8
      a0:	b.lo	0xbc
      a4:	b	0x188
      a8:	add	x8, x0, x1
      ac:	b	0x1f8
      b0:	mov	x10, x9
      b4:	cmp	x9, x8
      b8:	b.hs	0x188
      bc:	sub	x11, x8, x9
      c0:	mov	x9, x10
      c4:	ands	x12, x11, #0x3
      c8:	b.eq	0xe8
      cc:	mov	x9, x10
      d0:	ldrb	w13, [x9]
      d4:	cmp	w13, #0x2e
      d8:	b.eq	0x160
      dc:	add	x9, x9, #0x1
      e0:	subs	x12, x12, #0x1
      e4:	b.ne	0xd0
      e8:	sub	x12, x11, #0x1
      ec:	cmp	x12, #0x3
      f0:	b.lo	0x188
      f4:	add	x10, x10, x11
      f8:	add	x9, x9, #0x3
      fc:	ldurb	w11, [x9, #-0x3]
     100:	cmp	w11, #0x2e
     104:	b.eq	0x328
     108:	ldurb	w11, [x9, #-0x2]
     10c:	cmp	w11, #0x2e
     110:	b.eq	0x330
     114:	ldurb	w11, [x9, #-0x1]
     118:	cmp	w11, #0x2e
     11c:	b.eq	0x338
     120:	ldrb	w11, [x9]
     124:	cmp	w11, #0x2e
     128:	b.eq	0x160
     12c:	add	x11, x9, #0x1
     130:	add	x9, x9, #0x4
     134:	cmp	x11, x10
     138:	b.ne	0xfc
     13c:	b	0x188
     140:	rbit	x9, x14
     144:	clz	x9, x9
     148:	add	x9, x10, x9, lsr #3
     14c:	sub	x9, x9, #0x8
     150:	b	0x160
     154:	rbit	x9, x14
     158:	clz	x9, x9
     15c:	add	x9, x10, x9, lsr #3
     160:	sub	x8, x8, x9
     164:	cmp	x8, #0x4
     168:	b.ne	0x188
     16c:	ldr	w8, [x9]
     170:	mov	w9, #0x742e
     174:	movk	w9, #0x7478, lsl #16
     178:	cmp	w8, w9
     17c:	b.ne	0x188
     180:	mov	w0, #0x1
     184:	ret
     188:	add	x8, x0, x1
     18c:	cmp	x1, #0x10
     190:	b.lo	0x1f8
     194:	mov	x10, #-0x101010101010102
     198:	movk	x10, #0xfeff
     19c:	mov	x11, #0xe0e0e0e0e0e0e0e
     1a0:	orr	x11, x11, #0x2222222222222222
     1a4:	mov	x9, x0
     1a8:	ldr	x12, [x0]
     1ac:	eor	x13, x12, x11
     1b0:	add	x13, x13, x10
     1b4:	bic	x12, x13, x12
     1b8:	ands	x12, x12, #0x8080808080808080
     1bc:	b.ne	0x288
     1c0:	ldr	x12, [x0, #0x8]
     1c4:	eor	x13, x12, x11
     1c8:	add	x13, x13, x10
     1cc:	bic	x12, x13, x12
     1d0:	ands	x12, x12, #0x8080808080808080
     1d4:	b.ne	0x2a4
     1d8:	add	x0, x0, #0x10
     1dc:	add	x9, x9, #0x10
     1e0:	sub	x1, x1, #0x10
     1e4:	cmp	x1, #0xf
     1e8:	b.hi	0x1a8
     1ec:	cmp	x0, x8
     1f0:	b.lo	0x204
     1f4:	b	0x320
     1f8:	mov	x9, x0
     1fc:	cmp	x0, x8
     200:	b.hs	0x320
     204:	sub	x10, x8, x9
     208:	mov	x9, x0
     20c:	ands	x11, x10, #0x3
     210:	b.eq	0x230
     214:	mov	x9, x0
     218:	ldrb	w12, [x9]
     21c:	cmp	w12, #0x2e
     220:	b.eq	0x2dc
     224:	add	x9, x9, #0x1
     228:	subs	x11, x11, #0x1
     22c:	b.ne	0x218
     230:	sub	x11, x10, #0x1
     234:	cmp	x11, #0x3
     238:	b.lo	0x320
     23c:	add	x10, x0, x10
     240:	add	x9, x9, #0x3
     244:	ldurb	w11, [x9, #-0x3]
     248:	cmp	w11, #0x2e
     24c:	b.eq	0x2c4
     250:	ldurb	w11, [x9, #-0x2]
     254:	cmp	w11, #0x2e
     258:	b.eq	0x2d8
     25c:	ldurb	w11, [x9, #-0x1]
     260:	cmp	w11, #0x2e
     264:	b.eq	0x310
     268:	ldrb	w11, [x9]
     26c:	cmp	w11, #0x2e
     270:	b.eq	0x2dc
     274:	add	x11, x9, #0x1
     278:	add	x9, x9, #0x4
     27c:	cmp	x11, x10
     280:	b.ne	0x244
     284:	b	0x320
     288:	rbit	x9, x12
     28c:	clz	x9, x9
     290:	add	x9, x0, x9, lsr #3
     294:	sub	x8, x8, x9
     298:	cmp	x8, #0x5
     29c:	b.eq	0x2e8
     2a0:	b	0x320
     2a4:	rbit	x9, x12
     2a8:	clz	x9, x9
     2ac:	add	x9, x0, x9, lsr #3
     2b0:	add	x9, x9, #0x8
     2b4:	sub	x8, x8, x9
     2b8:	cmp	x8, #0x5
     2bc:	b.eq	0x2e8
     2c0:	b	0x320
     2c4:	sub	x9, x9, #0x3
     2c8:	sub	x8, x8, x9
     2cc:	cmp	x8, #0x5
     2d0:	b.eq	0x2e8
     2d4:	b	0x320
     2d8:	sub	x9, x9, #0x2
     2dc:	sub	x8, x8, x9
     2e0:	cmp	x8, #0x5
     2e4:	b.ne	0x320
     2e8:	ldr	w8, [x9]
     2ec:	mov	w10, #0x6a2e
     2f0:	movk	w10, #0x6f73, lsl #16
     2f4:	cmp	w8, w10
     2f8:	b.ne	0x320
     2fc:	ldrb	w8, [x9, #0x4]
     300:	cmp	w8, #0x6e
     304:	b.ne	0x320
     308:	mov	w0, #0x2
     30c:	ret
     310:	sub	x9, x9, #0x1
     314:	sub	x8, x8, x9
     318:	cmp	x8, #0x5
     31c:	b.eq	0x2e8
     320:	mov	x0, #0x0
     324:	ret
     328:	sub	x9, x9, #0x3
     32c:	b	0x160
     330:	sub	x9, x9, #0x2
     334:	b	0x160
     338:	sub	x9, x9, #0x1
     33c:	b	0x160
_one_capture:
     340:	subs	x10, x1, #0x5
     344:	b.lo	0x4cc
     348:	ldr	w8, [x0]
     34c:	mov	w9, #0x4547
     350:	movk	w9, #0x2054, lsl #16
     354:	cmp	w8, w9
     358:	b.ne	0x4cc
     35c:	ldrb	w8, [x0, #0x4]
     360:	cmp	w8, #0x2f
     364:	b.ne	0x4cc
     368:	add	x8, x0, #0x5
     36c:	add	x9, x0, x1
     370:	sub	x11, x1, #0x15
     374:	cmn	x11, #0x11
     378:	b.hi	0x3e8
     37c:	mov	x13, #-0x101010101010102
     380:	movk	x13, #0xfeff
     384:	mov	x14, #0xe0e0e0e0e0e0e0e
     388:	orr	x14, x14, #0x2222222222222222
     38c:	add	x12, x0, #0xd
     390:	mov	x11, x8
     394:	ldur	x15, [x12, #-0x8]
     398:	eor	x16, x15, x14
     39c:	add	x16, x16, x13
     3a0:	bic	x15, x16, x15
     3a4:	ands	x15, x15, #0x8080808080808080
     3a8:	b.ne	0x480
     3ac:	ldr	x15, [x12]
     3b0:	eor	x16, x15, x14
     3b4:	add	x16, x16, x13
     3b8:	bic	x15, x16, x15
     3bc:	ands	x15, x15, #0x8080808080808080
     3c0:	b.ne	0x494
     3c4:	add	x12, x12, #0x10
     3c8:	add	x11, x11, #0x10
     3cc:	sub	x10, x10, #0x10
     3d0:	cmp	x10, #0xf
     3d4:	b.hi	0x394
     3d8:	sub	x12, x12, #0x8
     3dc:	cmp	x12, x9
     3e0:	b.lo	0x3f8
     3e4:	b	0x4cc
     3e8:	mov	x11, x8
     3ec:	mov	x12, x8
     3f0:	cmp	x8, x9
     3f4:	b.hs	0x4cc
     3f8:	sub	x11, x9, x11
     3fc:	mov	x10, x12
     400:	ands	x13, x11, #0x3
     404:	b.eq	0x424
     408:	mov	x10, x12
     40c:	ldrb	w14, [x10]
     410:	cmp	w14, #0x2e
     414:	b.eq	0x4a0
     418:	add	x10, x10, #0x1
     41c:	subs	x13, x13, #0x1
     420:	b.ne	0x40c
     424:	sub	x13, x11, #0x1
     428:	cmp	x13, #0x3
     42c:	b.lo	0x4cc
     430:	add	x11, x12, x11
     434:	add	x10, x10, #0x3
     438:	ldurb	w12, [x10, #-0x3]
     43c:	cmp	w12, #0x2e
     440:	b.eq	0x4d4
     444:	ldurb	w12, [x10, #-0x2]
     448:	cmp	w12, #0x2e
     44c:	b.eq	0x4dc
     450:	ldurb	w12, [x10, #-0x1]
     454:	cmp	w12, #0x2e
     458:	b.eq	0x4e4
     45c:	ldrb	w12, [x10]
     460:	cmp	w12, #0x2e
     464:	b.eq	0x4a0
     468:	mov	x0, #0x0
     46c:	add	x12, x10, #0x1
     470:	add	x10, x10, #0x4
     474:	cmp	x12, x11
     478:	b.ne	0x438
     47c:	b	0x4d0
     480:	rbit	x10, x15
     484:	clz	x10, x10
     488:	add	x10, x12, x10, lsr #3
     48c:	sub	x10, x10, #0x8
     490:	b	0x4a0
     494:	rbit	x10, x15
     498:	clz	x10, x10
     49c:	add	x10, x12, x10, lsr #3
     4a0:	sub	x9, x9, x10
     4a4:	cmp	x9, #0x4
     4a8:	b.ne	0x4cc
     4ac:	ldr	w9, [x10]
     4b0:	mov	w11, #0x742e
     4b4:	movk	w11, #0x7478, lsl #16
     4b8:	sub	x8, x10, x8
     4bc:	add	x8, x8, #0xa
     4c0:	cmp	w9, w11
     4c4:	csel	x0, xzr, x8, ne
     4c8:	ret
     4cc:	mov	x0, #0x0
     4d0:	ret
     4d4:	sub	x10, x10, #0x3
     4d8:	b	0x4a0
     4dc:	sub	x10, x10, #0x2
     4e0:	b	0x4a0
     4e4:	sub	x10, x10, #0x1
     4e8:	b	0x4a0
_two_capture:
     4ec:	subs	x10, x1, #0x3
     4f0:	b.lo	0x804
     4f4:	ldrh	w8, [x0]
     4f8:	mov	w9, #0x6f66
     4fc:	cmp	w8, w9
     500:	b.ne	0x804
     504:	ldrb	w8, [x0, #0x2]
     508:	cmp	w8, #0x6f
     50c:	b.ne	0x804
     510:	add	x8, x0, #0x3
     514:	add	x9, x0, x1
     518:	sub	x11, x1, #0x13
     51c:	cmn	x11, #0x11
     520:	b.hi	0x590
     524:	mov	x13, #-0x101010101010102
     528:	movk	x13, #0xfeff
     52c:	mov	x14, #0x2222222222222222
     530:	orr	x14, x14, #0x4040404040404040
     534:	add	x12, x0, #0xb
     538:	mov	x11, x8
     53c:	ldur	x15, [x12, #-0x8]
     540:	eor	x16, x15, x14
     544:	add	x16, x16, x13
     548:	bic	x15, x16, x15
     54c:	ands	x15, x15, #0x8080808080808080
     550:	b.ne	0x628
     554:	ldr	x15, [x12]
     558:	eor	x16, x15, x14
     55c:	add	x16, x16, x13
     560:	bic	x15, x16, x15
     564:	ands	x15, x15, #0x8080808080808080
     568:	b.ne	0x63c
     56c:	add	x12, x12, #0x10
     570:	add	x11, x11, #0x10
     574:	sub	x10, x10, #0x10
     578:	cmp	x10, #0xf
     57c:	b.hi	0x53c
     580:	sub	x12, x12, #0x8
     584:	cmp	x12, x9
     588:	b.lo	0x5a0
     58c:	b	0x804
     590:	mov	x11, x8
     594:	mov	x12, x8
     598:	cmp	x8, x9
     59c:	b.hs	0x804
     5a0:	sub	x11, x9, x11
     5a4:	mov	x10, x12
     5a8:	ands	x13, x11, #0x3
     5ac:	b.eq	0x5cc
     5b0:	mov	x10, x12
     5b4:	ldrb	w14, [x10]
     5b8:	cmp	w14, #0x62
     5bc:	b.eq	0x648
     5c0:	add	x10, x10, #0x1
     5c4:	subs	x13, x13, #0x1
     5c8:	b.ne	0x5b4
     5cc:	sub	x13, x11, #0x1
     5d0:	cmp	x13, #0x3
     5d4:	b.lo	0x804
     5d8:	add	x11, x12, x11
     5dc:	add	x10, x10, #0x3
     5e0:	ldurb	w12, [x10, #-0x3]
     5e4:	cmp	w12, #0x62
     5e8:	b.eq	0x780
     5ec:	ldurb	w12, [x10, #-0x2]
     5f0:	cmp	w12, #0x62
     5f4:	b.eq	0x788
     5f8:	ldurb	w12, [x10, #-0x1]
     5fc:	cmp	w12, #0x62
     600:	b.eq	0x790
     604:	ldrb	w12, [x10]
     608:	cmp	w12, #0x62
     60c:	b.eq	0x648
     610:	mov	x0, #0x0
     614:	add	x12, x10, #0x1
     618:	add	x10, x10, #0x4
     61c:	cmp	x12, x11
     620:	b.ne	0x5e0
     624:	b	0x808
     628:	rbit	x10, x15
     62c:	clz	x10, x10
     630:	add	x10, x12, x10, lsr #3
     634:	sub	x10, x10, #0x8
     638:	b	0x648
     63c:	rbit	x10, x15
     640:	clz	x10, x10
     644:	add	x10, x12, x10, lsr #3
     648:	sub	x11, x9, x10
     64c:	cmp	x11, #0x3
     650:	b.lo	0x804
     654:	ldrh	w11, [x10]
     658:	mov	w12, #0x6162
     65c:	cmp	w11, w12
     660:	b.ne	0x804
     664:	ldrb	w11, [x10, #0x2]
     668:	cmp	w11, #0x7a
     66c:	b.ne	0x804
     670:	add	x11, x10, #0x3
     674:	sub	x13, x9, x11
     678:	cmp	x13, #0x10
     67c:	b.lo	0x6e8
     680:	mov	x12, #0x0
     684:	mov	x14, #-0x101010101010102
     688:	movk	x14, #0xfeff
     68c:	mov	x15, #0x6666666666666666
     690:	eor	x15, x15, #0x303030303030303
     694:	add	x16, x10, x12
     698:	ldur	x17, [x16, #0x3]
     69c:	eor	x0, x17, x15
     6a0:	add	x0, x0, x14
     6a4:	bic	x17, x0, x17
     6a8:	ands	x17, x17, #0x8080808080808080
     6ac:	b.ne	0x798
     6b0:	ldur	x16, [x16, #0xb]
     6b4:	eor	x17, x16, x15
     6b8:	add	x17, x17, x14
     6bc:	bic	x16, x17, x16
     6c0:	ands	x16, x16, #0x8080808080808080
     6c4:	b.ne	0x7b0
     6c8:	add	x12, x12, #0x10
     6cc:	sub	x13, x13, #0x10
     6d0:	cmp	x13, #0xf
     6d4:	b.hi	0x694
     6d8:	add	x13, x10, x12
     6dc:	add	x13, x13, #0x3
     6e0:	add	x12, x11, x12
     6e4:	b	0x6f0
     6e8:	mov	x12, x11
     6ec:	mov	x13, x11
     6f0:	cmp	x13, x9
     6f4:	b.hs	0x804
     6f8:	sub	x14, x9, x12
     6fc:	mov	x12, x13
     700:	ands	x15, x14, #0x3
     704:	b.eq	0x724
     708:	mov	x12, x13
     70c:	ldrb	w16, [x12]
     710:	cmp	w16, #0x65
     714:	b.eq	0x7c4
     718:	add	x12, x12, #0x1
     71c:	subs	x15, x15, #0x1
     720:	b.ne	0x70c
     724:	sub	x15, x14, #0x1
     728:	cmp	x15, #0x3
     72c:	b.lo	0x804
     730:	add	x13, x13, x14
     734:	add	x12, x12, #0x3
     738:	ldurb	w14, [x12, #-0x3]
     73c:	cmp	w14, #0x65
     740:	b.eq	0x80c
     744:	ldurb	w14, [x12, #-0x2]
     748:	cmp	w14, #0x65
     74c:	b.eq	0x814
     750:	ldurb	w14, [x12, #-0x1]
     754:	cmp	w14, #0x65
     758:	b.eq	0x81c
     75c:	ldrb	w14, [x12]
     760:	cmp	w14, #0x65
     764:	b.eq	0x7c4
     768:	mov	x0, #0x0
     76c:	add	x14, x12, #0x1
     770:	add	x12, x12, #0x4
     774:	cmp	x14, x13
     778:	b.ne	0x738
     77c:	b	0x808
     780:	sub	x10, x10, #0x3
     784:	b	0x648
     788:	sub	x10, x10, #0x2
     78c:	b	0x648
     790:	sub	x10, x10, #0x1
     794:	b	0x648
     798:	rbit	x13, x17
     79c:	clz	x13, x13
     7a0:	add	x13, x10, x13, lsr #3
     7a4:	add	x12, x13, x12
     7a8:	add	x12, x12, #0x3
     7ac:	b	0x7c4
     7b0:	rbit	x13, x16
     7b4:	clz	x13, x13
     7b8:	add	x13, x10, x13, lsr #3
     7bc:	add	x12, x13, x12
     7c0:	add	x12, x12, #0xb
     7c4:	sub	x9, x9, x12
     7c8:	cmp	x9, #0x3
     7cc:	b.ne	0x804
     7d0:	ldrh	w9, [x12]
     7d4:	mov	w13, #0x7465
     7d8:	cmp	w9, w13
     7dc:	b.ne	0x804
     7e0:	ldrb	w9, [x12, #0x2]
     7e4:	sub	x11, x12, x11
     7e8:	add	x11, x11, x11, lsl #1
     7ec:	sub	x8, x10, x8
     7f0:	add	x8, x8, x11
     7f4:	add	x8, x8, #0xb
     7f8:	cmp	w9, #0x63
     7fc:	csel	x0, xzr, x8, ne
     800:	ret
     804:	mov	x0, #0x0
     808:	ret
     80c:	sub	x12, x12, #0x3
     810:	b	0x7c4
     814:	sub	x12, x12, #0x2
     818:	b	0x7c4
     81c:	sub	x12, x12, #0x1
     820:	b	0x7c4
_branchy:
     824:	add	x8, x0, x1
     828:	cmp	x1, #0x5
     82c:	b.ls	0x8d4
     830:	ldr	w9, [x0]
     834:	mov	w10, #0x6c61
     838:	movk	w10, #0x6870, lsl #16
     83c:	cmp	w9, w10
     840:	b.ne	0xb48
     844:	ldrh	w9, [x0, #0x4]
     848:	mov	w10, #0x3a61
     84c:	cmp	w9, w10
     850:	b.ne	0xb48
     854:	add	x9, x0, #0x6
     858:	sub	x10, x1, #0x16
     85c:	cmn	x10, #0x11
     860:	b.hi	0x8e0
     864:	mov	x12, #-0x101010101010102
     868:	movk	x12, #0xfeff
     86c:	mov	x13, #0x3838383838383838
     870:	orr	x13, x13, #0x3333333333333333
     874:	add	x11, x0, #0xe
     878:	sub	x14, x1, #0x6
     87c:	mov	x10, x9
     880:	ldur	x15, [x11, #-0x8]
     884:	eor	x16, x15, x13
     888:	add	x16, x16, x12
     88c:	bic	x15, x16, x15
     890:	ands	x15, x15, #0x8080808080808080
     894:	b.ne	0x974
     898:	ldr	x15, [x11]
     89c:	eor	x16, x15, x13
     8a0:	add	x16, x16, x12
     8a4:	bic	x15, x16, x15
     8a8:	ands	x15, x15, #0x8080808080808080
     8ac:	b.ne	0x988
     8b0:	add	x11, x11, #0x10
     8b4:	add	x10, x10, #0x10
     8b8:	sub	x14, x14, #0x10
     8bc:	cmp	x14, #0xf
     8c0:	b.hi	0x880
     8c4:	sub	x11, x11, #0x8
     8c8:	cmp	x11, x8
     8cc:	b.lo	0x8f0
     8d0:	b	0xb48
     8d4:	mov	x9, x0
     8d8:	mov	x10, x0
     8dc:	b	0xbb8
     8e0:	mov	x10, x9
     8e4:	mov	x11, x9
     8e8:	cmp	x9, x8
     8ec:	b.hs	0xb48
     8f0:	sub	x12, x8, x10
     8f4:	mov	x10, x11
     8f8:	ands	x13, x12, #0x3
     8fc:	b.eq	0x91c
     900:	mov	x10, x11
     904:	ldrb	w14, [x10]
     908:	cmp	w14, #0x3b
     90c:	b.eq	0x994
     910:	add	x10, x10, #0x1
     914:	subs	x13, x13, #0x1
     918:	b.ne	0x904
     91c:	sub	x13, x12, #0x1
     920:	cmp	x13, #0x3
     924:	b.lo	0xb48
     928:	add	x11, x11, x12
     92c:	add	x10, x10, #0x3
     930:	ldurb	w12, [x10, #-0x3]
     934:	cmp	w12, #0x3b
     938:	b.eq	0xad0
     93c:	ldurb	w12, [x10, #-0x2]
     940:	cmp	w12, #0x3b
     944:	b.eq	0xad8
     948:	ldurb	w12, [x10, #-0x1]
     94c:	cmp	w12, #0x3b
     950:	b.eq	0xae0
     954:	ldrb	w12, [x10]
     958:	cmp	w12, #0x3b
     95c:	b.eq	0x994
     960:	add	x12, x10, #0x1
     964:	add	x10, x10, #0x4
     968:	cmp	x12, x11
     96c:	b.ne	0x930
     970:	b	0xb48
     974:	rbit	x10, x15
     978:	clz	x10, x10
     97c:	add	x10, x11, x10, lsr #3
     980:	sub	x10, x10, #0x8
     984:	b	0x994
     988:	rbit	x10, x15
     98c:	clz	x10, x10
     990:	add	x10, x11, x10, lsr #3
     994:	sub	x11, x8, x10
     998:	cmp	x11, #0xa
     99c:	b.lo	0xb48
     9a0:	ldr	w11, [x10]
     9a4:	mov	w12, #0x623b
     9a8:	movk	w12, #0x7465, lsl #16
     9ac:	cmp	w11, w12
     9b0:	b.ne	0xb48
     9b4:	ldrh	w11, [x10, #0x4]
     9b8:	mov	w12, #0x3a61
     9bc:	cmp	w11, w12
     9c0:	b.ne	0xb48
     9c4:	add	x11, x10, #0x6
     9c8:	sub	x13, x8, x11
     9cc:	cmp	x13, #0x10
     9d0:	b.lo	0xa3c
     9d4:	mov	x12, #0x0
     9d8:	mov	x14, #-0x101010101010102
     9dc:	movk	x14, #0xfeff
     9e0:	mov	x15, #0x3838383838383838
     9e4:	orr	x15, x15, #0x3333333333333333
     9e8:	add	x16, x10, x12
     9ec:	ldur	x17, [x16, #0x6]
     9f0:	eor	x2, x17, x15
     9f4:	add	x2, x2, x14
     9f8:	bic	x17, x2, x17
     9fc:	ands	x17, x17, #0x8080808080808080
     a00:	b.ne	0xae8
     a04:	ldur	x16, [x16, #0xe]
     a08:	eor	x17, x16, x15
     a0c:	add	x17, x17, x14
     a10:	bic	x16, x17, x16
     a14:	ands	x16, x16, #0x8080808080808080
     a18:	b.ne	0xb00
     a1c:	add	x12, x12, #0x10
     a20:	sub	x13, x13, #0x10
     a24:	cmp	x13, #0xf
     a28:	b.hi	0x9e8
     a2c:	add	x13, x10, x12
     a30:	add	x13, x13, #0x6
     a34:	add	x12, x11, x12
     a38:	b	0xa44
     a3c:	mov	x12, x11
     a40:	mov	x13, x11
     a44:	cmp	x13, x8
     a48:	b.hs	0xb48
     a4c:	sub	x14, x8, x12
     a50:	mov	x12, x13
     a54:	ands	x15, x14, #0x3
     a58:	b.eq	0xa78
     a5c:	mov	x12, x13
     a60:	ldrb	w16, [x12]
     a64:	cmp	w16, #0x3b
     a68:	b.eq	0xb14
     a6c:	add	x12, x12, #0x1
     a70:	subs	x15, x15, #0x1
     a74:	b.ne	0xa60
     a78:	sub	x15, x14, #0x1
     a7c:	cmp	x15, #0x3
     a80:	b.lo	0xb48
     a84:	add	x13, x13, x14
     a88:	add	x12, x12, #0x3
     a8c:	ldurb	w14, [x12, #-0x3]
     a90:	cmp	w14, #0x3b
     a94:	b.eq	0xea8
     a98:	ldurb	w14, [x12, #-0x2]
     a9c:	cmp	w14, #0x3b
     aa0:	b.eq	0xeb0
     aa4:	ldurb	w14, [x12, #-0x1]
     aa8:	cmp	w14, #0x3b
     aac:	b.eq	0xeb8
     ab0:	ldrb	w14, [x12]
     ab4:	cmp	w14, #0x3b
     ab8:	b.eq	0xb14
     abc:	add	x14, x12, #0x1
     ac0:	add	x12, x12, #0x4
     ac4:	cmp	x14, x13
     ac8:	b.ne	0xa8c
     acc:	b	0xb48
     ad0:	sub	x10, x10, #0x3
     ad4:	b	0x994
     ad8:	sub	x10, x10, #0x2
     adc:	b	0x994
     ae0:	sub	x10, x10, #0x1
     ae4:	b	0x994
     ae8:	rbit	x13, x17
     aec:	clz	x13, x13
     af0:	add	x13, x10, x13, lsr #3
     af4:	add	x12, x13, x12
     af8:	add	x12, x12, #0x6
     afc:	b	0xb14
     b00:	rbit	x13, x16
     b04:	clz	x13, x13
     b08:	add	x13, x10, x13, lsr #3
     b0c:	add	x12, x13, x12
     b10:	add	x12, x12, #0xe
     b14:	sub	x13, x8, x12
     b18:	cmp	x13, #0x4
     b1c:	b.ne	0xb48
     b20:	ldr	w13, [x12]
     b24:	mov	w14, #0x653b
     b28:	movk	w14, #0x646e, lsl #16
     b2c:	cmp	w13, w14
     b30:	b.ne	0xb48
     b34:	add	x8, x9, x11
     b38:	sub	x8, x10, x8
     b3c:	add	x8, x8, x12
     b40:	add	x0, x8, #0x64
     b44:	ret
     b48:	mov	x9, x0
     b4c:	mov	x10, x0
     b50:	cmp	x1, #0x10
     b54:	b.lo	0xbb8
     b58:	mov	x11, #-0x101010101010102
     b5c:	movk	x11, #0xfeff
     b60:	mov	x12, #0xe0e0e0e0e0e0e0e
     b64:	orr	x12, x12, #0x2222222222222222
     b68:	mov	x13, x1
     b6c:	mov	x9, x0
     b70:	mov	x10, x0
     b74:	ldr	x14, [x10]
     b78:	eor	x15, x14, x12
     b7c:	add	x15, x15, x11
     b80:	bic	x14, x15, x14
     b84:	ands	x14, x14, #0x8080808080808080
     b88:	b.ne	0xc44
     b8c:	ldr	x14, [x10, #0x8]
     b90:	eor	x15, x14, x12
     b94:	add	x15, x15, x11
     b98:	bic	x14, x15, x14
     b9c:	ands	x14, x14, #0x8080808080808080
     ba0:	b.ne	0xc60
     ba4:	add	x10, x10, #0x10
     ba8:	add	x9, x9, #0x10
     bac:	sub	x13, x13, #0x10
     bb0:	cmp	x13, #0xf
     bb4:	b.hi	0xb74
     bb8:	cmp	x10, x8
     bbc:	b.hs	0xce0
     bc0:	sub	x11, x8, x9
     bc4:	mov	x9, x10
     bc8:	ands	x12, x11, #0x3
     bcc:	b.eq	0xbec
     bd0:	mov	x9, x10
     bd4:	ldrb	w13, [x9]
     bd8:	cmp	w13, #0x2e
     bdc:	b.eq	0xc98
     be0:	add	x9, x9, #0x1
     be4:	subs	x12, x12, #0x1
     be8:	b.ne	0xbd4
     bec:	sub	x12, x11, #0x1
     bf0:	cmp	x12, #0x3
     bf4:	b.lo	0xce0
     bf8:	add	x10, x10, x11
     bfc:	add	x9, x9, #0x3
     c00:	ldurb	w11, [x9, #-0x3]
     c04:	cmp	w11, #0x2e
     c08:	b.eq	0xc80
     c0c:	ldurb	w11, [x9, #-0x2]
     c10:	cmp	w11, #0x2e
     c14:	b.eq	0xc94
     c18:	ldurb	w11, [x9, #-0x1]
     c1c:	cmp	w11, #0x2e
     c20:	b.eq	0xcd0
     c24:	ldrb	w11, [x9]
     c28:	cmp	w11, #0x2e
     c2c:	b.eq	0xc98
     c30:	add	x11, x9, #0x1
     c34:	add	x9, x9, #0x4
     c38:	cmp	x11, x10
     c3c:	b.ne	0xc00
     c40:	b	0xce0
     c44:	rbit	x9, x14
     c48:	clz	x9, x9
     c4c:	add	x9, x10, x9, lsr #3
     c50:	sub	x10, x8, x9
     c54:	cmp	x10, #0x5
     c58:	b.eq	0xca4
     c5c:	b	0xce0
     c60:	rbit	x9, x14
     c64:	clz	x9, x9
     c68:	add	x9, x10, x9, lsr #3
     c6c:	add	x9, x9, #0x8
     c70:	sub	x10, x8, x9
     c74:	cmp	x10, #0x5
     c78:	b.eq	0xca4
     c7c:	b	0xce0
     c80:	sub	x9, x9, #0x3
     c84:	sub	x10, x8, x9
     c88:	cmp	x10, #0x5
     c8c:	b.eq	0xca4
     c90:	b	0xce0
     c94:	sub	x9, x9, #0x2
     c98:	sub	x10, x8, x9
     c9c:	cmp	x10, #0x5
     ca0:	b.ne	0xce0
     ca4:	ldr	w10, [x9]
     ca8:	mov	w11, #0x6a2e
     cac:	movk	w11, #0x6f73, lsl #16
     cb0:	cmp	w10, w11
     cb4:	b.ne	0xce0
     cb8:	ldrb	w10, [x9, #0x4]
     cbc:	cmp	w10, #0x6e
     cc0:	b.ne	0xce0
     cc4:	sub	x8, x9, x0
     cc8:	add	x0, x8, #0xc8
     ccc:	ret
     cd0:	sub	x9, x9, #0x1
     cd4:	sub	x10, x8, x9
     cd8:	cmp	x10, #0x5
     cdc:	b.eq	0xca4
     ce0:	cmp	x1, #0x4
     ce4:	b.lo	0xe88
     ce8:	ldr	w9, [x0]
     cec:	mov	w10, #0x6f6c
     cf0:	movk	w10, #0x3a67, lsl #16
     cf4:	cmp	w9, w10
     cf8:	b.ne	0xe88
     cfc:	add	x9, x0, #0x4
     d00:	sub	x10, x1, #0x14
     d04:	cmn	x10, #0x11
     d08:	b.hi	0xd84
     d0c:	mov	x10, #0x0
     d10:	mov	x11, #-0x101010101010102
     d14:	movk	x11, #0xfeff
     d18:	mov	x12, #0x2222222222222222
     d1c:	orr	x12, x12, #0x3838383838383838
     d20:	sub	x13, x8, x0
     d24:	sub	x13, x13, #0x4
     d28:	add	x14, x0, x10
     d2c:	ldur	x15, [x14, #0x4]
     d30:	eor	x16, x15, x12
     d34:	add	x16, x16, x11
     d38:	bic	x15, x16, x15
     d3c:	ands	x15, x15, #0x8080808080808080
     d40:	b.ne	0xe18
     d44:	ldur	x14, [x14, #0xc]
     d48:	eor	x15, x14, x12
     d4c:	add	x15, x15, x11
     d50:	bic	x14, x15, x14
     d54:	ands	x14, x14, #0x8080808080808080
     d58:	b.ne	0xe30
     d5c:	add	x10, x10, #0x10
     d60:	sub	x13, x13, #0x10
     d64:	cmp	x13, #0xf
     d68:	b.hi	0xd28
     d6c:	add	x11, x0, x10
     d70:	add	x11, x11, #0x4
     d74:	add	x9, x9, x10
     d78:	cmp	x11, x8
     d7c:	b.lo	0xd90
     d80:	b	0xe88
     d84:	mov	x11, x9
     d88:	cmp	x9, x8
     d8c:	b.hs	0xe88
     d90:	sub	x10, x8, x9
     d94:	mov	x9, x11
     d98:	ands	x12, x10, #0x3
     d9c:	b.eq	0xdbc
     da0:	mov	x9, x11
     da4:	ldrb	w13, [x9]
     da8:	cmp	w13, #0x3a
     dac:	b.eq	0xe44
     db0:	add	x9, x9, #0x1
     db4:	subs	x12, x12, #0x1
     db8:	b.ne	0xda4
     dbc:	sub	x12, x10, #0x1
     dc0:	cmp	x12, #0x3
     dc4:	b.lo	0xe88
     dc8:	add	x10, x11, x10
     dcc:	add	x9, x9, #0x3
     dd0:	ldurb	w11, [x9, #-0x3]
     dd4:	cmp	w11, #0x3a
     dd8:	b.eq	0xe90
     ddc:	ldurb	w11, [x9, #-0x2]
     de0:	cmp	w11, #0x3a
     de4:	b.eq	0xe98
     de8:	ldurb	w11, [x9, #-0x1]
     dec:	cmp	w11, #0x3a
     df0:	b.eq	0xea0
     df4:	ldrb	w11, [x9]
     df8:	cmp	w11, #0x3a
     dfc:	b.eq	0xe44
     e00:	mov	x0, #0x0
     e04:	add	x11, x9, #0x1
     e08:	add	x9, x9, #0x4
     e0c:	cmp	x11, x10
     e10:	b.ne	0xdd0
     e14:	ret
     e18:	rbit	x9, x15
     e1c:	clz	x9, x9
     e20:	add	x9, x0, x9, lsr #3
     e24:	add	x9, x9, x10
     e28:	add	x9, x9, #0x4
     e2c:	b	0xe44
     e30:	rbit	x9, x14
     e34:	clz	x9, x9
     e38:	add	x9, x0, x9, lsr #3
     e3c:	add	x9, x9, x10
     e40:	add	x9, x9, #0xc
     e44:	sub	x8, x8, x9
     e48:	cmp	x8, #0x7
     e4c:	b.lo	0xe88
     e50:	ldr	w10, [x9]
     e54:	mov	w11, #0x6c3a
     e58:	movk	w11, #0x7665, lsl #16
     e5c:	cmp	w10, w11
     e60:	b.ne	0xe88
     e64:	ldrh	w10, [x9, #0x4]
     e68:	mov	w11, #0x6c65
     e6c:	cmp	w10, w11
     e70:	b.ne	0xe88
     e74:	ldrb	w9, [x9, #0x6]
     e78:	add	x8, x8, #0x125
     e7c:	cmp	w9, #0x3a
     e80:	csel	x0, xzr, x8, ne
     e84:	ret
     e88:	mov	x0, #0x0
     e8c:	ret
     e90:	sub	x9, x9, #0x3
     e94:	b	0xe44
     e98:	sub	x9, x9, #0x2
     e9c:	b	0xe44
     ea0:	sub	x9, x9, #0x1
     ea4:	b	0xe44
     ea8:	sub	x12, x12, #0x3
     eac:	b	0xb14
     eb0:	sub	x12, x12, #0x2
     eb4:	b	0xb14
     eb8:	sub	x12, x12, #0x1
     ebc:	b	0xb14
_long_literal:
     ec0:	subs	x10, x1, #0x10
     ec4:	b.lo	0x1030
     ec8:	ldp	x8, x9, [x0]
     ecc:	mov	x11, #0x6261
     ed0:	movk	x11, #0x6463, lsl #16
     ed4:	movk	x11, #0x6665, lsl #32
     ed8:	movk	x11, #0x6867, lsl #48
     edc:	cmp	x8, x11
     ee0:	mov	x8, #0x6a69
     ee4:	movk	x8, #0x6c6b, lsl #16
     ee8:	movk	x8, #0x6e6d, lsl #32
     eec:	movk	x8, #0x706f, lsl #48
     ef0:	ccmp	x9, x8, #0x0, eq
     ef4:	b.ne	0x1030
     ef8:	add	x8, x0, #0x10
     efc:	add	x9, x0, x1
     f00:	and	x11, x1, #0xfffffffffffffff0
     f04:	cmp	x11, #0x10
     f08:	b.ne	0xf18
     f0c:	mov	x12, x8
     f10:	mov	x11, x8
     f14:	b	0xf74
     f18:	mov	x13, #-0x101010101010102
     f1c:	movk	x13, #0xfeff
     f20:	mov	x14, #0x7070707070707070
     f24:	orr	x14, x14, #0x1111111111111111
     f28:	mov	x12, x8
     f2c:	mov	x11, x8
     f30:	ldr	x15, [x11]
     f34:	eor	x16, x15, x14
     f38:	add	x16, x16, x13
     f3c:	bic	x15, x16, x15
     f40:	ands	x15, x15, #0x8080808080808080
     f44:	b.ne	0x1004
     f48:	ldr	x15, [x11, #0x8]
     f4c:	eor	x16, x15, x14
     f50:	add	x16, x16, x13
     f54:	bic	x15, x16, x15
     f58:	ands	x15, x15, #0x8080808080808080
     f5c:	b.ne	0x1014
     f60:	add	x11, x11, #0x10
     f64:	add	x12, x12, #0x10
     f68:	sub	x10, x10, #0x10
     f6c:	cmp	x10, #0xf
     f70:	b.hi	0xf30
     f74:	cmp	x11, x9
     f78:	b.hs	0x1030
     f7c:	sub	x12, x9, x12
     f80:	mov	x10, x11
     f84:	ands	x13, x12, #0x3
     f88:	b.eq	0xfa8
     f8c:	mov	x10, x11
     f90:	ldrb	w14, [x10]
     f94:	cmp	w14, #0x71
     f98:	b.eq	0x1024
     f9c:	add	x10, x10, #0x1
     fa0:	subs	x13, x13, #0x1
     fa4:	b.ne	0xf90
     fa8:	sub	x13, x12, #0x1
     fac:	cmp	x13, #0x3
     fb0:	b.lo	0x1030
     fb4:	add	x11, x11, x12
     fb8:	add	x10, x10, #0x3
     fbc:	ldurb	w12, [x10, #-0x3]
     fc0:	cmp	w12, #0x71
     fc4:	b.eq	0x1074
     fc8:	ldurb	w12, [x10, #-0x2]
     fcc:	cmp	w12, #0x71
     fd0:	b.eq	0x107c
     fd4:	ldurb	w12, [x10, #-0x1]
     fd8:	cmp	w12, #0x71
     fdc:	b.eq	0x1084
     fe0:	ldrb	w12, [x10]
     fe4:	cmp	w12, #0x71
     fe8:	b.eq	0x1024
     fec:	mov	x0, #0x0
     ff0:	add	x12, x10, #0x1
     ff4:	add	x10, x10, #0x4
     ff8:	cmp	x12, x11
     ffc:	b.ne	0xfbc
    1000:	b	0x1034
    1004:	rbit	x10, x15
    1008:	clz	x10, x10
    100c:	add	x10, x11, x10, lsr #3
    1010:	b	0x1024
    1014:	rbit	x10, x15
    1018:	clz	x10, x10
    101c:	add	x10, x11, x10, lsr #3
    1020:	add	x10, x10, #0x8
    1024:	sub	x9, x9, x10
    1028:	cmp	x9, #0x10
    102c:	b.hs	0x1038
    1030:	mov	x0, #0x0
    1034:	ret
    1038:	ldp	x9, x11, [x10]
    103c:	mov	x12, #0x7271
    1040:	movk	x12, #0x7473, lsl #16
    1044:	movk	x12, #0x7675, lsl #32
    1048:	movk	x12, #0x7877, lsl #48
    104c:	sub	x8, x10, x8
    1050:	add	x8, x8, #0x190
    1054:	cmp	x9, x12
    1058:	mov	x9, #0x7a79
    105c:	movk	x9, #0x3130, lsl #16
    1060:	movk	x9, #0x3332, lsl #32
    1064:	movk	x9, #0x3534, lsl #48
    1068:	ccmp	x11, x9, #0x0, eq
    106c:	csel	x0, xzr, x8, ne
    1070:	ret
    1074:	sub	x10, x10, #0x3
    1078:	b	0x1024
    107c:	sub	x10, x10, #0x2
    1080:	b	0x1024
    1084:	sub	x10, x10, #0x1
    1088:	b	0x1024
_width_literals:
    108c:	subs	x10, x1, #0x2
    1090:	b.lo	0x151c
    1094:	add	x9, x0, x1
    1098:	ldrh	w8, [x0]
    109c:	mov	w11, #0x6261
    10a0:	cmp	w8, w11
    10a4:	b.ne	0x1200
    10a8:	add	x8, x0, #0x2
    10ac:	sub	x11, x1, #0x12
    10b0:	cmn	x11, #0x11
    10b4:	b.hi	0x1124
    10b8:	mov	x13, #-0x101010101010102
    10bc:	movk	x13, #0xfeff
    10c0:	mov	x14, #0x6060606060606060
    10c4:	orr	x14, x14, #0x303030303030303
    10c8:	add	x12, x0, #0xa
    10cc:	mov	x11, x8
    10d0:	ldur	x15, [x12, #-0x8]
    10d4:	eor	x16, x15, x14
    10d8:	add	x16, x16, x13
    10dc:	bic	x15, x16, x15
    10e0:	ands	x15, x15, #0x8080808080808080
    10e4:	b.ne	0x11b8
    10e8:	ldr	x15, [x12]
    10ec:	eor	x16, x15, x14
    10f0:	add	x16, x16, x13
    10f4:	bic	x15, x16, x15
    10f8:	ands	x15, x15, #0x8080808080808080
    10fc:	b.ne	0x11cc
    1100:	add	x12, x12, #0x10
    1104:	add	x11, x11, #0x10
    1108:	sub	x10, x10, #0x10
    110c:	cmp	x10, #0xf
    1110:	b.hi	0x10d0
    1114:	sub	x12, x12, #0x8
    1118:	cmp	x12, x9
    111c:	b.lo	0x1134
    1120:	b	0x1200
    1124:	mov	x11, x8
    1128:	mov	x12, x8
    112c:	cmp	x8, x9
    1130:	b.hs	0x1200
    1134:	sub	x11, x9, x11
    1138:	mov	x10, x12
    113c:	ands	x13, x11, #0x3
    1140:	b.eq	0x1160
    1144:	mov	x10, x12
    1148:	ldrb	w14, [x10]
    114c:	cmp	w14, #0x63
    1150:	b.eq	0x11d8
    1154:	add	x10, x10, #0x1
    1158:	subs	x13, x13, #0x1
    115c:	b.ne	0x1148
    1160:	sub	x13, x11, #0x1
    1164:	cmp	x13, #0x3
    1168:	b.lo	0x1200
    116c:	add	x11, x12, x11
    1170:	add	x10, x10, #0x3
    1174:	ldurb	w12, [x10, #-0x3]
    1178:	cmp	w12, #0x63
    117c:	b.eq	0x1528
    1180:	ldurb	w12, [x10, #-0x2]
    1184:	cmp	w12, #0x63
    1188:	b.eq	0x1530
    118c:	ldurb	w12, [x10, #-0x1]
    1190:	cmp	w12, #0x63
    1194:	b.eq	0x1538
    1198:	ldrb	w12, [x10]
    119c:	cmp	w12, #0x63
    11a0:	b.eq	0x11d8
    11a4:	add	x12, x10, #0x1
    11a8:	add	x10, x10, #0x4
    11ac:	cmp	x12, x11
    11b0:	b.ne	0x1174
    11b4:	b	0x1200
    11b8:	rbit	x10, x15
    11bc:	clz	x10, x10
    11c0:	add	x10, x12, x10, lsr #3
    11c4:	sub	x10, x10, #0x8
    11c8:	b	0x11d8
    11cc:	rbit	x10, x15
    11d0:	clz	x10, x10
    11d4:	add	x10, x12, x10, lsr #3
    11d8:	sub	x11, x9, x10
    11dc:	cmp	x11, #0x2
    11e0:	b.ne	0x1200
    11e4:	ldrh	w11, [x10]
    11e8:	mov	w12, #0x6463
    11ec:	cmp	w11, w12
    11f0:	b.ne	0x1200
    11f4:	sub	x8, x10, x8
    11f8:	add	x0, x8, #0x2
    11fc:	ret
    1200:	subs	x12, x1, #0x8
    1204:	b.lo	0x151c
    1208:	ldr	x10, [x0]
    120c:	mov	x11, #0x6261
    1210:	movk	x11, #0x6463, lsl #16
    1214:	movk	x11, #0x6665, lsl #32
    1218:	movk	x11, #0x6867, lsl #48
    121c:	cmp	x10, x11
    1220:	b.ne	0x137c
    1224:	add	x8, x0, #0x8
    1228:	sub	x13, x1, #0x18
    122c:	cmn	x13, #0x11
    1230:	b.hi	0x1294
    1234:	mov	x15, #-0x101010101010102
    1238:	movk	x15, #0xfeff
    123c:	mov	x16, #0x1111111111111111
    1240:	eor	x16, x16, #0x7878787878787878
    1244:	mov	x14, x8
    1248:	mov	x13, x8
    124c:	ldr	x17, [x13]
    1250:	eor	x2, x17, x16
    1254:	add	x2, x2, x15
    1258:	bic	x17, x2, x17
    125c:	ands	x17, x17, #0x8080808080808080
    1260:	b.ne	0x1328
    1264:	ldr	x17, [x13, #0x8]
    1268:	eor	x2, x17, x16
    126c:	add	x2, x2, x15
    1270:	bic	x17, x2, x17
    1274:	ands	x17, x17, #0x8080808080808080
    1278:	b.ne	0x1338
    127c:	add	x13, x13, #0x10
    1280:	add	x14, x14, #0x10
    1284:	sub	x12, x12, #0x10
    1288:	cmp	x12, #0xf
    128c:	b.hi	0x124c
    1290:	b	0x129c
    1294:	mov	x14, x8
    1298:	mov	x13, x8
    129c:	cmp	x13, x9
    12a0:	b.hs	0x137c
    12a4:	sub	x14, x9, x14
    12a8:	mov	x12, x13
    12ac:	ands	x15, x14, #0x3
    12b0:	b.eq	0x12d0
    12b4:	mov	x12, x13
    12b8:	ldrb	w16, [x12]
    12bc:	cmp	w16, #0x69
    12c0:	b.eq	0x1348
    12c4:	add	x12, x12, #0x1
    12c8:	subs	x15, x15, #0x1
    12cc:	b.ne	0x12b8
    12d0:	sub	x15, x14, #0x1
    12d4:	cmp	x15, #0x3
    12d8:	b.lo	0x137c
    12dc:	add	x13, x13, x14
    12e0:	add	x12, x12, #0x3
    12e4:	ldurb	w14, [x12, #-0x3]
    12e8:	cmp	w14, #0x69
    12ec:	b.eq	0x1540
    12f0:	ldurb	w14, [x12, #-0x2]
    12f4:	cmp	w14, #0x69
    12f8:	b.eq	0x1548
    12fc:	ldurb	w14, [x12, #-0x1]
    1300:	cmp	w14, #0x69
    1304:	b.eq	0x1550
    1308:	ldrb	w14, [x12]
    130c:	cmp	w14, #0x69
    1310:	b.eq	0x1348
    1314:	add	x14, x12, #0x1
    1318:	add	x12, x12, #0x4
    131c:	cmp	x14, x13
    1320:	b.ne	0x12e4
    1324:	b	0x137c
    1328:	rbit	x12, x17
    132c:	clz	x12, x12
    1330:	add	x12, x13, x12, lsr #3
    1334:	b	0x1348
    1338:	rbit	x12, x17
    133c:	clz	x12, x12
    1340:	add	x12, x13, x12, lsr #3
    1344:	add	x12, x12, #0x8
    1348:	sub	x13, x9, x12
    134c:	cmp	x13, #0x8
    1350:	b.ne	0x137c
    1354:	ldr	x13, [x12]
    1358:	mov	x14, #0x6a69
    135c:	movk	x14, #0x6c6b, lsl #16
    1360:	movk	x14, #0x6e6d, lsl #32
    1364:	movk	x14, #0x706f, lsl #48
    1368:	cmp	x13, x14
    136c:	b.ne	0x137c
    1370:	sub	x8, x12, x8
    1374:	add	x0, x8, #0x8
    1378:	ret
    137c:	mov	x8, #0x0
    1380:	subs	x12, x1, #0x10
    1384:	b.lo	0x1520
    1388:	cmp	x10, x11
    138c:	b.ne	0x1520
    1390:	ldr	x10, [x0, #0x8]
    1394:	mov	x11, #0x6a69
    1398:	movk	x11, #0x6c6b, lsl #16
    139c:	movk	x11, #0x6e6d, lsl #32
    13a0:	movk	x11, #0x706f, lsl #48
    13a4:	cmp	x10, x11
    13a8:	b.ne	0x1520
    13ac:	add	x10, x0, #0x10
    13b0:	and	x8, x1, #0xfffffffffffffff0
    13b4:	cmp	x8, #0x10
    13b8:	b.ne	0x13c8
    13bc:	mov	x11, x10
    13c0:	mov	x8, x10
    13c4:	b	0x1424
    13c8:	mov	x13, #-0x101010101010102
    13cc:	movk	x13, #0xfeff
    13d0:	mov	x14, #0x7070707070707070
    13d4:	orr	x14, x14, #0x1111111111111111
    13d8:	mov	x11, x10
    13dc:	mov	x8, x10
    13e0:	ldr	x15, [x8]
    13e4:	eor	x16, x15, x14
    13e8:	add	x16, x16, x13
    13ec:	bic	x15, x16, x15
    13f0:	ands	x15, x15, #0x8080808080808080
    13f4:	b.ne	0x14b4
    13f8:	ldr	x15, [x8, #0x8]
    13fc:	eor	x16, x15, x14
    1400:	add	x16, x16, x13
    1404:	bic	x15, x16, x15
    1408:	ands	x15, x15, #0x8080808080808080
    140c:	b.ne	0x14c4
    1410:	add	x8, x8, #0x10
    1414:	add	x11, x11, #0x10
    1418:	sub	x12, x12, #0x10
    141c:	cmp	x12, #0xf
    1420:	b.hi	0x13e0
    1424:	cmp	x8, x9
    1428:	b.hs	0x151c
    142c:	sub	x12, x9, x11
    1430:	mov	x11, x8
    1434:	ands	x13, x12, #0x3
    1438:	b.eq	0x1458
    143c:	mov	x11, x8
    1440:	ldrb	w14, [x11]
    1444:	cmp	w14, #0x71
    1448:	b.eq	0x14d4
    144c:	add	x11, x11, #0x1
    1450:	subs	x13, x13, #0x1
    1454:	b.ne	0x1440
    1458:	sub	x13, x12, #0x1
    145c:	cmp	x13, #0x3
    1460:	b.lo	0x151c
    1464:	add	x12, x8, x12
    1468:	add	x11, x11, #0x3
    146c:	ldurb	w8, [x11, #-0x3]
    1470:	cmp	w8, #0x71
    1474:	b.eq	0x1558
    1478:	ldurb	w8, [x11, #-0x2]
    147c:	cmp	w8, #0x71
    1480:	b.eq	0x1560
    1484:	ldurb	w8, [x11, #-0x1]
    1488:	cmp	w8, #0x71
    148c:	b.eq	0x1568
    1490:	ldrb	w8, [x11]
    1494:	cmp	w8, #0x71
    1498:	b.eq	0x14d4
    149c:	mov	x8, #0x0
    14a0:	add	x13, x11, #0x1
    14a4:	add	x11, x11, #0x4
    14a8:	cmp	x13, x12
    14ac:	b.ne	0x146c
    14b0:	b	0x1520
    14b4:	rbit	x11, x15
    14b8:	clz	x11, x11
    14bc:	add	x11, x8, x11, lsr #3
    14c0:	b	0x14d4
    14c4:	rbit	x11, x15
    14c8:	clz	x11, x11
    14cc:	add	x8, x8, x11, lsr #3
    14d0:	add	x11, x8, #0x8
    14d4:	sub	x8, x9, x11
    14d8:	cmp	x8, #0x10
    14dc:	b.ne	0x151c
    14e0:	ldp	x8, x9, [x11]
    14e4:	mov	x12, #0x7271
    14e8:	movk	x12, #0x7473, lsl #16
    14ec:	movk	x12, #0x7675, lsl #32
    14f0:	movk	x12, #0x7877, lsl #48
    14f4:	sub	x10, x11, x10
    14f8:	add	x10, x10, #0x10
    14fc:	cmp	x8, x12
    1500:	mov	x8, #0x7a79
    1504:	movk	x8, #0x3130, lsl #16
    1508:	movk	x8, #0x3332, lsl #32
    150c:	movk	x8, #0x3534, lsl #48
    1510:	ccmp	x9, x8, #0x0, eq
    1514:	csel	x0, xzr, x10, ne
    1518:	ret
    151c:	mov	x8, #0x0
    1520:	mov	x0, x8
    1524:	ret
    1528:	sub	x10, x10, #0x3
    152c:	b	0x11d8
    1530:	sub	x10, x10, #0x2
    1534:	b	0x11d8
    1538:	sub	x10, x10, #0x1
    153c:	b	0x11d8
    1540:	sub	x12, x12, #0x3
    1544:	b	0x1348
    1548:	sub	x12, x12, #0x2
    154c:	b	0x1348
    1550:	sub	x12, x12, #0x1
    1554:	b	0x1348
    1558:	sub	x11, x11, #0x3
    155c:	b	0x14d4
    1560:	sub	x11, x11, #0x2
    1564:	b	0x14d4
    1568:	sub	x11, x11, #0x1
    156c:	b	0x14d4
