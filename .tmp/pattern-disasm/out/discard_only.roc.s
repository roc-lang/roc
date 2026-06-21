Archive : .tmp/pattern-disasm/out/discard_only.a
.tmp/pattern-disasm/out/discard_only.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_6:
       0:	sub	sp, sp, #0x40
       4:	stp	x20, x19, [sp, #0x20]
       8:	stp	x29, x30, [sp, #0x30]
       c:	ldp	x8, x9, [x1]
      10:	ldr	x10, [x1, #0x10]
      14:	stp	x8, x9, [sp, #0x8]
      18:	str	x10, [sp, #0x18]
      1c:	ubfx	x11, x10, #56, #7
      20:	cmn	x10, #0x1
      24:	add	x15, sp, #0x8
      28:	csel	x12, x8, x15, gt
      2c:	csel	x11, x10, x11, gt
      30:	subs	x13, x11, #0x5
      34:	b.hs	0x44
      38:	mov	x19, #0x0
      3c:	tbz	x10, #0x3f, 0x254
      40:	b	0x288
      44:	mov	x14, #0xe0e0e0e0e0e0e0e
      48:	orr	x14, x14, #0x2222222222222222
      4c:	ldr	w16, [x12]
      50:	cmn	x10, #0x1
      54:	csel	x15, x8, x15, gt
      58:	ldur	w15, [x15, #0x1]
      5c:	mov	w17, #0x4547
      60:	movk	w17, #0x2054, lsl #16
      64:	cmp	w16, w17
      68:	mov	w16, #0x5445
      6c:	movk	w16, #0x2f20, lsl #16
      70:	ccmp	w15, w16, #0x0, eq
      74:	b.eq	0x154
      78:	sub	x16, x11, #0x14
      7c:	cmn	x16, #0x11
      80:	b.hi	0x2d0
      84:	mov	x15, #0x0
      88:	mov	x17, #-0x101010101010102
      8c:	movk	x17, #0xfeff
      90:	mov	x1, #-0x101010101010102
      94:	add	x2, x12, x15
      98:	ldp	x3, x2, [x2]
      9c:	eor	x4, x2, x14
      a0:	eor	x5, x3, x14
      a4:	adds	x5, x5, x17
      a8:	bic	x3, x5, x3
      ac:	adc	x4, x4, x1
      b0:	bic	x4, x4, x2
      b4:	and	x2, x3, #0x8080808080808080
      b8:	and	x3, x4, #0x8080808080808080
      bc:	orr	x4, x2, x3
      c0:	cbnz	x4, 0x108
      c4:	add	x15, x15, #0x10
      c8:	cmp	x15, x13
      cc:	csel	x2, xzr, x16, hi
      d0:	sub	x16, x16, #0x10
      d4:	cmp	x2, #0xf
      d8:	b.hi	0x94
      dc:	cbz	x2, 0x38
      e0:	ldrb	w14, [x12, x15]
      e4:	cmp	w14, #0x2e
      e8:	b.eq	0x128
      ec:	mov	x19, #0x0
      f0:	cmp	x13, x15
      f4:	b.eq	0x250
      f8:	add	x15, x15, #0x1
      fc:	cmp	x15, x13
     100:	b.ls	0xe0
     104:	b	0x250
     108:	rbit	x13, x2
     10c:	clz	x13, x13
     110:	rbit	x14, x3
     114:	clz	x14, x14
     118:	add	x14, x14, #0x40
     11c:	cmp	x2, #0x0
     120:	csel	x13, x13, x14, ne
     124:	add	x15, x15, x13, lsr #3
     128:	add	x12, x12, x15
     12c:	ldur	w12, [x12, #0x1]
     130:	add	x13, x15, #0x5
     134:	cmp	x13, x11
     138:	mov	w11, #0x736a
     13c:	movk	w11, #0x6e6f, lsl #16
     140:	ccmp	w12, w11, #0x0, eq
     144:	mov	w11, #0x2
     148:	csel	x19, x11, xzr, eq
     14c:	tbz	x10, #0x3f, 0x254
     150:	b	0x288
     154:	sub	x15, x11, #0x4
     158:	sub	x16, x11, #0x8
     15c:	cmp	x15, #0x5
     160:	csel	x16, xzr, x16, lo
     164:	cmp	x16, #0x10
     168:	b.lo	0x2d8
     16c:	mov	x16, #0x0
     170:	sub	x17, x11, #0x18
     174:	add	x1, x12, #0x5
     178:	mov	x2, #-0x101010101010102
     17c:	movk	x2, #0xfeff
     180:	mov	x3, #-0x101010101010102
     184:	add	x4, x1, x16
     188:	ldp	x5, x4, [x4]
     18c:	eor	x6, x4, x14
     190:	eor	x7, x5, x14
     194:	adds	x7, x7, x2
     198:	bic	x5, x7, x5
     19c:	adc	x6, x6, x3
     1a0:	bic	x6, x6, x4
     1a4:	and	x4, x5, #0x8080808080808080
     1a8:	and	x5, x6, #0x8080808080808080
     1ac:	orr	x6, x4, x5
     1b0:	cbnz	x6, 0x1fc
     1b4:	add	x4, x16, #0x15
     1b8:	cmp	x4, x15
     1bc:	csel	x4, xzr, x17, hi
     1c0:	add	x16, x16, #0x10
     1c4:	sub	x17, x17, #0x10
     1c8:	cmp	x4, #0xf
     1cc:	b.hi	0x184
     1d0:	cbz	x4, 0x78
     1d4:	add	x16, x16, #0x5
     1d8:	ldrb	w17, [x12, x16]
     1dc:	cmp	w17, #0x2e
     1e0:	b.eq	0x220
     1e4:	cmp	x15, x16
     1e8:	b.eq	0x78
     1ec:	add	x16, x16, #0x1
     1f0:	cmp	x16, x15
     1f4:	b.ls	0x1d8
     1f8:	b	0x78
     1fc:	rbit	x15, x4
     200:	clz	x15, x15
     204:	rbit	x17, x5
     208:	clz	x17, x17
     20c:	add	x17, x17, #0x40
     210:	cmp	x4, #0x0
     214:	csel	x15, x15, x17, ne
     218:	add	x15, x16, x15, lsr #3
     21c:	add	x16, x15, #0x5
     220:	add	x15, x16, #0x4
     224:	cmp	x15, x11
     228:	b.ne	0x78
     22c:	add	x15, x12, x16
     230:	ldurh	w16, [x15, #0x1]
     234:	ldrh	w15, [x15, #0x2]
     238:	mov	w17, #0x7874
     23c:	cmp	w16, w17
     240:	mov	w16, #0x7478
     244:	ccmp	w15, w16, #0x0, eq
     248:	b.ne	0x78
     24c:	mov	w19, #0x1
     250:	tbnz	x10, #0x3f, 0x288
     254:	and	x10, x9, #0xfffffffffffffffe
     258:	tst	x9, #0x1
     25c:	csel	x8, x8, x10, eq
     260:	cbz	x8, 0x288
     264:	and	x8, x8, #0xfffffffffffffff8
     268:	ldr	x9, [x8, #-0x8]!
     26c:	strb	wzr, [sp, #0x8]
     270:	cbz	x9, 0x288
     274:	ldrb	w10, [sp, #0x8]
     278:	tbz	w10, #0x0, 0x29c
     27c:	subs	x9, x9, #0x1
     280:	str	x9, [x8]
     284:	b.eq	0x2ac
     288:	str	x19, [x0]
     28c:	ldp	x29, x30, [sp, #0x30]
     290:	ldp	x20, x19, [sp, #0x20]
     294:	add	sp, sp, #0x40
     298:	ret
     29c:	mov	x9, #-0x1
     2a0:	.long	0xf8290109
     2a4:	cmp	x9, #0x1
     2a8:	b.ne	0x288
     2ac:	mov	x20, x0
     2b0:	mov	x0, x8
     2b4:	mov	w1, #0x8
     2b8:	bl	_roc_dealloc
     2bc:	str	x19, [x20]
     2c0:	ldp	x29, x30, [sp, #0x30]
     2c4:	ldp	x20, x19, [sp, #0x20]
     2c8:	add	sp, sp, #0x40
     2cc:	ret
     2d0:	mov	x15, #0x0
     2d4:	b	0xe0
     2d8:	cbz	x16, 0x78
     2dc:	mov	w16, #0x5
     2e0:	b	0x1d8
_roc__proc_8:
     2e4:	ldr	q0, [x1]
     2e8:	str	q0, [sp, #-0x20]!
     2ec:	ldr	x8, [x1, #0x10]
     2f0:	str	x8, [sp, #0x10]
     2f4:	tbz	x8, #0x3f, 0x318
     2f8:	ubfx	x8, x8, #56, #7
     2fc:	mov	x9, sp
     300:	subs	x10, x8, #0x5
     304:	b.hs	0x324
     308:	mov	x11, #0x0
     30c:	str	x11, [x0]
     310:	add	sp, sp, #0x20
     314:	ret
     318:	ldr	x9, [sp]
     31c:	subs	x10, x8, #0x5
     320:	b.lo	0x308
     324:	mov	x11, #0xe0e0e0e0e0e0e0e
     328:	orr	x11, x11, #0x2222222222222222
     32c:	ldr	w12, [x9]
     330:	ldur	w13, [x9, #0x1]
     334:	mov	w14, #0x4547
     338:	movk	w14, #0x2054, lsl #16
     33c:	cmp	w12, w14
     340:	mov	w12, #0x5445
     344:	movk	w12, #0x2f20, lsl #16
     348:	ccmp	w13, w12, #0x0, eq
     34c:	b.eq	0x430
     350:	sub	x13, x8, #0x14
     354:	cmn	x13, #0x11
     358:	b.hi	0x548
     35c:	mov	x12, #0x0
     360:	mov	x14, #-0x101010101010102
     364:	movk	x14, #0xfeff
     368:	mov	x15, #-0x101010101010102
     36c:	add	x16, x9, x12
     370:	ldp	x17, x16, [x16]
     374:	eor	x1, x16, x11
     378:	eor	x2, x17, x11
     37c:	adds	x2, x2, x14
     380:	bic	x17, x2, x17
     384:	adc	x1, x1, x15
     388:	bic	x1, x1, x16
     38c:	and	x16, x17, #0x8080808080808080
     390:	and	x17, x1, #0x8080808080808080
     394:	orr	x1, x16, x17
     398:	cbnz	x1, 0x3e0
     39c:	add	x12, x12, #0x10
     3a0:	cmp	x12, x10
     3a4:	csel	x16, xzr, x13, hi
     3a8:	sub	x13, x13, #0x10
     3ac:	cmp	x16, #0xf
     3b0:	b.hi	0x36c
     3b4:	cbz	x16, 0x538
     3b8:	ldrb	w11, [x9, x12]
     3bc:	cmp	w11, #0x2e
     3c0:	b.eq	0x400
     3c4:	mov	x11, #0x0
     3c8:	cmp	x10, x12
     3cc:	b.eq	0x30c
     3d0:	add	x12, x12, #0x1
     3d4:	cmp	x12, x10
     3d8:	b.ls	0x3b8
     3dc:	b	0x30c
     3e0:	rbit	x10, x16
     3e4:	clz	x10, x10
     3e8:	rbit	x11, x17
     3ec:	clz	x11, x11
     3f0:	add	x11, x11, #0x40
     3f4:	cmp	x16, #0x0
     3f8:	csel	x10, x10, x11, ne
     3fc:	add	x12, x12, x10, lsr #3
     400:	add	x9, x9, x12
     404:	ldur	w9, [x9, #0x1]
     408:	add	x10, x12, #0x5
     40c:	cmp	x10, x8
     410:	mov	w8, #0x736a
     414:	movk	w8, #0x6e6f, lsl #16
     418:	ccmp	w9, w8, #0x0, eq
     41c:	mov	w8, #0x2
     420:	csel	x11, x8, xzr, eq
     424:	str	x11, [x0]
     428:	add	sp, sp, #0x20
     42c:	ret
     430:	sub	x12, x8, #0x4
     434:	sub	x13, x8, #0x8
     438:	cmp	x12, #0x5
     43c:	csel	x13, xzr, x13, lo
     440:	cmp	x13, #0x10
     444:	b.lo	0x550
     448:	mov	x13, #0x0
     44c:	sub	x14, x8, #0x18
     450:	add	x15, x9, #0x5
     454:	mov	x16, #-0x101010101010102
     458:	movk	x16, #0xfeff
     45c:	mov	x17, #-0x101010101010102
     460:	add	x1, x15, x13
     464:	ldp	x2, x1, [x1]
     468:	eor	x3, x1, x11
     46c:	eor	x4, x2, x11
     470:	adds	x4, x4, x16
     474:	bic	x2, x4, x2
     478:	adc	x3, x3, x17
     47c:	bic	x3, x3, x1
     480:	and	x1, x2, #0x8080808080808080
     484:	and	x2, x3, #0x8080808080808080
     488:	orr	x3, x1, x2
     48c:	cbnz	x3, 0x4d8
     490:	add	x1, x13, #0x15
     494:	cmp	x1, x12
     498:	csel	x1, xzr, x14, hi
     49c:	add	x13, x13, #0x10
     4a0:	sub	x14, x14, #0x10
     4a4:	cmp	x1, #0xf
     4a8:	b.hi	0x460
     4ac:	cbz	x1, 0x350
     4b0:	add	x13, x13, #0x5
     4b4:	ldrb	w14, [x9, x13]
     4b8:	cmp	w14, #0x2e
     4bc:	b.eq	0x4fc
     4c0:	cmp	x12, x13
     4c4:	b.eq	0x350
     4c8:	add	x13, x13, #0x1
     4cc:	cmp	x13, x12
     4d0:	b.ls	0x4b4
     4d4:	b	0x350
     4d8:	rbit	x12, x1
     4dc:	clz	x12, x12
     4e0:	rbit	x14, x2
     4e4:	clz	x14, x14
     4e8:	add	x14, x14, #0x40
     4ec:	cmp	x1, #0x0
     4f0:	csel	x12, x12, x14, ne
     4f4:	add	x12, x13, x12, lsr #3
     4f8:	add	x13, x12, #0x5
     4fc:	add	x12, x13, #0x4
     500:	cmp	x12, x8
     504:	b.ne	0x350
     508:	add	x12, x9, x13
     50c:	ldurh	w13, [x12, #0x1]
     510:	ldrh	w12, [x12, #0x2]
     514:	mov	w14, #0x7874
     518:	cmp	w13, w14
     51c:	mov	w13, #0x7478
     520:	ccmp	w12, w13, #0x0, eq
     524:	b.ne	0x350
     528:	mov	w11, #0x1
     52c:	str	x11, [x0]
     530:	add	sp, sp, #0x20
     534:	ret
     538:	mov	x11, #0x0
     53c:	str	x11, [x0]
     540:	add	sp, sp, #0x20
     544:	ret
     548:	mov	x12, #0x0
     54c:	b	0x3b8
     550:	cbz	x13, 0x350
     554:	mov	w13, #0x5
     558:	b	0x4b4
_roc_main:
     55c:	sub	sp, sp, #0x40
     560:	stp	x20, x19, [sp, #0x20]
     564:	stp	x29, x30, [sp, #0x30]
     568:	ldp	x8, x9, [x0]
     56c:	ldr	x10, [x0, #0x10]
     570:	stp	x8, x9, [sp, #0x8]
     574:	str	x10, [sp, #0x18]
     578:	ubfx	x11, x10, #56, #7
     57c:	cmn	x10, #0x1
     580:	add	x15, sp, #0x8
     584:	csel	x12, x8, x15, gt
     588:	csel	x11, x10, x11, gt
     58c:	subs	x13, x11, #0x5
     590:	b.hs	0x5a0
     594:	mov	x0, #0x0
     598:	tbz	x10, #0x3f, 0x7b0
     59c:	b	0x7e4
     5a0:	mov	x14, #0xe0e0e0e0e0e0e0e
     5a4:	orr	x14, x14, #0x2222222222222222
     5a8:	ldr	w16, [x12]
     5ac:	cmn	x10, #0x1
     5b0:	csel	x15, x8, x15, gt
     5b4:	ldur	w15, [x15, #0x1]
     5b8:	mov	w17, #0x4547
     5bc:	movk	w17, #0x2054, lsl #16
     5c0:	cmp	w16, w17
     5c4:	mov	w16, #0x5445
     5c8:	movk	w16, #0x2f20, lsl #16
     5cc:	ccmp	w15, w16, #0x0, eq
     5d0:	b.eq	0x6b0
     5d4:	sub	x16, x11, #0x14
     5d8:	cmn	x16, #0x11
     5dc:	b.hi	0x828
     5e0:	mov	x15, #0x0
     5e4:	mov	x17, #-0x101010101010102
     5e8:	movk	x17, #0xfeff
     5ec:	mov	x0, #-0x101010101010102
     5f0:	add	x1, x12, x15
     5f4:	ldp	x2, x1, [x1]
     5f8:	eor	x3, x1, x14
     5fc:	eor	x4, x2, x14
     600:	adds	x4, x4, x17
     604:	bic	x2, x4, x2
     608:	adc	x3, x3, x0
     60c:	bic	x3, x3, x1
     610:	and	x1, x2, #0x8080808080808080
     614:	and	x2, x3, #0x8080808080808080
     618:	orr	x3, x1, x2
     61c:	cbnz	x3, 0x664
     620:	add	x15, x15, #0x10
     624:	cmp	x15, x13
     628:	csel	x1, xzr, x16, hi
     62c:	sub	x16, x16, #0x10
     630:	cmp	x1, #0xf
     634:	b.hi	0x5f0
     638:	cbz	x1, 0x594
     63c:	ldrb	w14, [x12, x15]
     640:	cmp	w14, #0x2e
     644:	b.eq	0x684
     648:	mov	x0, #0x0
     64c:	cmp	x13, x15
     650:	b.eq	0x7ac
     654:	add	x15, x15, #0x1
     658:	cmp	x15, x13
     65c:	b.ls	0x63c
     660:	b	0x7ac
     664:	rbit	x13, x1
     668:	clz	x13, x13
     66c:	rbit	x14, x2
     670:	clz	x14, x14
     674:	add	x14, x14, #0x40
     678:	cmp	x1, #0x0
     67c:	csel	x13, x13, x14, ne
     680:	add	x15, x15, x13, lsr #3
     684:	add	x12, x12, x15
     688:	ldur	w12, [x12, #0x1]
     68c:	add	x13, x15, #0x5
     690:	cmp	x13, x11
     694:	mov	w11, #0x736a
     698:	movk	w11, #0x6e6f, lsl #16
     69c:	ccmp	w12, w11, #0x0, eq
     6a0:	mov	w11, #0x2
     6a4:	csel	x0, x11, xzr, eq
     6a8:	tbz	x10, #0x3f, 0x7b0
     6ac:	b	0x7e4
     6b0:	sub	x15, x11, #0x4
     6b4:	sub	x16, x11, #0x8
     6b8:	cmp	x15, #0x5
     6bc:	csel	x16, xzr, x16, lo
     6c0:	cmp	x16, #0x10
     6c4:	b.lo	0x830
     6c8:	mov	x16, #0x0
     6cc:	sub	x17, x11, #0x18
     6d0:	add	x0, x12, #0x5
     6d4:	mov	x1, #-0x101010101010102
     6d8:	movk	x1, #0xfeff
     6dc:	mov	x2, #-0x101010101010102
     6e0:	add	x3, x0, x16
     6e4:	ldp	x4, x3, [x3]
     6e8:	eor	x5, x3, x14
     6ec:	eor	x6, x4, x14
     6f0:	adds	x6, x6, x1
     6f4:	bic	x4, x6, x4
     6f8:	adc	x5, x5, x2
     6fc:	bic	x5, x5, x3
     700:	and	x3, x4, #0x8080808080808080
     704:	and	x4, x5, #0x8080808080808080
     708:	orr	x5, x3, x4
     70c:	cbnz	x5, 0x758
     710:	add	x3, x16, #0x15
     714:	cmp	x3, x15
     718:	csel	x3, xzr, x17, hi
     71c:	add	x16, x16, #0x10
     720:	sub	x17, x17, #0x10
     724:	cmp	x3, #0xf
     728:	b.hi	0x6e0
     72c:	cbz	x3, 0x5d4
     730:	add	x16, x16, #0x5
     734:	ldrb	w17, [x12, x16]
     738:	cmp	w17, #0x2e
     73c:	b.eq	0x77c
     740:	cmp	x15, x16
     744:	b.eq	0x5d4
     748:	add	x16, x16, #0x1
     74c:	cmp	x16, x15
     750:	b.ls	0x734
     754:	b	0x5d4
     758:	rbit	x15, x3
     75c:	clz	x15, x15
     760:	rbit	x17, x4
     764:	clz	x17, x17
     768:	add	x17, x17, #0x40
     76c:	cmp	x3, #0x0
     770:	csel	x15, x15, x17, ne
     774:	add	x15, x16, x15, lsr #3
     778:	add	x16, x15, #0x5
     77c:	add	x15, x16, #0x4
     780:	cmp	x15, x11
     784:	b.ne	0x5d4
     788:	add	x15, x12, x16
     78c:	ldurh	w16, [x15, #0x1]
     790:	ldrh	w15, [x15, #0x2]
     794:	mov	w17, #0x7874
     798:	cmp	w16, w17
     79c:	mov	w16, #0x7478
     7a0:	ccmp	w15, w16, #0x0, eq
     7a4:	b.ne	0x5d4
     7a8:	mov	w0, #0x1
     7ac:	tbnz	x10, #0x3f, 0x7e4
     7b0:	and	x10, x9, #0xfffffffffffffffe
     7b4:	tst	x9, #0x1
     7b8:	csel	x8, x8, x10, eq
     7bc:	cbz	x8, 0x7e4
     7c0:	and	x8, x8, #0xfffffffffffffff8
     7c4:	ldr	x9, [x8, #-0x8]!
     7c8:	strb	wzr, [sp, #0x8]
     7cc:	cbz	x9, 0x7e4
     7d0:	ldrb	w10, [sp, #0x8]
     7d4:	tbz	w10, #0x0, 0x7f4
     7d8:	subs	x9, x9, #0x1
     7dc:	str	x9, [x8]
     7e0:	b.eq	0x804
     7e4:	ldp	x29, x30, [sp, #0x30]
     7e8:	ldp	x20, x19, [sp, #0x20]
     7ec:	add	sp, sp, #0x40
     7f0:	ret
     7f4:	mov	x9, #-0x1
     7f8:	.long	0xf8290109
     7fc:	cmp	x9, #0x1
     800:	b.ne	0x7e4
     804:	mov	x19, x0
     808:	mov	x0, x8
     80c:	mov	w1, #0x8
     810:	bl	_roc_dealloc
     814:	mov	x0, x19
     818:	ldp	x29, x30, [sp, #0x30]
     81c:	ldp	x20, x19, [sp, #0x20]
     820:	add	sp, sp, #0x40
     824:	ret
     828:	mov	x15, #0x0
     82c:	b	0x63c
     830:	cbz	x16, 0x5d4
     834:	mov	w16, #0x5
     838:	b	0x734
_roc__proc_7:
     83c:	b	_roc__proc_6
