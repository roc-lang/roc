Archive : .tmp/pattern-disasm/out/branchy.a
.tmp/pattern-disasm/out/branchy.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_11:
       0:	stp	x29, x30, [sp, #-0x10]!
       4:	mov	x29, sp
       8:	sub	sp, sp, #0x20
       c:	ldr	q0, [x1]
      10:	stur	q0, [x29, #-0x20]
      14:	ldr	x9, [x1, #0x10]
      18:	stur	x9, [x29, #-0x10]
      1c:	tbz	x9, #0x3f, 0x2a8
      20:	ubfx	x9, x9, #56, #7
      24:	sub	x8, x29, #0x20
      28:	subs	x13, x9, #0x6
      2c:	b.hs	0x2b4
      30:	sub	x10, sp, #0x10
      34:	mov	sp, x10
      38:	sub	x11, sp, #0x10
      3c:	mov	sp, x11
      40:	sub	x12, sp, #0x10
      44:	mov	sp, x12
      48:	cmp	x9, #0x5
      4c:	b.lo	0x150
      50:	sub	x14, x9, #0x5
      54:	sub	x15, x9, #0x14
      58:	cmn	x15, #0x11
      5c:	b.hi	0x300
      60:	mov	x13, #0x0
      64:	mov	x16, #0xe0e0e0e0e0e0e0e
      68:	orr	x16, x16, #0x2222222222222222
      6c:	mov	x17, #-0x101010101010102
      70:	movk	x17, #0xfeff
      74:	mov	x1, #-0x101010101010102
      78:	add	x2, x8, x13
      7c:	ldp	x3, x2, [x2]
      80:	eor	x4, x2, x16
      84:	eor	x5, x3, x16
      88:	adds	x5, x5, x17
      8c:	bic	x3, x5, x3
      90:	adc	x4, x4, x1
      94:	bic	x4, x4, x2
      98:	and	x2, x3, #0x8080808080808080
      9c:	and	x3, x4, #0x8080808080808080
      a0:	orr	x4, x2, x3
      a4:	cbnz	x4, 0xe8
      a8:	add	x13, x13, #0x10
      ac:	cmp	x13, x14
      b0:	csel	x2, xzr, x15, hi
      b4:	sub	x15, x15, #0x10
      b8:	cmp	x2, #0xf
      bc:	b.hi	0x78
      c0:	cbz	x2, 0x150
      c4:	ldrb	w15, [x8, x13]
      c8:	cmp	w15, #0x2e
      cc:	b.eq	0x108
      d0:	cmp	x14, x13
      d4:	b.eq	0x150
      d8:	add	x13, x13, #0x1
      dc:	cmp	x13, x14
      e0:	b.ls	0xc4
      e4:	b	0x150
      e8:	rbit	x14, x2
      ec:	clz	x14, x14
      f0:	rbit	x15, x3
      f4:	clz	x15, x15
      f8:	add	x15, x15, #0x40
      fc:	cmp	x2, #0x0
     100:	csel	x14, x14, x15, ne
     104:	add	x13, x13, x14, lsr #3
     108:	str	x13, [x12]
     10c:	add	x14, x8, x13
     110:	ldur	w14, [x14, #0x1]
     114:	mov	w15, #0x736a
     118:	movk	w15, #0x6e6f, lsl #16
     11c:	cmp	w14, w15
     120:	b.ne	0x150
     124:	str	xzr, [x10]
     128:	str	x13, [x11]
     12c:	add	x10, x13, #0x5
     130:	str	x10, [x12]
     134:	cmp	x10, x9
     138:	b.ne	0x150
     13c:	add	x11, x13, #0xc8
     140:	str	x11, [x0]
     144:	mov	sp, x29
     148:	ldp	x29, x30, [sp], #0x10
     14c:	ret
     150:	cmp	x9, #0x4
     154:	b.hs	0x16c
     158:	mov	x11, #0x0
     15c:	str	x11, [x0]
     160:	mov	sp, x29
     164:	ldp	x29, x30, [sp], #0x10
     168:	ret
     16c:	mov	x11, #0x0
     170:	subs	x10, x9, #0x7
     174:	b.lo	0x238
     178:	ldr	w12, [x8]
     17c:	mov	w13, #0x6f6c
     180:	movk	w13, #0x3a67, lsl #16
     184:	cmp	w12, w13
     188:	b.ne	0x238
     18c:	sub	x11, x9, #0xa
     190:	cmp	x10, #0x4
     194:	csel	x11, xzr, x11, lo
     198:	cmp	x11, #0x10
     19c:	b.lo	0x524
     1a0:	mov	x11, #0x0
     1a4:	sub	x9, x9, #0x1a
     1a8:	add	x12, x8, #0x4
     1ac:	mov	x13, #0x2222222222222222
     1b0:	orr	x13, x13, #0x3838383838383838
     1b4:	mov	x14, #-0x101010101010102
     1b8:	movk	x14, #0xfeff
     1bc:	mov	x15, #-0x101010101010102
     1c0:	add	x16, x12, x11
     1c4:	ldp	x17, x16, [x16]
     1c8:	eor	x1, x16, x13
     1cc:	eor	x2, x17, x13
     1d0:	adds	x2, x2, x14
     1d4:	bic	x17, x2, x17
     1d8:	adc	x1, x1, x15
     1dc:	bic	x1, x1, x16
     1e0:	and	x16, x17, #0x8080808080808080
     1e4:	and	x17, x1, #0x8080808080808080
     1e8:	orr	x1, x16, x17
     1ec:	cbnz	x1, 0x248
     1f0:	add	x16, x11, #0x14
     1f4:	cmp	x16, x10
     1f8:	csel	x16, xzr, x9, hi
     1fc:	add	x11, x11, #0x10
     200:	sub	x9, x9, #0x10
     204:	cmp	x16, #0xf
     208:	b.hi	0x1c0
     20c:	cbz	x16, 0x158
     210:	add	x9, x11, #0x4
     214:	ldrb	w11, [x8, x9]
     218:	cmp	w11, #0x3a
     21c:	b.eq	0x26c
     220:	mov	x11, #0x0
     224:	cmp	x10, x9
     228:	b.eq	0x238
     22c:	add	x9, x9, #0x1
     230:	cmp	x9, x10
     234:	b.ls	0x214
     238:	str	x11, [x0]
     23c:	mov	sp, x29
     240:	ldp	x29, x30, [sp], #0x10
     244:	ret
     248:	rbit	x9, x16
     24c:	clz	x9, x9
     250:	rbit	x10, x17
     254:	clz	x10, x10
     258:	add	x10, x10, #0x40
     25c:	cmp	x16, #0x0
     260:	csel	x9, x9, x10, ne
     264:	add	x9, x11, x9, lsr #3
     268:	add	x9, x9, #0x4
     26c:	add	x8, x8, x9
     270:	ldur	w9, [x8, #0x1]
     274:	ldur	w8, [x8, #0x3]
     278:	mov	w10, #0x6576
     27c:	movk	w10, #0x3a6c, lsl #16
     280:	cmp	w8, w10
     284:	mov	w8, #0x656c
     288:	movk	w8, #0x6576, lsl #16
     28c:	ccmp	w9, w8, #0x0, eq
     290:	mov	w8, #0x12c
     294:	csel	x11, xzr, x8, ne
     298:	str	x11, [x0]
     29c:	mov	sp, x29
     2a0:	ldp	x29, x30, [sp], #0x10
     2a4:	ret
     2a8:	ldur	x8, [x29, #-0x20]
     2ac:	subs	x13, x9, #0x6
     2b0:	b.lo	0x30
     2b4:	mov	w11, #0x6870
     2b8:	movk	w11, #0x3a61, lsl #16
     2bc:	ldr	w10, [x8]
     2c0:	ldur	w12, [x8, #0x2]
     2c4:	mov	w14, #0x6c61
     2c8:	movk	w14, #0x6870, lsl #16
     2cc:	cmp	w10, w14
     2d0:	ccmp	w12, w11, #0x0, eq
     2d4:	b.eq	0x314
     2d8:	sub	x10, sp, #0x10
     2dc:	mov	sp, x10
     2e0:	sub	x11, sp, #0x10
     2e4:	mov	sp, x11
     2e8:	sub	x12, sp, #0x10
     2ec:	mov	sp, x12
     2f0:	sub	x14, x9, #0x5
     2f4:	sub	x15, x9, #0x14
     2f8:	cmn	x15, #0x11
     2fc:	b.ls	0x60
     300:	mov	x13, #0x0
     304:	ldrb	w15, [x8, x13]
     308:	cmp	w15, #0x2e
     30c:	b.ne	0xd0
     310:	b	0x108
     314:	mov	x10, #0x3838383838383838
     318:	orr	x10, x10, #0x3333333333333333
     31c:	sub	x12, x9, #0xb
     320:	cmp	x13, #0x6
     324:	csel	x12, xzr, x12, lo
     328:	cmp	x12, #0x10
     32c:	b.lo	0x518
     330:	mov	x12, #0x0
     334:	sub	x14, x9, #0x1b
     338:	add	x15, x8, #0x6
     33c:	mov	x16, #-0x101010101010102
     340:	movk	x16, #0xfeff
     344:	mov	x17, #-0x101010101010102
     348:	add	x1, x15, x12
     34c:	ldp	x2, x1, [x1]
     350:	eor	x3, x1, x10
     354:	eor	x4, x2, x10
     358:	adds	x4, x4, x16
     35c:	bic	x2, x4, x2
     360:	adc	x3, x3, x17
     364:	bic	x3, x3, x1
     368:	and	x1, x2, #0x8080808080808080
     36c:	and	x2, x3, #0x8080808080808080
     370:	orr	x3, x1, x2
     374:	cbnz	x3, 0x3c0
     378:	add	x1, x12, #0x16
     37c:	cmp	x1, x13
     380:	csel	x1, xzr, x14, hi
     384:	add	x12, x12, #0x10
     388:	sub	x14, x14, #0x10
     38c:	cmp	x1, #0xf
     390:	b.hi	0x348
     394:	cbz	x1, 0x30
     398:	add	x12, x12, #0x6
     39c:	ldrb	w14, [x8, x12]
     3a0:	cmp	w14, #0x3b
     3a4:	b.eq	0x3e4
     3a8:	cmp	x13, x12
     3ac:	b.eq	0x30
     3b0:	add	x12, x12, #0x1
     3b4:	cmp	x12, x13
     3b8:	b.ls	0x39c
     3bc:	b	0x30
     3c0:	rbit	x13, x1
     3c4:	clz	x13, x13
     3c8:	rbit	x14, x2
     3cc:	clz	x14, x14
     3d0:	add	x14, x14, #0x40
     3d4:	cmp	x1, #0x0
     3d8:	csel	x13, x13, x14, ne
     3dc:	add	x12, x12, x13, lsr #3
     3e0:	add	x12, x12, #0x6
     3e4:	add	x13, x8, x12
     3e8:	ldur	w14, [x13, #0x1]
     3ec:	ldur	w13, [x13, #0x2]
     3f0:	add	w11, w11, #0xbf5
     3f4:	mov	w15, #0x6562
     3f8:	movk	w15, #0x6174, lsl #16
     3fc:	cmp	w14, w15
     400:	ccmp	w13, w11, #0x0, eq
     404:	b.ne	0x30
     408:	add	x13, x12, #0x6
     40c:	sub	x11, x9, #0x4
     410:	sub	x14, x9, x12
     414:	sub	x15, x14, #0x9
     418:	cmp	x13, x11
     41c:	csel	x15, xzr, x15, hi
     420:	cmp	x15, #0x10
     424:	b.lo	0x530
     428:	sub	x13, x14, #0x19
     42c:	add	x14, x8, #0x6
     430:	mov	x15, #-0x101010101010102
     434:	movk	x15, #0xfeff
     438:	mov	x16, #-0x101010101010102
     43c:	add	x17, x14, x12
     440:	ldp	x1, x17, [x17]
     444:	eor	x2, x17, x10
     448:	eor	x3, x1, x10
     44c:	adds	x3, x3, x15
     450:	bic	x1, x3, x1
     454:	adc	x2, x2, x16
     458:	bic	x2, x2, x17
     45c:	and	x17, x1, #0x8080808080808080
     460:	and	x1, x2, #0x8080808080808080
     464:	orr	x2, x17, x1
     468:	cbnz	x2, 0x4b4
     46c:	add	x17, x12, #0x16
     470:	cmp	x17, x11
     474:	csel	x17, xzr, x13, hi
     478:	add	x12, x12, #0x10
     47c:	sub	x13, x13, #0x10
     480:	cmp	x17, #0xf
     484:	b.hi	0x43c
     488:	cbz	x17, 0x30
     48c:	add	x13, x12, #0x6
     490:	ldrb	w10, [x8, x13]
     494:	cmp	w10, #0x3b
     498:	b.eq	0x4d8
     49c:	cmp	x11, x13
     4a0:	b.eq	0x30
     4a4:	add	x13, x13, #0x1
     4a8:	cmp	x13, x11
     4ac:	b.ls	0x490
     4b0:	b	0x30
     4b4:	rbit	x10, x17
     4b8:	clz	x10, x10
     4bc:	rbit	x11, x1
     4c0:	clz	x11, x11
     4c4:	add	x11, x11, #0x40
     4c8:	cmp	x17, #0x0
     4cc:	csel	x10, x10, x11, ne
     4d0:	add	x10, x12, x10, lsr #3
     4d4:	add	x13, x10, #0x6
     4d8:	add	x10, x13, #0x4
     4dc:	cmp	x10, x9
     4e0:	b.ne	0x30
     4e4:	add	x10, x8, x13
     4e8:	ldurh	w11, [x10, #0x1]
     4ec:	ldrh	w10, [x10, #0x2]
     4f0:	mov	w12, #0x6e65
     4f4:	cmp	w11, w12
     4f8:	mov	w11, #0x646e
     4fc:	ccmp	w10, w11, #0x0, eq
     500:	b.ne	0x30
     504:	add	x11, x13, #0x58
     508:	str	x11, [x0]
     50c:	mov	sp, x29
     510:	ldp	x29, x30, [sp], #0x10
     514:	ret
     518:	cbz	x12, 0x2d8
     51c:	mov	w12, #0x6
     520:	b	0x39c
     524:	cbz	x11, 0x238
     528:	mov	w9, #0x4
     52c:	b	0x214
     530:	cbnz	x15, 0x490
     534:	b	0x30
_roc__proc_10:
     538:	sub	sp, sp, #0x60
     53c:	stp	x22, x21, [sp, #0x30]
     540:	stp	x20, x19, [sp, #0x40]
     544:	stp	x29, x30, [sp, #0x50]
     548:	mov	x19, x0
     54c:	str	xzr, [sp, #0x20]
     550:	ldp	x20, x21, [x1]
     554:	ldr	x22, [x1, #0x10]
     558:	stp	x20, x21, [sp]
     55c:	str	x22, [sp, #0x10]
     560:	add	x0, sp, #0x20
     564:	mov	x1, sp
     568:	bl	_roc__proc_11
     56c:	tbnz	x22, #0x3f, 0x5c0
     570:	and	x8, x21, #0xfffffffffffffffe
     574:	tst	x21, #0x1
     578:	csel	x8, x20, x8, eq
     57c:	cbz	x8, 0x5c0
     580:	and	x0, x8, #0xfffffffffffffff8
     584:	ldr	x8, [x0, #-0x8]!
     588:	strb	wzr, [sp, #0x2f]
     58c:	cbz	x8, 0x5c0
     590:	ldrb	w9, [sp, #0x2f]
     594:	tbz	w9, #0x0, 0x5a8
     598:	subs	x8, x8, #0x1
     59c:	str	x8, [x0]
     5a0:	b.eq	0x5b8
     5a4:	b	0x5c0
     5a8:	mov	x8, #-0x1
     5ac:	.long	0xf8280008
     5b0:	cmp	x8, #0x1
     5b4:	b.ne	0x5c0
     5b8:	mov	w1, #0x8
     5bc:	bl	_roc_dealloc
     5c0:	ldr	x8, [sp, #0x20]
     5c4:	str	x8, [x19]
     5c8:	ldp	x29, x30, [sp, #0x50]
     5cc:	ldp	x20, x19, [sp, #0x40]
     5d0:	ldp	x22, x21, [sp, #0x30]
     5d4:	add	sp, sp, #0x60
     5d8:	ret
_roc__proc_12:
     5dc:	ldp	x8, x9, [x1]
     5e0:	add	x8, x9, x8
     5e4:	str	x8, [x0]
     5e8:	ret
_roc__proc_13:
     5ec:	ldr	x8, [x1, #0x10]
     5f0:	ubfx	x9, x8, #56, #7
     5f4:	cmp	x8, #0x0
     5f8:	csel	x8, x9, x8, lt
     5fc:	str	x8, [x0]
     600:	ret
_roc_main:
     604:	sub	sp, sp, #0x60
     608:	stp	x22, x21, [sp, #0x30]
     60c:	stp	x20, x19, [sp, #0x40]
     610:	stp	x29, x30, [sp, #0x50]
     614:	ldp	x19, x20, [x0]
     618:	ldr	x21, [x0, #0x10]
     61c:	str	xzr, [sp, #0x20]
     620:	stp	x19, x20, [sp]
     624:	str	x21, [sp, #0x10]
     628:	add	x0, sp, #0x20
     62c:	mov	x1, sp
     630:	bl	_roc__proc_11
     634:	tbnz	x21, #0x3f, 0x688
     638:	and	x8, x20, #0xfffffffffffffffe
     63c:	tst	x20, #0x1
     640:	csel	x8, x19, x8, eq
     644:	cbz	x8, 0x688
     648:	and	x0, x8, #0xfffffffffffffff8
     64c:	ldr	x8, [x0, #-0x8]!
     650:	strb	wzr, [sp, #0x2f]
     654:	cbz	x8, 0x688
     658:	ldrb	w9, [sp, #0x2f]
     65c:	tbz	w9, #0x0, 0x670
     660:	subs	x8, x8, #0x1
     664:	str	x8, [x0]
     668:	b.eq	0x680
     66c:	b	0x688
     670:	mov	x8, #-0x1
     674:	.long	0xf8280008
     678:	cmp	x8, #0x1
     67c:	b.ne	0x688
     680:	mov	w1, #0x8
     684:	bl	_roc_dealloc
     688:	ldr	x0, [sp, #0x20]
     68c:	ldp	x29, x30, [sp, #0x50]
     690:	ldp	x20, x19, [sp, #0x40]
     694:	ldp	x22, x21, [sp, #0x30]
     698:	add	sp, sp, #0x60
     69c:	ret
_roc__proc_f:
     6a0:	b	_roc__proc_10
