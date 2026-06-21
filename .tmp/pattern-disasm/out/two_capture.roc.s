Archive : .tmp/pattern-disasm/out/two_capture.a
.tmp/pattern-disasm/out/two_capture.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_10:
       0:	sub	sp, sp, #0x40
       4:	stp	x20, x19, [sp, #0x20]
       8:	stp	x29, x30, [sp, #0x30]
       c:	ldp	x8, x9, [x1]
      10:	ldr	x10, [x1, #0x10]
      14:	stp	x8, x9, [sp, #0x8]
      18:	str	x10, [sp, #0x18]
      1c:	ubfx	x11, x10, #56, #7
      20:	cmn	x10, #0x1
      24:	add	x14, sp, #0x8
      28:	csel	x12, x8, x14, gt
      2c:	csel	x11, x10, x11, gt
      30:	subs	x13, x11, #0x3
      34:	b.lo	0x264
      38:	ldrh	w15, [x12]
      3c:	cmn	x10, #0x1
      40:	csel	x14, x8, x14, gt
      44:	ldurh	w14, [x14, #0x1]
      48:	mov	w16, #0x6f66
      4c:	cmp	w15, w16
      50:	mov	w15, #0x6f6f
      54:	ccmp	w14, w15, #0x0, eq
      58:	b.ne	0x264
      5c:	sub	x15, x11, #0x5
      60:	cmp	x13, #0x3
      64:	csel	x14, xzr, x15, lo
      68:	cmp	x14, #0x10
      6c:	b.lo	0x250
      70:	mov	x14, #0x0
      74:	sub	x16, x11, #0x15
      78:	add	x17, x12, #0x3
      7c:	mov	x1, #0x2222222222222222
      80:	orr	x1, x1, #0x4040404040404040
      84:	mov	x2, #-0x101010101010102
      88:	movk	x2, #0xfeff
      8c:	mov	x3, #-0x101010101010102
      90:	add	x4, x17, x14
      94:	ldp	x5, x4, [x4]
      98:	eor	x6, x4, x1
      9c:	eor	x7, x5, x1
      a0:	adds	x7, x7, x2
      a4:	bic	x5, x7, x5
      a8:	adc	x6, x6, x3
      ac:	bic	x6, x6, x4
      b0:	and	x4, x5, #0x8080808080808080
      b4:	and	x5, x6, #0x8080808080808080
      b8:	orr	x6, x4, x5
      bc:	cbnz	x6, 0x10c
      c0:	add	x4, x14, #0x13
      c4:	cmp	x4, x13
      c8:	csel	x4, xzr, x16, hi
      cc:	add	x14, x14, #0x10
      d0:	sub	x16, x16, #0x10
      d4:	cmp	x4, #0xf
      d8:	b.hi	0x90
      dc:	cbz	x4, 0x264
      e0:	add	x14, x14, #0x3
      e4:	ldrb	w16, [x12, x14]
      e8:	cmp	w16, #0x62
      ec:	b.eq	0x130
      f0:	mov	x19, #0x0
      f4:	cmp	x13, x14
      f8:	b.eq	0x268
      fc:	add	x14, x14, #0x1
     100:	cmp	x14, x13
     104:	b.ls	0xe4
     108:	b	0x268
     10c:	rbit	x16, x4
     110:	clz	x16, x16
     114:	rbit	x17, x5
     118:	clz	x17, x17
     11c:	add	x17, x17, #0x40
     120:	cmp	x4, #0x0
     124:	csel	x16, x16, x17, ne
     128:	add	x14, x14, x16, lsr #3
     12c:	add	x14, x14, #0x3
     130:	add	x16, x12, x14
     134:	ldurh	w16, [x16, #0x1]
     138:	mov	w17, #0x7a61
     13c:	cmp	w16, w17
     140:	b.ne	0x264
     144:	add	x16, x14, #0x3
     148:	sub	x15, x15, x14
     14c:	cmp	x16, x13
     150:	csel	x17, xzr, x15, hi
     154:	cmp	x17, #0x10
     158:	b.lo	0x25c
     15c:	sub	x15, x11, x14
     160:	sub	x17, x15, #0x15
     164:	add	x1, x12, #0x3
     168:	mov	x2, #0x6666666666666666
     16c:	eor	x2, x2, #0x303030303030303
     170:	mov	x3, #-0x101010101010102
     174:	movk	x3, #0xfeff
     178:	mov	x4, #-0x101010101010102
     17c:	mov	x15, x14
     180:	add	x5, x1, x15
     184:	ldp	x6, x5, [x5]
     188:	eor	x7, x5, x2
     18c:	eor	x19, x6, x2
     190:	adds	x19, x19, x3
     194:	bic	x6, x19, x6
     198:	adc	x7, x7, x4
     19c:	bic	x7, x7, x5
     1a0:	and	x5, x6, #0x8080808080808080
     1a4:	and	x6, x7, #0x8080808080808080
     1a8:	orr	x7, x5, x6
     1ac:	cbnz	x7, 0x1fc
     1b0:	add	x5, x15, #0x13
     1b4:	cmp	x5, x13
     1b8:	csel	x5, xzr, x17, hi
     1bc:	add	x15, x15, #0x10
     1c0:	sub	x17, x17, #0x10
     1c4:	cmp	x5, #0xf
     1c8:	b.hi	0x180
     1cc:	cbz	x5, 0x264
     1d0:	add	x15, x15, #0x3
     1d4:	ldrb	w17, [x12, x15]
     1d8:	cmp	w17, #0x65
     1dc:	b.eq	0x220
     1e0:	mov	x19, #0x0
     1e4:	cmp	x13, x15
     1e8:	b.eq	0x268
     1ec:	add	x15, x15, #0x1
     1f0:	cmp	x15, x13
     1f4:	b.ls	0x1d4
     1f8:	b	0x268
     1fc:	rbit	x13, x5
     200:	clz	x13, x13
     204:	rbit	x17, x6
     208:	clz	x17, x17
     20c:	add	x17, x17, #0x40
     210:	cmp	x5, #0x0
     214:	csel	x13, x13, x17, ne
     218:	add	x13, x15, x13, lsr #3
     21c:	add	x15, x13, #0x3
     220:	add	x12, x12, x15
     224:	ldurh	w12, [x12, #0x1]
     228:	add	x13, x15, #0x3
     22c:	mov	w17, #0x6374
     230:	sub	x15, x15, x16
     234:	add	x15, x15, x15, lsl #1
     238:	add	x14, x14, x15
     23c:	add	x14, x14, #0x8
     240:	cmp	w12, w17
     244:	ccmp	x13, x11, #0x0, eq
     248:	csel	x19, x14, xzr, eq
     24c:	b	0x268
     250:	cbz	x14, 0x264
     254:	mov	w14, #0x3
     258:	b	0xe4
     25c:	mov	x15, x16
     260:	cbnz	x17, 0x1d4
     264:	mov	x19, #0x0
     268:	tbnz	x10, #0x3f, 0x2b4
     26c:	and	x10, x9, #0xfffffffffffffffe
     270:	tst	x9, #0x1
     274:	csel	x8, x8, x10, eq
     278:	cbz	x8, 0x2b4
     27c:	and	x8, x8, #0xfffffffffffffff8
     280:	ldr	x9, [x8, #-0x8]!
     284:	strb	wzr, [sp, #0x8]
     288:	cbz	x9, 0x2b4
     28c:	ldrb	w10, [sp, #0x8]
     290:	tbz	w10, #0x0, 0x2c8
     294:	subs	x9, x9, #0x1
     298:	str	x9, [x8]
     29c:	b.ne	0x2b4
     2a0:	mov	x20, x0
     2a4:	mov	x0, x8
     2a8:	mov	w1, #0x8
     2ac:	bl	_roc_dealloc
     2b0:	mov	x0, x20
     2b4:	str	x19, [x0]
     2b8:	ldp	x29, x30, [sp, #0x30]
     2bc:	ldp	x20, x19, [sp, #0x20]
     2c0:	add	sp, sp, #0x40
     2c4:	ret
     2c8:	mov	x9, #-0x1
     2cc:	.long	0xf8290109
     2d0:	cmp	x9, #0x1
     2d4:	b.eq	0x2a0
     2d8:	b	0x2b4
_roc__proc_12:
     2dc:	ldr	q0, [x1]
     2e0:	str	q0, [sp, #-0x20]!
     2e4:	ldr	x8, [x1, #0x10]
     2e8:	str	x8, [sp, #0x10]
     2ec:	tbz	x8, #0x3f, 0x310
     2f0:	ubfx	x8, x8, #56, #7
     2f4:	mov	x9, sp
     2f8:	subs	x10, x8, #0x3
     2fc:	b.hs	0x31c
     300:	mov	x13, #0x0
     304:	str	x13, [x0]
     308:	add	sp, sp, #0x20
     30c:	ret
     310:	ldr	x9, [sp]
     314:	subs	x10, x8, #0x3
     318:	b.lo	0x300
     31c:	ldrh	w11, [x9]
     320:	ldurh	w12, [x9, #0x1]
     324:	mov	w13, #0x6f66
     328:	cmp	w11, w13
     32c:	mov	w11, #0x6f6f
     330:	ccmp	w12, w11, #0x0, eq
     334:	b.ne	0x548
     338:	sub	x11, x8, #0x5
     33c:	cmp	x10, #0x3
     340:	csel	x11, xzr, x11, lo
     344:	cmp	x11, #0x10
     348:	b.lo	0x534
     34c:	mov	x11, #0x0
     350:	sub	x12, x8, #0x15
     354:	add	x13, x9, #0x3
     358:	mov	x14, #0x2222222222222222
     35c:	orr	x14, x14, #0x4040404040404040
     360:	mov	x15, #-0x101010101010102
     364:	movk	x15, #0xfeff
     368:	mov	x16, #-0x101010101010102
     36c:	add	x17, x13, x11
     370:	ldp	x1, x17, [x17]
     374:	eor	x2, x17, x14
     378:	eor	x3, x1, x14
     37c:	adds	x3, x3, x15
     380:	bic	x1, x3, x1
     384:	adc	x2, x2, x16
     388:	bic	x2, x2, x17
     38c:	and	x17, x1, #0x8080808080808080
     390:	and	x1, x2, #0x8080808080808080
     394:	orr	x2, x17, x1
     398:	cbnz	x2, 0x3e8
     39c:	add	x17, x11, #0x13
     3a0:	cmp	x17, x10
     3a4:	csel	x17, xzr, x12, hi
     3a8:	add	x11, x11, #0x10
     3ac:	sub	x12, x12, #0x10
     3b0:	cmp	x17, #0xf
     3b4:	b.hi	0x36c
     3b8:	cbz	x17, 0x548
     3bc:	add	x11, x11, #0x3
     3c0:	ldrb	w12, [x9, x11]
     3c4:	cmp	w12, #0x62
     3c8:	b.eq	0x40c
     3cc:	mov	x13, #0x0
     3d0:	cmp	x10, x11
     3d4:	b.eq	0x304
     3d8:	add	x11, x11, #0x1
     3dc:	cmp	x11, x10
     3e0:	b.ls	0x3c0
     3e4:	b	0x304
     3e8:	rbit	x12, x17
     3ec:	clz	x12, x12
     3f0:	rbit	x13, x1
     3f4:	clz	x13, x13
     3f8:	add	x13, x13, #0x40
     3fc:	cmp	x17, #0x0
     400:	csel	x12, x12, x13, ne
     404:	add	x11, x11, x12, lsr #3
     408:	add	x11, x11, #0x3
     40c:	add	x12, x9, x11
     410:	ldurh	w12, [x12, #0x1]
     414:	mov	w13, #0x7a61
     418:	cmp	w12, w13
     41c:	b.ne	0x548
     420:	add	x12, x11, #0x3
     424:	sub	x13, x8, x11
     428:	sub	x14, x13, #0x5
     42c:	cmp	x12, x10
     430:	csel	x15, xzr, x14, hi
     434:	cmp	x15, #0x10
     438:	b.lo	0x540
     43c:	sub	x14, x13, #0x15
     440:	add	x15, x9, #0x3
     444:	mov	x16, #0x6666666666666666
     448:	eor	x16, x16, #0x303030303030303
     44c:	mov	x17, #-0x101010101010102
     450:	movk	x17, #0xfeff
     454:	mov	x1, #-0x101010101010102
     458:	mov	x13, x11
     45c:	add	x2, x15, x13
     460:	ldp	x3, x2, [x2]
     464:	eor	x4, x2, x16
     468:	eor	x5, x3, x16
     46c:	adds	x5, x5, x17
     470:	bic	x3, x5, x3
     474:	adc	x4, x4, x1
     478:	bic	x4, x4, x2
     47c:	and	x2, x3, #0x8080808080808080
     480:	and	x3, x4, #0x8080808080808080
     484:	orr	x4, x2, x3
     488:	cbnz	x4, 0x4d8
     48c:	add	x2, x13, #0x13
     490:	cmp	x2, x10
     494:	csel	x2, xzr, x14, hi
     498:	add	x13, x13, #0x10
     49c:	sub	x14, x14, #0x10
     4a0:	cmp	x2, #0xf
     4a4:	b.hi	0x45c
     4a8:	cbz	x2, 0x548
     4ac:	add	x14, x13, #0x3
     4b0:	ldrb	w13, [x9, x14]
     4b4:	cmp	w13, #0x65
     4b8:	b.eq	0x4fc
     4bc:	mov	x13, #0x0
     4c0:	cmp	x10, x14
     4c4:	b.eq	0x304
     4c8:	add	x14, x14, #0x1
     4cc:	cmp	x14, x10
     4d0:	b.ls	0x4b0
     4d4:	b	0x304
     4d8:	rbit	x10, x2
     4dc:	clz	x10, x10
     4e0:	rbit	x14, x3
     4e4:	clz	x14, x14
     4e8:	add	x14, x14, #0x40
     4ec:	cmp	x2, #0x0
     4f0:	csel	x10, x10, x14, ne
     4f4:	add	x10, x13, x10, lsr #3
     4f8:	add	x14, x10, #0x3
     4fc:	add	x9, x9, x14
     500:	ldurh	w9, [x9, #0x1]
     504:	add	x10, x14, #0x3
     508:	mov	w13, #0x6374
     50c:	sub	x12, x14, x12
     510:	add	x12, x12, x12, lsl #1
     514:	add	x11, x11, x12
     518:	add	x11, x11, #0x8
     51c:	cmp	w9, w13
     520:	ccmp	x10, x8, #0x0, eq
     524:	csel	x13, x11, xzr, eq
     528:	str	x13, [x0]
     52c:	add	sp, sp, #0x20
     530:	ret
     534:	cbz	x11, 0x548
     538:	mov	w11, #0x3
     53c:	b	0x3c0
     540:	mov	x14, x12
     544:	cbnz	x15, 0x4b0
     548:	mov	x13, #0x0
     54c:	str	x13, [x0]
     550:	add	sp, sp, #0x20
     554:	ret
_roc__proc_13:
     558:	ldp	x8, x9, [x1]
     55c:	add	x8, x9, x8
     560:	str	x8, [x0]
     564:	ret
_roc__proc_14:
     568:	ldp	x8, x9, [x1]
     56c:	mul	x8, x9, x8
     570:	str	x8, [x0]
     574:	ret
_roc__proc_15:
     578:	ldr	x8, [x1, #0x10]
     57c:	ubfx	x9, x8, #56, #7
     580:	cmp	x8, #0x0
     584:	csel	x8, x9, x8, lt
     588:	str	x8, [x0]
     58c:	ret
_roc_main:
     590:	sub	sp, sp, #0x40
     594:	stp	x20, x19, [sp, #0x20]
     598:	stp	x29, x30, [sp, #0x30]
     59c:	ldp	x8, x9, [x0]
     5a0:	ldr	x10, [x0, #0x10]
     5a4:	stp	x8, x9, [sp, #0x8]
     5a8:	str	x10, [sp, #0x18]
     5ac:	ubfx	x11, x10, #56, #7
     5b0:	cmn	x10, #0x1
     5b4:	add	x14, sp, #0x8
     5b8:	csel	x12, x8, x14, gt
     5bc:	csel	x11, x10, x11, gt
     5c0:	subs	x13, x11, #0x3
     5c4:	b.lo	0x7f4
     5c8:	ldrh	w15, [x12]
     5cc:	cmn	x10, #0x1
     5d0:	csel	x14, x8, x14, gt
     5d4:	ldurh	w14, [x14, #0x1]
     5d8:	mov	w16, #0x6f66
     5dc:	cmp	w15, w16
     5e0:	mov	w15, #0x6f6f
     5e4:	ccmp	w14, w15, #0x0, eq
     5e8:	b.ne	0x7f4
     5ec:	sub	x15, x11, #0x5
     5f0:	cmp	x13, #0x3
     5f4:	csel	x14, xzr, x15, lo
     5f8:	cmp	x14, #0x10
     5fc:	b.lo	0x7e0
     600:	mov	x14, #0x0
     604:	sub	x16, x11, #0x15
     608:	add	x17, x12, #0x3
     60c:	mov	x0, #0x2222222222222222
     610:	orr	x0, x0, #0x4040404040404040
     614:	mov	x1, #-0x101010101010102
     618:	movk	x1, #0xfeff
     61c:	mov	x2, #-0x101010101010102
     620:	add	x3, x17, x14
     624:	ldp	x4, x3, [x3]
     628:	eor	x5, x3, x0
     62c:	eor	x6, x4, x0
     630:	adds	x6, x6, x1
     634:	bic	x4, x6, x4
     638:	adc	x5, x5, x2
     63c:	bic	x5, x5, x3
     640:	and	x3, x4, #0x8080808080808080
     644:	and	x4, x5, #0x8080808080808080
     648:	orr	x5, x3, x4
     64c:	cbnz	x5, 0x69c
     650:	add	x3, x14, #0x13
     654:	cmp	x3, x13
     658:	csel	x3, xzr, x16, hi
     65c:	add	x14, x14, #0x10
     660:	sub	x16, x16, #0x10
     664:	cmp	x3, #0xf
     668:	b.hi	0x620
     66c:	cbz	x3, 0x7f4
     670:	add	x14, x14, #0x3
     674:	ldrb	w16, [x12, x14]
     678:	cmp	w16, #0x62
     67c:	b.eq	0x6c0
     680:	mov	x0, #0x0
     684:	cmp	x13, x14
     688:	b.eq	0x7f8
     68c:	add	x14, x14, #0x1
     690:	cmp	x14, x13
     694:	b.ls	0x674
     698:	b	0x7f8
     69c:	rbit	x16, x3
     6a0:	clz	x16, x16
     6a4:	rbit	x17, x4
     6a8:	clz	x17, x17
     6ac:	add	x17, x17, #0x40
     6b0:	cmp	x3, #0x0
     6b4:	csel	x16, x16, x17, ne
     6b8:	add	x14, x14, x16, lsr #3
     6bc:	add	x14, x14, #0x3
     6c0:	add	x16, x12, x14
     6c4:	ldurh	w16, [x16, #0x1]
     6c8:	mov	w17, #0x7a61
     6cc:	cmp	w16, w17
     6d0:	b.ne	0x7f4
     6d4:	add	x16, x14, #0x3
     6d8:	sub	x15, x15, x14
     6dc:	cmp	x16, x13
     6e0:	csel	x17, xzr, x15, hi
     6e4:	cmp	x17, #0x10
     6e8:	b.lo	0x7ec
     6ec:	sub	x15, x11, x14
     6f0:	sub	x17, x15, #0x15
     6f4:	add	x0, x12, #0x3
     6f8:	mov	x1, #0x6666666666666666
     6fc:	eor	x1, x1, #0x303030303030303
     700:	mov	x2, #-0x101010101010102
     704:	movk	x2, #0xfeff
     708:	mov	x3, #-0x101010101010102
     70c:	mov	x15, x14
     710:	add	x4, x0, x15
     714:	ldp	x5, x4, [x4]
     718:	eor	x6, x4, x1
     71c:	eor	x7, x5, x1
     720:	adds	x7, x7, x2
     724:	bic	x5, x7, x5
     728:	adc	x6, x6, x3
     72c:	bic	x6, x6, x4
     730:	and	x4, x5, #0x8080808080808080
     734:	and	x5, x6, #0x8080808080808080
     738:	orr	x6, x4, x5
     73c:	cbnz	x6, 0x78c
     740:	add	x4, x15, #0x13
     744:	cmp	x4, x13
     748:	csel	x4, xzr, x17, hi
     74c:	add	x15, x15, #0x10
     750:	sub	x17, x17, #0x10
     754:	cmp	x4, #0xf
     758:	b.hi	0x710
     75c:	cbz	x4, 0x7f4
     760:	add	x15, x15, #0x3
     764:	ldrb	w17, [x12, x15]
     768:	cmp	w17, #0x65
     76c:	b.eq	0x7b0
     770:	mov	x0, #0x0
     774:	cmp	x13, x15
     778:	b.eq	0x7f8
     77c:	add	x15, x15, #0x1
     780:	cmp	x15, x13
     784:	b.ls	0x764
     788:	b	0x7f8
     78c:	rbit	x13, x4
     790:	clz	x13, x13
     794:	rbit	x17, x5
     798:	clz	x17, x17
     79c:	add	x17, x17, #0x40
     7a0:	cmp	x4, #0x0
     7a4:	csel	x13, x13, x17, ne
     7a8:	add	x13, x15, x13, lsr #3
     7ac:	add	x15, x13, #0x3
     7b0:	add	x12, x12, x15
     7b4:	ldurh	w12, [x12, #0x1]
     7b8:	add	x13, x15, #0x3
     7bc:	mov	w17, #0x6374
     7c0:	sub	x15, x15, x16
     7c4:	add	x15, x15, x15, lsl #1
     7c8:	add	x14, x14, x15
     7cc:	add	x14, x14, #0x8
     7d0:	cmp	w12, w17
     7d4:	ccmp	x13, x11, #0x0, eq
     7d8:	csel	x0, x14, xzr, eq
     7dc:	b	0x7f8
     7e0:	cbz	x14, 0x7f4
     7e4:	mov	w14, #0x3
     7e8:	b	0x674
     7ec:	mov	x15, x16
     7f0:	cbnz	x17, 0x764
     7f4:	mov	x0, #0x0
     7f8:	tbnz	x10, #0x3f, 0x844
     7fc:	and	x10, x9, #0xfffffffffffffffe
     800:	tst	x9, #0x1
     804:	csel	x8, x8, x10, eq
     808:	cbz	x8, 0x844
     80c:	and	x8, x8, #0xfffffffffffffff8
     810:	ldr	x9, [x8, #-0x8]!
     814:	strb	wzr, [sp, #0x8]
     818:	cbz	x9, 0x844
     81c:	ldrb	w10, [sp, #0x8]
     820:	tbz	w10, #0x0, 0x854
     824:	subs	x9, x9, #0x1
     828:	str	x9, [x8]
     82c:	b.ne	0x844
     830:	mov	x19, x0
     834:	mov	x0, x8
     838:	mov	w1, #0x8
     83c:	bl	_roc_dealloc
     840:	mov	x0, x19
     844:	ldp	x29, x30, [sp, #0x30]
     848:	ldp	x20, x19, [sp, #0x20]
     84c:	add	sp, sp, #0x40
     850:	ret
     854:	mov	x9, #-0x1
     858:	.long	0xf8290109
     85c:	cmp	x9, #0x1
     860:	b.eq	0x830
     864:	b	0x844
_roc__proc_11:
     868:	b	_roc__proc_10
