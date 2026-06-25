Archive : .tmp/pattern-disasm/out/width_literals.a
.tmp/pattern-disasm/out/width_literals.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_e:
       0:	sub	sp, sp, #0x40
       4:	stp	x20, x19, [sp, #0x20]
       8:	stp	x29, x30, [sp, #0x30]
       c:	ldp	x8, x9, [x1]
      10:	ldr	x10, [x1, #0x10]
      14:	stp	x8, x9, [sp, #0x8]
      18:	str	x10, [sp, #0x18]
      1c:	ubfx	x11, x10, #56, #7
      20:	cmn	x10, #0x1
      24:	add	x12, sp, #0x8
      28:	csel	x12, x8, x12, gt
      2c:	csel	x11, x10, x11, gt
      30:	subs	x13, x11, #0x2
      34:	b.lo	0x3b0
      38:	ldrh	w14, [x12]
      3c:	mov	w15, #0x6261
      40:	cmp	w14, w15
      44:	b.ne	0x130
      48:	sub	x14, x11, #0x3
      4c:	cmp	x13, #0x2
      50:	csel	x14, xzr, x14, lo
      54:	cmp	x14, #0x10
      58:	b.lo	0x38c
      5c:	mov	x14, #0x0
      60:	sub	x15, x11, #0x13
      64:	add	x16, x12, #0x2
      68:	mov	x17, #0x6060606060606060
      6c:	orr	x17, x17, #0x303030303030303
      70:	mov	x1, #-0x101010101010102
      74:	movk	x1, #0xfeff
      78:	mov	x2, #-0x101010101010102
      7c:	add	x3, x16, x14
      80:	ldp	x4, x3, [x3]
      84:	eor	x5, x3, x17
      88:	eor	x6, x4, x17
      8c:	adds	x6, x6, x1
      90:	bic	x4, x6, x4
      94:	adc	x5, x5, x2
      98:	bic	x5, x5, x3
      9c:	and	x3, x4, #0x8080808080808080
      a0:	and	x4, x5, #0x8080808080808080
      a4:	orr	x5, x3, x4
      a8:	cbnz	x5, 0xf4
      ac:	add	x3, x14, #0x12
      b0:	cmp	x3, x13
      b4:	csel	x3, xzr, x15, hi
      b8:	add	x14, x14, #0x10
      bc:	sub	x15, x15, #0x10
      c0:	cmp	x3, #0xf
      c4:	b.hi	0x7c
      c8:	cbz	x3, 0x130
      cc:	add	x19, x14, #0x2
      d0:	ldrb	w14, [x12, x19]
      d4:	cmp	w14, #0x63
      d8:	b.eq	0x118
      dc:	cmp	x13, x19
      e0:	b.eq	0x130
      e4:	add	x19, x19, #0x1
      e8:	cmp	x19, x13
      ec:	b.ls	0xd0
      f0:	b	0x130
      f4:	rbit	x13, x3
      f8:	clz	x13, x13
      fc:	rbit	x15, x4
     100:	clz	x15, x15
     104:	add	x15, x15, #0x40
     108:	cmp	x3, #0x0
     10c:	csel	x13, x13, x15, ne
     110:	add	x13, x14, x13, lsr #3
     114:	add	x19, x13, #0x2
     118:	add	x13, x12, x19
     11c:	ldrb	w13, [x13, #0x1]
     120:	add	x14, x19, #0x2
     124:	cmp	w13, #0x64
     128:	ccmp	x14, x11, #0x0, eq
     12c:	b.eq	0x3b4
     130:	subs	x14, x11, #0x8
     134:	b.lo	0x3b0
     138:	ldr	x15, [x12]
     13c:	mov	x13, #0x6261
     140:	movk	x13, #0x6463, lsl #16
     144:	movk	x13, #0x6665, lsl #32
     148:	movk	x13, #0x6867, lsl #48
     14c:	cmp	x15, x13
     150:	b.ne	0x258
     154:	sub	x15, x11, #0xf
     158:	cmp	x14, #0x8
     15c:	csel	x15, xzr, x15, lo
     160:	cmp	x15, #0x10
     164:	b.lo	0x398
     168:	mov	x15, #0x0
     16c:	sub	x16, x11, #0x1f
     170:	add	x17, x12, #0x8
     174:	mov	x1, #0x1111111111111111
     178:	eor	x1, x1, #0x7878787878787878
     17c:	mov	x2, #-0x101010101010102
     180:	movk	x2, #0xfeff
     184:	mov	x3, #-0x101010101010102
     188:	add	x4, x17, x15
     18c:	ldp	x5, x4, [x4]
     190:	eor	x6, x4, x1
     194:	eor	x7, x5, x1
     198:	adds	x7, x7, x2
     19c:	bic	x5, x7, x5
     1a0:	adc	x6, x6, x3
     1a4:	bic	x6, x6, x4
     1a8:	and	x4, x5, #0x8080808080808080
     1ac:	and	x5, x6, #0x8080808080808080
     1b0:	orr	x6, x4, x5
     1b4:	cbnz	x6, 0x200
     1b8:	add	x4, x15, #0x18
     1bc:	cmp	x4, x14
     1c0:	csel	x4, xzr, x16, hi
     1c4:	add	x15, x15, #0x10
     1c8:	sub	x16, x16, #0x10
     1cc:	cmp	x4, #0xf
     1d0:	b.hi	0x188
     1d4:	cbz	x4, 0x258
     1d8:	add	x19, x15, #0x8
     1dc:	ldrb	w15, [x12, x19]
     1e0:	cmp	w15, #0x69
     1e4:	b.eq	0x224
     1e8:	cmp	x14, x19
     1ec:	b.eq	0x258
     1f0:	add	x19, x19, #0x1
     1f4:	cmp	x19, x14
     1f8:	b.ls	0x1dc
     1fc:	b	0x258
     200:	rbit	x14, x4
     204:	clz	x14, x14
     208:	rbit	x16, x5
     20c:	clz	x16, x16
     210:	add	x16, x16, #0x40
     214:	cmp	x4, #0x0
     218:	csel	x14, x14, x16, ne
     21c:	add	x14, x15, x14, lsr #3
     220:	add	x19, x14, #0x8
     224:	add	x14, x19, #0x8
     228:	cmp	x14, x11
     22c:	b.ne	0x258
     230:	add	x14, x12, x19
     234:	ldur	w15, [x14, #0x1]
     238:	ldr	w14, [x14, #0x4]
     23c:	mov	w16, #0x6b6a
     240:	movk	w16, #0x6d6c, lsl #16
     244:	cmp	w15, w16
     248:	mov	w15, #0x6e6d
     24c:	movk	w15, #0x706f, lsl #16
     250:	ccmp	w14, w15, #0x0, eq
     254:	b.eq	0x3b4
     258:	subs	x14, x11, #0x10
     25c:	b.lo	0x3b0
     260:	ldp	x15, x16, [x12]
     264:	mov	x17, #0x6a69
     268:	movk	x17, #0x6c6b, lsl #16
     26c:	movk	x17, #0x6e6d, lsl #32
     270:	movk	x17, #0x706f, lsl #48
     274:	cmp	x16, x17
     278:	ccmp	x15, x13, #0x0, eq
     27c:	b.ne	0x3b0
     280:	sub	x13, x11, #0x1f
     284:	cmp	x14, #0x10
     288:	csel	x13, xzr, x13, lo
     28c:	cmp	x13, #0x10
     290:	b.lo	0x3a4
     294:	sub	x15, x11, #0x2f
     298:	mov	w13, #0x10
     29c:	mov	x16, #0x7070707070707070
     2a0:	orr	x16, x16, #0x1111111111111111
     2a4:	mov	x17, #-0x101010101010102
     2a8:	movk	x17, #0xfeff
     2ac:	mov	x1, #-0x101010101010102
     2b0:	add	x2, x12, x13
     2b4:	ldp	x3, x2, [x2]
     2b8:	eor	x4, x2, x16
     2bc:	eor	x5, x3, x16
     2c0:	adds	x5, x5, x17
     2c4:	bic	x3, x5, x3
     2c8:	adc	x4, x4, x1
     2cc:	bic	x4, x4, x2
     2d0:	and	x2, x3, #0x8080808080808080
     2d4:	and	x3, x4, #0x8080808080808080
     2d8:	orr	x4, x2, x3
     2dc:	cbnz	x4, 0x324
     2e0:	add	x13, x13, #0x10
     2e4:	cmp	x13, x14
     2e8:	csel	x2, xzr, x15, hi
     2ec:	sub	x15, x15, #0x10
     2f0:	cmp	x2, #0xf
     2f4:	b.hi	0x2b0
     2f8:	cbz	x2, 0x3b0
     2fc:	ldrb	w15, [x12, x13]
     300:	cmp	w15, #0x71
     304:	b.eq	0x344
     308:	mov	x19, #0x0
     30c:	cmp	x14, x13
     310:	b.eq	0x3b4
     314:	add	x13, x13, #0x1
     318:	cmp	x13, x14
     31c:	b.ls	0x2fc
     320:	b	0x3b4
     324:	rbit	x14, x2
     328:	clz	x14, x14
     32c:	rbit	x15, x3
     330:	clz	x15, x15
     334:	add	x15, x15, #0x40
     338:	cmp	x2, #0x0
     33c:	csel	x14, x14, x15, ne
     340:	add	x13, x13, x14, lsr #3
     344:	add	x12, x12, x13
     348:	ldur	x14, [x12, #0x1]
     34c:	ldr	x12, [x12, #0x8]
     350:	mov	x15, #0x7372
     354:	movk	x15, #0x7574, lsl #16
     358:	movk	x15, #0x7776, lsl #32
     35c:	movk	x15, #0x7978, lsl #48
     360:	add	x16, x13, #0x10
     364:	cmp	x16, x11
     368:	csel	x11, x13, xzr, eq
     36c:	cmp	x14, x15
     370:	mov	x13, #0x7a79
     374:	movk	x13, #0x3130, lsl #16
     378:	movk	x13, #0x3332, lsl #32
     37c:	movk	x13, #0x3534, lsl #48
     380:	ccmp	x12, x13, #0x0, eq
     384:	csel	x19, xzr, x11, ne
     388:	b	0x3b4
     38c:	cbz	x14, 0x130
     390:	mov	w19, #0x2
     394:	b	0xd0
     398:	cbz	x15, 0x258
     39c:	mov	w19, #0x8
     3a0:	b	0x1dc
     3a4:	cbz	x13, 0x3b0
     3a8:	mov	w13, #0x10
     3ac:	b	0x2fc
     3b0:	mov	x19, #0x0
     3b4:	tbnz	x10, #0x3f, 0x400
     3b8:	and	x10, x9, #0xfffffffffffffffe
     3bc:	tst	x9, #0x1
     3c0:	csel	x8, x8, x10, eq
     3c4:	cbz	x8, 0x400
     3c8:	and	x8, x8, #0xfffffffffffffff8
     3cc:	ldr	x9, [x8, #-0x8]!
     3d0:	strb	wzr, [sp, #0x8]
     3d4:	cbz	x9, 0x400
     3d8:	ldrb	w10, [sp, #0x8]
     3dc:	tbz	w10, #0x0, 0x414
     3e0:	subs	x9, x9, #0x1
     3e4:	str	x9, [x8]
     3e8:	b.ne	0x400
     3ec:	mov	x20, x0
     3f0:	mov	x0, x8
     3f4:	mov	w1, #0x8
     3f8:	bl	_roc_dealloc
     3fc:	mov	x0, x20
     400:	str	x19, [x0]
     404:	ldp	x29, x30, [sp, #0x30]
     408:	ldp	x20, x19, [sp, #0x20]
     40c:	add	sp, sp, #0x40
     410:	ret
     414:	mov	x9, #-0x1
     418:	.long	0xf8290109
     41c:	cmp	x9, #0x1
     420:	b.eq	0x3ec
     424:	b	0x400
_roc__proc_10:
     428:	ldr	q0, [x1]
     42c:	str	q0, [sp, #-0x20]!
     430:	ldr	x8, [x1, #0x10]
     434:	str	x8, [sp, #0x10]
     438:	tbz	x8, #0x3f, 0x45c
     43c:	ubfx	x8, x8, #56, #7
     440:	mov	x9, sp
     444:	subs	x10, x8, #0x2
     448:	b.hs	0x468
     44c:	mov	x11, #0x0
     450:	str	x11, [x0]
     454:	add	sp, sp, #0x20
     458:	ret
     45c:	ldr	x9, [sp]
     460:	subs	x10, x8, #0x2
     464:	b.lo	0x44c
     468:	ldrh	w11, [x9]
     46c:	mov	w12, #0x6261
     470:	cmp	w11, w12
     474:	b.ne	0x560
     478:	sub	x11, x8, #0x3
     47c:	cmp	x10, #0x2
     480:	csel	x11, xzr, x11, lo
     484:	cmp	x11, #0x10
     488:	b.lo	0x7c4
     48c:	mov	x11, #0x0
     490:	sub	x12, x8, #0x13
     494:	add	x13, x9, #0x2
     498:	mov	x14, #0x6060606060606060
     49c:	orr	x14, x14, #0x303030303030303
     4a0:	mov	x15, #-0x101010101010102
     4a4:	movk	x15, #0xfeff
     4a8:	mov	x16, #-0x101010101010102
     4ac:	add	x17, x13, x11
     4b0:	ldp	x1, x17, [x17]
     4b4:	eor	x2, x17, x14
     4b8:	eor	x3, x1, x14
     4bc:	adds	x3, x3, x15
     4c0:	bic	x1, x3, x1
     4c4:	adc	x2, x2, x16
     4c8:	bic	x2, x2, x17
     4cc:	and	x17, x1, #0x8080808080808080
     4d0:	and	x1, x2, #0x8080808080808080
     4d4:	orr	x2, x17, x1
     4d8:	cbnz	x2, 0x524
     4dc:	add	x17, x11, #0x12
     4e0:	cmp	x17, x10
     4e4:	csel	x17, xzr, x12, hi
     4e8:	add	x11, x11, #0x10
     4ec:	sub	x12, x12, #0x10
     4f0:	cmp	x17, #0xf
     4f4:	b.hi	0x4ac
     4f8:	cbz	x17, 0x560
     4fc:	add	x11, x11, #0x2
     500:	ldrb	w12, [x9, x11]
     504:	cmp	w12, #0x63
     508:	b.eq	0x548
     50c:	cmp	x10, x11
     510:	b.eq	0x560
     514:	add	x11, x11, #0x1
     518:	cmp	x11, x10
     51c:	b.ls	0x500
     520:	b	0x560
     524:	rbit	x10, x17
     528:	clz	x10, x10
     52c:	rbit	x12, x1
     530:	clz	x12, x12
     534:	add	x12, x12, #0x40
     538:	cmp	x17, #0x0
     53c:	csel	x10, x10, x12, ne
     540:	add	x10, x11, x10, lsr #3
     544:	add	x11, x10, #0x2
     548:	add	x10, x9, x11
     54c:	ldrb	w10, [x10, #0x1]
     550:	add	x12, x11, #0x2
     554:	cmp	w10, #0x64
     558:	ccmp	x12, x8, #0x0, eq
     55c:	b.eq	0x450
     560:	subs	x12, x8, #0x8
     564:	b.lo	0x7e8
     568:	ldr	x11, [x9]
     56c:	mov	x10, #0x6261
     570:	movk	x10, #0x6463, lsl #16
     574:	movk	x10, #0x6665, lsl #32
     578:	movk	x10, #0x6867, lsl #48
     57c:	cmp	x11, x10
     580:	b.ne	0x688
     584:	sub	x11, x8, #0xf
     588:	cmp	x12, #0x8
     58c:	csel	x11, xzr, x11, lo
     590:	cmp	x11, #0x10
     594:	b.lo	0x7d0
     598:	mov	x11, #0x0
     59c:	sub	x13, x8, #0x1f
     5a0:	add	x14, x9, #0x8
     5a4:	mov	x15, #0x1111111111111111
     5a8:	eor	x15, x15, #0x7878787878787878
     5ac:	mov	x16, #-0x101010101010102
     5b0:	movk	x16, #0xfeff
     5b4:	mov	x17, #-0x101010101010102
     5b8:	add	x1, x14, x11
     5bc:	ldp	x2, x1, [x1]
     5c0:	eor	x3, x1, x15
     5c4:	eor	x4, x2, x15
     5c8:	adds	x4, x4, x16
     5cc:	bic	x2, x4, x2
     5d0:	adc	x3, x3, x17
     5d4:	bic	x3, x3, x1
     5d8:	and	x1, x2, #0x8080808080808080
     5dc:	and	x2, x3, #0x8080808080808080
     5e0:	orr	x3, x1, x2
     5e4:	cbnz	x3, 0x630
     5e8:	add	x1, x11, #0x18
     5ec:	cmp	x1, x12
     5f0:	csel	x1, xzr, x13, hi
     5f4:	add	x11, x11, #0x10
     5f8:	sub	x13, x13, #0x10
     5fc:	cmp	x1, #0xf
     600:	b.hi	0x5b8
     604:	cbz	x1, 0x688
     608:	add	x11, x11, #0x8
     60c:	ldrb	w13, [x9, x11]
     610:	cmp	w13, #0x69
     614:	b.eq	0x654
     618:	cmp	x12, x11
     61c:	b.eq	0x688
     620:	add	x11, x11, #0x1
     624:	cmp	x11, x12
     628:	b.ls	0x60c
     62c:	b	0x688
     630:	rbit	x12, x1
     634:	clz	x12, x12
     638:	rbit	x13, x2
     63c:	clz	x13, x13
     640:	add	x13, x13, #0x40
     644:	cmp	x1, #0x0
     648:	csel	x12, x12, x13, ne
     64c:	add	x11, x11, x12, lsr #3
     650:	add	x11, x11, #0x8
     654:	add	x12, x11, #0x8
     658:	cmp	x12, x8
     65c:	b.ne	0x688
     660:	add	x12, x9, x11
     664:	ldur	w13, [x12, #0x1]
     668:	ldr	w12, [x12, #0x4]
     66c:	mov	w14, #0x6b6a
     670:	movk	w14, #0x6d6c, lsl #16
     674:	cmp	w13, w14
     678:	mov	w13, #0x6e6d
     67c:	movk	w13, #0x706f, lsl #16
     680:	ccmp	w12, w13, #0x0, eq
     684:	b.eq	0x450
     688:	subs	x12, x8, #0x10
     68c:	b.lo	0x7e8
     690:	ldp	x11, x13, [x9]
     694:	mov	x14, #0x6a69
     698:	movk	x14, #0x6c6b, lsl #16
     69c:	movk	x14, #0x6e6d, lsl #32
     6a0:	movk	x14, #0x706f, lsl #48
     6a4:	cmp	x13, x14
     6a8:	ccmp	x11, x10, #0x0, eq
     6ac:	b.ne	0x7e8
     6b0:	sub	x10, x8, #0x1f
     6b4:	cmp	x12, #0x10
     6b8:	csel	x10, xzr, x10, lo
     6bc:	cmp	x10, #0x10
     6c0:	b.lo	0x7dc
     6c4:	sub	x11, x8, #0x2f
     6c8:	mov	w10, #0x10
     6cc:	mov	x13, #0x7070707070707070
     6d0:	orr	x13, x13, #0x1111111111111111
     6d4:	mov	x14, #-0x101010101010102
     6d8:	movk	x14, #0xfeff
     6dc:	mov	x15, #-0x101010101010102
     6e0:	add	x16, x9, x10
     6e4:	ldp	x17, x16, [x16]
     6e8:	eor	x1, x16, x13
     6ec:	eor	x2, x17, x13
     6f0:	adds	x2, x2, x14
     6f4:	bic	x17, x2, x17
     6f8:	adc	x1, x1, x15
     6fc:	bic	x1, x1, x16
     700:	and	x16, x17, #0x8080808080808080
     704:	and	x17, x1, #0x8080808080808080
     708:	orr	x1, x16, x17
     70c:	cbnz	x1, 0x754
     710:	add	x10, x10, #0x10
     714:	cmp	x10, x12
     718:	csel	x16, xzr, x11, hi
     71c:	sub	x11, x11, #0x10
     720:	cmp	x16, #0xf
     724:	b.hi	0x6e0
     728:	cbz	x16, 0x7e8
     72c:	ldrb	w11, [x9, x10]
     730:	cmp	w11, #0x71
     734:	b.eq	0x774
     738:	mov	x11, #0x0
     73c:	cmp	x12, x10
     740:	b.eq	0x450
     744:	add	x10, x10, #0x1
     748:	cmp	x10, x12
     74c:	b.ls	0x72c
     750:	b	0x450
     754:	rbit	x11, x16
     758:	clz	x11, x11
     75c:	rbit	x12, x17
     760:	clz	x12, x12
     764:	add	x12, x12, #0x40
     768:	cmp	x16, #0x0
     76c:	csel	x11, x11, x12, ne
     770:	add	x10, x10, x11, lsr #3
     774:	add	x9, x9, x10
     778:	ldur	x11, [x9, #0x1]
     77c:	ldr	x9, [x9, #0x8]
     780:	mov	x12, #0x7372
     784:	movk	x12, #0x7574, lsl #16
     788:	movk	x12, #0x7776, lsl #32
     78c:	movk	x12, #0x7978, lsl #48
     790:	add	x13, x10, #0x10
     794:	cmp	x13, x8
     798:	csel	x8, x10, xzr, eq
     79c:	cmp	x11, x12
     7a0:	mov	x10, #0x7a79
     7a4:	movk	x10, #0x3130, lsl #16
     7a8:	movk	x10, #0x3332, lsl #32
     7ac:	movk	x10, #0x3534, lsl #48
     7b0:	ccmp	x9, x10, #0x0, eq
     7b4:	csel	x11, xzr, x8, ne
     7b8:	str	x11, [x0]
     7bc:	add	sp, sp, #0x20
     7c0:	ret
     7c4:	cbz	x11, 0x560
     7c8:	mov	w11, #0x2
     7cc:	b	0x500
     7d0:	cbz	x11, 0x688
     7d4:	mov	w11, #0x8
     7d8:	b	0x60c
     7dc:	cbz	x10, 0x7e8
     7e0:	mov	w10, #0x10
     7e4:	b	0x72c
     7e8:	mov	x11, #0x0
     7ec:	str	x11, [x0]
     7f0:	add	sp, sp, #0x20
     7f4:	ret
_roc__proc_11:
     7f8:	ldp	x8, x9, [x1]
     7fc:	add	x8, x9, x8
     800:	str	x8, [x0]
     804:	ret
_roc__proc_12:
     808:	ldr	x8, [x1, #0x10]
     80c:	ubfx	x9, x8, #56, #7
     810:	cmp	x8, #0x0
     814:	csel	x8, x9, x8, lt
     818:	str	x8, [x0]
     81c:	ret
_roc_main:
     820:	sub	sp, sp, #0x40
     824:	stp	x20, x19, [sp, #0x20]
     828:	stp	x29, x30, [sp, #0x30]
     82c:	ldp	x8, x9, [x0]
     830:	ldr	x10, [x0, #0x10]
     834:	stp	x8, x9, [sp, #0x8]
     838:	str	x10, [sp, #0x18]
     83c:	ubfx	x11, x10, #56, #7
     840:	cmn	x10, #0x1
     844:	add	x12, sp, #0x8
     848:	csel	x12, x8, x12, gt
     84c:	csel	x11, x10, x11, gt
     850:	subs	x13, x11, #0x2
     854:	b.lo	0xbd0
     858:	ldrh	w14, [x12]
     85c:	mov	w15, #0x6261
     860:	cmp	w14, w15
     864:	b.ne	0x950
     868:	sub	x14, x11, #0x3
     86c:	cmp	x13, #0x2
     870:	csel	x14, xzr, x14, lo
     874:	cmp	x14, #0x10
     878:	b.lo	0xbac
     87c:	mov	x14, #0x0
     880:	sub	x15, x11, #0x13
     884:	add	x16, x12, #0x2
     888:	mov	x17, #0x6060606060606060
     88c:	orr	x17, x17, #0x303030303030303
     890:	mov	x0, #-0x101010101010102
     894:	movk	x0, #0xfeff
     898:	mov	x1, #-0x101010101010102
     89c:	add	x2, x16, x14
     8a0:	ldp	x3, x2, [x2]
     8a4:	eor	x4, x2, x17
     8a8:	eor	x5, x3, x17
     8ac:	adds	x5, x5, x0
     8b0:	bic	x3, x5, x3
     8b4:	adc	x4, x4, x1
     8b8:	bic	x4, x4, x2
     8bc:	and	x2, x3, #0x8080808080808080
     8c0:	and	x3, x4, #0x8080808080808080
     8c4:	orr	x4, x2, x3
     8c8:	cbnz	x4, 0x914
     8cc:	add	x2, x14, #0x12
     8d0:	cmp	x2, x13
     8d4:	csel	x2, xzr, x15, hi
     8d8:	add	x14, x14, #0x10
     8dc:	sub	x15, x15, #0x10
     8e0:	cmp	x2, #0xf
     8e4:	b.hi	0x89c
     8e8:	cbz	x2, 0x950
     8ec:	add	x0, x14, #0x2
     8f0:	ldrb	w14, [x12, x0]
     8f4:	cmp	w14, #0x63
     8f8:	b.eq	0x938
     8fc:	cmp	x13, x0
     900:	b.eq	0x950
     904:	add	x0, x0, #0x1
     908:	cmp	x0, x13
     90c:	b.ls	0x8f0
     910:	b	0x950
     914:	rbit	x13, x2
     918:	clz	x13, x13
     91c:	rbit	x15, x3
     920:	clz	x15, x15
     924:	add	x15, x15, #0x40
     928:	cmp	x2, #0x0
     92c:	csel	x13, x13, x15, ne
     930:	add	x13, x14, x13, lsr #3
     934:	add	x0, x13, #0x2
     938:	add	x13, x12, x0
     93c:	ldrb	w13, [x13, #0x1]
     940:	add	x14, x0, #0x2
     944:	cmp	w13, #0x64
     948:	ccmp	x14, x11, #0x0, eq
     94c:	b.eq	0xbd4
     950:	subs	x14, x11, #0x8
     954:	b.lo	0xbd0
     958:	ldr	x15, [x12]
     95c:	mov	x13, #0x6261
     960:	movk	x13, #0x6463, lsl #16
     964:	movk	x13, #0x6665, lsl #32
     968:	movk	x13, #0x6867, lsl #48
     96c:	cmp	x15, x13
     970:	b.ne	0xa78
     974:	sub	x15, x11, #0xf
     978:	cmp	x14, #0x8
     97c:	csel	x15, xzr, x15, lo
     980:	cmp	x15, #0x10
     984:	b.lo	0xbb8
     988:	mov	x15, #0x0
     98c:	sub	x16, x11, #0x1f
     990:	add	x17, x12, #0x8
     994:	mov	x0, #0x1111111111111111
     998:	eor	x0, x0, #0x7878787878787878
     99c:	mov	x1, #-0x101010101010102
     9a0:	movk	x1, #0xfeff
     9a4:	mov	x2, #-0x101010101010102
     9a8:	add	x3, x17, x15
     9ac:	ldp	x4, x3, [x3]
     9b0:	eor	x5, x3, x0
     9b4:	eor	x6, x4, x0
     9b8:	adds	x6, x6, x1
     9bc:	bic	x4, x6, x4
     9c0:	adc	x5, x5, x2
     9c4:	bic	x5, x5, x3
     9c8:	and	x3, x4, #0x8080808080808080
     9cc:	and	x4, x5, #0x8080808080808080
     9d0:	orr	x5, x3, x4
     9d4:	cbnz	x5, 0xa20
     9d8:	add	x3, x15, #0x18
     9dc:	cmp	x3, x14
     9e0:	csel	x3, xzr, x16, hi
     9e4:	add	x15, x15, #0x10
     9e8:	sub	x16, x16, #0x10
     9ec:	cmp	x3, #0xf
     9f0:	b.hi	0x9a8
     9f4:	cbz	x3, 0xa78
     9f8:	add	x0, x15, #0x8
     9fc:	ldrb	w15, [x12, x0]
     a00:	cmp	w15, #0x69
     a04:	b.eq	0xa44
     a08:	cmp	x14, x0
     a0c:	b.eq	0xa78
     a10:	add	x0, x0, #0x1
     a14:	cmp	x0, x14
     a18:	b.ls	0x9fc
     a1c:	b	0xa78
     a20:	rbit	x14, x3
     a24:	clz	x14, x14
     a28:	rbit	x16, x4
     a2c:	clz	x16, x16
     a30:	add	x16, x16, #0x40
     a34:	cmp	x3, #0x0
     a38:	csel	x14, x14, x16, ne
     a3c:	add	x14, x15, x14, lsr #3
     a40:	add	x0, x14, #0x8
     a44:	add	x14, x0, #0x8
     a48:	cmp	x14, x11
     a4c:	b.ne	0xa78
     a50:	add	x14, x12, x0
     a54:	ldur	w15, [x14, #0x1]
     a58:	ldr	w14, [x14, #0x4]
     a5c:	mov	w16, #0x6b6a
     a60:	movk	w16, #0x6d6c, lsl #16
     a64:	cmp	w15, w16
     a68:	mov	w15, #0x6e6d
     a6c:	movk	w15, #0x706f, lsl #16
     a70:	ccmp	w14, w15, #0x0, eq
     a74:	b.eq	0xbd4
     a78:	subs	x14, x11, #0x10
     a7c:	b.lo	0xbd0
     a80:	ldp	x15, x16, [x12]
     a84:	mov	x17, #0x6a69
     a88:	movk	x17, #0x6c6b, lsl #16
     a8c:	movk	x17, #0x6e6d, lsl #32
     a90:	movk	x17, #0x706f, lsl #48
     a94:	cmp	x16, x17
     a98:	ccmp	x15, x13, #0x0, eq
     a9c:	b.ne	0xbd0
     aa0:	sub	x13, x11, #0x1f
     aa4:	cmp	x14, #0x10
     aa8:	csel	x13, xzr, x13, lo
     aac:	cmp	x13, #0x10
     ab0:	b.lo	0xbc4
     ab4:	sub	x15, x11, #0x2f
     ab8:	mov	w13, #0x10
     abc:	mov	x16, #0x7070707070707070
     ac0:	orr	x16, x16, #0x1111111111111111
     ac4:	mov	x17, #-0x101010101010102
     ac8:	movk	x17, #0xfeff
     acc:	mov	x0, #-0x101010101010102
     ad0:	add	x1, x12, x13
     ad4:	ldp	x2, x1, [x1]
     ad8:	eor	x3, x1, x16
     adc:	eor	x4, x2, x16
     ae0:	adds	x4, x4, x17
     ae4:	bic	x2, x4, x2
     ae8:	adc	x3, x3, x0
     aec:	bic	x3, x3, x1
     af0:	and	x1, x2, #0x8080808080808080
     af4:	and	x2, x3, #0x8080808080808080
     af8:	orr	x3, x1, x2
     afc:	cbnz	x3, 0xb44
     b00:	add	x13, x13, #0x10
     b04:	cmp	x13, x14
     b08:	csel	x1, xzr, x15, hi
     b0c:	sub	x15, x15, #0x10
     b10:	cmp	x1, #0xf
     b14:	b.hi	0xad0
     b18:	cbz	x1, 0xbd0
     b1c:	ldrb	w15, [x12, x13]
     b20:	cmp	w15, #0x71
     b24:	b.eq	0xb64
     b28:	mov	x0, #0x0
     b2c:	cmp	x14, x13
     b30:	b.eq	0xbd4
     b34:	add	x13, x13, #0x1
     b38:	cmp	x13, x14
     b3c:	b.ls	0xb1c
     b40:	b	0xbd4
     b44:	rbit	x14, x1
     b48:	clz	x14, x14
     b4c:	rbit	x15, x2
     b50:	clz	x15, x15
     b54:	add	x15, x15, #0x40
     b58:	cmp	x1, #0x0
     b5c:	csel	x14, x14, x15, ne
     b60:	add	x13, x13, x14, lsr #3
     b64:	add	x12, x12, x13
     b68:	ldur	x14, [x12, #0x1]
     b6c:	ldr	x12, [x12, #0x8]
     b70:	mov	x15, #0x7372
     b74:	movk	x15, #0x7574, lsl #16
     b78:	movk	x15, #0x7776, lsl #32
     b7c:	movk	x15, #0x7978, lsl #48
     b80:	add	x16, x13, #0x10
     b84:	cmp	x16, x11
     b88:	csel	x11, x13, xzr, eq
     b8c:	cmp	x14, x15
     b90:	mov	x13, #0x7a79
     b94:	movk	x13, #0x3130, lsl #16
     b98:	movk	x13, #0x3332, lsl #32
     b9c:	movk	x13, #0x3534, lsl #48
     ba0:	ccmp	x12, x13, #0x0, eq
     ba4:	csel	x0, xzr, x11, ne
     ba8:	b	0xbd4
     bac:	cbz	x14, 0x950
     bb0:	mov	w0, #0x2
     bb4:	b	0x8f0
     bb8:	cbz	x15, 0xa78
     bbc:	mov	w0, #0x8
     bc0:	b	0x9fc
     bc4:	cbz	x13, 0xbd0
     bc8:	mov	w13, #0x10
     bcc:	b	0xb1c
     bd0:	mov	x0, #0x0
     bd4:	tbnz	x10, #0x3f, 0xc20
     bd8:	and	x10, x9, #0xfffffffffffffffe
     bdc:	tst	x9, #0x1
     be0:	csel	x8, x8, x10, eq
     be4:	cbz	x8, 0xc20
     be8:	and	x8, x8, #0xfffffffffffffff8
     bec:	ldr	x9, [x8, #-0x8]!
     bf0:	strb	wzr, [sp, #0x8]
     bf4:	cbz	x9, 0xc20
     bf8:	ldrb	w10, [sp, #0x8]
     bfc:	tbz	w10, #0x0, 0xc30
     c00:	subs	x9, x9, #0x1
     c04:	str	x9, [x8]
     c08:	b.ne	0xc20
     c0c:	mov	x19, x0
     c10:	mov	x0, x8
     c14:	mov	w1, #0x8
     c18:	bl	_roc_dealloc
     c1c:	mov	x0, x19
     c20:	ldp	x29, x30, [sp, #0x30]
     c24:	ldp	x20, x19, [sp, #0x20]
     c28:	add	sp, sp, #0x40
     c2c:	ret
     c30:	mov	x9, #-0x1
     c34:	.long	0xf8290109
     c38:	cmp	x9, #0x1
     c3c:	b.eq	0xc0c
     c40:	b	0xc20
_roc__proc_f:
     c44:	b	_roc__proc_e
