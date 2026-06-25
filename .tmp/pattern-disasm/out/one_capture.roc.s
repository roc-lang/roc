Archive : .tmp/pattern-disasm/out/one_capture.a
.tmp/pattern-disasm/out/one_capture.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_c:
       0:	sub	sp, sp, #0x40
       4:	stp	x20, x19, [sp, #0x20]
       8:	stp	x29, x30, [sp, #0x30]
       c:	ldp	x8, x9, [x1]
      10:	ldr	x10, [x1, #0x10]
      14:	stp	x8, x9, [sp, #0x8]
      18:	str	x10, [sp, #0x18]
      1c:	ubfx	x11, x10, #56, #7
      20:	cmn	x10, #0x1
      24:	add	x13, sp, #0x8
      28:	csel	x12, x8, x13, gt
      2c:	csel	x11, x10, x11, gt
      30:	cmp	x11, #0x5
      34:	b.lo	0x64
      38:	ldr	w14, [x12]
      3c:	cmn	x10, #0x1
      40:	csel	x13, x8, x13, gt
      44:	ldur	w13, [x13, #0x1]
      48:	mov	w15, #0x4547
      4c:	movk	w15, #0x2054, lsl #16
      50:	cmp	w14, w15
      54:	mov	w14, #0x5445
      58:	movk	w14, #0x2f20, lsl #16
      5c:	ccmp	w13, w14, #0x0, eq
      60:	b.eq	0x6c
      64:	mov	x19, #0x0
      68:	b	0x170
      6c:	sub	x13, x11, #0x4
      70:	sub	x14, x11, #0x8
      74:	cmp	x13, #0x5
      78:	csel	x14, xzr, x14, lo
      7c:	cmp	x14, #0x10
      80:	b.lo	0x1e4
      84:	mov	x14, #0x0
      88:	sub	x15, x11, #0x18
      8c:	add	x16, x12, #0x5
      90:	mov	x17, #0xe0e0e0e0e0e0e0e
      94:	orr	x17, x17, #0x2222222222222222
      98:	mov	x1, #-0x101010101010102
      9c:	movk	x1, #0xfeff
      a0:	mov	x2, #-0x101010101010102
      a4:	add	x3, x16, x14
      a8:	ldp	x4, x3, [x3]
      ac:	eor	x5, x3, x17
      b0:	eor	x6, x4, x17
      b4:	adds	x6, x6, x1
      b8:	bic	x4, x6, x4
      bc:	adc	x5, x5, x2
      c0:	bic	x5, x5, x3
      c4:	and	x3, x4, #0x8080808080808080
      c8:	and	x4, x5, #0x8080808080808080
      cc:	orr	x5, x3, x4
      d0:	cbnz	x5, 0x120
      d4:	add	x3, x14, #0x15
      d8:	cmp	x3, x13
      dc:	csel	x3, xzr, x15, hi
      e0:	add	x14, x14, #0x10
      e4:	sub	x15, x15, #0x10
      e8:	cmp	x3, #0xf
      ec:	b.hi	0xa4
      f0:	cbz	x3, 0x64
      f4:	add	x14, x14, #0x5
      f8:	ldrb	w15, [x12, x14]
      fc:	cmp	w15, #0x2e
     100:	b.eq	0x144
     104:	mov	x19, #0x0
     108:	cmp	x13, x14
     10c:	b.eq	0x170
     110:	add	x14, x14, #0x1
     114:	cmp	x14, x13
     118:	b.ls	0xf8
     11c:	b	0x170
     120:	rbit	x13, x3
     124:	clz	x13, x13
     128:	rbit	x15, x4
     12c:	clz	x15, x15
     130:	add	x15, x15, #0x40
     134:	cmp	x3, #0x0
     138:	csel	x13, x13, x15, ne
     13c:	add	x13, x14, x13, lsr #3
     140:	add	x14, x13, #0x5
     144:	add	x12, x12, x14
     148:	ldurh	w13, [x12, #0x1]
     14c:	ldrh	w12, [x12, #0x2]
     150:	mov	w15, #0x7478
     154:	cmp	w12, w15
     158:	mov	w12, #0x7874
     15c:	ccmp	w13, w12, #0x0, eq
     160:	add	x12, x14, #0x4
     164:	ccmp	x12, x11, #0x0, eq
     168:	add	x11, x14, #0x5
     16c:	csel	x19, x11, xzr, eq
     170:	tbnz	x10, #0x3f, 0x1bc
     174:	and	x10, x9, #0xfffffffffffffffe
     178:	tst	x9, #0x1
     17c:	csel	x8, x8, x10, eq
     180:	cbz	x8, 0x1bc
     184:	and	x8, x8, #0xfffffffffffffff8
     188:	ldr	x9, [x8, #-0x8]!
     18c:	strb	wzr, [sp, #0x8]
     190:	cbz	x9, 0x1bc
     194:	ldrb	w10, [sp, #0x8]
     198:	tbz	w10, #0x0, 0x1d0
     19c:	subs	x9, x9, #0x1
     1a0:	str	x9, [x8]
     1a4:	b.ne	0x1bc
     1a8:	mov	x20, x0
     1ac:	mov	x0, x8
     1b0:	mov	w1, #0x8
     1b4:	bl	_roc_dealloc
     1b8:	mov	x0, x20
     1bc:	str	x19, [x0]
     1c0:	ldp	x29, x30, [sp, #0x30]
     1c4:	ldp	x20, x19, [sp, #0x20]
     1c8:	add	sp, sp, #0x40
     1cc:	ret
     1d0:	mov	x9, #-0x1
     1d4:	.long	0xf8290109
     1d8:	cmp	x9, #0x1
     1dc:	b.eq	0x1a8
     1e0:	b	0x1bc
     1e4:	cbz	x14, 0x64
     1e8:	mov	w14, #0x5
     1ec:	b	0xf8
_roc__proc_e:
     1f0:	ldr	q0, [x1]
     1f4:	str	q0, [sp, #-0x20]!
     1f8:	ldr	x8, [x1, #0x10]
     1fc:	str	x8, [sp, #0x10]
     200:	tbz	x8, #0x3f, 0x224
     204:	ubfx	x8, x8, #56, #7
     208:	mov	x9, sp
     20c:	cmp	x8, #0x5
     210:	b.hs	0x230
     214:	mov	x12, #0x0
     218:	str	x12, [x0]
     21c:	add	sp, sp, #0x20
     220:	ret
     224:	ldr	x9, [sp]
     228:	cmp	x8, #0x5
     22c:	b.lo	0x214
     230:	ldr	w10, [x9]
     234:	ldur	w11, [x9, #0x1]
     238:	mov	w12, #0x4547
     23c:	movk	w12, #0x2054, lsl #16
     240:	cmp	w10, w12
     244:	mov	w10, #0x5445
     248:	movk	w10, #0x2f20, lsl #16
     24c:	ccmp	w11, w10, #0x0, eq
     250:	b.eq	0x264
     254:	mov	x12, #0x0
     258:	str	x12, [x0]
     25c:	add	sp, sp, #0x20
     260:	ret
     264:	sub	x10, x8, #0x4
     268:	sub	x11, x8, #0x8
     26c:	cmp	x10, #0x5
     270:	csel	x11, xzr, x11, lo
     274:	cmp	x11, #0x10
     278:	b.lo	0x374
     27c:	mov	x11, #0x0
     280:	sub	x12, x8, #0x18
     284:	add	x13, x9, #0x5
     288:	mov	x14, #0xe0e0e0e0e0e0e0e
     28c:	orr	x14, x14, #0x2222222222222222
     290:	mov	x15, #-0x101010101010102
     294:	movk	x15, #0xfeff
     298:	mov	x16, #-0x101010101010102
     29c:	add	x17, x13, x11
     2a0:	ldp	x1, x17, [x17]
     2a4:	eor	x2, x17, x14
     2a8:	eor	x3, x1, x14
     2ac:	adds	x3, x3, x15
     2b0:	bic	x1, x3, x1
     2b4:	adc	x2, x2, x16
     2b8:	bic	x2, x2, x17
     2bc:	and	x17, x1, #0x8080808080808080
     2c0:	and	x1, x2, #0x8080808080808080
     2c4:	orr	x2, x17, x1
     2c8:	cbnz	x2, 0x318
     2cc:	add	x17, x11, #0x15
     2d0:	cmp	x17, x10
     2d4:	csel	x17, xzr, x12, hi
     2d8:	add	x11, x11, #0x10
     2dc:	sub	x12, x12, #0x10
     2e0:	cmp	x17, #0xf
     2e4:	b.hi	0x29c
     2e8:	cbz	x17, 0x254
     2ec:	add	x11, x11, #0x5
     2f0:	ldrb	w12, [x9, x11]
     2f4:	cmp	w12, #0x2e
     2f8:	b.eq	0x33c
     2fc:	mov	x12, #0x0
     300:	cmp	x10, x11
     304:	b.eq	0x218
     308:	add	x11, x11, #0x1
     30c:	cmp	x11, x10
     310:	b.ls	0x2f0
     314:	b	0x218
     318:	rbit	x10, x17
     31c:	clz	x10, x10
     320:	rbit	x12, x1
     324:	clz	x12, x12
     328:	add	x12, x12, #0x40
     32c:	cmp	x17, #0x0
     330:	csel	x10, x10, x12, ne
     334:	add	x10, x11, x10, lsr #3
     338:	add	x11, x10, #0x5
     33c:	add	x9, x9, x11
     340:	ldurh	w10, [x9, #0x1]
     344:	ldrh	w9, [x9, #0x2]
     348:	mov	w12, #0x7478
     34c:	cmp	w9, w12
     350:	mov	w9, #0x7874
     354:	ccmp	w10, w9, #0x0, eq
     358:	add	x9, x11, #0x4
     35c:	ccmp	x9, x8, #0x0, eq
     360:	add	x8, x11, #0x5
     364:	csel	x12, x8, xzr, eq
     368:	str	x12, [x0]
     36c:	add	sp, sp, #0x20
     370:	ret
     374:	cbz	x11, 0x254
     378:	mov	w11, #0x5
     37c:	b	0x2f0
_roc__proc_f:
     380:	ldp	x8, x9, [x1]
     384:	add	x8, x9, x8
     388:	str	x8, [x0]
     38c:	ret
_roc__proc_10:
     390:	ldr	x8, [x1, #0x10]
     394:	ubfx	x9, x8, #56, #7
     398:	cmp	x8, #0x0
     39c:	csel	x8, x9, x8, lt
     3a0:	str	x8, [x0]
     3a4:	ret
_roc_main:
     3a8:	sub	sp, sp, #0x40
     3ac:	stp	x20, x19, [sp, #0x20]
     3b0:	stp	x29, x30, [sp, #0x30]
     3b4:	ldp	x8, x9, [x0]
     3b8:	ldr	x10, [x0, #0x10]
     3bc:	stp	x8, x9, [sp, #0x8]
     3c0:	str	x10, [sp, #0x18]
     3c4:	ubfx	x11, x10, #56, #7
     3c8:	cmn	x10, #0x1
     3cc:	add	x13, sp, #0x8
     3d0:	csel	x12, x8, x13, gt
     3d4:	csel	x11, x10, x11, gt
     3d8:	cmp	x11, #0x5
     3dc:	b.lo	0x40c
     3e0:	ldr	w14, [x12]
     3e4:	cmn	x10, #0x1
     3e8:	csel	x13, x8, x13, gt
     3ec:	ldur	w13, [x13, #0x1]
     3f0:	mov	w15, #0x4547
     3f4:	movk	w15, #0x2054, lsl #16
     3f8:	cmp	w14, w15
     3fc:	mov	w14, #0x5445
     400:	movk	w14, #0x2f20, lsl #16
     404:	ccmp	w13, w14, #0x0, eq
     408:	b.eq	0x414
     40c:	mov	x0, #0x0
     410:	b	0x518
     414:	sub	x13, x11, #0x4
     418:	sub	x14, x11, #0x8
     41c:	cmp	x13, #0x5
     420:	csel	x14, xzr, x14, lo
     424:	cmp	x14, #0x10
     428:	b.lo	0x588
     42c:	mov	x14, #0x0
     430:	sub	x15, x11, #0x18
     434:	add	x16, x12, #0x5
     438:	mov	x17, #0xe0e0e0e0e0e0e0e
     43c:	orr	x17, x17, #0x2222222222222222
     440:	mov	x0, #-0x101010101010102
     444:	movk	x0, #0xfeff
     448:	mov	x1, #-0x101010101010102
     44c:	add	x2, x16, x14
     450:	ldp	x3, x2, [x2]
     454:	eor	x4, x2, x17
     458:	eor	x5, x3, x17
     45c:	adds	x5, x5, x0
     460:	bic	x3, x5, x3
     464:	adc	x4, x4, x1
     468:	bic	x4, x4, x2
     46c:	and	x2, x3, #0x8080808080808080
     470:	and	x3, x4, #0x8080808080808080
     474:	orr	x4, x2, x3
     478:	cbnz	x4, 0x4c8
     47c:	add	x2, x14, #0x15
     480:	cmp	x2, x13
     484:	csel	x2, xzr, x15, hi
     488:	add	x14, x14, #0x10
     48c:	sub	x15, x15, #0x10
     490:	cmp	x2, #0xf
     494:	b.hi	0x44c
     498:	cbz	x2, 0x40c
     49c:	add	x14, x14, #0x5
     4a0:	ldrb	w15, [x12, x14]
     4a4:	cmp	w15, #0x2e
     4a8:	b.eq	0x4ec
     4ac:	mov	x0, #0x0
     4b0:	cmp	x13, x14
     4b4:	b.eq	0x518
     4b8:	add	x14, x14, #0x1
     4bc:	cmp	x14, x13
     4c0:	b.ls	0x4a0
     4c4:	b	0x518
     4c8:	rbit	x13, x2
     4cc:	clz	x13, x13
     4d0:	rbit	x15, x3
     4d4:	clz	x15, x15
     4d8:	add	x15, x15, #0x40
     4dc:	cmp	x2, #0x0
     4e0:	csel	x13, x13, x15, ne
     4e4:	add	x13, x14, x13, lsr #3
     4e8:	add	x14, x13, #0x5
     4ec:	add	x12, x12, x14
     4f0:	ldurh	w13, [x12, #0x1]
     4f4:	ldrh	w12, [x12, #0x2]
     4f8:	mov	w15, #0x7478
     4fc:	cmp	w12, w15
     500:	mov	w12, #0x7874
     504:	ccmp	w13, w12, #0x0, eq
     508:	add	x12, x14, #0x4
     50c:	ccmp	x12, x11, #0x0, eq
     510:	add	x11, x14, #0x5
     514:	csel	x0, x11, xzr, eq
     518:	tbnz	x10, #0x3f, 0x564
     51c:	and	x10, x9, #0xfffffffffffffffe
     520:	tst	x9, #0x1
     524:	csel	x8, x8, x10, eq
     528:	cbz	x8, 0x564
     52c:	and	x8, x8, #0xfffffffffffffff8
     530:	ldr	x9, [x8, #-0x8]!
     534:	strb	wzr, [sp, #0x8]
     538:	cbz	x9, 0x564
     53c:	ldrb	w10, [sp, #0x8]
     540:	tbz	w10, #0x0, 0x574
     544:	subs	x9, x9, #0x1
     548:	str	x9, [x8]
     54c:	b.ne	0x564
     550:	mov	x19, x0
     554:	mov	x0, x8
     558:	mov	w1, #0x8
     55c:	bl	_roc_dealloc
     560:	mov	x0, x19
     564:	ldp	x29, x30, [sp, #0x30]
     568:	ldp	x20, x19, [sp, #0x20]
     56c:	add	sp, sp, #0x40
     570:	ret
     574:	mov	x9, #-0x1
     578:	.long	0xf8290109
     57c:	cmp	x9, #0x1
     580:	b.eq	0x550
     584:	b	0x564
     588:	cbz	x14, 0x40c
     58c:	mov	w14, #0x5
     590:	b	0x4a0
_roc__proc_d:
     594:	b	_roc__proc_c
