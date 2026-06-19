Archive : .tmp/pattern-disasm/out/long_literal.a
.tmp/pattern-disasm/out/long_literal.a(/Users/rtfeldman/Library/Caches/roc/debug-02932267/src/roc_build/roc_app_llvm_arm64mac_speed_17c7dfcc.o):
(__TEXT,__text) section
_roc__proc_c:
       0:	sub	sp, sp, #0x40
       4:	stp	x20, x19, [sp, #0x20]
       8:	stp	x29, x30, [sp, #0x30]
       c:	ldp	x8, x9, [x1]
      10:	ldr	x10, [x1, #0x10]
      14:	stp	x8, x9, [sp, #0x8]
      18:	str	x10, [sp, #0x18]
      1c:	ubfx	x12, x10, #56, #7
      20:	cmn	x10, #0x1
      24:	add	x11, sp, #0x8
      28:	csel	x11, x8, x11, gt
      2c:	csel	x13, x10, x12, gt
      30:	subs	x12, x13, #0x10
      34:	b.lo	0x178
      38:	ldp	x14, x15, [x11]
      3c:	mov	x16, #0x6a69
      40:	movk	x16, #0x6c6b, lsl #16
      44:	movk	x16, #0x6e6d, lsl #32
      48:	movk	x16, #0x706f, lsl #48
      4c:	cmp	x15, x16
      50:	mov	x15, #0x6261
      54:	movk	x15, #0x6463, lsl #16
      58:	movk	x15, #0x6665, lsl #32
      5c:	movk	x15, #0x6867, lsl #48
      60:	ccmp	x14, x15, #0x0, eq
      64:	b.ne	0x178
      68:	sub	x14, x13, #0x1f
      6c:	cmp	x12, #0x10
      70:	csel	x14, xzr, x14, lo
      74:	cmp	x14, #0x10
      78:	b.lo	0x16c
      7c:	sub	x14, x13, #0x2f
      80:	mov	w13, #0x10
      84:	mov	x15, #0x7070707070707070
      88:	orr	x15, x15, #0x1111111111111111
      8c:	mov	x16, #-0x101010101010102
      90:	movk	x16, #0xfeff
      94:	mov	x17, #-0x101010101010102
      98:	add	x1, x11, x13
      9c:	ldp	x2, x1, [x1]
      a0:	eor	x3, x1, x15
      a4:	eor	x4, x2, x15
      a8:	adds	x4, x4, x16
      ac:	bic	x2, x4, x2
      b0:	adc	x3, x3, x17
      b4:	bic	x3, x3, x1
      b8:	and	x1, x2, #0x8080808080808080
      bc:	and	x2, x3, #0x8080808080808080
      c0:	orr	x3, x1, x2
      c4:	cbnz	x3, 0x10c
      c8:	add	x13, x13, #0x10
      cc:	cmp	x13, x12
      d0:	csel	x1, xzr, x14, hi
      d4:	sub	x14, x14, #0x10
      d8:	cmp	x1, #0xf
      dc:	b.hi	0x98
      e0:	cbz	x1, 0x178
      e4:	ldrb	w14, [x11, x13]
      e8:	cmp	w14, #0x71
      ec:	b.eq	0x12c
      f0:	mov	x19, #0x0
      f4:	cmp	x12, x13
      f8:	b.eq	0x17c
      fc:	add	x13, x13, #0x1
     100:	cmp	x13, x12
     104:	b.ls	0xe4
     108:	b	0x17c
     10c:	rbit	x12, x1
     110:	clz	x12, x12
     114:	rbit	x14, x2
     118:	clz	x14, x14
     11c:	add	x14, x14, #0x40
     120:	cmp	x1, #0x0
     124:	csel	x12, x12, x14, ne
     128:	add	x13, x13, x12, lsr #3
     12c:	add	x11, x11, x13
     130:	ldur	x12, [x11, #0x1]
     134:	ldr	x11, [x11, #0x8]
     138:	mov	x14, #0x7a79
     13c:	movk	x14, #0x3130, lsl #16
     140:	movk	x14, #0x3332, lsl #32
     144:	movk	x14, #0x3534, lsl #48
     148:	cmp	x11, x14
     14c:	mov	x11, #0x7372
     150:	movk	x11, #0x7574, lsl #16
     154:	movk	x11, #0x7776, lsl #32
     158:	movk	x11, #0x7978, lsl #48
     15c:	ccmp	x12, x11, #0x0, eq
     160:	add	x11, x13, #0x180
     164:	csel	x19, xzr, x11, ne
     168:	b	0x17c
     16c:	cbz	x14, 0x178
     170:	mov	w13, #0x10
     174:	b	0xe4
     178:	mov	x19, #0x0
     17c:	tbnz	x10, #0x3f, 0x1c8
     180:	and	x10, x9, #0xfffffffffffffffe
     184:	tst	x9, #0x1
     188:	csel	x8, x8, x10, eq
     18c:	cbz	x8, 0x1c8
     190:	and	x8, x8, #0xfffffffffffffff8
     194:	ldr	x9, [x8, #-0x8]!
     198:	strb	wzr, [sp, #0x8]
     19c:	cbz	x9, 0x1c8
     1a0:	ldrb	w10, [sp, #0x8]
     1a4:	tbz	w10, #0x0, 0x1dc
     1a8:	subs	x9, x9, #0x1
     1ac:	str	x9, [x8]
     1b0:	b.ne	0x1c8
     1b4:	mov	x20, x0
     1b8:	mov	x0, x8
     1bc:	mov	w1, #0x8
     1c0:	bl	_roc_dealloc
     1c4:	mov	x0, x20
     1c8:	str	x19, [x0]
     1cc:	ldp	x29, x30, [sp, #0x30]
     1d0:	ldp	x20, x19, [sp, #0x20]
     1d4:	add	sp, sp, #0x40
     1d8:	ret
     1dc:	mov	x9, #-0x1
     1e0:	.long	0xf8290109
     1e4:	cmp	x9, #0x1
     1e8:	b.eq	0x1b4
     1ec:	b	0x1c8
_roc__proc_e:
     1f0:	ldr	q0, [x1]
     1f4:	str	q0, [sp, #-0x20]!
     1f8:	ldr	x9, [x1, #0x10]
     1fc:	str	x9, [sp, #0x10]
     200:	tbz	x9, #0x3f, 0x224
     204:	ubfx	x9, x9, #56, #7
     208:	mov	x8, sp
     20c:	subs	x10, x9, #0x10
     210:	b.hs	0x230
     214:	mov	x11, #0x0
     218:	str	x11, [x0]
     21c:	add	sp, sp, #0x20
     220:	ret
     224:	ldr	x8, [sp]
     228:	subs	x10, x9, #0x10
     22c:	b.lo	0x214
     230:	ldp	x11, x12, [x8]
     234:	mov	x13, #0x6a69
     238:	movk	x13, #0x6c6b, lsl #16
     23c:	movk	x13, #0x6e6d, lsl #32
     240:	movk	x13, #0x706f, lsl #48
     244:	cmp	x12, x13
     248:	mov	x12, #0x6261
     24c:	movk	x12, #0x6463, lsl #16
     250:	movk	x12, #0x6665, lsl #32
     254:	movk	x12, #0x6867, lsl #48
     258:	ccmp	x11, x12, #0x0, eq
     25c:	b.ne	0x304
     260:	sub	x11, x9, #0x1f
     264:	cmp	x10, #0x10
     268:	csel	x11, xzr, x11, lo
     26c:	cmp	x11, #0x10
     270:	b.lo	0x37c
     274:	sub	x11, x9, #0x2f
     278:	mov	w9, #0x10
     27c:	mov	x12, #0x7070707070707070
     280:	orr	x12, x12, #0x1111111111111111
     284:	mov	x13, #-0x101010101010102
     288:	movk	x13, #0xfeff
     28c:	mov	x14, #-0x101010101010102
     290:	add	x15, x8, x9
     294:	ldp	x16, x15, [x15]
     298:	eor	x17, x15, x12
     29c:	eor	x1, x16, x12
     2a0:	adds	x1, x1, x13
     2a4:	bic	x16, x1, x16
     2a8:	adc	x17, x17, x14
     2ac:	bic	x17, x17, x15
     2b0:	and	x15, x16, #0x8080808080808080
     2b4:	and	x16, x17, #0x8080808080808080
     2b8:	orr	x17, x15, x16
     2bc:	cbnz	x17, 0x314
     2c0:	add	x9, x9, #0x10
     2c4:	cmp	x9, x10
     2c8:	csel	x15, xzr, x11, hi
     2cc:	sub	x11, x11, #0x10
     2d0:	cmp	x15, #0xf
     2d4:	b.hi	0x290
     2d8:	cbz	x15, 0x304
     2dc:	ldrb	w11, [x8, x9]
     2e0:	cmp	w11, #0x71
     2e4:	b.eq	0x334
     2e8:	mov	x11, #0x0
     2ec:	cmp	x10, x9
     2f0:	b.eq	0x218
     2f4:	add	x9, x9, #0x1
     2f8:	cmp	x9, x10
     2fc:	b.ls	0x2dc
     300:	b	0x218
     304:	mov	x11, #0x0
     308:	str	x11, [x0]
     30c:	add	sp, sp, #0x20
     310:	ret
     314:	rbit	x10, x15
     318:	clz	x10, x10
     31c:	rbit	x11, x16
     320:	clz	x11, x11
     324:	add	x11, x11, #0x40
     328:	cmp	x15, #0x0
     32c:	csel	x10, x10, x11, ne
     330:	add	x9, x9, x10, lsr #3
     334:	add	x8, x8, x9
     338:	ldur	x10, [x8, #0x1]
     33c:	ldr	x8, [x8, #0x8]
     340:	mov	x11, #0x7a79
     344:	movk	x11, #0x3130, lsl #16
     348:	movk	x11, #0x3332, lsl #32
     34c:	movk	x11, #0x3534, lsl #48
     350:	cmp	x8, x11
     354:	mov	x8, #0x7372
     358:	movk	x8, #0x7574, lsl #16
     35c:	movk	x8, #0x7776, lsl #32
     360:	movk	x8, #0x7978, lsl #48
     364:	ccmp	x10, x8, #0x0, eq
     368:	add	x8, x9, #0x180
     36c:	csel	x11, xzr, x8, ne
     370:	str	x11, [x0]
     374:	add	sp, sp, #0x20
     378:	ret
     37c:	cbz	x11, 0x218
     380:	mov	w9, #0x10
     384:	b	0x2dc
_roc__proc_f:
     388:	ldp	x8, x9, [x1]
     38c:	add	x8, x9, x8
     390:	str	x8, [x0]
     394:	ret
_roc__proc_10:
     398:	ldr	x8, [x1, #0x10]
     39c:	ubfx	x9, x8, #56, #7
     3a0:	cmp	x8, #0x0
     3a4:	csel	x8, x9, x8, lt
     3a8:	str	x8, [x0]
     3ac:	ret
_roc_main:
     3b0:	sub	sp, sp, #0x40
     3b4:	stp	x20, x19, [sp, #0x20]
     3b8:	stp	x29, x30, [sp, #0x30]
     3bc:	ldp	x8, x9, [x0]
     3c0:	ldr	x10, [x0, #0x10]
     3c4:	stp	x8, x9, [sp, #0x8]
     3c8:	str	x10, [sp, #0x18]
     3cc:	ubfx	x12, x10, #56, #7
     3d0:	cmn	x10, #0x1
     3d4:	add	x11, sp, #0x8
     3d8:	csel	x11, x8, x11, gt
     3dc:	csel	x13, x10, x12, gt
     3e0:	subs	x12, x13, #0x10
     3e4:	b.lo	0x538
     3e8:	ldp	x14, x15, [x11]
     3ec:	mov	x16, #0x6a69
     3f0:	movk	x16, #0x6c6b, lsl #16
     3f4:	movk	x16, #0x6e6d, lsl #32
     3f8:	movk	x16, #0x706f, lsl #48
     3fc:	cmp	x15, x16
     400:	mov	x15, #0x6261
     404:	movk	x15, #0x6463, lsl #16
     408:	movk	x15, #0x6665, lsl #32
     40c:	movk	x15, #0x6867, lsl #48
     410:	ccmp	x14, x15, #0x0, eq
     414:	b.ne	0x538
     418:	sub	x14, x13, #0x1f
     41c:	cmp	x12, #0x10
     420:	csel	x14, xzr, x14, lo
     424:	cmp	x14, #0x10
     428:	b.lo	0x520
     42c:	sub	x14, x13, #0x2f
     430:	mov	w13, #0x10
     434:	mov	x15, #0x7070707070707070
     438:	orr	x15, x15, #0x1111111111111111
     43c:	mov	x16, #-0x101010101010102
     440:	movk	x16, #0xfeff
     444:	mov	x17, #-0x101010101010102
     448:	add	x0, x11, x13
     44c:	ldp	x1, x0, [x0]
     450:	eor	x2, x0, x15
     454:	eor	x3, x1, x15
     458:	adds	x3, x3, x16
     45c:	bic	x1, x3, x1
     460:	adc	x2, x2, x17
     464:	bic	x2, x2, x0
     468:	and	x0, x1, #0x8080808080808080
     46c:	and	x1, x2, #0x8080808080808080
     470:	orr	x2, x0, x1
     474:	cbnz	x2, 0x4bc
     478:	add	x13, x13, #0x10
     47c:	cmp	x13, x12
     480:	csel	x0, xzr, x14, hi
     484:	sub	x14, x14, #0x10
     488:	cmp	x0, #0xf
     48c:	b.hi	0x448
     490:	cbz	x0, 0x518
     494:	ldrb	w14, [x11, x13]
     498:	cmp	w14, #0x71
     49c:	b.eq	0x4dc
     4a0:	mov	x0, #0x0
     4a4:	cmp	x12, x13
     4a8:	b.eq	0x518
     4ac:	add	x13, x13, #0x1
     4b0:	cmp	x13, x12
     4b4:	b.ls	0x494
     4b8:	b	0x518
     4bc:	rbit	x12, x0
     4c0:	clz	x12, x12
     4c4:	rbit	x14, x1
     4c8:	clz	x14, x14
     4cc:	add	x14, x14, #0x40
     4d0:	cmp	x0, #0x0
     4d4:	csel	x12, x12, x14, ne
     4d8:	add	x13, x13, x12, lsr #3
     4dc:	add	x11, x11, x13
     4e0:	ldur	x12, [x11, #0x1]
     4e4:	ldr	x11, [x11, #0x8]
     4e8:	mov	x14, #0x7a79
     4ec:	movk	x14, #0x3130, lsl #16
     4f0:	movk	x14, #0x3332, lsl #32
     4f4:	movk	x14, #0x3534, lsl #48
     4f8:	cmp	x11, x14
     4fc:	mov	x11, #0x7372
     500:	movk	x11, #0x7574, lsl #16
     504:	movk	x11, #0x7776, lsl #32
     508:	movk	x11, #0x7978, lsl #48
     50c:	ccmp	x12, x11, #0x0, eq
     510:	add	x11, x13, #0x180
     514:	csel	x0, xzr, x11, ne
     518:	tbz	x10, #0x3f, 0x540
     51c:	b	0x5a8
     520:	cbz	x14, 0x538
     524:	mov	w13, #0x10
     528:	ldrb	w14, [x11, x13]
     52c:	cmp	w14, #0x71
     530:	b.ne	0x4a0
     534:	b	0x4dc
     538:	mov	x0, #0x0
     53c:	tbnz	x10, #0x3f, 0x5a8
     540:	and	x10, x9, #0xfffffffffffffffe
     544:	tst	x9, #0x1
     548:	csel	x8, x8, x10, eq
     54c:	cbz	x8, 0x5a8
     550:	and	x8, x8, #0xfffffffffffffff8
     554:	ldr	x9, [x8, #-0x8]!
     558:	strb	wzr, [sp, #0x8]
     55c:	cbz	x9, 0x5a8
     560:	ldrb	w10, [sp, #0x8]
     564:	tbz	w10, #0x0, 0x598
     568:	subs	x9, x9, #0x1
     56c:	str	x9, [x8]
     570:	b.ne	0x5a8
     574:	mov	x19, x0
     578:	mov	x0, x8
     57c:	mov	w1, #0x8
     580:	bl	_roc_dealloc
     584:	mov	x0, x19
     588:	ldp	x29, x30, [sp, #0x30]
     58c:	ldp	x20, x19, [sp, #0x20]
     590:	add	sp, sp, #0x40
     594:	ret
     598:	mov	x9, #-0x1
     59c:	.long	0xf8290109
     5a0:	cmp	x9, #0x1
     5a4:	b.eq	0x574
     5a8:	ldp	x29, x30, [sp, #0x30]
     5ac:	ldp	x20, x19, [sp, #0x20]
     5b0:	add	sp, sp, #0x40
     5b4:	ret
_roc__proc_d:
     5b8:	b	_roc__proc_c
