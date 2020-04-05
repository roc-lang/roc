	.text
	.file	"roc_builtins_bitcode.d1f89fme-cgu.0"
	.globl	i64_to_f64_
	.p2align	4, 0x90
	.type	i64_to_f64_,@function
i64_to_f64_:
	.cfi_startproc
	vcvtsi2sdq	%rdi, %xmm0, %xmm0
	retq
.Lfunc_end0:
	.size	i64_to_f64_, .Lfunc_end0-i64_to_f64_
	.cfi_endproc

	.section	.rodata.cst32,"aM",@progbits,32
	.p2align	5
.LCPI1_0:
	.quad	4614162998222441677
	.quad	4617428107952285286
	.quad	4625506439783881114
	.quad	4626153832230315622
	.text
	.globl	$Test.main
	.p2align	4, 0x90
	.type	$Test.main,@function
$Test.main:
	.cfi_startproc
	movq	%rdi, %rax
	movabsq	$4626463454704697344, %rcx
	movq	%rcx, 32(%rdi)
	vmovaps	.LCPI1_0(%rip), %ymm0
	vmovups	%ymm0, (%rdi)
	vzeroupper
	retq
.Lfunc_end1:
	.size	$Test.main, .Lfunc_end1-($Test.main)
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
