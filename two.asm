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

	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3
.LCPI1_0:
	.quad	4614162998222441677
.LCPI1_1:
	.quad	4617428107952285286
	.text
	.globl	$Test.main
	.p2align	4, 0x90
	.type	$Test.main,@function
$Test.main:
	.cfi_startproc
	vmovsd	.LCPI1_0(%rip), %xmm0
	vmovsd	.LCPI1_1(%rip), %xmm1
	retq
.Lfunc_end1:
	.size	$Test.main, .Lfunc_end1-($Test.main)
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
