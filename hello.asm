	.text
	.file	"app"
	.globl	$Test.main
	.p2align	4, 0x90
	.type	$Test.main,@function
$Test.main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$14, %edi
	callq	malloc
	movabsq	$6278066737626506568, %rcx
	movq	%rcx, (%rax)
	movl	$1684828783, 8(%rax)
	movw	$33, 12(%rax)
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	$Test.main, .Lfunc_end0-($Test.main)
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
