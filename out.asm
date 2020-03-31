	.text
	.file	"app"
	.p2align	4, 0x90
	.type	.L_0,@function
.L_0:
	.cfi_startproc
	movq	%rdi, %rax
	movq	%rdi, %rdx
	retq
.Lfunc_end0:
	.size	.L_0, .Lfunc_end0-.L_0
	.cfi_endproc

	.globl	$Test.main
	.p2align	4, 0x90
	.type	$Test.main,@function
$Test.main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$9, %edi
	callq	.L_0
	imulq	%rdx, %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	$Test.main, .Lfunc_end1-($Test.main)
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
