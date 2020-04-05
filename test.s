	.text
	.intel_syntax noprefix
	.file	"test.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function f
.LCPI0_0:
	.quad	4614613358185178726     # double 3.2999999999999998
.LCPI0_1:
	.quad	4612136378390124954     # double 2.2000000000000002
.LCPI0_2:
	.quad	4607632778762754458     # double 1.1000000000000001
	.text
	.globl	f
	.p2align	4, 0x90
	.type	f,@function
f:                                      # @f
	.cfi_startproc
# %bb.0:
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset rbp, -16
	mov	rbp, rsp
	.cfi_def_cfa_register rbp
	mov	rax, rdi
	movsd	xmm0, qword ptr [rip + .LCPI0_0] # xmm0 = mem[0],zero
	movsd	xmm1, qword ptr [rip + .LCPI0_1] # xmm1 = mem[0],zero
	movsd	xmm2, qword ptr [rip + .LCPI0_2] # xmm2 = mem[0],zero
	movsd	qword ptr [rdi], xmm2
	movsd	qword ptr [rdi + 8], xmm1
	movsd	qword ptr [rdi + 16], xmm0
	pop	rbp
	.cfi_def_cfa rsp, 8
	ret
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset rbp, -16
	mov	rbp, rsp
	.cfi_def_cfa_register rbp
	sub	rsp, 32
	mov	dword ptr [rbp - 4], 0
	lea	rdi, [rbp - 32]
	call	f
	xor	eax, eax
	add	rsp, 32
	pop	rbp
	.cfi_def_cfa rsp, 8
	ret
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function

	.ident	"clang version 8.0.1-3build1 (tags/RELEASE_801/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym f
