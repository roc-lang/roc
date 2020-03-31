	.text
	.file	"rust.7rcbfp3g-cgu.0"
	.section	.text._ZN3std2rt10lang_start17h10ea254a893e0692E,"ax",@progbits
	.hidden	_ZN3std2rt10lang_start17h10ea254a893e0692E
	.globl	_ZN3std2rt10lang_start17h10ea254a893e0692E
	.p2align	4, 0x90
	.type	_ZN3std2rt10lang_start17h10ea254a893e0692E,@function
_ZN3std2rt10lang_start17h10ea254a893e0692E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	leaq	.L__unnamed_1(%rip), %rax
	movq	%rdi, 32(%rsp)
	leaq	32(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rsi, 24(%rsp)
	movq	%rax, %rsi
	movq	24(%rsp), %rax
	movq	%rdx, 16(%rsp)
	movq	%rax, %rdx
	movq	16(%rsp), %rcx
	callq	*_ZN3std2rt19lang_start_internal17h9cf8802361ad86c2E@GOTPCREL(%rip)
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	_ZN3std2rt10lang_start17h10ea254a893e0692E, .Lfunc_end0-_ZN3std2rt10lang_start17h10ea254a893e0692E
	.cfi_endproc

	.section	".text._ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE,@function
_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	*(%rdi)
	callq	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE
	movl	%eax, 4(%rsp)
	movl	4(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE, .Lfunc_end1-_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE
	.cfi_endproc

	.section	.text._ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E,@function
_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E:
	.cfi_startproc
	movzbl	(%rdi), %eax
	retq
.Lfunc_end2:
	.size	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E, .Lfunc_end2-_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E
	.cfi_endproc

	.section	".text._ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE,@function
_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rdi
	callq	_ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE
	movl	%eax, 12(%rsp)
	movl	12(%rsp), %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE, .Lfunc_end3-_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE
	.cfi_endproc

	.section	.text._ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE,@function
_ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE:
.Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception0
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 8(%rsp)
.Ltmp0:
	leaq	8(%rsp), %rdi
	callq	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE
.Ltmp1:
	movl	%eax, 4(%rsp)
	jmp	.LBB4_1
.LBB4_1:
	jmp	.LBB4_2
.LBB4_2:
	movl	4(%rsp), %eax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB4_3:
	.cfi_def_cfa_offset 48
	jmp	.LBB4_4
.LBB4_4:
	movq	24(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB4_5:
.Ltmp2:
	movq	%rax, 24(%rsp)
	movl	%edx, 32(%rsp)
	jmp	.LBB4_3
.Lfunc_end4:
	.size	_ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE, .Lfunc_end4-_ZN4core3ops8function6FnOnce9call_once17h95777eb19c893e7cE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table4:
.Lexception0:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end0-.Lcst_begin0
.Lcst_begin0:
	.uleb128 .Ltmp0-.Lfunc_begin0
	.uleb128 .Ltmp1-.Ltmp0
	.uleb128 .Ltmp2-.Lfunc_begin0
	.byte	0
	.uleb128 .Ltmp1-.Lfunc_begin0
	.uleb128 .Lfunc_end4-.Ltmp1
	.byte	0
	.byte	0
.Lcst_end0:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17h5ea43678993f7f74E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h5ea43678993f7f74E,@function
_ZN4core3ptr13drop_in_place17h5ea43678993f7f74E:
	.cfi_startproc
	retq
.Lfunc_end5:
	.size	_ZN4core3ptr13drop_in_place17h5ea43678993f7f74E, .Lfunc_end5-_ZN4core3ptr13drop_in_place17h5ea43678993f7f74E
	.cfi_endproc

	.section	".text._ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE,@function
_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	callq	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E
	movl	%eax, 4(%rsp)
	movl	4(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end6:
	.size	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE, .Lfunc_end6-_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h4c1d7168636eac2bE
	.cfi_endproc

	.section	".text._ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E,@function
_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%dil, 7(%rsp)
	leaq	7(%rsp), %rdi
	callq	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217hafc7a17b3af818e6E
	movl	%eax, (%rsp)
	movl	(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end7:
	.size	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E, .Lfunc_end7-_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17h4641d8c7c31fa834E
	.cfi_endproc

	.section	.text._ZN4rust3foo17h24346e1203f74a2dE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4rust3foo17h24346e1203f74a2dE,@function
_ZN4rust3foo17h24346e1203f74a2dE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, (%rsp)
	movq	%rdi, 8(%rsp)
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end8:
	.size	_ZN4rust3foo17h24346e1203f74a2dE, .Lfunc_end8-_ZN4rust3foo17h24346e1203f74a2dE
	.cfi_endproc

	.section	.text._ZN4rust9test_main17h45325f9e1ca54b4bE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4rust9test_main17h45325f9e1ca54b4bE,@function
_ZN4rust9test_main17h45325f9e1ca54b4bE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	$9, %edi
	callq	_ZN4rust3foo17h24346e1203f74a2dE
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rcx
	imulq	%rcx, %rax
	seto	%dl
	testb	$1, %dl
	movq	%rax, (%rsp)
	jne	.LBB9_3
	movq	(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB9_3:
	.cfi_def_cfa_offset 32
	leaq	str.0(%rip), %rdi
	leaq	.L__unnamed_2(%rip), %rdx
	movq	_ZN4core9panicking5panic17hcdc9f0ba8d71d265E@GOTPCREL(%rip), %rax
	movl	$33, %esi
	callq	*%rax
	ud2
.Lfunc_end9:
	.size	_ZN4rust9test_main17h45325f9e1ca54b4bE, .Lfunc_end9-_ZN4rust9test_main17h45325f9e1ca54b4bE
	.cfi_endproc

	.section	.text._ZN4rust4main17h523dcf5432fcfd88E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4rust4main17h523dcf5432fcfd88E,@function
_ZN4rust4main17h523dcf5432fcfd88E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4rust9test_main17h45325f9e1ca54b4bE
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end10:
	.size	_ZN4rust4main17h523dcf5432fcfd88E, .Lfunc_end10-_ZN4rust4main17h523dcf5432fcfd88E
	.cfi_endproc

	.section	.text.main,"ax",@progbits
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movslq	%edi, %rax
	leaq	_ZN4rust4main17h523dcf5432fcfd88E(%rip), %rdi
	movq	%rsi, (%rsp)
	movq	%rax, %rsi
	movq	(%rsp), %rdx
	callq	_ZN3std2rt10lang_start17h10ea254a893e0692E
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end11:
	.size	main, .Lfunc_end11-main
	.cfi_endproc

	.type	.L__unnamed_1,@object
	.section	.data.rel.ro..L__unnamed_1,"aw",@progbits
	.p2align	3
.L__unnamed_1:
	.quad	_ZN4core3ptr13drop_in_place17h5ea43678993f7f74E
	.quad	8
	.quad	8
	.quad	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE
	.quad	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17haa4f2d7d298b0acfE
	.quad	_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h3c0881b3435f345aE
	.size	.L__unnamed_1, 48

	.type	.L__unnamed_3,@object
	.section	.rodata..L__unnamed_3,"a",@progbits
.L__unnamed_3:
	.ascii	"rust.rs"
	.size	.L__unnamed_3, 7

	.type	.L__unnamed_2,@object
	.section	.data.rel.ro..L__unnamed_2,"aw",@progbits
	.p2align	3
.L__unnamed_2:
	.quad	.L__unnamed_3
	.asciz	"\007\000\000\000\000\000\000\000\b\000\000\000\005\000\000"
	.size	.L__unnamed_2, 24

	.type	str.0,@object
	.section	.rodata.str.0,"a",@progbits
	.p2align	4
str.0:
	.ascii	"attempt to multiply with overflow"
	.size	str.0, 33

	.hidden	DW.ref.rust_eh_personality
	.weak	DW.ref.rust_eh_personality
	.section	.data.DW.ref.rust_eh_personality,"aGw",@progbits,DW.ref.rust_eh_personality,comdat
	.p2align	3
	.type	DW.ref.rust_eh_personality,@object
	.size	DW.ref.rust_eh_personality, 8
DW.ref.rust_eh_personality:
	.quad	rust_eh_personality

	.section	".note.GNU-stack","",@progbits
