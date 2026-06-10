/* Fixed assembly trampoline for calling a hosted function with the platform C ABI without
 * generating code at runtime (see host_trampoline.zig for the full rationale).
 *
 * It is shipped as an assembly source so the real assembler emits it ahead of time: inline
 * assembly cannot encode SSE memory operands, and Zig drops a non-root module's file-scope
 * global assembly from the object. The control block arrives in the C-ABI first integer
 * argument register (x0 / rdi). `Call`'s extern layout fixes the byte offsets (8 bytes per
 * field): target@0 gp@8 sse@16 stack@24 stack_size@32 sret@40 res_gp@48 res_sse@56.
 *
 * Both the bare and underscore-prefixed symbols are defined so the Zig `extern` resolves on
 * ELF (no prefix) and Mach-O (leading underscore) alike. */

#if defined(__aarch64__)

.global rocCallTrampoline
.global _rocCallTrampoline
rocCallTrampoline:
_rocCallTrampoline:
	stp x29, x30, [sp, #-32]!
	stp x19, x20, [sp, #16]
	mov x29, sp
	mov x19, x0
	ldr x20, [x19, #32]
	cbz x20, 2f
	sub sp, sp, x20
	ldr x9, [x19, #24]
	mov x10, #0
1:
	ldr x11, [x9, x10]
	str x11, [sp, x10]
	add x10, x10, #8
	cmp x10, x20
	b.lo 1b
2:
	ldr x9, [x19, #16]
	ldp q0, q1, [x9, #0]
	ldp q2, q3, [x9, #32]
	ldp q4, q5, [x9, #64]
	ldp q6, q7, [x9, #96]
	ldr x8, [x19, #40]
	ldr x9, [x19, #8]
	ldp x0, x1, [x9, #0]
	ldp x2, x3, [x9, #16]
	ldp x4, x5, [x9, #32]
	ldp x6, x7, [x9, #48]
	ldr x9, [x19, #0]
	blr x9
	ldr x9, [x19, #48]
	stp x0, x1, [x9]
	ldr x9, [x19, #56]
	stp q0, q1, [x9]
	mov sp, x29
	ldp x19, x20, [sp, #16]
	ldp x29, x30, [sp], #32
	ret

#elif defined(__x86_64__)

.global rocCallTrampoline
.global _rocCallTrampoline
rocCallTrampoline:
_rocCallTrampoline:
#if defined(_WIN64)
	push %rbp
	mov %rsp, %rbp
	push %rbx
	push %rsi
	push %rdi
	push %r12
	mov %rcx, %rbx
	mov 32(%rbx), %r12
	add $32, %r12
	sub %r12, %rsp
	mov 32(%rbx), %r12
	test %r12, %r12
	jz 2f
	mov 24(%rbx), %rsi
	xor %rdi, %rdi
1:
	mov (%rsi,%rdi,1), %rax
	mov %rax, 32(%rsp,%rdi,1)
	add $8, %rdi
	cmp %r12, %rdi
	jb 1b
2:
	mov 16(%rbx), %rax
	movups 0(%rax), %xmm0
	movups 16(%rax), %xmm1
	movups 32(%rax), %xmm2
	movups 48(%rax), %xmm3
	mov 8(%rbx), %rax
	mov 0(%rax), %rcx
	mov 8(%rax), %rdx
	mov 16(%rax), %r8
	mov 24(%rax), %r9
	call *0(%rbx)
	mov 48(%rbx), %rcx
	mov %rax, 0(%rcx)
	mov %rdx, 8(%rcx)
	mov 56(%rbx), %rcx
	movups %xmm0, 0(%rcx)
	movups %xmm1, 16(%rcx)
	lea -32(%rbp), %rsp
	pop %r12
	pop %rdi
	pop %rsi
	pop %rbx
	pop %rbp
	ret
#else
	push %rbp
	mov %rsp, %rbp
	push %rbx
	push %r12
	mov %rdi, %rbx
	mov 32(%rbx), %r12
	test %r12, %r12
	jz 2f
	sub %r12, %rsp
	mov 24(%rbx), %rsi
	xor %rcx, %rcx
1:
	mov (%rsi,%rcx,1), %rax
	mov %rax, (%rsp,%rcx,1)
	add $8, %rcx
	cmp %r12, %rcx
	jb 1b
2:
	mov 16(%rbx), %rax
	movups 0(%rax), %xmm0
	movups 16(%rax), %xmm1
	movups 32(%rax), %xmm2
	movups 48(%rax), %xmm3
	movups 64(%rax), %xmm4
	movups 80(%rax), %xmm5
	movups 96(%rax), %xmm6
	movups 112(%rax), %xmm7
	mov 8(%rbx), %rax
	mov 0(%rax), %rdi
	mov 8(%rax), %rsi
	mov 16(%rax), %rdx
	mov 24(%rax), %rcx
	mov 32(%rax), %r8
	mov 40(%rax), %r9
	mov $8, %al
	call *0(%rbx)
	mov 48(%rbx), %rcx
	mov %rax, 0(%rcx)
	mov %rdx, 8(%rcx)
	mov 56(%rbx), %rcx
	movups %xmm0, 0(%rcx)
	movups %xmm1, 16(%rcx)
	lea -16(%rbp), %rsp
	pop %r12
	pop %rbx
	pop %rbp
	ret
#endif

#endif
