.global _switch_context_impl
_switch_context_impl:
.global switch_context_impl
switch_context_impl:

# Save all callee saved registers to current context.
# General
movq %r12, 0x00(%rdi)
movq %r13, 0x08(%rdi)
movq %r14, 0x10(%rdi)
movq %r15, 0x18(%rdi)
movq %rbx, 0x20(%rdi)
# Special
movq %rbp, 0x28(%rdi)
movq %rsp, 0x30(%rdi)


# Load target context.
# General
movq 0x00(%rsi), %r12
movq 0x08(%rsi), %r13
movq 0x10(%rsi), %r14
movq 0x18(%rsi), %r15
movq 0x20(%rsi), %rbx
# Special
movq 0x28(%rsi), %rbp
movq 0x30(%rsi), %rsp

ret

