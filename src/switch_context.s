.global _switch_context_impl
_switch_context_impl:
.global switch_context_impl
switch_context_impl:

# Save all callee saved registers to current context.
mov x9,  sp
# General
stp x19, x20, [x0, 0*8]
stp x21, x22, [x0, 2*8]
stp x23, x24, [x0, 4*8]
stp x25, x26, [x0, 6*8]
stp x27, x28, [x0, 8*8]
# Double
stp d8,  d9,  [x0, 10*8]
stp d10, d11, [x0, 12*8]
stp d12, d13, [x0, 14*8]
stp d14, d15, [x0, 16*8]
# Special
stp fp,  lr,  [x0, 18*8]
str x9,       [x0, 20*8]


# Load target context.
# General
ldp x19, x20, [x1, 0*8]
ldp x21, x22, [x1, 2*8]
ldp x23, x24, [x1, 4*8]
ldp x25, x26, [x1, 6*8]
ldp x27, x28, [x1, 8*8]
# Double
ldp d8,  d9,  [x1, 10*8]
ldp d10, d11, [x1, 12*8]
ldp d12, d13, [x1, 14*8]
ldp d14, d15, [x1, 16*8]
# Special
ldp fp,  lr,  [x1, 18*8]
ldr x9,       [x1, 20*8]
mov sp, x9


ret