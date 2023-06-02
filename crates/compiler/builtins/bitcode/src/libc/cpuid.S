// Check if AVX2 is supported.
// Returns 1 if AVX2 is supported, 0 otherwise.
.global {[function_prefix]s}supports_avx2;
{[function_prefix]s}supports_avx2:
    // Save the EBX register.
    push %rbx

    // Call the CPUID instruction with the EAX register set to 7 and ECX set to 0.
    // This will get the CPUID information for the current CPU.
    mov $7, %eax
    mov $0, %ecx
    cpuid

    // The AVX2 feature flag is located in the EBX register at bit 5.
    bt $5, %ebx
    jc .avx2_supported

    // AVX2 is not supported.
    pop %rbx
    mov $0, %eax
    ret

    .avx2_supported:
    pop %rbx
    mov $1, %eax
    ret

 // Check if prefetchw is supported.
 // Returns 1 if the prefetchw instruction is supported, 0 otherwise.
.global {[function_prefix]s}supports_prefetchw;
{[function_prefix]s}supports_prefetchw:
    // Save the EBX register.
    push %rbx

    // Call the CPUID instruction with the EAX register set to 0x80000001 and ECX set to 0.
    // This will get the CPUID information for the current CPU.
    mov $0x80000001, %eax
    mov $0, %ecx
    cpuid

    // The prefetchw feature flag is located in the ECX register at bit 8.
    bt $8, %ecx
    jc .prefetchw_supported

    // AVX2 is not supported.
    pop %rbx
    mov $0, %eax
    ret

    .prefetchw_supported:
    pop %rbx
    mov $1, %eax
    ret
