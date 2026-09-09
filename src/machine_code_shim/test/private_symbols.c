// The COFF build-time boundary must keep both ordinary strong definitions and
// LLVM/MSVC COMDAT constants private to the shim object.
__declspec(selectany) const int compiler_private_constant = 42;
int compiler_private_helper(void) { return compiler_private_constant; }
void *roc_shim_get_ops(void) { return (void *)&compiler_private_helper; }
void roc_entrypoint(void) {}
void roc_shim_default_main(void) {}
