// Strong platform-owned names must coexist with the checked shim's local
// helpers. These deliberately poisoned implementations are link-only fixtures.
unsigned long long __udivdi3(unsigned long long a, unsigned long long b) {
    (void)a;
    (void)b;
    return 97;
}

#if defined(__arm__)
__attribute__((pcs("aapcs")))
double __aeabi_ul2d(unsigned long long a) {
    (void)a;
    return 97.0;
}
#endif
