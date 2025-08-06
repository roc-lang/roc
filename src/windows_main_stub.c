// Windows stub for __main symbol
// This symbol is required by MinGW-style initialization but not used in pure Zig code

#ifdef _WIN32
#ifdef __cplusplus
extern "C"
#endif
void __main(void) {
    // No-op stub - Zig handles initialization differently
    // This satisfies the linker when building with MinGW-style ABI
}
#endif