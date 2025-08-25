// C shim to provide main entry point that calls into Zig platform code
// This avoids Zig generating _start when we export main from a static library

// Forward declaration of the Zig platform entry point
extern int roc_platform_host_main(int argc, char** argv);

// Standard C main that will be called by the C runtime (crt0/Scrt1.o)
int main(int argc, char** argv) {
    // Simply forward to the Zig implementation
    return roc_platform_host_main(argc, argv);
}