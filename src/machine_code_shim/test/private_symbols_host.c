extern void *roc_shim_get_ops(void);
extern void roc_entrypoint(void);
extern void roc_shim_default_main(void);

// Deliberate collisions. Platform definitions must coexist with the shim's
// local definitions, and neither may satisfy or interpose the other's uses.
const int compiler_private_constant = 99;
int compiler_private_helper(void) { return compiler_private_constant; }
int mainCRTStartup(void) {
    int (*shim_helper)(void) = (int (*)(void))roc_shim_get_ops();
    roc_entrypoint();
    roc_shim_default_main();
    return shim_helper() == 42 && compiler_private_helper() == 99 ? 0 : 1;
}
