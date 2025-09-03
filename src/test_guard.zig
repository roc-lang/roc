const std = @import("std");
const os = std.posix;
const testing = std.testing;

const c = @cImport({
    @cInclude("signal.h");
    @cInclude("setjmp.h");
});

var jmp_buf: c.jmp_buf = undefined;
var segfault_caught: bool = false;

fn handleSegfault(sig: c_int) callconv(.C) void {
    _ = sig;
    segfault_caught = true;
    _ = c.longjmp(&jmp_buf, 1);
}

test "guard page test" {
    // Install signal handler for both SIGSEGV and SIGBUS
    var old_segv: c.struct_sigaction = undefined;
    var old_bus: c.struct_sigaction = undefined;
    var new_action = c.struct_sigaction{
        .__sigaction_u = .{ .__sa_handler = handleSegfault },
        .sa_flags = 0,
        .sa_mask = undefined,
    };
    _ = c.sigemptyset(&new_action.sa_mask);
    
    var ret = c.sigaction(c.SIGSEGV, &new_action, &old_segv);
    try testing.expect(ret == 0);
    defer _ = c.sigaction(c.SIGSEGV, &old_segv, null);
    
    ret = c.sigaction(c.SIGBUS, &new_action, &old_bus);
    try testing.expect(ret == 0);
    defer _ = c.sigaction(c.SIGBUS, &old_bus, null);
    
    if (c.setjmp(&jmp_buf) == 0) {
        // Allocate memory with guard page
        const page_size = 16384;
        const total_size = page_size * 2;
        
        const memory = try os.mmap(
            null,
            total_size,
            os.PROT.READ | os.PROT.WRITE,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        defer os.munmap(@alignCast(memory));
        
        // Protect first page
        try os.mprotect(memory[0..page_size], os.PROT.NONE);
        
        // Try to write to guard page - should segfault
        memory[0] = 42;
        
        // Should not reach here
        try testing.expect(false);
    } else {
        // We longjmp'd here from signal handler
        try testing.expect(segfault_caught);
    }
}