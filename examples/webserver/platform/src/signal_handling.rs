use libc::{
    sigaction, sigemptyset, sighandler_t, siginfo_t, sigset_t, SA_SIGINFO, SIGBUS, SIGFPE, SIGILL,
    SIGSEGV, SIG_DFL,
};
use std::os::raw::c_int;

pub fn install_signal_handlers() {
    setup_signal(SIGSEGV);
    setup_signal(SIGILL);
    setup_signal(SIGFPE);
    setup_signal(SIGBUS);
}

unsafe extern "C" fn signal_handler(sig: c_int, _: *mut siginfo_t, _: *mut libc::c_void) {
    println!("Server caught a signal {}; aborting", sig);

    std::process::abort();
}

#[cfg(target_os = "macos")]
fn setup_signal(sig: c_int) {
    let sa = libc::sigaction {
        sa_sigaction: signal_handler as sighandler_t,
        sa_mask: sigset_t::default(),
        sa_flags: SA_SIGINFO,
    };

    let mut old_sa = libc::sigaction {
        sa_sigaction: SIG_DFL,
        sa_mask: sigset_t::default(),
        sa_flags: 0,
    };

    unsafe {
        sigemptyset(&mut old_sa.sa_mask as *mut sigset_t);
        sigaction(sig, &sa, &mut old_sa);
    }
}

#[cfg(target_os = "linux")]
fn setup_signal(sig: c_int) {
    let sa = libc::sigaction {
        sa_sigaction: signal_handler as sighandler_t,
        sa_mask: unsafe { std::mem::zeroed() },
        sa_flags: SA_SIGINFO,
        sa_restorer: None,
    };

    let mut old_sa = libc::sigaction {
        sa_sigaction: SIG_DFL,
        sa_mask: unsafe { std::mem::zeroed() },
        sa_flags: 0,
        sa_restorer: None,
    };

    unsafe {
        sigemptyset(&mut old_sa.sa_mask as *mut sigset_t);
        sigaction(sig, &sa, &mut old_sa);
    }
}
