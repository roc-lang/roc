use libc::{
    sigaction, sigemptyset, sighandler_t, siginfo_t, sigset_t, SA_SIGINFO, SIGBUS, SIGFPE, SIGILL,
    SIGSEGV, SIG_DFL,
};
use roc_std::RocStr;
use std::cell::RefCell;
use std::os::raw::{c_int, c_long, c_void};

// If we have a roc_panic or a segfault, these will be used to record where to jump back to
// (a point at which we can return a different response).
thread_local! {
    // 64 is the biggest jmp_buf in setjmp.h
    pub static SETJMP_ENV: RefCell<[c_long; 64]> = RefCell::new([0 as c_long; 64]);
    pub static ROC_CRASH_MSG: RefCell<RocStr> = RefCell::new(RocStr::empty());
    pub static SIGNAL_CAUGHT: RefCell<c_int> = RefCell::new(0);
}

pub fn install_signal_handlers() {
    setup_signal(SIGSEGV);
    setup_signal(SIGILL);
    setup_signal(SIGFPE);
    setup_signal(SIGBUS);
}

extern "C" {
    #[link_name = "setjmp"]
    pub fn setjmp(env: *mut c_void) -> c_int;

    #[link_name = "longjmp"]
    pub fn longjmp(env: *mut c_void, val: c_int);
}

unsafe extern "C" fn signal_handler(sig: c_int, _: *mut siginfo_t, _: *mut libc::c_void) {
    SIGNAL_CAUGHT.with(|val| {
        *val.borrow_mut() = sig;
    });

    SETJMP_ENV.with(|env| {
        longjmp(env.borrow_mut().as_mut_ptr().cast(), 1);
    });
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
