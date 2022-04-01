#![cfg(target_family = "wasm")]
/*
For the Web REPL (repl_www), we build the compiler as a Wasm module.
SystemTime is the only thing in the compiler that would need a special implementation for this.
There is a WASI implementation for it, but we are targeting the browser, not WASI!
It's possible to write browser versions of WASI's low-level ABI but we'd rather avoid it.
Instead we use these dummy implementations, which should just disappear at compile time.
*/

#[derive(Debug, Clone, Copy)]
pub struct SystemTime;

impl SystemTime {
    pub fn now() -> Self {
        SystemTime
    }
    pub fn duration_since(&self, _: SystemTime) -> Result<Duration, String> {
        Ok(Duration)
    }
    pub fn elapsed(&self) -> Result<Duration, String> {
        Ok(Duration)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Duration;

impl Duration {
    pub fn checked_sub(&self, _: Duration) -> Option<Duration> {
        Some(Duration)
    }
}

impl Default for Duration {
    fn default() -> Self {
        Duration
    }
}

impl std::ops::AddAssign for Duration {
    fn add_assign(&mut self, _: Duration) {}
}
