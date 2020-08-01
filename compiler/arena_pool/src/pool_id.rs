use std::sync::atomic::{AtomicUsize, Ordering};

static mut NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PoolId(usize);

impl Default for PoolId {
    fn default() -> Self {
        let id = unsafe { NEXT_ID.fetch_add(1, Ordering::Relaxed) };

        Self(id)
    }
}
