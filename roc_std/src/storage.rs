use core::num::NonZeroIsize;

const REFCOUNT_1: isize = isize::MIN;

#[derive(Clone, Copy, Debug)]
pub enum Storage {
    Readonly,
    ReferenceCounted(NonZeroIsize),
}

impl Storage {
    pub fn new_reference_counted() -> Self {
        Self::ReferenceCounted(NonZeroIsize::new(REFCOUNT_1).unwrap())
    }

    /// Increment the reference count.
    pub fn increment_reference_count(&mut self) {
        match self {
            Storage::Readonly => {
                // Do nothing.
            }
            Storage::ReferenceCounted(rc) => {
                let new_rc = rc.get() + 1;
                if let Some(new_rc) = NonZeroIsize::new(new_rc) {
                    *self = Storage::ReferenceCounted(new_rc);
                } else {
                    *self = Storage::Readonly;
                }
            }
        }
    }

    /// Decrease the reference count.
    ///
    /// Returns `true` once there are no more references left.
    pub fn decrease(&mut self) -> bool {
        match self {
            Storage::Readonly => false,
            Storage::ReferenceCounted(rc) => {
                let rc_as_isize = rc.get();
                if rc_as_isize == REFCOUNT_1 {
                    true
                } else {
                    *rc = NonZeroIsize::new(rc_as_isize - 1).unwrap();
                    false
                }
            }
        }
    }

    pub fn is_readonly(&self) -> bool {
        matches!(self, Self::Readonly)
    }
}
