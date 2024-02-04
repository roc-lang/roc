// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

use core::{
    hash::Hash,
    num::{NonZeroU16, NonZeroU32},
};

/// A unique identifier for a module.
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ModuleId {
    value: NonZeroU32,
}

/// A unique identifier within a module.
#[derive(Debug)]
#[cfg_attr(not(debug_assertions), repr(transparent))]
pub struct IdentId {
    value: NonZeroU16,

    #[cfg(debug_assertions)]
    module_id: ModuleId,
}

impl Eq for IdentId {}

impl PartialEq for IdentId {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(debug_assertions)]
        {
            if self.module_id != other.module_id {
                panic!("Tried to compare two IdentIds for equality, but they came from different modules! One came from {:?} and the other came from {:?}");
            }
        }

        self.value == other.value
    }
}

impl Hash for IdentId {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}
