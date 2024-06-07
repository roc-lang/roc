/// Either a binding or a lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LowercaseId(pub(crate) u16);

impl LowercaseId {
    pub(crate) fn to_index(&self) -> usize {
        self.0 as usize
    }
}

/// Either a binding or a lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UppercaseId(pub(crate) u16);

impl UppercaseId {
    pub(crate) fn to_index(&self) -> usize {
        self.0 as usize
    }
}

/// An imported module
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(pub(crate) u16);

impl ModuleId {
    pub const HOME: Self = Self(0);

    pub(crate) fn to_index(&self) -> usize {
        self.0 as usize
    }
}
