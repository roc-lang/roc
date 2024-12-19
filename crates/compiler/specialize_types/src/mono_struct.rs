#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MonoFieldId {
    inner: u16,
}
impl MonoFieldId {
    pub fn new(index: u16) -> Self {
        Self { inner: index }
    }

    pub fn as_index(self) -> usize {
        self.inner as usize
    }
}
