#[doc(hidden)]
pub use pretty_assertions::assert_eq as _pretty_assert_eq;

#[derive(PartialEq)]
pub struct DebugAsDisplay<T>(pub T);

impl<T: std::fmt::Display> std::fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[macro_export]
macro_rules! assert_multiline_str_eq {
    ($a:expr, $b:expr) => {
        $crate::_pretty_assert_eq!($crate::DebugAsDisplay($a), $crate::DebugAsDisplay($b))
    };
}
