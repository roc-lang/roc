use native_str::NativeCStr;

#[macro_export]
macro_rules! path {
    ($s:literal) => {
        Path::from(native_str::native_cstr!($s))
    };
}

/// On Windows,
pub struct Path(NativeCStr);

impl<'a> From<&'a Path> for &'a NativeCStr {
    fn from(path: &'a Path) -> Self {
        path.as_native_cstr()
    }
}

impl<'a> From<&'a NativeCStr> for &'a Path {
    fn from(c_str: &'a NativeCStr) -> Self {
        Path::from_native_cstr(c_str)
    }
}

impl Path {
    pub fn from_native_cstr(c_str: &NativeCStr) -> &Self {
        unsafe { &*(c_str as *const NativeCStr as *const Path) }
    }

    pub fn as_native_cstr(&self) -> &NativeCStr {
        &self.0
    }
}
