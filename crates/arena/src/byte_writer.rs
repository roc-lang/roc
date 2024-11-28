use core::fmt;

/// Used to give us `write!` on stack-allocated buffers in no_std environments.
pub(crate) struct ByteWriter<'a>(pub(crate) &'a mut [u8]);

impl<'a> fmt::Write for ByteWriter<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let bytes = s.as_bytes();
        let len = bytes.len().min(self.0.len());
        self.0[..len].copy_from_slice(&bytes[..len]);
        self.0 = &mut self.0[len..];
        Ok(())
    }
}
