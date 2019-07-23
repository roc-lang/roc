/// An i64 that always panics on overflow.
pub struct Int(i64);

impl Int {
    pub fn abs(&self) -> Self {
        let Int(int_self) = self;

        let (output, underflowed) = int_self.overflowing_abs();

        if underflowed {
            underflow_panic();
        }

        Int(output)
    }
}

fn underflow_panic() -> ! {
    panic!("Underflow!");
}