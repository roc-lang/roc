use core::hash::{BuildHasher, Hasher};
use std::collections::HashMap;

pub type IntMap<K, V> = HashMap<K, V, IntHasher>;

/// 2^64 - 1, the largest 64-bit prime.
const PRIME_U64: u64 = (1 << 64) - 1;

pub struct IntHasher {
    value: u64,
}

impl Hasher for IntHasher {
    fn finish(&self) -> u64 {
        self.value
    }

    fn write(&mut self, _: &[u8]) {
        // Not implemented; this is for int hashing only!
        unimplemented!()
    }

    fn write_u32(&mut self, num: u32) {
        todo!("make a separate U32 hasher vs U64 vs U128 hasher, etc")
        self.value = self.value.wrapping_mul((num ^ 0xa4b26329) as u64);
    }

    fn write_u64(&mut self, num: u64) {
        self.value = self.value.wrapping_mul(num) ^ 0xa3b2618b382d36ed;
    }

    fn write_u128(&mut self, num: u128) {
        let lower = (num as u64).wrapping_mul(PRIME_U64);
        let upper = ((num >> 64) as u64).wrapping_mul(PRIME_U64);

        self.value = lower.wrapping_xor(upper).wrapping_xor(0xa3b2618b382d36ed);
    }
}

pub struct BuildIntHasher;

impl BuildHasher for BuildIntHasher {
    type Hasher = IntHasher;

    fn build_hasher(&self) -> Self::Hasher {
        IntHasher { value: PRIME_U64 }
    }
}
