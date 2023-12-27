use std::{fs::File, io};

use bumpalo::Bump;
use roc_parse::src64::Src64;

pub struct Module<'a> {
    arena: Bump,
    src: Src64<'a>,
}

impl<'a> Default for Module<'a> {
    fn default() -> Self {
        Self {
            arena: Bump::new(),
            src: Default::default(),
        }
    }
}

pub enum InitErr {
    /// The file's size exceeds the maximum supported
    FileTooLong,
}

pub enum CacheLoadOutcome {
    CacheHit,
    CacheMiss,
}

impl<'a> Module<'a> {
    pub fn load_cache_file(&mut self, cache_file: File) -> io::Result<()> {
        let arena = Bump::new();

        Self { arena }
    }
}

pub struct CacheHeader {}
