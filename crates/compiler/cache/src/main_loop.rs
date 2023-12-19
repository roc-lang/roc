use bumpalo::Bump;

use crate::{path::Path, vec::Vec, report::Report};

/// The main loop which takes care of:
/// - Managing and allocating central state
/// - Performing async I/O
/// - Spawning worker threads (if max_threads > 1) which don't do any I/O, and coordinating them.

pub struct Cfg<'a> {
    pub paths: &'a [Path<'a>],
    pub max_threads: u32,
    pub mode: Mode,
    pub watch: bool,
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Mode {
    Check,
    Build,
    Run,
    RunIfNoErrors,
    Test,
}

struct State<'a> {
    cfg: Cfg<'a>,
    arena: Bump,
}

pub struct Run<'a> {
    errors: Vec<'a, Report<'a>>,
    warnings: Vec<'a, Report<'a>>,
    // timings: Timings<'a>,
}


impl <'a> State<'a> {
    pub fn with_capacity(cfg: Cfg<'a>, capacity: usize) -> Self {
        debug_assert_ne!(cfg.paths.len(), 0, "new()'s caller should have passed at least one path!");

        let arena = Bump::with_capacity(capacity);

        Self { cfg, arena }
    }

    pub fn run(&mut self) -> Run<'a> {
        loop {

        }
    }
}
