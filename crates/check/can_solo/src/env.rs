use std::path::Path;

use bumpalo::Bump;
use roc_module::symbol::ModuleId;
use roc_problem::can::Problem;
use roc_region::all::LineInfo;

/// The canonicalization environment for a particular module.
#[derive(Debug)]
pub struct SoloEnv<'a> {
    pub arena: &'a Bump,

    pub module_path: &'a Path,

    pub solo_home: ModuleId,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    pub problems: Vec<Problem>,

    pub src: &'a str,

    /// Lazily calculated line info. This data is only needed if the code contains calls to `dbg`,
    /// otherwise we can leave it as `None` and never pay the cost of scanning the source an extra
    /// time.
    pub lazy_line_info: &'a mut Option<LineInfo>,
}

impl<'a> SoloEnv<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(arena: &'a Bump, src: &'a str, module_path: &'a Path) -> SoloEnv<'a> {
        SoloEnv {
            arena,
            module_path,
            solo_home: ModuleId::first_after_builtins(),
            src,
            problems: Vec::new(),
            lazy_line_info: arena.alloc(None),
        }
    }

    pub fn line_info(&mut self) -> &LineInfo {
        if self.lazy_line_info.is_none() {
            *self.lazy_line_info = Some(LineInfo::new(self.src));
        }

        self.lazy_line_info.as_ref().unwrap()
    }
}
