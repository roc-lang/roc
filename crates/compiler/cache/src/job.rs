use core::marker::PhantomData;

use crate::path::Path;

pub struct Shorthands<'a> {
    //
}

pub struct Module<'a> {
    /// This is None iff this module is the original root module
    /// passed to the `roc` CLI. Other root modules can come up
    /// when (for example) the original root module leads to packages
    /// being loaded (at which point they become the root module),
    /// and those packages load other packages.
    root_module: Option<ModuleId<'a>>,
    filename: &'a Path,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ModuleId<'a> {
    /// Index into the SoA of Module entries (so, basically a u32 pointer)
    index: u32,
    _phantom: PhantomData<&'a ()>,
}

pub enum Job<'a> {
    /// The main thread will read the contents of a .roc file
    /// and add it as a module to the module graph.
    ///
    /// If the read fails, the module will be added to the graph
    /// as an Invalid module. That way the requesting module
    /// will know what happened when it goes to look it up later.
    ReadSourceFile {
        /// When loading a source file, we need to know who the current root module is.
        /// At first, this is always the original .roc file passed into the `roc` CLI,
        /// but it can change when (for example) a package loads another package.
        root_module: ModuleId<'a>,

        /// The main thread will turn this into a path,
        /// based on the root module's path.
        module_name: &'a str,
    },
    ReadCacheFile {
        /// When loading a cache file, we need to know who the current root module is.
        /// At first, this is always the original .roc file passed into the `roc` CLI,
        /// but it can change when (for example) a package loads another package.
        root_module: ModuleId<'a>,

        /// The BLAKE3 hash of the source file, which tells us which cache file to load.
        /// (This is the output of https://docs.rs/blake3/1.5.0/blake3/struct.OutputReader.html)
        hash: [u8; 32],
    },
    /// The main thread will download from this URL, verify it,
    /// and add it as a module to the module graph.
    ///
    /// If the download fails, the module will be added to the graph
    /// as an Invalid module. That way the requesting module
    /// will know what happened when it goes to look it up later.
    Download {
        /// The URL contains all the information necessary, including
        /// the file format. We don't need to know the root module because
        /// this will be its own root.
        url: &'a str,
    },
    /// Given some bytes that have been loaded into memory from a .roc file,
    /// parse, canonicalize, and constrain them, and then cache the result.
    ParseSourceFileContents {
        /// The bytes that were in the file.
        bytes: &'a mut [u8],

        // Info carried forward from previous steps
        root_module: ModuleId<'a>,
        src_filename: &'a Path,
    },
    LoadCacheFileContents {
        /// The bytes that were in the cache file.
        bytes: &'a mut [u8],

        // Info carried forward from previous steps
        root_module: ModuleId<'a>,
        cache_filename: &'a Path,
    },
    /// Note: this *also* verifies names for module imports,
    /// because the module should have a known root_module (and therefore shorthands),
    /// and all of the modules it imports should have solved already.
    TypeCheck {
        module_id: ModuleId<'a>,
    }
}

pub struct TypeCheckArgs<'a> {
    /// The module to type-check. From this we can get the shorthands of its root module.
    /// Since those must already be solved, we should already have integer IDs for them all.
    /// Also, since all of its dependencies are solved, we should have integer IDs for them too.
    module_id: ModuleId<'a>,
}
