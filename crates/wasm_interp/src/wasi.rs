use roc_wasm_module::Value;

pub trait ImportDispatcher<ImportId: Sized> {
    /// Translate a module name and function name into your own ImportId enum.
    /// On Instance construction, the interpreter will check that all imports have
    /// some ImportId, and fail otherwise.
    fn get_function_id(module_name: &str, function_name: &str) -> Option<ImportId>;

    /// Dispatch a call from the WebAssembly module to your own code, based on the ImportId.
    /// The call arguments are passed in, along with a mutable pointer to WebAssembly memory.
    fn dispatch(
        &mut self,
        import_id: ImportId,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value>;
}

#[repr(u8)]
#[derive(Debug)]
enum WasiFunctionId {
    ArgsGet,
    ArgsSizesGet,
    EnvironGet,
    EnvironSizesGet,
    ClockResGet,
    ClockTimeGet,
    FdAdvise,
    FdAllocate,
    FdClose,
    FdDatasync,
    FdFdstatGet,
    FdFdstatSetFlags,
    FdFdstatSetRights,
    FdFilestatGet,
    FdFilestatSetSize,
    FdFilestatSetTimes,
    FdPread,
    FdPrestatGet,
    FdPrestatDirName,
    FdPwrite,
    FdRead,
    FdReaddir,
    FdRenumber,
    FdSeek,
    FdSync,
    FdTell,
    FdWrite,
    PathCreateDirectory,
    PathFilestatGet,
    PathFilestatSetTimes,
    PathLink,
    PathOpen,
    PathReadlink,
    PathRemoveDirectory,
    PathRename,
    PathSymlink,
    PathUnlinkFile,
    PollOneoff,
    ProcExit,
    ProcRaise,
    SchedYield,
    RandomGet,
    SockRecv,
    SockSend,
    SockShutdown,
}

pub struct WasiImportDispatcher {}

impl ImportDispatcher<WasiFunctionId> for WasiImportDispatcher {
    fn get_function_id(module_name: &str, function_name: &str) -> Option<WasiFunctionId> {
        use WasiFunctionId::*;
        assert_eq!(module_name, "wasi_snapshot_preview1");
        match function_name {
            "args_get" => Some(ArgsGet),
            "args_sizes_get" => Some(ArgsSizesGet),
            "environ_get" => Some(EnvironGet),
            "environ_sizes_get" => Some(EnvironSizesGet),
            "clock_res_get" => Some(ClockResGet),
            "clock_time_get" => Some(ClockTimeGet),
            "fd_advise" => Some(FdAdvise),
            "fd_allocate" => Some(FdAllocate),
            "fd_close" => Some(FdClose),
            "fd_datasync" => Some(FdDatasync),
            "fd_fdstat_get" => Some(FdFdstatGet),
            "fd_fdstat_set_flags" => Some(FdFdstatSetFlags),
            "fd_fdstat_set_rights" => Some(FdFdstatSetRights),
            "fd_filestat_get" => Some(FdFilestatGet),
            "fd_filestat_set_size" => Some(FdFilestatSetSize),
            "fd_filestat_set_times" => Some(FdFilestatSetTimes),
            "fd_pread" => Some(FdPread),
            "fd_prestat_get" => Some(FdPrestatGet),
            "fd_prestat_dir_name" => Some(FdPrestatDirName),
            "fd_pwrite" => Some(FdPwrite),
            "fd_read" => Some(FdRead),
            "fd_readdir" => Some(FdReaddir),
            "fd_renumber" => Some(FdRenumber),
            "fd_seek" => Some(FdSeek),
            "fd_sync" => Some(FdSync),
            "fd_tell" => Some(FdTell),
            "fd_write" => Some(FdWrite),
            "path_create_directory" => Some(PathCreateDirectory),
            "path_filestat_get" => Some(PathFilestatGet),
            "path_filestat_set_times" => Some(PathFilestatSetTimes),
            "path_link" => Some(PathLink),
            "path_open" => Some(PathOpen),
            "path_readlink" => Some(PathReadlink),
            "path_remove_directory" => Some(PathRemoveDirectory),
            "path_rename" => Some(PathRename),
            "path_symlink" => Some(PathSymlink),
            "path_unlink_file" => Some(PathUnlinkFile),
            "poll_oneoff" => Some(PollOneoff),
            "proc_exit" => Some(ProcExit),
            "proc_raise" => Some(ProcRaise),
            "sched_yield" => Some(SchedYield),
            "random_get" => Some(RandomGet),
            "sock_recv" => Some(SockRecv),
            "sock_send" => Some(SockSend),
            "sock_shutdown" => Some(SockShutdown),
            _ => None,
        }
    }

    fn dispatch(
        &mut self,
        import_id: WasiFunctionId,
        _arguments: &[Value],
        _memory: &mut [u8],
    ) -> Option<Value> {
        use WasiFunctionId::*;

        match import_id {
            ArgsGet => eprintln!("Called WASI {:?}", import_id),
            ArgsSizesGet => eprintln!("Called WASI {:?}", import_id),
            EnvironGet => eprintln!("Called WASI {:?}", import_id),
            EnvironSizesGet => eprintln!("Called WASI {:?}", import_id),
            ClockResGet => eprintln!("Called WASI {:?}", import_id),
            ClockTimeGet => eprintln!("Called WASI {:?}", import_id),
            FdAdvise => eprintln!("Called WASI {:?}", import_id),
            FdAllocate => eprintln!("Called WASI {:?}", import_id),
            FdClose => eprintln!("Called WASI {:?}", import_id),
            FdDatasync => eprintln!("Called WASI {:?}", import_id),
            FdFdstatGet => eprintln!("Called WASI {:?}", import_id),
            FdFdstatSetFlags => eprintln!("Called WASI {:?}", import_id),
            FdFdstatSetRights => eprintln!("Called WASI {:?}", import_id),
            FdFilestatGet => eprintln!("Called WASI {:?}", import_id),
            FdFilestatSetSize => eprintln!("Called WASI {:?}", import_id),
            FdFilestatSetTimes => eprintln!("Called WASI {:?}", import_id),
            FdPread => eprintln!("Called WASI {:?}", import_id),
            FdPrestatGet => eprintln!("Called WASI {:?}", import_id),
            FdPrestatDirName => eprintln!("Called WASI {:?}", import_id),
            FdPwrite => eprintln!("Called WASI {:?}", import_id),
            FdRead => eprintln!("Called WASI {:?}", import_id),
            FdReaddir => eprintln!("Called WASI {:?}", import_id),
            FdRenumber => eprintln!("Called WASI {:?}", import_id),
            FdSeek => eprintln!("Called WASI {:?}", import_id),
            FdSync => eprintln!("Called WASI {:?}", import_id),
            FdTell => eprintln!("Called WASI {:?}", import_id),
            FdWrite => eprintln!("Called WASI {:?}", import_id),
            PathCreateDirectory => eprintln!("Called WASI {:?}", import_id),
            PathFilestatGet => eprintln!("Called WASI {:?}", import_id),
            PathFilestatSetTimes => eprintln!("Called WASI {:?}", import_id),
            PathLink => eprintln!("Called WASI {:?}", import_id),
            PathOpen => eprintln!("Called WASI {:?}", import_id),
            PathReadlink => eprintln!("Called WASI {:?}", import_id),
            PathRemoveDirectory => eprintln!("Called WASI {:?}", import_id),
            PathRename => eprintln!("Called WASI {:?}", import_id),
            PathSymlink => eprintln!("Called WASI {:?}", import_id),
            PathUnlinkFile => eprintln!("Called WASI {:?}", import_id),
            PollOneoff => eprintln!("Called WASI {:?}", import_id),
            ProcExit => eprintln!("Called WASI {:?}", import_id),
            ProcRaise => eprintln!("Called WASI {:?}", import_id),
            SchedYield => eprintln!("Called WASI {:?}", import_id),
            RandomGet => eprintln!("Called WASI {:?}", import_id),
            SockRecv => eprintln!("Called WASI {:?}", import_id),
            SockSend => eprintln!("Called WASI {:?}", import_id),
            SockShutdown => eprintln!("Called WASI {:?}", import_id),
        }
        Some(Value::I32(0))
    }
}
