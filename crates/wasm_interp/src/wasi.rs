use roc_wasm_module::Value;

pub trait ImportDispatcher {
    /// Dispatch a call from WebAssembly to your own code, based on module and function name.
    /// The call arguments are passed in, along with a mutable pointer to WebAssembly memory.
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value>;
}

pub trait ImportDispatcherModule {
    const NAME: &'static str;

    /// Dispatch a call from WebAssembly to your own code, based on the function name.
    /// The call arguments are passed in, along with a mutable pointer to WebAssembly memory.
    fn dispatch(
        &mut self,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value>;
}

pub struct WasiDispatcher {}

impl ImportDispatcherModule for WasiDispatcher {
    const NAME: &'static str = "wasi_snapshot_preview1";

    fn dispatch(
        &mut self,
        function_name: &str,
        arguments: &[Value],
        _memory: &mut [u8],
    ) -> Option<Value> {
        match function_name {
            "args_get" => todo!("WASI {}({:?})", function_name, arguments),
            "args_sizes_get" => todo!("WASI {}({:?})", function_name, arguments),
            "environ_get" => todo!("WASI {}({:?})", function_name, arguments),
            "environ_sizes_get" => todo!("WASI {}({:?})", function_name, arguments),
            "clock_res_get" => todo!("WASI {}({:?})", function_name, arguments),
            "clock_time_get" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_advise" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_allocate" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_close" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_datasync" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_fdstat_get" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_fdstat_set_flags" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_fdstat_set_rights" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_get" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_set_size" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_set_times" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_pread" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_prestat_get" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_prestat_dir_name" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_pwrite" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_read" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_readdir" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_renumber" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_seek" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_sync" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_tell" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_write" => todo!("WASI {}({:?})", function_name, arguments),
            "path_create_directory" => todo!("WASI {}({:?})", function_name, arguments),
            "path_filestat_get" => todo!("WASI {}({:?})", function_name, arguments),
            "path_filestat_set_times" => todo!("WASI {}({:?})", function_name, arguments),
            "path_link" => todo!("WASI {}({:?})", function_name, arguments),
            "path_open" => todo!("WASI {}({:?})", function_name, arguments),
            "path_readlink" => todo!("WASI {}({:?})", function_name, arguments),
            "path_remove_directory" => todo!("WASI {}({:?})", function_name, arguments),
            "path_rename" => todo!("WASI {}({:?})", function_name, arguments),
            "path_symlink" => todo!("WASI {}({:?})", function_name, arguments),
            "path_unlink_file" => todo!("WASI {}({:?})", function_name, arguments),
            "poll_oneoff" => todo!("WASI {}({:?})", function_name, arguments),
            "proc_exit" => todo!("WASI {}({:?})", function_name, arguments),
            "proc_raise" => todo!("WASI {}({:?})", function_name, arguments),
            "sched_yield" => todo!("WASI {}({:?})", function_name, arguments),
            "random_get" => todo!("WASI {}({:?})", function_name, arguments),
            "sock_recv" => todo!("WASI {}({:?})", function_name, arguments),
            "sock_send" => todo!("WASI {}({:?})", function_name, arguments),
            "sock_shutdown" => todo!("WASI {}({:?})", function_name, arguments),
            _ => panic!("Unknown WASI function {}({:?})", function_name, arguments),
        }
    }
}
