use rand::prelude::*;
use roc_wasm_module::Value;
use std::io::{self, Read, StderrLock, StdoutLock, Write};
use std::process::exit;

pub const MODULE_NAME: &str = "wasi_snapshot_preview1";

pub struct WasiDispatcher<'a> {
    pub args: &'a [&'a [u8]],
    pub rng: ThreadRng,
    pub files: Vec<WasiFile>,
}

impl Default for WasiDispatcher<'_> {
    fn default() -> Self {
        WasiDispatcher::new(&[])
    }
}

pub enum WasiFile {
    ReadOnly(Vec<u8>),
    WriteOnly(Vec<u8>),
    ReadWrite(Vec<u8>),
    HostSystemFile,
}

enum WriteLock<'a> {
    StdOut(StdoutLock<'a>),
    Stderr(StderrLock<'a>),
    RegularFile(&'a mut Vec<u8>),
}

/// Implementation of WASI syscalls
/// References for other engines:
/// https://github.com/wasmerio/wasmer/blob/ef8d2f651ed29b4b06fdc2070eb8189922c54d82/lib/wasi/src/syscalls/mod.rs
/// https://github.com/wasm3/wasm3/blob/045040a97345e636b8be4f3086e6db59cdcc785f/source/extra/wasi_core.h
impl<'a> WasiDispatcher<'a> {
    pub fn new(args: &'a [&'a [u8]]) -> Self {
        WasiDispatcher {
            args,
            rng: thread_rng(),
            files: vec![
                WasiFile::HostSystemFile,
                WasiFile::HostSystemFile,
                WasiFile::HostSystemFile,
            ],
        }
    }

    pub fn dispatch(
        &mut self,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value> {
        let success_code = Some(Value::I32(Errno::Success as i32));
        match function_name {
            "args_get" => {
                // uint8_t ** argv,
                let mut ptr_ptr_argv = arguments[0].expect_i32().unwrap() as usize;
                // uint8_t * argv_buf
                let mut ptr_argv_buf = arguments[1].expect_i32().unwrap() as usize;

                for arg in self.args {
                    write_u32(memory, ptr_ptr_argv, ptr_argv_buf as u32);
                    let bytes_target = &mut memory[ptr_argv_buf..][..arg.len()];
                    bytes_target.copy_from_slice(arg);
                    memory[ptr_argv_buf + arg.len()] = 0; // C string zero termination
                    ptr_argv_buf += arg.len() + 1;
                    ptr_ptr_argv += 4;
                }

                success_code
            }
            "args_sizes_get" => {
                // (i32, i32) -> i32

                // number of string arguments
                let ptr_argc = arguments[0].expect_i32().unwrap() as usize;
                // size of string arguments buffer
                let ptr_argv_buf_size = arguments[1].expect_i32().unwrap() as usize;

                let argc = self.args.len() as u32;
                write_u32(memory, ptr_argc, argc);

                let argv_buf_size: u32 = self.args.iter().map(|a| 1 + a.len() as u32).sum();
                write_u32(memory, ptr_argv_buf_size, argv_buf_size);

                success_code
            }
            "environ_get" => {
                // `environ_sizes_get` always reports 0 environment variables
                // so we don't have to do anything here.

                success_code
            }
            "environ_sizes_get" => {
                let num_env_ptr = arguments[0].expect_i32().unwrap() as usize;
                let size_env_ptr = arguments[1].expect_i32().unwrap() as usize;

                // Calculate the total size required for environment variables
                let total_size = 0;
                let count = 0;

                write_u32(memory, num_env_ptr, count);
                write_u32(memory, size_env_ptr, total_size as u32);

                success_code
            }
            "clock_res_get" => success_code, // this dummy implementation seems to be good enough for some functions
            "clock_time_get" => success_code,
            "fd_advise" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_allocate" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_close" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_datasync" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_fdstat_get" => {
                // (i32, i32) -> i32

                // file descriptor
                let fd = arguments[0].expect_i32().unwrap() as usize;
                // ptr to a wasi_fdstat_t
                let stat_mut_ptr = arguments[1].expect_i32().unwrap() as usize;

                match fd {
                    1 | 2 => {
                        // Tell WASI that stdout and stderr are a tty (no seek or tell)
                        // https://github.com/WebAssembly/wasi-libc/blob/659ff414560721b1660a19685110e484a081c3d4/libc-bottom-half/sources/isatty.c
                        // *Not* a tty if:
                        //     (statbuf.fs_filetype != __WASI_FILETYPE_CHARACTER_DEVICE ||
                        //         (statbuf.fs_rights_base & (__WASI_RIGHTS_FD_SEEK | __WASI_RIGHTS_FD_TELL)) != 0)
                        // So it's sufficient to set:
                        //     .fs_filetype = __WASI_FILETYPE_CHARACTER_DEVICE
                        //     .fs_rights_base = 0

                        const WASI_FILETYPE_CHARACTER_DEVICE: u8 = 2;
                        memory[stat_mut_ptr] = WASI_FILETYPE_CHARACTER_DEVICE;

                        for b in memory[stat_mut_ptr + 1..stat_mut_ptr + 24].iter_mut() {
                            *b = 0;
                        }
                    }
                    _ => todo!("WASI {}({:?})", function_name, arguments),
                }

                success_code
            }
            "fd_fdstat_set_flags" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_fdstat_set_rights" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_get" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_set_size" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_filestat_set_times" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_pread" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_prestat_get" => {
                // The preopened file descriptor to query
                let fd = arguments[0].expect_i32().unwrap() as usize;
                // ptr_buf: Where the metadata will be written
                //  preopen type: 4 bytes, where 0=dir is the only one supported, it seems
                //  preopen name length: 4 bytes
                let ptr_buf = arguments[1].expect_i32().unwrap() as usize;
                memory[ptr_buf..][..8].copy_from_slice(&0u64.to_le_bytes());
                if fd < self.files.len() {
                    success_code
                } else {
                    println!("WASI warning: file descriptor {fd} does not exist");
                    Some(Value::I32(Errno::Badf as i32))
                }
            }
            "fd_prestat_dir_name" => {
                // We're not giving names to any of our files so just return success
                success_code
            }
            "fd_pwrite" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_read" => {
                use WasiFile::*;

                // file descriptor
                let fd = arguments[0].expect_i32().unwrap() as usize;
                // Array of IO vectors
                let ptr_iovs = arguments[1].expect_i32().unwrap() as usize;
                // Length of array
                let iovs_len = arguments[2].expect_i32().unwrap();
                // Out param: number of bytes read
                let ptr_nread = arguments[3].expect_i32().unwrap() as usize;

                // https://man7.org/linux/man-pages/man2/readv.2.html
                // struct iovec {
                //     void  *iov_base;    /* Starting address */
                //     size_t iov_len;     /* Number of bytes to transfer */
                // };

                let mut n_read: usize = 0;
                match self.files.get(fd) {
                    Some(ReadOnly(content) | ReadWrite(content)) => {
                        for _ in 0..iovs_len {
                            let iov_base = read_u32(memory, ptr_iovs) as usize;
                            let iov_len = read_i32(memory, ptr_iovs + 4) as usize;
                            let remaining = content.len() - n_read;
                            let len = remaining.min(iov_len);
                            if len == 0 {
                                break;
                            }
                            memory[iov_base..][..len].copy_from_slice(&content[n_read..][..len]);
                            n_read += len;
                        }
                    }
                    Some(HostSystemFile) if fd == 0 => {
                        let mut stdin = io::stdin();
                        for _ in 0..iovs_len {
                            let iov_base = read_u32(memory, ptr_iovs) as usize;
                            let iov_len = read_i32(memory, ptr_iovs + 4) as usize;
                            match stdin.read(&mut memory[iov_base..][..iov_len]) {
                                Ok(n) => {
                                    n_read += n;
                                }
                                Err(_) => {
                                    break;
                                }
                            }
                        }
                    }
                    _ => return Some(Value::I32(Errno::Badf as i32)),
                };

                memory[ptr_nread..][..4].copy_from_slice(&(n_read as u32).to_le_bytes());
                success_code
            }
            "fd_readdir" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_renumber" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_seek" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_sync" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_tell" => todo!("WASI {}({:?})", function_name, arguments),
            "fd_write" => {
                use WasiFile::*;

                // file descriptor
                let fd = arguments[0].expect_i32().unwrap() as usize;
                // Array of IO vectors
                let ptr_iovs = arguments[1].expect_i32().unwrap() as usize;
                // Length of array
                let iovs_len = arguments[2].expect_i32().unwrap();
                // Out param: number of bytes written
                let ptr_nwritten = arguments[3].expect_i32().unwrap() as usize;

                // Grab a lock for stdout/stderr before the loop rather than re-acquiring over and over.
                // Not really necessary for other files, but it's easier to use the same structure.
                let mut write_lock = match self.files.get_mut(fd) {
                    Some(HostSystemFile) => match fd {
                        1 => WriteLock::StdOut(io::stdout().lock()),
                        2 => WriteLock::Stderr(io::stderr().lock()),
                        _ => return Some(Value::I32(Errno::Inval as i32)),
                    },
                    Some(WriteOnly(content) | ReadWrite(content)) => {
                        WriteLock::RegularFile(content)
                    }
                    _ => return Some(Value::I32(Errno::Badf as i32)),
                };

                let mut n_written: i32 = 0;
                let mut negative_length_count = 0;
                let mut write_result = Ok(());
                for i in 0..iovs_len {
                    // https://man7.org/linux/man-pages/man2/readv.2.html
                    // struct iovec {
                    //     void  *iov_base;    /* Starting address */
                    //     size_t iov_len;     /* Number of bytes to transfer */
                    // };
                    let ptr_iov = ptr_iovs + (8 * i as usize); // index into the array of iovec's
                    let iov_base = read_u32(memory, ptr_iov) as usize;
                    let iov_len = read_i32(memory, ptr_iov + 4);
                    if iov_len < 0 {
                        // I found negative-length iov's when I implemented this in JS for the web REPL (see wasi.js)
                        // I'm not sure why, but this solution worked, and it's the same WASI libc - there's only one.
                        n_written += iov_len;
                        negative_length_count += 1;
                        continue;
                    }
                    let bytes = &memory[iov_base..][..iov_len as usize];

                    write_result = match &mut write_lock {
                        WriteLock::StdOut(stdout) => stdout.write_all(bytes),
                        WriteLock::Stderr(stderr) => stderr.write_all(bytes),
                        WriteLock::RegularFile(content) => content.write_all(bytes),
                    };
                    if write_result.is_err() {
                        break;
                    }
                    n_written += bytes.len() as i32;
                }

                write_i32(memory, ptr_nwritten, n_written);
                if negative_length_count > 0 {
                    // Let's see if we ever get this message. If not, we can remove this negative-length stuff.
                    eprintln!(
                        "WASI DEV INFO: found {negative_length_count} negative-length iovecs."
                    );
                }

                match write_result {
                    Ok(()) => success_code,
                    Err(_) => Some(Value::I32(Errno::Io as i32)),
                }
            }
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
            "proc_exit" => {
                let exit_code = arguments[0].expect_i32().unwrap();
                exit(exit_code);
            }
            "proc_raise" => todo!("WASI {}({:?})", function_name, arguments),
            "sched_yield" => todo!("WASI {}({:?})", function_name, arguments),
            "random_get" => {
                // A pointer to a buffer where the random bytes will be written
                let ptr_buf = arguments[0].expect_i32().unwrap() as usize;
                // The number of bytes that will be written
                let buf_len = arguments[1].expect_i32().unwrap() as usize;
                for i in 0..buf_len {
                    memory[ptr_buf + i] = self.rng.gen();
                }
                success_code
            }
            "sock_recv" => todo!("WASI {}({:?})", function_name, arguments),
            "sock_send" => todo!("WASI {}({:?})", function_name, arguments),
            "sock_shutdown" => todo!("WASI {}({:?})", function_name, arguments),
            _ => panic!("Unknown WASI function {function_name}({arguments:?})"),
        }
    }
}

fn read_u32(memory: &[u8], addr: usize) -> u32 {
    let mut bytes = [0; 4];
    bytes.copy_from_slice(&memory[addr..][..4]);
    u32::from_le_bytes(bytes)
}

fn read_i32(memory: &[u8], addr: usize) -> i32 {
    let mut bytes = [0; 4];
    bytes.copy_from_slice(&memory[addr..][..4]);
    i32::from_le_bytes(bytes)
}

fn write_u32(memory: &mut [u8], addr: usize, value: u32) {
    memory[addr..][..4].copy_from_slice(&value.to_le_bytes());
}

fn write_i32(memory: &mut [u8], addr: usize, value: i32) {
    memory[addr..][..4].copy_from_slice(&value.to_le_bytes());
}

/// Error codes returned by functions.
/// Not all of these error codes are returned by the functions provided by this
/// API; some are used in higher-level library layers, and others are provided
/// merely for alignment with POSIX.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Errno {
    /// No error occurred. System call completed successfully.
    Success,
    /// Argument list too long.
    Toobig,
    /// Permission denied.
    Access,
    /// Address in use.
    Addrinuse,
    /// Address not available.
    Addrnotavail,
    /// Address family not supported.
    Afnosupport,
    /// Resource unavailable, or operation would block.
    Again,
    /// Connection already in progress.
    Already,
    /// Bad file descriptor.
    Badf,
    /// Bad message.
    Badmsg,
    /// Device or resource busy.
    Busy,
    /// Operation canceled.
    Canceled,
    /// No child processes.
    Child,
    /// Connection aborted.
    Connaborted,
    /// Connection refused.
    Connrefused,
    /// Connection reset.
    Connreset,
    /// Resource deadlock would occur.
    Deadlk,
    /// Destination address required.
    Destaddrreq,
    /// Mathematics argument out of domain of function.
    Dom,
    /// Reserved.
    Dquot,
    /// File exists.
    Exist,
    /// Bad address.
    Fault,
    /// File too large.
    Fbig,
    /// Host is unreachable.
    Hostunreach,
    /// Identifier removed.
    Idrm,
    /// Illegal byte sequence.
    Ilseq,
    /// Operation in progress.
    Inprogress,
    /// Interrupted function.
    Intr,
    /// Invalid argument.
    Inval,
    /// I/O error.
    Io,
    /// Socket is connected.
    Isconn,
    /// Is a directory.
    Isdir,
    /// Too many levels of symbolic links.
    Loop,
    /// File descriptor value too large.
    Mfile,
    /// Too many links.
    Mlink,
    /// Message too large.
    Msgsize,
    /// Reserved.
    Multihop,
    /// Filename too long.
    Nametoolong,
    /// Network is down.
    Netdown,
    /// Connection aborted by network.
    Netreset,
    /// Network unreachable.
    Netunreach,
    /// Too many files open in system.
    Nfile,
    /// No buffer space available.
    Nobufs,
    /// No such device.
    Nodev,
    /// No such file or directory.
    Noent,
    /// Executable file format error.
    Noexec,
    /// No locks available.
    Nolck,
    /// Reserved.
    Nolink,
    /// Not enough space.
    Nomem,
    /// No message of the desired type.
    Nomsg,
    /// Protocol not available.
    Noprotoopt,
    /// No space left on device.
    Nospc,
    /// Function not supported.
    Nosys,
    /// The socket is not connected.
    Notconn,
    /// Not a directory or a symbolic link to a directory.
    Notdir,
    /// Directory not empty.
    Notempty,
    /// State not recoverable.
    Notrecoverable,
    /// Not a socket.
    Notsock,
    /// Not supported, or operation not supported on socket.
    Notsup,
    /// Inappropriate I/O control operation.
    Notty,
    /// No such device or address.
    Nxio,
    /// Value too large to be stored in data type.
    Overflow,
    /// Previous owner died.
    Ownerdead,
    /// Operation not permitted.
    Perm,
    /// Broken pipe.
    Pipe,
    /// Protocol error.
    Proto,
    /// Protocol not supported.
    Protonosupport,
    /// Protocol wrong type for socket.
    Prototype,
    /// Result too large.
    Range,
    /// Read-only file system.
    Rofs,
    /// Invalid seek.
    Spipe,
    /// No such process.
    Srch,
    /// Reserved.
    Stale,
    /// Connection timed out.
    Timedout,
    /// Text file busy.
    Txtbsy,
    /// Cross-device link.
    Xdev,
    /// Extension: Capabilities insufficient.
    Notcapable,
}
