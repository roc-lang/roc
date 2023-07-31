#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

mod command_glue;
mod file_glue;
mod glue;
mod tcp_glue;

pub use glue::*;

use roc_std::{RocDict, RocList, RocResult, RocStr};
use std::borrow::{Borrow, Cow};
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Read, Write};
use std::net::TcpStream;
use std::path::Path;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use file_glue::ReadErr;
use file_glue::WriteErr;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct RocFunction_89 {
    closure_data: roc_std::RocList<std::mem::MaybeUninit<u8>>,
}

impl RocFunction_89 {
    pub fn force_thunk(self) -> roc_std::RocStr {
        extern "C" {
            fn roc__mainForHost_0_caller(
                arg0: *const (),
                closure_data: *mut u8,
                output: *mut roc_std::RocStr,
            );
        }

        let mut output = core::mem::MaybeUninit::uninit();
        let closure_ptr =
            (&mut core::mem::ManuallyDrop::new(self.closure_data)) as *mut _ as *mut u8;

        unsafe {
            roc__mainForHost_0_caller(&(), closure_ptr, output.as_mut_ptr());

            output.assume_init()
        }
    }
}

#[no_mangle]
pub fn mainForHost(arg0: roc_std::RocStr) -> RocFunction_89 {
    extern "C" {
        fn roc__mainForHost_1_exposed_generic(
            _: *mut RocFunction_89,
            _: &mut core::mem::ManuallyDrop<roc_std::RocStr>,
        );
    }

    let mut ret = core::mem::MaybeUninit::uninit();

    unsafe {
        roc__mainForHost_1_exposed_generic(
            ret.as_mut_ptr(),
            &mut core::mem::ManuallyDrop::new(arg0),
        );

        ret.assume_init()
    }
}

pub fn main(roc_str: RocStr) -> roc_std::RocStr {
    let task = mainForHost(roc_str);

    task.force_thunk()
}

#[no_mangle]
pub extern "C" fn roc_fx_envDict() -> RocDict<RocStr, RocStr> {
    std::env::vars_os()
        .map(|(key, val)| {
            (
                RocStr::from(key.to_string_lossy().borrow()),
                RocStr::from(val.to_string_lossy().borrow()),
            )
        })
        .collect()
}

#[no_mangle]
pub extern "C" fn roc_fx_args() -> RocList<RocStr> {
    std::env::args_os()
        .map(|os_str| RocStr::from(os_str.to_string_lossy().borrow()))
        .collect()
}

#[no_mangle]
pub extern "C" fn roc_fx_setCwd(roc_path: &RocList<u8>) -> RocResult<(), ()> {
    match std::env::set_current_dir(path_from_roc_path(roc_path)) {
        Ok(()) => RocResult::ok(()),
        Err(_) => RocResult::err(()),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_exePath(_roc_str: &RocStr) -> RocResult<RocList<u8>, ()> {
    match std::env::current_exe() {
        Ok(path_buf) => RocResult::ok(os_str_to_roc_path(path_buf.as_path().as_os_str())),
        Err(_) => RocResult::err(()),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_stdinLine() -> RocStr {
    let stdin = std::io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from(line1.as_str())
}

#[no_mangle]
pub extern "C" fn roc_fx_stdinBytes() -> RocList<u8> {
    let stdin = std::io::stdin();
    let mut buffer: [u8; 256] = [0; 256];

    match stdin.lock().read(&mut buffer) {
        Ok(bytes_read) => RocList::from(&buffer[0..bytes_read]),
        Err(_) => RocList::from((&[]).as_slice()),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_stdoutLine(line: &RocStr) {
    let string = line.as_str();
    println!("{}", string);
}

#[no_mangle]
pub extern "C" fn roc_fx_stdoutWrite(text: &RocStr) {
    let string = text.as_str();
    print!("{}", string);
    std::io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn roc_fx_stderrLine(line: &RocStr) {
    let string = line.as_str();
    eprintln!("{}", string);
}

#[no_mangle]
pub extern "C" fn roc_fx_stderrWrite(text: &RocStr) {
    let string = text.as_str();
    eprint!("{}", string);
    std::io::stderr().flush().unwrap();
}

// #[no_mangle]
// pub extern "C" fn roc_fx_fileWriteUtf8(
//     roc_path: &RocList<u8>,
//     roc_string: &RocStr,
//     // ) -> RocResult<(), WriteErr> {
// ) -> (u8, u8) {
//     let _ = write_slice(roc_path, roc_string.as_str().as_bytes());

//     (255, 255)
// }

// #[no_mangle]
// pub extern "C" fn roc_fx_fileWriteUtf8(roc_path: &RocList<u8>, roc_string: &RocStr) -> Fail {
//     write_slice2(roc_path, roc_string.as_str().as_bytes())
// }
#[no_mangle]
pub extern "C" fn roc_fx_fileWriteUtf8(
    roc_path: &RocList<u8>,
    roc_str: &RocStr,
) -> RocResult<(), WriteErr> {
    write_slice(roc_path, roc_str.as_str().as_bytes())
}

#[no_mangle]
pub extern "C" fn roc_fx_fileWriteBytes(
    roc_path: &RocList<u8>,
    roc_bytes: &RocList<u8>,
) -> RocResult<(), WriteErr> {
    write_slice(roc_path, roc_bytes.as_slice())
}

fn write_slice(roc_path: &RocList<u8>, bytes: &[u8]) -> RocResult<(), WriteErr> {
    match File::create(path_from_roc_path(roc_path)) {
        Ok(mut file) => match file.write_all(bytes) {
            Ok(()) => RocResult::ok(()),
            Err(err) => RocResult::err(toRocWriteError(err)),
        },
        Err(err) => RocResult::err(toRocWriteError(err)),
    }
}

#[cfg(target_family = "unix")]
fn path_from_roc_path(bytes: &RocList<u8>) -> Cow<'_, Path> {
    use std::os::unix::ffi::OsStrExt;
    let os_str = OsStr::from_bytes(bytes.as_slice());
    Cow::Borrowed(Path::new(os_str))
}

#[cfg(target_family = "windows")]
fn path_from_roc_path(bytes: &RocList<u8>) -> Cow<'_, Path> {
    use std::os::windows::ffi::OsStringExt;

    let bytes = bytes.as_slice();
    assert_eq!(bytes.len() % 2, 0);
    let characters: &[u16] =
        unsafe { std::slice::from_raw_parts(bytes.as_ptr().cast(), bytes.len() / 2) };

    let os_string = std::ffi::OsString::from_wide(characters);

    Cow::Owned(std::path::PathBuf::from(os_string))
}

#[no_mangle]
pub extern "C" fn roc_fx_fileReadBytes(roc_path: &RocList<u8>) -> RocResult<RocList<u8>, ReadErr> {
    let mut bytes = Vec::new();

    match File::open(path_from_roc_path(roc_path)) {
        Ok(mut file) => match file.read_to_end(&mut bytes) {
            Ok(_bytes_read) => RocResult::ok(RocList::from(bytes.as_slice())),
            Err(err) => RocResult::err(toRocReadError(err)),
        },
        Err(err) => RocResult::err(toRocReadError(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_fileDelete(roc_path: &RocList<u8>) -> RocResult<(), ReadErr> {
    match std::fs::remove_file(path_from_roc_path(roc_path)) {
        Ok(()) => RocResult::ok(()),
        Err(err) => RocResult::err(toRocReadError(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_cwd() -> RocList<u8> {
    // TODO instead, call getcwd on UNIX and GetCurrentDirectory on Windows
    match std::env::current_dir() {
        Ok(path_buf) => os_str_to_roc_path(path_buf.into_os_string().as_os_str()),
        Err(_) => {
            // Default to empty path
            RocList::empty()
        }
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_posixTime() -> roc_std::U128 {
    // TODO in future may be able to avoid this panic by using C APIs
    let since_epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards");

    roc_std::U128::from(since_epoch.as_nanos())
}

#[no_mangle]
pub extern "C" fn roc_fx_sleepMillis(milliseconds: u64) {
    let duration = Duration::from_millis(milliseconds);
    std::thread::sleep(duration);
}

#[no_mangle]
pub extern "C" fn roc_fx_dirList(
    // TODO: this RocResult should use Dir.WriteErr - but right now it's File.WriteErr
    // because glue doesn't have Dir.WriteErr yet.
    roc_path: &RocList<u8>,
) -> RocResult<RocList<RocList<u8>>, WriteErr> {
    println!("Dir.list...");
    match std::fs::read_dir(path_from_roc_path(roc_path)) {
        Ok(dir_entries) => RocResult::ok(
            dir_entries
                .map(|opt_dir_entry| match opt_dir_entry {
                    Ok(entry) => os_str_to_roc_path(entry.path().into_os_string().as_os_str()),
                    Err(_) => {
                        todo!("handle dir_entry path didn't resolve")
                    }
                })
                .collect::<RocList<RocList<u8>>>(),
        ),
        Err(err) => RocResult::err(toRocWriteError(err)),
    }
}

#[cfg(target_family = "unix")]
fn os_str_to_roc_path(os_str: &OsStr) -> RocList<u8> {
    use std::os::unix::ffi::OsStrExt;

    RocList::from(os_str.as_bytes())
}

#[cfg(target_family = "windows")]
fn os_str_to_roc_path(os_str: &OsStr) -> RocList<u8> {
    use std::os::windows::ffi::OsStrExt;

    let bytes: Vec<_> = os_str.encode_wide().flat_map(|c| c.to_be_bytes()).collect();

    RocList::from(bytes.as_slice())
}

fn toRocWriteError(err: std::io::Error) -> file_glue::WriteErr {
    match err.kind() {
        ErrorKind::NotFound => file_glue::WriteErr::NotFound,
        ErrorKind::AlreadyExists => file_glue::WriteErr::AlreadyExists,
        ErrorKind::Interrupted => file_glue::WriteErr::Interrupted,
        ErrorKind::OutOfMemory => file_glue::WriteErr::OutOfMemory,
        ErrorKind::PermissionDenied => file_glue::WriteErr::PermissionDenied,
        ErrorKind::TimedOut => file_glue::WriteErr::TimedOut,
        // TODO investigate support the following IO errors may need to update API
        ErrorKind::WriteZero => file_glue::WriteErr::WriteZero,
        _ => file_glue::WriteErr::Unsupported,
        // TODO investigate support the following IO errors
        // std::io::ErrorKind::FileTooLarge <- unstable language feature
        // std::io::ErrorKind::ExecutableFileBusy <- unstable language feature
        // std::io::ErrorKind::FilesystemQuotaExceeded <- unstable language feature
        // std::io::ErrorKind::InvalidFilename <- unstable language feature
        // std::io::ErrorKind::ResourceBusy <- unstable language feature
        // std::io::ErrorKind::ReadOnlyFilesystem <- unstable language feature
        // std::io::ErrorKind::TooManyLinks <- unstable language feature
        // std::io::ErrorKind::StaleNetworkFileHandle <- unstable language feature
        // std::io::ErrorKind::StorageFull <- unstable language feature
    }
}

fn toRocReadError(err: std::io::Error) -> file_glue::ReadErr {
    match err.kind() {
        ErrorKind::Interrupted => file_glue::ReadErr::Interrupted,
        ErrorKind::NotFound => file_glue::ReadErr::NotFound,
        ErrorKind::OutOfMemory => file_glue::ReadErr::OutOfMemory,
        ErrorKind::PermissionDenied => file_glue::ReadErr::PermissionDenied,
        ErrorKind::TimedOut => file_glue::ReadErr::TimedOut,
        // TODO investigate support the following IO errors may need to update API
        // std::io::ErrorKind:: => file_glue::ReadErr::TooManyHardlinks,
        // std::io::ErrorKind:: => file_glue::ReadErr::TooManySymlinks,
        // std::io::ErrorKind:: => file_glue::ReadErr::Unrecognized,
        // std::io::ErrorKind::StaleNetworkFileHandle <- unstable language feature
        // std::io::ErrorKind::InvalidFilename <- unstable language feature
        _ => file_glue::ReadErr::Unsupported,
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpConnect(host: &RocStr, port: u16) -> tcp_glue::ConnectResult {
    match TcpStream::connect((host.as_str(), port)) {
        Ok(stream) => {
            let reader = BufReader::new(stream);
            let ptr = Box::into_raw(Box::new(reader)) as u64;

            tcp_glue::ConnectResult::Connected(ptr)
        }
        Err(err) => tcp_glue::ConnectResult::Error(to_tcp_connect_err(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpClose(stream_ptr: *mut BufReader<TcpStream>) {
    unsafe {
        drop(Box::from_raw(stream_ptr));
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpReadUpTo(
    bytes_to_read: usize,
    stream_ptr: *mut BufReader<TcpStream>,
) -> tcp_glue::ReadResult {
    let reader = unsafe { &mut *stream_ptr };

    let mut chunk = reader.take(bytes_to_read as u64);

    match chunk.fill_buf() {
        Ok(received) => {
            let received = received.to_vec();
            reader.consume(received.len());

            let rocList = RocList::from(&received[..]);
            tcp_glue::ReadResult::Read(rocList)
        }

        Err(err) => tcp_glue::ReadResult::Error(to_tcp_stream_err(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpReadExactly(
    bytes_to_read: usize,
    stream_ptr: *mut BufReader<TcpStream>,
) -> tcp_glue::ReadExactlyResult {
    let reader = unsafe { &mut *stream_ptr };

    let mut buffer = Vec::with_capacity(bytes_to_read);
    let mut chunk = reader.take(bytes_to_read as u64);

    match chunk.read_to_end(&mut buffer) {
        Ok(read) => {
            if read < bytes_to_read {
                tcp_glue::ReadExactlyResult::UnexpectedEOF
            } else {
                let rocList = RocList::from(&buffer[..]);
                tcp_glue::ReadExactlyResult::Read(rocList)
            }
        }

        Err(err) => tcp_glue::ReadExactlyResult::Error(to_tcp_stream_err(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpReadUntil(
    byte: u8,
    stream_ptr: *mut BufReader<TcpStream>,
) -> tcp_glue::ReadResult {
    let reader = unsafe { &mut *stream_ptr };

    let mut buffer = vec![];

    match reader.read_until(byte, &mut buffer) {
        Ok(_) => {
            let rocList = RocList::from(&buffer[..]);
            tcp_glue::ReadResult::Read(rocList)
        }

        Err(err) => tcp_glue::ReadResult::Error(to_tcp_stream_err(err)),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_tcpWrite(
    msg: &RocList<u8>,
    stream_ptr: *mut BufReader<TcpStream>,
) -> tcp_glue::WriteResult {
    let reader = unsafe { &mut *stream_ptr };
    let mut stream = reader.get_ref();

    match stream.write_all(msg.as_slice()) {
        Ok(_) => tcp_glue::WriteResult::Wrote,
        Err(err) => tcp_glue::WriteResult::Error(to_tcp_stream_err(err)),
    }
}

fn to_tcp_connect_err(err: std::io::Error) -> tcp_glue::ConnectErr {
    let kind = err.kind();
    match kind {
        ErrorKind::PermissionDenied => tcp_glue::ConnectErr::PermissionDenied,
        ErrorKind::AddrInUse => tcp_glue::ConnectErr::AddrInUse,
        ErrorKind::AddrNotAvailable => tcp_glue::ConnectErr::AddrNotAvailable,
        ErrorKind::ConnectionRefused => tcp_glue::ConnectErr::ConnectionRefused,
        ErrorKind::Interrupted => tcp_glue::ConnectErr::Interrupted,
        ErrorKind::TimedOut => tcp_glue::ConnectErr::TimedOut,
        ErrorKind::Unsupported => tcp_glue::ConnectErr::Unsupported,
        _ => tcp_glue::ConnectErr::Unrecognized(
            RocStr::from(kind.to_string().borrow()),
            err.raw_os_error().unwrap_or_default(),
        ),
    }
}

fn to_tcp_stream_err(err: std::io::Error) -> tcp_glue::StreamErr {
    let kind = err.kind();
    match kind {
        ErrorKind::PermissionDenied => tcp_glue::StreamErr::PermissionDenied,
        ErrorKind::ConnectionRefused => tcp_glue::StreamErr::ConnectionRefused,
        ErrorKind::ConnectionReset => tcp_glue::StreamErr::ConnectionReset,
        ErrorKind::Interrupted => tcp_glue::StreamErr::Interrupted,
        ErrorKind::OutOfMemory => tcp_glue::StreamErr::OutOfMemory,
        ErrorKind::BrokenPipe => tcp_glue::StreamErr::BrokenPipe,
        _ => tcp_glue::StreamErr::Unrecognized(
            RocStr::from(kind.to_string().borrow()),
            err.raw_os_error().unwrap_or_default(),
        ),
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_commandStatus(
    roc_cmd: &command_glue::Command,
) -> RocResult<(), command_glue::CommandErr> {
    let args = roc_cmd.args.into_iter().map(|arg| arg.as_str());
    let num_envs = roc_cmd.envs.len() / 2;
    let flat_envs = &roc_cmd.envs;

    // Environment vairables must be passed in key=value pairs
    assert_eq!(flat_envs.len() % 2, 0);

    let mut envs = Vec::with_capacity(num_envs);
    for chunk in flat_envs.chunks(2) {
        let key = chunk[0].as_str();
        let value = chunk[1].as_str();
        envs.push((key, value));
    }

    // Create command
    let mut cmd = std::process::Command::new(roc_cmd.program.as_str());

    // Set arguments
    cmd.args(args);

    // Clear environment variables if cmd.clearEnvs set
    // otherwise inherit environment variables if cmd.clearEnvs is not set
    if roc_cmd.clearEnvs {
        cmd.env_clear();
    };

    // Set environment variables
    cmd.envs(envs);

    match cmd.status() {
        Ok(status) => {
            if status.success() {
                RocResult::ok(())
            } else {
                match status.code() {
                    Some(code) => {
                        let error = command_glue::CommandErr::ExitCode(code);
                        RocResult::err(error)
                    }
                    None => {
                        // If no exit code is returned, the process was terminated by a signal.
                        let error = command_glue::CommandErr::KilledBySignal();
                        RocResult::err(error)
                    }
                }
            }
        }
        Err(err) => {
            let str = RocStr::from(err.to_string().borrow());
            let error = command_glue::CommandErr::IOError(str);
            RocResult::err(error)
        }
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_commandOutput(roc_cmd: &command_glue::Command) -> command_glue::Output {
    let args = roc_cmd.args.into_iter().map(|arg| arg.as_str());
    let num_envs = roc_cmd.envs.len() / 2;
    let flat_envs = &roc_cmd.envs;

    // Environment vairables must be passed in key=value pairs
    assert_eq!(flat_envs.len() % 2, 0);

    let mut envs = Vec::with_capacity(num_envs);
    for chunk in flat_envs.chunks(2) {
        let key = chunk[0].as_str();
        let value = chunk[1].as_str();
        envs.push((key, value));
    }

    // Create command
    let mut cmd = std::process::Command::new(roc_cmd.program.as_str());

    // Set arguments
    cmd.args(args);

    // Clear environment variables if cmd.clearEnvs set
    // otherwise inherit environment variables if cmd.clearEnvs is not set
    if roc_cmd.clearEnvs {
        cmd.env_clear();
    };

    // Set environment variables
    cmd.envs(envs);

    match cmd.output() {
        Ok(output) => {
            // Status of the child process, successful/exit code/killed by signal
            let status = if output.status.success() {
                RocResult::ok(())
            } else {
                match output.status.code() {
                    Some(code) => {
                        let error = command_glue::CommandErr::ExitCode(code);
                        RocResult::err(error)
                    }
                    None => {
                        // If no exit code is returned, the process was terminated by a signal.
                        let error = command_glue::CommandErr::KilledBySignal();
                        RocResult::err(error)
                    }
                }
            };

            command_glue::Output {
                status: status,
                stdout: RocList::from(&output.stdout[..]),
                stderr: RocList::from(&output.stderr[..]),
            }
        }
        Err(err) => command_glue::Output {
            status: RocResult::err(command_glue::CommandErr::IOError(RocStr::from(
                err.to_string().borrow(),
            ))),
            stdout: RocList::empty(),
            stderr: RocList::empty(),
        },
    }
}
