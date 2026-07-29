//! Windows Job Object support for subprocess trees in test runners.
//!
//! A child is spawned suspended, assigned to a kill-on-close job, and only
//! then resumed. This closes the race where the child could create a
//! descendant before the parent had put it in the job.

const std = @import("std");
const builtin = @import("builtin");

/// Native Job Object handle on Windows; zero-sized on other targets.
pub const Handle = if (builtin.os.tag == .windows) std.os.windows.HANDLE else void;

/// Failures while creating a job or attaching and starting its first process.
pub const Error = error{
    CreateJobObjectFailed,
    ConfigureJobObjectFailed,
    AssignProcessToJobObjectFailed,
    ResumeProcessFailed,
};

const windows_impl = if (builtin.os.tag == .windows) struct {
    const windows = std.os.windows;
    const HANDLE = windows.HANDLE;
    const BOOL = windows.BOOL;
    const DWORD = windows.DWORD;

    const job_object_limit_kill_on_job_close: DWORD = 0x00002000;
    const job_object_extended_limit_information: c_int = 9;
    const resume_thread_failed: DWORD = std.math.maxInt(DWORD);

    const BasicLimitInformation = extern struct {
        PerProcessUserTimeLimit: windows.LARGE_INTEGER,
        PerJobUserTimeLimit: windows.LARGE_INTEGER,
        LimitFlags: DWORD,
        MinimumWorkingSetSize: usize,
        MaximumWorkingSetSize: usize,
        ActiveProcessLimit: DWORD,
        Affinity: usize,
        PriorityClass: DWORD,
        SchedulingClass: DWORD,
    };

    const IoCounters = extern struct {
        ReadOperationCount: u64,
        WriteOperationCount: u64,
        OtherOperationCount: u64,
        ReadTransferCount: u64,
        WriteTransferCount: u64,
        OtherTransferCount: u64,
    };

    const ExtendedLimitInformation = extern struct {
        BasicLimitInformation: BasicLimitInformation,
        IoInfo: IoCounters,
        ProcessMemoryLimit: usize,
        JobMemoryLimit: usize,
        PeakProcessMemoryUsed: usize,
        PeakJobMemoryUsed: usize,
    };

    extern "kernel32" fn CreateJobObjectW(
        job_attributes: ?*anyopaque,
        name: ?[*:0]const u16,
    ) callconv(.winapi) ?HANDLE;

    extern "kernel32" fn SetInformationJobObject(
        job: HANDLE,
        info_class: c_int,
        info: *anyopaque,
        info_len: DWORD,
    ) callconv(.winapi) BOOL;

    extern "kernel32" fn AssignProcessToJobObject(
        job: HANDLE,
        process: HANDLE,
    ) callconv(.winapi) BOOL;

    extern "kernel32" fn TerminateJobObject(
        job: HANDLE,
        exit_code: c_uint,
    ) callconv(.winapi) BOOL;

    extern "kernel32" fn ResumeThread(thread: HANDLE) callconv(.winapi) DWORD;
} else struct {};

/// Create a Job Object that terminates all associated processes when closed.
pub fn create() Error!Handle {
    if (comptime builtin.os.tag != .windows) return {};

    const job = windows_impl.CreateJobObjectW(null, null) orelse
        return error.CreateJobObjectFailed;
    errdefer std.os.windows.CloseHandle(job);

    var info: windows_impl.ExtendedLimitInformation = std.mem.zeroes(windows_impl.ExtendedLimitInformation);
    info.BasicLimitInformation.LimitFlags = windows_impl.job_object_limit_kill_on_job_close;
    if (windows_impl.SetInformationJobObject(
        job,
        windows_impl.job_object_extended_limit_information,
        &info,
        @sizeOf(windows_impl.ExtendedLimitInformation),
    ) == .FALSE) return error.ConfigureJobObjectFailed;

    return job;
}

/// Assign a suspended child process to `job` before it can spawn descendants.
pub fn assign(job: Handle, process: std.process.Child.Id) Error!void {
    if (comptime builtin.os.tag == .windows) {
        if (windows_impl.AssignProcessToJobObject(job, process) == .FALSE) {
            return error.AssignProcessToJobObjectFailed;
        }
    }
}

/// Resume a child after it has been successfully assigned to its Job Object.
pub fn resumeChild(child: *std.process.Child) Error!void {
    if (comptime builtin.os.tag == .windows) {
        if (windows_impl.ResumeThread(child.thread_handle) == windows_impl.resume_thread_failed) {
            return error.ResumeProcessFailed;
        }
    }
}

/// Terminate every process currently associated with `job`.
pub fn terminate(job: Handle, exit_code: c_uint) void {
    if (comptime builtin.os.tag == .windows) {
        _ = windows_impl.TerminateJobObject(job, exit_code);
    }
}

/// Close a Job Object handle, triggering its kill-on-close policy if needed.
pub fn close(job: Handle) void {
    if (comptime builtin.os.tag == .windows) {
        std.os.windows.CloseHandle(job);
    }
}

test "Windows Job Object declarations compile" {
    std.testing.refAllDecls(@This());
}
