/**
 * Browser implementation of the WebAssembly System Interface (WASI)
 * The REPL generates an "app" from the user's Roc code and it can import from WASI
 *
 * We only implement writes to stdout/stderr as console.log/console.err, and proc_exit as `throw`
 * The rest of the interface just consists of dummy functions with the right number of arguments
 *
 * The wasiLinkObject provides a reference to the app so we can write to its memory
 */

export function getMockWasiImports(wasiLinkObject) {
  const decoder = new TextDecoder();

  // If the app ever resizes its memory, there will be a new buffer instance
  // so we get a fresh reference every time, just in case
  function getMemory8() {
    return new Uint8Array(wasiLinkObject.instance.exports.memory.buffer);
  }

  function getMemory32() {
    return new Uint32Array(wasiLinkObject.instance.exports.memory.buffer);
  }

  // fd_close : (i32) -> i32
  // Close a file descriptor. Note: This is similar to close in POSIX.
  // https://docs.rs/wasi/latest/wasi/wasi_snapshot_preview1/fn.fd_close.html
  function fd_close(fd) {
    console.warn(`fd_close: ${{ fd }}`);
    return 0; // error code
  }

  // fd_fdstat_get : (i32, i32) -> i32
  // Get the attributes of a file descriptor.
  // https://docs.rs/wasi/latest/wasi/wasi_snapshot_preview1/fn.fd_fdstat_get.html
  function fd_fdstat_get(fd, stat_mut_ptr) {
    /*
    Tell WASI that stdout is a tty (no seek or tell)

    https://github.com/WebAssembly/wasi-libc/blob/659ff414560721b1660a19685110e484a081c3d4/libc-bottom-half/sources/isatty.c

    *Not* a tty if:
        (statbuf.fs_filetype != __WASI_FILETYPE_CHARACTER_DEVICE ||
            (statbuf.fs_rights_base & (__WASI_RIGHTS_FD_SEEK | __WASI_RIGHTS_FD_TELL)) != 0)

    So it's sufficient to set:
        .fs_filetype = __WASI_FILETYPE_CHARACTER_DEVICE
        .fs_rights_base = 0

    https://github.com/WebAssembly/wasi-libc/blob/659ff414560721b1660a19685110e484a081c3d4/libc-bottom-half/headers/public/wasi/api.h

        typedef uint8_t __wasi_filetype_t;
        typedef uint16_t __wasi_fdflags_t;
        typedef uint64_t __wasi_rights_t;
        #define __WASI_FILETYPE_CHARACTER_DEVICE (UINT8_C(2))
        typedef struct __wasi_fdstat_t { // 24 bytes total
            __wasi_filetype_t fs_filetype;        // 1 byte
                                                    // 1 byte padding
            __wasi_fdflags_t fs_flags;            // 2 bytes
                                                    // 4 bytes padding
            __wasi_rights_t fs_rights_base;       // 8 bytes
            __wasi_rights_t fs_rights_inheriting; // 8 bytes
        } __wasi_fdstat_t;
    */
    // console.warn(`fd_fdstat_get: ${{ fd, stat_mut_ptr }}`);
    const WASI_FILETYPE_CHARACTER_DEVICE = 2;
    const memory8 = getMemory8();
    memory8[stat_mut_ptr] = WASI_FILETYPE_CHARACTER_DEVICE;
    memory8.slice(stat_mut_ptr + 1, stat_mut_ptr + 24).fill(0);

    return 0; // error code
  }

  // fd_seek : (i32, i64, i32, i32) -> i32
  // Move the offset of a file descriptor. Note: This is similar to lseek in POSIX.
  // https://docs.rs/wasi/latest/wasi/wasi_snapshot_preview1/fn.fd_seek.html
  function fd_seek(fd, offset, whence, newoffset_mut_ptr) {
    console.warn(`fd_seek: ${{ fd, offset, whence, newoffset_mut_ptr }}`);
    return 0;
  }

  // fd_write : (i32, i32, i32, i32) -> i32
  // Write to a file descriptor. Note: This is similar to `writev` in POSIX.
  // https://docs.rs/wasi/latest/wasi/wasi_snapshot_preview1/fn.fd_write.html
  function fd_write(fd, iovs_ptr, iovs_len, nwritten_mut_ptr) {
    let string_buffer = "";
    let nwritten = 0;
    const memory32 = getMemory32();

    for (let i = 0; i < iovs_len; i++) {
      const index32 = iovs_ptr >> 2;
      const base = memory32[index32];
      const len = memory32[index32 + 1];
      iovs_ptr += 8;

      if (!len) continue;

      nwritten += len;

      // For some reason we often get negative-looking buffer lengths with junk data.
      // Just skip the console.log, but still increase nwritten or it will loop forever.
      // Dunno why this happens, but it's working fine for printf debugging ¯\_(ツ)_/¯
      if (len >> 31) {
        break;
      }

      const buf = getMemory8().slice(base, base + len);
      const chunk = decoder.decode(buf);
      string_buffer += chunk;
    }
    memory32[nwritten_mut_ptr >> 2] = nwritten;
    if (string_buffer) {
      console.log(string_buffer);
    }
    return 0;
  }

  // proc_exit : (i32) -> nil
  function proc_exit(exit_code) {
    if (exit_code) {
      throw new Error(`Wasm exited with code ${exit_code}`);
    }
  }

  // Signatures from wasm_test_platform.o
  const sig2 = (i32) => {};
  const sig6 = (i32a, i32b) => 0;
  const sig7 = (i32a, i32b, i32c) => 0;
  const sig9 = (i32a, i64b, i32c) => 0;
  const sig10 = (i32a, i64b, i64c, i32d) => 0;
  const sig11 = (i32a, i64b, i64c) => 0;
  const sig12 = (i32a) => 0;
  const sig13 = (i32a, i64b) => 0;
  const sig14 = (i32a, i32b, i32c, i64d, i32e) => 0;
  const sig15 = (i32a, i32b, i32c, i32d) => 0;
  const sig16 = (i32a, i64b, i32c, i32d) => 0;
  const sig17 = (i32a, i32b, i32c, i32d, i32e) => 0;
  const sig18 = (i32a, i32b, i32c, i32d, i64e, i64f, i32g) => 0;
  const sig19 = (i32a, i32b, i32c, i32d, i32e, i32f, i32g) => 0;
  const sig20 = (i32a, i32b, i32c, i32d, i32e, i64f, i64g, i32h, i32i) => 0;
  const sig21 = (i32a, i32b, i32c, i32d, i32e, i32f) => 0;
  const sig22 = () => 0;

  return {
    wasi_snapshot_preview1: {
      args_get: sig6,
      args_sizes_get: sig6,
      environ_get: sig6,
      environ_sizes_get: sig6,
      clock_res_get: sig6,
      clock_time_get: sig9,
      fd_advise: sig10,
      fd_allocate: sig11,
      fd_close,
      fd_datasync: sig12,
      fd_fdstat_get,
      fd_fdstat_set_flags: sig6,
      fd_fdstat_set_rights: sig11,
      fd_filestat_get: sig6,
      fd_filestat_set_size: sig13,
      fd_filestat_set_times: sig10,
      fd_pread: sig14,
      fd_prestat_get: sig6,
      fd_prestat_dir_name: sig7,
      fd_pwrite: sig14,
      fd_read: sig15,
      fd_readdir: sig14,
      fd_renumber: sig6,
      fd_seek,
      fd_sync: sig12,
      fd_tell: sig6,
      fd_write,
      path_create_directory: sig7,
      path_filestat_get: sig17,
      path_filestat_set_times: sig18,
      path_link: sig19,
      path_open: sig20,
      path_readlink: sig21,
      path_remove_directory: sig7,
      path_rename: sig21,
      path_symlink: sig17,
      path_unlink_file: sig7,
      poll_oneoff: sig15,
      proc_exit,
      proc_raise: sig12,
      sched_yield: sig22,
      random_get: sig6,
      sock_recv: sig21,
      sock_send: sig17,
      sock_shutdown: sig6,
    },
  };
}
