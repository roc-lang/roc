async function roc_web_platform_run(wasm_filename, callback) {
  const decoder = new TextDecoder();
  let memory_bytes;
  let exit_code;

  function js_display_roc_string(str_bytes, str_len) {
    const utf8_bytes = memory_bytes.subarray(str_bytes, str_bytes + str_len);
    const js_string = decoder.decode(utf8_bytes);
    callback(js_string);
  }

  const importObj = {
    wasi_snapshot_preview1: {
      proc_exit: (code) => {
        if (code !== 0) {
          console.error(`Exited with code ${code}`);
        }
        exit_code = code;
      },
      fd_write: (x) => {
        console.error(`fd_write not supported: ${x}`);
      },
    },
    env: {
      js_display_roc_string,
      roc_panic: (_pointer, _tag_id) => {
        throw "Roc panicked!";
      },
      roc_dbg: (_loc, _msg) => {
        // TODO write a proper impl.
        throw "Roc dbg not supported!";
      },
    },
  };

  const fetchPromise = fetch(wasm_filename);

  let wasm;
  if (WebAssembly.instantiateStreaming) {
    // streaming API has better performance if available
    // It can start compiling Wasm before it has fetched all of the bytes, so we don't `await` the request!
    wasm = await WebAssembly.instantiateStreaming(fetchPromise, importObj);
  } else {
    const response = await fetchPromise;
    const module_bytes = await response.arrayBuffer();
    wasm = await WebAssembly.instantiate(module_bytes, importObj);
  }

  memory_bytes = new Uint8Array(wasm.instance.exports.memory.buffer);

  try {
    wasm.instance.exports._start();
  } catch (e) {
    const is_ok = e.message === "unreachable" && exit_code === 0;
    if (!is_ok) {
      console.error(e);
    }
  }
}

if (typeof module !== "undefined") {
  module.exports = {
    roc_web_platform_run,
  };
}
