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
    },
    env: {
      js_display_roc_string,
      roc_panic: (_pointer, _tag_id) => {
        throw "Roc panicked!";
      },
    },
  };

  let wasm;

  const response = await fetch(wasm_filename);

  if (WebAssembly.instantiateStreaming) {
    // streaming API has better performance if available
    wasm = await WebAssembly.instantiateStreaming(response, importObj);
  } else {
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
