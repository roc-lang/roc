async function roc_web_platform_run(wasm_filename) {
  let exit_code;

  const importObj = {
    wasi_snapshot_preview1: {
      proc_exit: (code) => {
        if (code !== 0) {
          console.error(`Exited with code ${code}`);
        }
        exit_code = code;
      },
        roc_panic: (_pointer, _tag_id) => {
            throw 'Roc panicked!';
        }
    },
    env: {
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

  try {
    wasm.instance.exports._start();
  } catch (e) {
    const is_ok = e.message === "unreachable" && exit_code === 0;
    if (!is_ok) {
      console.error(e);
    }
  }
}

if (typeof module !== 'undefined') {
  module.exports = {
    roc_web_platform_run,
  };
}
