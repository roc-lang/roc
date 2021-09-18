const test_decoder = true;

async function roc_web_platform_run(wasm_filename, dom_node) {
  const decoder = new TextDecoder();
  let memory_bytes;
  let exit_code;

  function js_display_roc_string(str_bytes, str_len) {
    const utf8_bytes = memory_bytes.subarray(str_bytes, str_bytes + str_len);
    const js_string = decoder.decode(utf8_bytes);
    dom_node.textContent = js_string;
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
    },
  };

  const wasm = await WebAssembly.instantiateStreaming(
    fetch(wasm_filename),
    importObj
  );

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
