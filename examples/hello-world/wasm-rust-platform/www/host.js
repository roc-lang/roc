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
        throw "Roc panicked!";
      },
    },
    env: {},
  };

  // `instantiateStreaming` is faster than `instantiate`, especially if you don't `await` the response,
  // so it can start compiling from the first received packet. Available in all browsers, but not Node.
  const responsePromise = fetch(wasm_filename);
  const wasm = await WebAssembly.instantiateStreaming(
    responsePromise,
    importObj
  );

  try {
    wasm.instance.exports.rust_main();
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
