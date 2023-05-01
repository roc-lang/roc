const fs = require('fs/promises');
const { TextDecoder } = require('util');

async function roc_nodejs_platform_run(wasm_filename, callback) {
    const decoder = new TextDecoder();
    let wasmMemoryBuffer;
    let exit_code;

    function js_display_roc_string(str_bytes, str_len) {
        const utf8_bytes = wasmMemoryBuffer.subarray(str_bytes, str_bytes + str_len);
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
        },
    };

    const module_bytes = await fs.readFile(wasm_filename);
    const wasm = await WebAssembly.instantiate(module_bytes, importObj);

    wasmMemoryBuffer = new Uint8Array(wasm.instance.exports.memory.buffer);

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
        roc_nodejs_platform_run,
    };
}

// Run the code with a sample WebAssembly file
roc_nodejs_platform_run('./roc-app.wasm', (output) => {
    console.log(output);
});
