const fs = require('fs');
const { TextDecoder } = require('util');

const wasmFilename = './roc-app.wasm';
const moduleBytes = fs.readFileSync(wasmFilename);
const wasmModule = new WebAssembly.Module(moduleBytes);

function hello() {
    const decoder = new TextDecoder();
    let wasmMemoryBuffer;
    let exitCode;
    let returnVal;

    function js_display_roc_string(strBytes, strLen) {
        const utf8Bytes = wasmMemoryBuffer.subarray(strBytes, strBytes + strLen);

        returnVal = decoder.decode(utf8Bytes);
    }

    const importObj = {
        wasi_snapshot_preview1: {
            proc_exit: (code) => {
                if (code !== 0) {
                    console.error(`Exited with code ${code}`);
                }
                exitCode = code;
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

    const instance = new WebAssembly.Instance(wasmModule, importObj);

    wasmMemoryBuffer = new Uint8Array(instance.exports.memory.buffer);

    try {
        instance.exports._start();
    } catch (e) {
        const isOk = e.message === "unreachable" && exitCode === 0;

        if (!isOk) {
            throw e;
        }
    }

    return returnVal;
}

if (typeof module === "object") {
    module.exports = { hello };
}

// As an example, run hello() from Roc and print the string it returns
console.log(hello());
