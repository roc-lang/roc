const fs = require('fs');
const { TextDecoder } = require('util');

const wasmFilename = './roc-app.wasm';
const moduleBytes = fs.readFileSync(wasmFilename);
const wasmModule = new WebAssembly.Module(moduleBytes);

let allocatedBytes = 0;

function alloc(bytes, align) {
    const ret = allocatedBytes; // TODO handle alignment here

    allocatedBytes = ret + bytes;

    // TODO grow the memory if necessary

    return ret;
}

function dealloc(_align) {
    // No-op
}

function realloc(addr, newSize, oldSize, align) {
    // If we happen to be reallocating the most recent allocation, reuse it.
    if (addr == allocatedBytes - oldSize) {
        // TODO if newSize won't fit in existing memory pages, grow() as needed
        return addr;
    } else {
        return alloc(newSize, align)
    }
}

function hello() {
    const decoder = new TextDecoder();
    let wasmMemoryBuffer;
    let exitCode;

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
            roc_alloc: alloc,
            roc_dealloc: dealloc,
            roc_realloc: realloc,
            roc_panic: (_pointer, _tag_id) => {
                throw "Roc panicked!";
            },
        },
    };

    const instance = new WebAssembly.Instance(wasmModule, importObj);

    wasmMemoryBuffer = new Uint8Array(instance.exports.memory.buffer);

    try {
        const usize = 4; // size_of::<usize>() on a 32-bit target (namely, wasm32)
        const rocStrSize = 3 * usize;
        const retAddr = alloc(rocStrSize);

        // Write the RocStr into the wasm memory
        instance.exports.roc__mainForHost_1_exposed_generic(retAddr);

        const rocStr = wasmMemoryBuffer.subarray(retAddr, retAddr + (usize * 3));
        const dataView = new DataView(rocStr.buffer);

        // TODO handle small strings

        const rocStrHeapAddr = dataView.getUint32(0, true); // true because wasm is little-endian
        const rocStrLen = dataView.getUint32(usize, true); // true because wasm is little-endian
        const utf8Bytes = wasmMemoryBuffer.subarray(rocStrHeapAddr, rocStrHeapAddr + rocStrLen);

        return decoder.decode(utf8Bytes);
    } catch (e) {
        const isOk = e.message === "unreachable" && exitCode === 0;

        if (!isOk) {
            throw e;
        }
    }

    return null;
}

if (typeof module === "object") {
    module.exports = { hello };
}

// As an example, run hello() from Roc and print the string it returns
console.log("Roc says:", hello());
