const fs = require('fs');
const { TextDecoder } = require('util');

const wasmFilename = './roc-app.wasm';
const moduleBytes = fs.readFileSync(wasmFilename);
const wasmModule = new WebAssembly.Module(moduleBytes);
const decoder = new TextDecoder();

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
    try {
        const instance = new WebAssembly.Instance(wasmModule, {
            env: {
                roc_alloc: alloc,
                roc_dealloc: dealloc,
                roc_realloc: realloc,
                roc_panic: (_pointer, _tag_id) => {
                    throw "Roc panicked!";
                },
            },
        });
        const wasmMemoryBuffer = new Uint8Array(instance.exports.memory.buffer);

        const usize = 4; // size_of::<usize>() on a 32-bit target (namely, wasm32)
        const rocStrSize = 3 * usize;
        const retAddr = alloc(rocStrSize);

        // Write the RocStr into the wasm memory
        instance.exports.roc__mainForHost_1_exposed_generic(retAddr);

        const rocStr = wasmMemoryBuffer.subarray(retAddr, retAddr + (usize * 3));
        const dataView = new DataView(rocStr.buffer);
        const rocStrHeapAddr = dataView.getUint32(0, true); // true because wasm is little-endian
        const rocStrCapacity = dataView.getInt32(usize * 2, true);
        const isSmallStr = rocStrCapacity < 0;
        const rocStrLen =
            isSmallStr
                ? dataView.getUint8((usize * 3) - 1) // in small strings, the length is stored in the very last byte
                : dataView.getUint32(usize, true);
        const utf8Bytes = wasmMemoryBuffer.subarray(rocStrHeapAddr, rocStrHeapAddr + rocStrLen);

        return decoder.decode(utf8Bytes);
    } catch (e) {
        throw new Error("Error running Roc WebAssembly code:" + e);
    }
}

if (typeof module === "object") {
    module.exports = { hello };
}

// As an example, run hello() from Roc and print the string it returns
console.log("Roc says:", hello());
