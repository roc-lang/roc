const fs = require('fs');
const { TextDecoder } = require('util');

const wasmFilename = './roc-app.wasm';
const moduleBytes = fs.readFileSync(wasmFilename);
const wasmModule = new WebAssembly.Module(moduleBytes);
const decoder = new TextDecoder();
const encoder = new TextEncoder();

const usize = 4; // size_of::<usize>() on a 32-bit target (namely, wasm32)
const refcountOne = 2147483648; // i32::MIN https://doc.rust-lang.org/std/primitive.i32.html#associatedconstant.MIN
const refcountSize = usize;
const rocStrSize = 3 * usize;

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

function fromRocStr(wasmMemory, addr) {
    // TODO handle seamless slices
    const rocStr = wasmMemory.subarray(addr, addr + (usize * 3));
    const dataView = new DataView(rocStr.buffer);
    const rocStrHeapAddr = dataView.getUint32(0, true); // true because wasm is little-endian
    const rocStrCapacity = dataView.getInt32(usize * 2, true);
    const isSmallStr = rocStrCapacity < 0;
    const rocStrLen =
        isSmallStr
            ? dataView.getUint8(rocStrSize - 1) // in small strings, the length is stored in the very last byte
            : dataView.getUint32(usize, true);
    const utf8Bytes = wasmMemory.subarray(rocStrHeapAddr, rocStrHeapAddr + rocStrLen);

    return decoder.decode(utf8Bytes);
}

function toRocStr(wasmMemory, jsString) {
    const strLen = jsString.length;

    // Allocate and populate the Roc Str based on the JS string
    const stackAddr = alloc(rocStrSize, usize); // TODO store as a small string if eligible
    const heapSize = strLen + refcountSize;
    const heapAddr = alloc(heapSize, usize);
    const heapBytes = wasmMemory.subarray(heapAddr, heapSize);

    // Encode the JS string's bytes into the allocated space
    encoder.encodeInto(jsString, heapBytes.subarray(refcountSize));

    // Initialize reference count to 1.
    new DataView(heapBytes.buffer, 0, refcountSize).setInt32(0, refcountOne);

    // Populate the stack struct
    const stackView = new DataView(wasmMemory.buffer, stackAddr, rocStrSize);

    stackView.setUint32(0, heapAddr + refcountSize); // pointer
    stackView.setUint32(usize, strLen); // length
    stackView.setUint32(usize * 2, strLen); // capacity

    return stackAddr;
}

function hello(strForRoc) {
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
        const wasmMemory = new Uint8Array(instance.exports.memory.buffer);

        // Allocate space for the Roc function to write the returned Str
        const retAddr = alloc(rocStrSize, usize);
        const argAddr = toRocStr(wasmMemory, strForRoc);

        // Call the Roc function. It will write the returned string into retAddr.
        instance.exports.roc__mainForHost_1_exposed_generic(retAddr, argAddr);

        return fromRocStr(wasmMemory, retAddr)
    } catch (e) {
        console.error("Error running Roc WebAssembly code")
        throw e;
    }
}

if (typeof module === "object") {
    module.exports = { hello };
}

// As an example, run hello() from Roc and print the string it returns
console.log("Roc says:", hello("Hello from JavaScript"));
