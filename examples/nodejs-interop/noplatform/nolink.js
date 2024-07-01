const fs = require('fs');
const { TextDecoder } = require('util');
const { WASI } = require('wasi');
const { argv, env } = require('node:process');

const wasmFilename = './out.wasm';
const moduleBytes = fs.readFileSync(wasmFilename);
const wasmModule = new WebAssembly.Module(moduleBytes);

function hello() {
    const decoder = new TextDecoder();
    let wasmMemoryBuffer;
    let exitCode;
    let returnVal;
    let memory = new WebAssembly.Memory({ initial: 16, maximum: 256 });
    let sp = new WebAssembly.Global({ value: "i32", mutable: true }, 0);

    //function js_display_roc_string(strBytes, strLen) {
    //    const utf8Bytes = wasmMemoryBuffer.subarray(strBytes, strBytes + strLen);

    //    returnVal = decoder.decode(utf8Bytes);
    //}

    const wasi = new WASI({
      version: "preview1",
      args: argv,
      env,
    });
    let heapPtr = 0;

    let importObj = wasi.getImportObject();
    importObj.env = importObj.env || {};
    //importObj.env.js_display_roc_string = js_display_roc_string;
    importObj.env.roc_panic = (pointer, tag_id) => {
        throw `Roc panicked! pointer=${pointer} tag_id=${tag_id}`;
    };
    importObj.env.roc_dbg = (loc, msg) => {
        console.log(`[${loc}] ${msg}`);
    };
    importObj.env.roc_alloc = (size, align) => {
      let out = heapPtr;
      heapPtr += size;
      return out;
    };
    importObj.env.roc_dealloc = (ptr, size, align) => {
      return;
    };
    importObj.env.memset = (ptr, value, size) => {
      let bytes = wasmMemoryBuffer.subarray(ptr, ptr + size);
      bytes.fill(value);
      return ptr;
    };
    importObj.env.memcpy = (dst, src, size) => {
      let dstBytes = wasmMemoryBuffer.subarray(dst, dst + size);
      let srcBytes = wasmMemoryBuffer.subarray(src, src + size);
      dstBytes.set(srcBytes);
      return dst;
    };
    importObj.env.roc_realloc = (ptr, new_size, old_size, alignment) => {
      let new_ptr = heapPtr;
      heapPtr += new_size;
      return new_ptr;
    }

    const instance = new WebAssembly.Instance(wasmModule, importObj);

    wasmMemoryBuffer = new Uint8Array(instance.exports.memory.buffer);

    try {
        let result = wasmMemoryBuffer.subarray(heapPtr, heapPtr + 64);
        instance.exports.roc__mainForHost_1_exposed(heapPtr);
        console.log(decoder.decode(result));
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
