// Node.js test for the Roc WASM module

import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Helper to decode strings from WASM memory
function decodeString(memory, ptr, len) {
    const bytes = new Uint8Array(memory.buffer, ptr, len);
    return new TextDecoder().decode(bytes);
}

async function main() {
    console.log('Loading WASM module...');

    const wasmPath = join(__dirname, 'app.wasm');
    const wasmBytes = await readFile(wasmPath);

    console.log(`Loaded ${wasmBytes.length} bytes`);

    let wasmModule = null;

    // Provide the required imports
    const imports = {
        env: {
            roc_panic: (ptr, len) => {
                const msg = decodeString(wasmModule.exports.memory, ptr, len);
                console.error(`[PANIC] ${msg}`);
                throw new Error(`Roc panic: ${msg}`);
            },
            roc_dbg: (ptr, len) => {
                const msg = decodeString(wasmModule.exports.memory, ptr, len);
                console.log(`[DBG] ${msg}`);
            },
            roc_expect_failed: (ptr, len) => {
                const msg = decodeString(wasmModule.exports.memory, ptr, len);
                console.error(`[EXPECT FAILED] ${msg}`);
            }
        }
    };

    console.log('Instantiating WASM module...');
    const result = await WebAssembly.instantiate(wasmBytes, imports);
    wasmModule = result.instance;

    console.log('Available exports:', Object.keys(wasmModule.exports));

    console.log('Calling wasm_main()...');
    try {
        const resultPtr = wasmModule.exports.wasm_main();
        const resultLen = wasmModule.exports.wasm_result_len();
        const heapUsed = wasmModule.exports.wasm_heap_used();

        console.log(`Result pointer: ${resultPtr}`);
        console.log(`Result length: ${resultLen}`);
        console.log(`Heap used: ${heapUsed} bytes`);

        if (resultPtr && resultLen > 0) {
            const resultStr = decodeString(wasmModule.exports.memory, resultPtr, resultLen);
            console.log(`Result: "${resultStr}"`);
        } else if (resultPtr) {
            console.log('Result: (empty string)');
        } else {
            console.log('Result: (null pointer)');
        }

        console.log('\n✅ WASM test completed successfully!');
    } catch (error) {
        console.error('Error:', error);
        process.exit(1);
    }
}

main().catch(err => {
    console.error('Fatal error:', err);
    process.exit(1);
});
