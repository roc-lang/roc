// Runs a wasm32 build of an app on test/wasm/platform: prints the Str that wasm_main returns.
// roc_panic and roc_crashed print their message and exit 3; other imports log their arguments to stderr.
const fs = require('fs');
const bytes = fs.readFileSync(process.argv[2]);
const module_ = new WebAssembly.Module(bytes);
let instance;
const text = (ptr, len) => Buffer.from(instance.exports.memory.buffer, ptr >>> 0, len >>> 0).toString('utf8');
const env = {};
for (const imp of WebAssembly.Module.imports(module_)) {
  if (imp.kind !== 'function') continue;
  if (imp.name === 'roc_panic' || imp.name === 'roc_crashed') {
    env[imp.name] = (ptr, len) => { console.error(`[${imp.name}] ${text(ptr, len)}`); process.exit(3); };
  } else {
    env[imp.name] = (...args) => { console.error(`[host ${imp.name}]`, ...args); };
  }
}
instance = new WebAssembly.Instance(module_, { env });
const ptr = instance.exports.wasm_main() >>> 0;
const len = instance.exports.wasm_result_len() >>> 0;
console.log(text(ptr, len));
