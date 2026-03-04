let wasm;
const decoder = new TextDecoder();
const encoder = new TextEncoder();
const outputEl = document.getElementById("output");
const errorsEl = document.getElementById("errors");

const importObject = {
  env: {
    js_echo(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      outputEl.textContent += decoder.decode(bytes) + "\n";
    },
    js_stderr(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      errorsEl.innerHTML += decoder.decode(bytes);
    },
  },
};

function wasmWrite(str) {
  const encoded = encoder.encode(str);
  const ptr = wasm.instance.exports.allocateBuffer(encoded.length);
  if (!ptr) throw new Error("allocateBuffer returned null");
  new Uint8Array(wasm.instance.exports.memory.buffer, ptr, encoded.length).set(encoded);
  return { ptr, len: encoded.length };
}

function run() {
  outputEl.textContent = "";
  errorsEl.innerHTML = "";
  wasm.instance.exports.init();

  for (const ta of document.querySelectorAll("textarea[data-module]")) {
    const n = wasmWrite(ta.dataset.module);
    const c = wasmWrite(ta.value);
    wasm.instance.exports.addFile(n.ptr, n.len, c.ptr, c.len);
  }

  const s = wasmWrite(document.getElementById("source").value);
  const code = wasm.instance.exports.compileAndRun(s.ptr, s.len);

  if (code === 255 && !errorsEl.innerHTML) {
    errorsEl.textContent = "Compilation or execution failed";
  } else if (code !== 0 && code !== 255) {
    outputEl.textContent += `Exit code: ${code}\n`;
  }
}

fetch("echo.wasm")
  .then((r) => WebAssembly.instantiateStreaming(r, importObject))
  .then((result) => {
    wasm = result;
    wasm.instance.exports.init();
    document.getElementById("run").addEventListener("click", run);
    document.getElementById("run").disabled = false;
  });
