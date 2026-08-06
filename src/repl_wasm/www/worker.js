let instance;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function readU32(memory, ptr) {
  const bytes = new Uint8Array(memory.buffer, ptr, 4);
  return bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
}

async function initialize() {
  const response = await fetch("repl.wasm");
  if (!response.ok) throw new Error(`Could not load repl.wasm (${response.status})`);
  const module = await WebAssembly.instantiateStreaming(response, {});
  instance = module.instance;
  postMessage({ type: "ready" });
}

function request(payload) {
  const exports = instance.exports;
  const input = encoder.encode(JSON.stringify(payload));
  const inputPtr = exports.roc_repl_alloc(input.length);
  if (!inputPtr) throw new Error("REPL request allocation failed");

  new Uint8Array(exports.memory.buffer, inputPtr, input.length).set(input);
  const responsePtr = exports.roc_repl_process(inputPtr, input.length);
  exports.roc_repl_free(inputPtr, input.length);
  if (!responsePtr) throw new Error("REPL request failed without a response");

  const responseLength = readU32(exports.memory, responsePtr);
  const responseBytes = new Uint8Array(exports.memory.buffer, responsePtr + 4, responseLength);
  const responseText = decoder.decode(responseBytes);
  exports.roc_repl_free_response(responsePtr);
  return JSON.parse(responseText);
}

onmessage = (event) => {
  if (event.data.type !== "request") return;
  try {
    postMessage({ type: "response", token: event.data.token, response: request(event.data.request) });
  } catch (error) {
    postMessage({ type: "failure", token: event.data.token, message: String(error) });
  }
};

initialize().catch((error) => postMessage({ type: "failure", token: null, message: String(error) }));
