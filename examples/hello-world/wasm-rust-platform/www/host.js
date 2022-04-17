// TODO: we shouldn't really need WASI but I already had a browser mock for it
import { getMockWasiImports } from './wasi.js';
window.__wbg_star0 = getMockWasiImports().wasi_snapshot_preview1; // Something in bindgen expects WASI
import init from "./helloWeb.js";

(async function () {
  const wasm_exports = await init();

  try {
    wasm_exports.rust_main();
  } catch (e) {
    const is_ok = e.message === "unreachable" && window.exit_code === 0;
    if (!is_ok) {
      console.error(e);
    }
  }
})();
