import test from "node:test";
import assert from "node:assert/strict";
import { existsSync, readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

import { SignalsRuntime } from "./runtime.mjs";
import { installDomDouble, findByText, findTextNode, fireEvent } from "./dom_double.mjs";

const wasmPath = fileURLToPath(new URL("./stable_text.wasm", import.meta.url));
const appPath = fileURLToPath(new URL("../apps/stable_text.roc", import.meta.url));
const servePath = fileURLToPath(new URL("../serve.py", import.meta.url));
const rocBinPath = fileURLToPath(new URL("../../../zig-out/bin/roc", import.meta.url));

function resolveStableTextWasm() {
  if (!existsSync(wasmPath)) {
    if (!existsSync(rocBinPath)) {
      return { bytes: null, skip: `roc compiler not built at ${rocBinPath}; run serve.py to build stable_text.wasm` };
    }
    const build = spawnSync("python3", [servePath, appPath, "--no-server", "--app-opt", "size"], {
      stdio: "inherit",
    });
    if (build.status !== 0) {
      return { bytes: null, skip: "failed to build stable_text.wasm via serve.py --no-server" };
    }
  }
  if (!existsSync(wasmPath)) {
    return { bytes: null, skip: "stable_text.wasm missing after build" };
  }
  return { bytes: readFileSync(wasmPath), skip: null };
}

const stableText = resolveStableTextWasm();
const skipReason = stableText.skip ?? false;

async function mountStableText() {
  const { instance } = await WebAssembly.instantiate(stableText.bytes, {});
  const root = installDomDouble();
  const runtime = new SignalsRuntime(instance.exports, root);
  runtime.mount();
  return { runtime, root };
}

test("wasm prunes an equal derived text update", { skip: skipReason }, async () => {
  const { runtime, root } = await mountStableText();

  assert.ok(findByText(root, "button", "Bump"), "button rendered");
  assert.equal(findTextNode(root, "Stable")?.nodeValue, "Stable");

  fireEvent(findByText(root, "button", "Bump"), "click");

  assert.deepEqual(runtime.lastCommands, []);
  assert.equal(findTextNode(root, "Stable")?.nodeValue, "Stable");
});
