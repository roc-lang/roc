import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { readFile } from "node:fs/promises";

import { SignalsRuntime } from "./runtime.mjs";
import { installDomDouble } from "./dom_double.mjs";

const repo = new URL("../../..", import.meta.url).pathname.replace(/\/$/, "");
const wasmPath = process.argv[2];
const mode = process.argv[3] ?? "success";

if (!wasmPath || (mode !== "success" && mode !== "failure")) {
  throw new Error("usage: node task_lifetime_harness.mjs <wasm-path> [success|failure]");
}

const payload = execFileSync("python3", [
  "-c",
  [
    "import pathlib, sys",
    `sys.path.insert(0, ${JSON.stringify(`${repo}/test/signals`)})`,
    "import serve",
    "sys.stdout.buffer.write(serve.ops_api_body('/api/ops/dashboard', now=1710000000))",
  ].join("; "),
], { cwd: repo }).toString("utf8");

const wasm = await readFile(wasmPath);
const { instance } = await WebAssembly.instantiate(wasm, {});
const root = installDomDouble();
const errors = [];
const taskCalls = [];

const readLastHostError = () => {
  const ptr = instance.exports.roc_ui_last_error_ptr?.() ?? 0;
  const len = instance.exports.roc_ui_last_error_len?.() ?? 0;
  if (ptr === 0 || len === 0) return "";
  return new TextDecoder().decode(new Uint8Array(instance.exports.memory.buffer, ptr, len));
};

const runtime = new SignalsRuntime(instance.exports, root, {
  taskHandler: ({ name, request }) => {
    taskCalls.push({ name, request });
    assert.equal(name, "http:get-text:dashboard");
    assert.equal(request, "/api/ops/dashboard");
    if (mode === "failure") {
      throw new Error(payload);
    }
    return payload;
  },
  onError: (err) => errors.push(err),
});

try {
  runtime.mount();
  await Promise.resolve();
  await Promise.resolve();
  if (errors.length !== 0) throw errors[0];

  const expectedText = mode === "failure" ? "failed bytes" : "ready bytes";
  const renderedText = root.textContent;
  assert.equal(renderedText.includes(expectedText), true);
  assert.equal(renderedText.includes("loading"), false);
  assert.equal(runtime.liveHostValues() > 0, true);

  runtime.unmount();
  if (errors.length !== 0) throw errors[0];
  assert.equal(runtime.liveHostValues(), 0);
  assert.deepEqual(taskCalls, [
    { name: "http:get-text:dashboard", request: "/api/ops/dashboard" },
  ]);

  console.log(renderedText);
} catch (err) {
  console.error("last host error:", readLastHostError());
  console.error("root text:", root.textContent);
  throw err;
}
