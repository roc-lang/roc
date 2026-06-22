import test from "node:test";
import assert from "node:assert/strict";
import { existsSync, readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

import { Op, SignalsRuntime } from "./runtime.mjs";
import { installDomDouble, findByText, findTextNode, fireEvent } from "./dom_double.mjs";

// End-to-end guard for the G-B4 counter milestone: the real Roc-compiled
// `counter.wasm` is driven through the real executor against the DOM double.
// It proves clicks change the count, the update budget is one `set_text`, and
// `roc_ui_unmount` drops every retained host value.
//
// The wasm artifact is gitignored and produced by `serve.py`. The test loads it
// if present, builds it on demand when the Roc toolchain is available, and skips
// loudly otherwise so a missing toolchain never reports a false green.

const wasmPath = fileURLToPath(new URL("./counter.wasm", import.meta.url));
const servePath = fileURLToPath(new URL("../serve.py", import.meta.url));
const rocBinPath = fileURLToPath(new URL("../../../zig-out/bin/roc", import.meta.url));

function resolveCounterWasm() {
  if (!existsSync(wasmPath)) {
    if (!existsSync(rocBinPath)) {
      return { bytes: null, skip: `roc compiler not built at ${rocBinPath}; run serve.py to build counter.wasm` };
    }
    const build = spawnSync("python3", [servePath, "--no-server", "--app-opt", "size"], {
      stdio: "inherit",
    });
    if (build.status !== 0) {
      return { bytes: null, skip: "failed to build counter.wasm via serve.py --no-server" };
    }
  }
  if (!existsSync(wasmPath)) {
    return { bytes: null, skip: "counter.wasm missing after build" };
  }
  return { bytes: readFileSync(wasmPath), skip: null };
}

const counter = resolveCounterWasm();
// node:test treats a non-`false`/`undefined` `skip` (including `null`) as a
// skip directive, so collapse "no reason" to `false`.
const skipReason = counter.skip ?? false;

async function mountCounter() {
  const { instance } = await WebAssembly.instantiate(counter.bytes, {});
  assert.equal(
    typeof instance.exports.roc_ui_live_host_values,
    "function",
    "counter.wasm is stale: rebuild with serve.py to export roc_ui_live_host_values",
  );
  const root = installDomDouble();
  const runtime = new SignalsRuntime(instance.exports, root);
  runtime.mount();
  return { runtime, root };
}

function countText(root) {
  return findTextNode(root, "Count: ")?.nodeValue;
}

test("counter.wasm mounts the initial tree with one retained state value", { skip: skipReason }, async () => {
  const { runtime, root } = await mountCounter();

  assert.ok(findByText(root, "h2", "Signals counter"), "heading rendered");
  assert.ok(findByText(root, "button", "Increment"), "increment button rendered");
  assert.ok(findByText(root, "button", "Decrement"), "decrement button rendered");
  assert.equal(countText(root), "Count: 0");

  // Mount budget: one DOM reset, two click bindings, the single count text node.
  const ops = runtime.lastCommands.map((record) => record.op);
  assert.equal(ops.filter((op) => op === Op.resetDom).length, 1);
  assert.equal(ops.filter((op) => op === Op.bindClick).length, 2);
  assert.equal(ops.filter((op) => op === Op.createText).length, 1);
  assert.equal(runtime.lastCommands.length, 17);

  assert.equal(runtime.liveHostValues(), 1);
});

test("clicking changes the count with exactly one set_text patch", { skip: skipReason }, async () => {
  const { runtime, root } = await mountCounter();
  const increment = findByText(root, "button", "Increment");
  const decrement = findByText(root, "button", "Decrement");

  fireEvent(increment, "click");
  assert.equal(countText(root), "Count: 1");
  assert.equal(runtime.lastCommands.length, 1);
  assert.equal(runtime.lastCommands[0].op, Op.setText);
  assert.equal(runtime.liveHostValues(), 1);

  fireEvent(decrement, "click");
  fireEvent(decrement, "click");
  assert.equal(countText(root), "Count: -1");
  assert.equal(runtime.lastCommands.length, 1);
  assert.equal(runtime.lastCommands[0].op, Op.setText);

  // Repeated dispatch never grows the retained host-value gauge.
  assert.equal(runtime.liveHostValues(), 1);
});

test("unmount clears the DOM and drops every retained host value", { skip: skipReason }, async () => {
  const { runtime, root } = await mountCounter();
  fireEvent(findByText(root, "button", "Increment"), "click");
  assert.equal(runtime.liveHostValues(), 1);

  runtime.unmount();
  assert.equal(root.childNodes.length, 0);
  assert.equal(runtime.liveHostValues(), 0);

  // A clean teardown re-mounts without tripping the host's leak assertion.
  runtime.mount();
  assert.equal(countText(root), "Count: 0");
  assert.equal(runtime.liveHostValues(), 1);
});
