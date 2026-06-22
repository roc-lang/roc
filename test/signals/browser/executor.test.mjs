import test from "node:test";
import assert from "node:assert/strict";

import { Op, PayloadKind, SignalsRuntime } from "./runtime.mjs";
import {
  installDomDouble,
  findByText,
  findTextNode,
  findNode,
  findAll,
  fireEvent,
  ELEMENT_NODE,
} from "./dom_double.mjs";

const PAGE = 65536;
const CMD_BASE = 1024; // 4-aligned command-record region
const STR_BASE = 16384; // string-payload region referenced by records
const DEFAULT_ALLOC_BASE = 40960; // bump region for runtime-produced payloads
const RECORD_WORDS = 6;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

// A JS stand-in for `wasm_host.zig` that implements exactly the export surface
// `runtime.mjs` consumes, backed by real linear memory. It lets the guards drive
// the real executor through every render op and capture event dispatches,
// without compiling a Roc app.
class MockHost {
  constructor({ allocBase = DEFAULT_ALLOC_BASE } = {}) {
    this.memory = new WebAssembly.Memory({ initial: 1 });
    this.cmdLen = 0;
    this.strLen = 0;
    this.allocPtr = allocBase;
    this.liveValues = 0;
    this.dispatches = [];
    this.mountScript = [];
    // Map<eventId, (dispatch) => commandScript>
    this.eventResponses = new Map();

    this.exports = {
      memory: this.memory,
      roc_ui_command_record_words: () => RECORD_WORDS,
      roc_ui_command_buffer_ptr: () => (this.cmdLen === 0 ? 0 : CMD_BASE),
      roc_ui_command_buffer_len: () => this.cmdLen,
      roc_ui_string_buffer_ptr: () => STR_BASE,
      roc_ui_string_buffer_len: () => this.strLen,
      roc_ui_live_host_values: () => this.liveValues,
      roc_alloc: (len) => this.alloc(len),
      roc_dealloc: () => {},
      roc_ui_mount: () => this.writeCommands(this.mountScript),
      roc_ui_unmount: () => {
        this.cmdLen = 0;
        this.strLen = 0;
        this.liveValues = 0;
      },
      roc_ui_event: (eventId, kind, ptr, len, boolValue) => {
        const dispatch = { eventId, kind };
        if (kind === PayloadKind.str) {
          dispatch.payload = decoder.decode(new Uint8Array(this.memory.buffer, ptr, len));
        } else if (kind === PayloadKind.bool) {
          dispatch.payload = boolValue !== 0;
        }
        this.dispatches.push(dispatch);
        const respond = this.eventResponses.get(eventId);
        this.writeCommands(respond ? respond(dispatch) : []);
      },
    };
  }

  alloc(len) {
    const ptr = this.allocPtr;
    const end = ptr + len;
    if (end > this.memory.buffer.byteLength) {
      const needed = Math.ceil((end - this.memory.buffer.byteLength) / PAGE);
      this.memory.grow(needed);
    }
    this.allocPtr = end;
    return ptr;
  }

  writeCommands(script) {
    const view = new DataView(this.memory.buffer);
    const bytes = new Uint8Array(this.memory.buffer);
    let strOffset = 0;
    script.forEach((entry, index) => {
      let { a = 0, b = 0, c = 0, d = 0, e = 0 } = entry;
      if (entry.s !== undefined) {
        const encoded = encoder.encode(entry.s);
        bytes.set(encoded, STR_BASE + strOffset);
        b = strOffset;
        c = encoded.length;
        strOffset += encoded.length;
      }
      const base = CMD_BASE + index * RECORD_WORDS * 4;
      view.setUint32(base, entry.op, true);
      view.setUint32(base + 4, a, true);
      view.setUint32(base + 8, b, true);
      view.setUint32(base + 12, c, true);
      view.setUint32(base + 16, d, true);
      view.setUint32(base + 20, e, true);
    });
    this.cmdLen = script.length;
    this.strLen = strOffset;
  }
}

function mountWith(mountScript, options) {
  const host = new MockHost(options);
  host.mountScript = mountScript;
  const root = installDomDouble();
  const runtime = new SignalsRuntime(host.exports, root);
  runtime.mount();
  return { host, root, runtime };
}

test("mount builds an element/text tree from the command stream", () => {
  const { root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "div" },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createElement, a: 2, s: "button" },
    { op: Op.setText, a: 2, s: "Click" },
    { op: Op.bindClick, a: 2, b: 1 },
    { op: Op.appendChild, a: 1, b: 2 },
    { op: Op.createText, a: 3, s: "Hello" },
    { op: Op.appendChild, a: 1, b: 3 },
  ]);

  assert.equal(root.childNodes.length, 1);
  const div = root.childNodes[0];
  assert.equal(div.tagName, "DIV");
  assert.equal(div.childNodes.length, 2);
  assert.ok(findByText(root, "button", "Click"));
  assert.ok(findTextNode(root, "Hello"));
  assert.equal(runtime.lastCommands.length, 9);
});

test("clicking a bound element dispatches a unit event and applies the response patch", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "Click" },
    { op: Op.bindClick, a: 1, b: 7 },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createText, a: 2, s: "before" },
    { op: Op.appendChild, a: 0, b: 2 },
  ]);

  host.eventResponses.set(7, () => [{ op: Op.setText, a: 2, s: "after" }]);

  const button = findByText(root, "button", "Click");
  fireEvent(button, "click");

  assert.deepEqual(host.dispatches, [{ eventId: 7, kind: PayloadKind.unit }]);
  assert.ok(findTextNode(root, "after"));
  assert.equal(runtime.lastCommands.length, 1);
  assert.equal(runtime.lastCommands[0].op, Op.setText);
});

test("input events serialize the string payload through roc_alloc memory", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "input" },
    { op: Op.bindInput, a: 1, b: 4 },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  input.value = "typed text";
  fireEvent(input, "input");

  assert.deepEqual(host.dispatches, [
    { eventId: 4, kind: PayloadKind.str, payload: "typed text" },
  ]);
});

test("check events send a bool payload", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "input" },
    { op: Op.setRole, a: 1, s: "checkbox" },
    { op: Op.bindCheck, a: 1, b: 9 },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  assert.equal(input.type, "checkbox");
  input.checked = true;
  fireEvent(input, "change");

  assert.deepEqual(host.dispatches, [
    { eventId: 9, kind: PayloadKind.bool, payload: true },
  ]);
});

test("attribute ops set element properties and metadata", () => {
  const { root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "input" },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.setValue, a: 1, s: "hello" },
    { op: Op.setChecked, a: 1, b: 1 },
    { op: Op.setDisabled, a: 1, b: 1 },
    { op: Op.setRole, a: 1, s: "textbox" },
    { op: Op.setLabel, a: 1, s: "Name" },
    { op: Op.setTestId, a: 1, s: "name-field" },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  assert.equal(input.value, "hello");
  assert.equal(input.checked, true);
  assert.equal(input.disabled, true);
  assert.equal(input.getAttribute("role"), "textbox");
  assert.equal(input.getAttribute("aria-label"), "Name");
  assert.equal(input.getAttribute("data-testid"), "name-field");
});

test("structural ops detach and reorder live nodes", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "ul" },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createElement, a: 2, s: "li" },
    { op: Op.setText, a: 2, s: "a" },
    { op: Op.appendChild, a: 1, b: 2 },
    { op: Op.createElement, a: 3, s: "li" },
    { op: Op.setText, a: 3, s: "b" },
    { op: Op.appendChild, a: 1, b: 3 },
    { op: Op.createElement, a: 4, s: "li" },
    { op: Op.setText, a: 4, s: "c" },
    { op: Op.appendChild, a: 1, b: 4 },
    { op: Op.createElement, a: 5, s: "button" },
    { op: Op.setText, a: 5, s: "act" },
    { op: Op.bindClick, a: 5, b: 1 },
    { op: Op.appendChild, a: 0, b: 5 },
  ]);

  const list = findNode(root, (node) => node.tagName === "UL");
  assert.deepEqual(textsOf(list), ["a", "b", "c"]);

  // Reorder c before a (move-only), then remove b.
  host.eventResponses.set(1, () => [
    { op: Op.moveBefore, a: 1, b: 4, c: 2 },
    { op: Op.removeNode, a: 3 },
  ]);
  fireEvent(findByText(root, "button", "act"), "click");

  assert.deepEqual(textsOf(list), ["c", "a"]);
});

test("memory growth during a dispatch keeps the command buffer readable", () => {
  // Position the bump allocator one byte from the page edge so the input
  // payload allocation forces a memory.grow mid-dispatch.
  const { host, root } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.createElement, a: 1, s: "input" },
      { op: Op.bindInput, a: 1, b: 2 },
      { op: Op.appendChild, a: 0, b: 1 },
      { op: Op.createText, a: 2, s: "start" },
      { op: Op.appendChild, a: 0, b: 2 },
    ],
    { allocBase: PAGE - 1 },
  );

  host.eventResponses.set(2, (dispatch) => [{ op: Op.setText, a: 2, s: dispatch.payload }]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  input.value = "after-grow";
  const before = host.memory.buffer.byteLength;
  fireEvent(input, "input");

  assert.ok(host.memory.buffer.byteLength > before, "expected memory to grow");
  assert.deepEqual(host.dispatches, [
    { eventId: 2, kind: PayloadKind.str, payload: "after-grow" },
  ]);
  assert.ok(findTextNode(root, "after-grow"));
});

test("unmount clears the DOM and unbinds listeners", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "Click" },
    { op: Op.bindClick, a: 1, b: 1 },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const button = findByText(root, "button", "Click");
  runtime.unmount();

  assert.equal(root.childNodes.length, 0);
  // The listener was removed: a stray click no longer reaches the host.
  fireEvent(button, "click");
  assert.deepEqual(host.dispatches, []);
});

function textsOf(parent) {
  return findAll(parent, (node) => node.nodeType === ELEMENT_NODE && node.tagName === "LI").map(
    (node) => node.textContent,
  );
}
