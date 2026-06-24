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
const CMD_BASE = 1024;
const STR_BASE = 16384;
const DEFAULT_ALLOC_BASE = 40960;
const RECORD_WORDS = 6;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

class MockHost {
  constructor({ allocBase = DEFAULT_ALLOC_BASE } = {}) {
    this.memory = new WebAssembly.Memory({ initial: 1 });
    this.cmdLen = 0;
    this.strLen = 0;
    this.allocPtr = allocBase;
    this.dispatches = [];
    this.timers = [];
    this.resolutions = [];
    this.mountScript = [];
    this.eventResponses = new Map();
    this.timerResponses = new Map();

    this.exports = {
      memory: this.memory,
      roc_ui_command_record_words: () => RECORD_WORDS,
      roc_ui_command_buffer_ptr: () => (this.cmdLen === 0 ? 0 : CMD_BASE),
      roc_ui_command_buffer_len: () => this.cmdLen,
      roc_ui_string_buffer_ptr: () => STR_BASE,
      roc_ui_string_buffer_len: () => this.strLen,
      roc_ui_live_host_values: () => 0,
      roc_alloc: (len) => this.alloc(len),
      roc_dealloc: () => {},
      roc_ui_mount: () => this.writeCommands(this.mountScript),
      roc_ui_unmount: () => {
        this.cmdLen = 0;
        this.strLen = 0;
      },
      roc_ui_timer: (token) => {
        this.timers.push(token);
        const respond = this.timerResponses.get(token);
        this.writeCommands(respond ? respond(token) : []);
      },
      roc_ui_resolve: (requestId, ptr, len, failed) => {
        this.resolutions.push({
          requestId,
          payload: decoder.decode(new Uint8Array(this.memory.buffer, ptr, len)),
          failed: failed !== 0,
        });
        this.writeCommands([]);
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
      this.memory.grow(Math.ceil((end - this.memory.buffer.byteLength) / PAGE));
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
      if (entry.strings !== undefined) {
        const [first, second] = entry.strings.map((value) => encoder.encode(value));
        bytes.set(first, STR_BASE + strOffset);
        b = strOffset;
        c = first.length;
        strOffset += first.length;
        bytes.set(second, STR_BASE + strOffset);
        d = strOffset;
        e = second.length;
        strOffset += second.length;
      } else if (entry.s !== undefined) {
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

test("command opcodes map to the expected DOM operations", () => {
  const { root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "section" },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createElement, a: 2, s: "input" },
    { op: Op.setValue, a: 2, s: "hello" },
    { op: Op.setChecked, a: 2, b: 1 },
    { op: Op.setDisabled, a: 2, b: 1 },
    { op: Op.setRole, a: 2, s: "textbox" },
    { op: Op.setLabel, a: 2, s: "Name" },
    { op: Op.setTestId, a: 2, s: "name-field" },
    { op: Op.appendChild, a: 1, b: 2 },
    { op: Op.createText, a: 3, s: "before" },
    { op: Op.appendChild, a: 1, b: 3 },
    { op: Op.createElement, a: 4, s: "span" },
    { op: Op.setText, a: 4, s: "label" },
    { op: Op.appendChild, a: 1, b: 4 },
    { op: Op.moveBefore, a: 1, b: 4, c: 2 },
    { op: Op.removeNode, a: 3 },
  ]);

  const section = findNode(root, (node) => node.tagName === "SECTION");
  const input = findNode(root, (node) => node.tagName === "INPUT");

  assert.deepEqual(
    section.childNodes
      .filter((node) => node.nodeType === ELEMENT_NODE)
      .map((node) => node.tagName),
    ["SPAN", "INPUT"],
  );
  assert.equal(input.value, "hello");
  assert.equal(input.checked, true);
  assert.equal(input.disabled, true);
  assert.equal(input.getAttribute("role"), "textbox");
  assert.equal(input.getAttribute("aria-label"), "Name");
  assert.equal(input.getAttribute("data-testid"), "name-field");
  assert.equal(findTextNode(root, "before"), null);
});

test("event payloads round-trip through the wasm memory boundary", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "click" },
    { op: Op.bindClick, a: 1, b: 10 },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createElement, a: 2, s: "input" },
    { op: Op.bindInput, a: 2, b: 11 },
    { op: Op.appendChild, a: 0, b: 2 },
    { op: Op.createElement, a: 3, s: "input" },
    { op: Op.setRole, a: 3, s: "checkbox" },
    { op: Op.bindCheck, a: 3, b: 12 },
    { op: Op.appendChild, a: 0, b: 3 },
  ]);

  fireEvent(findByText(root, "button", "click"), "click");
  const textInput = findAll(root, (node) => node.tagName === "INPUT")[0];
  textInput.value = "typed text";
  fireEvent(textInput, "input");
  const checkbox = findAll(root, (node) => node.tagName === "INPUT")[1];
  checkbox.checked = true;
  fireEvent(checkbox, "change");

  assert.deepEqual(host.dispatches, [
    { eventId: 10, kind: PayloadKind.unit },
    { eventId: 11, kind: PayloadKind.str, payload: "typed text" },
    { eventId: 12, kind: PayloadKind.bool, payload: true },
  ]);
});

test("clear_event and remove_node release DOM listeners", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "click" },
    { op: Op.bindClick, a: 1, b: 1 },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const button = findByText(root, "button", "click");
  runtime.applyCommand({ op: Op.clearEvent, a: 1, b: 1, c: 0, d: 0, e: 0 });
  fireEvent(button, "click");
  assert.deepEqual(host.dispatches, []);

  runtime.applyCommand({ op: Op.bindClick, a: 1, b: 2, c: 0, d: 0, e: 0 });
  runtime.applyCommand({ op: Op.removeNode, a: 1, b: 0, c: 0, d: 0, e: 0 });
  fireEvent(button, "click");
  assert.deepEqual(host.dispatches, []);
});

test("memory growth during dispatch keeps the response command stream readable", () => {
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

  assert.ok(host.memory.buffer.byteLength > before);
  assert.deepEqual(host.dispatches, [
    { eventId: 2, kind: PayloadKind.str, payload: "after-grow" },
  ]);
  assert.ok(findTextNode(root, "after-grow"));
});

test("timer commands register intervals and timer ticks re-enter wasm", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createText, a: 1, s: "tick-start" },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.startInterval, a: 7, b: 60000 },
  ]);
  host.timerResponses.set(7, () => [{ op: Op.setText, a: 1, s: "tick-fired" }]);

  try {
    assert.equal(runtime.intervals.has(7), true);
    runtime.tickTimer(7);
    assert.deepEqual(host.timers, [7]);
    assert.ok(findTextNode(root, "tick-fired"));

    runtime.applyCommand({ op: Op.cancelInterval, a: 7, b: 0, c: 0, d: 0, e: 0 });
    assert.equal(runtime.intervals.has(7), false);
  } finally {
    runtime.cancelInterval(7);
  }
});

test("task commands marshal request and resolve payloads by request id", () => {
  const { host, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.startTask, a: 5, strings: ["lookup", "roc"] },
  ]);

  assert.deepEqual(
    [...runtime.tasks.entries()].map(([requestId, task]) => ({
      requestId,
      name: task.name,
      request: task.request,
      aborted: task.controller.signal.aborted,
    })),
    [{ requestId: 5, name: "lookup", request: "roc", aborted: false }],
  );

  runtime.resolveTask(5, "Roc result");
  assert.deepEqual(host.resolutions, [
    { requestId: 5, payload: "Roc result", failed: false },
  ]);
  assert.equal(runtime.tasks.has(5), false);

  runtime.applyCommand({ op: Op.startTask, a: 6, b: 0, c: 6, d: 6, e: 3 });
  runtime.applyCommand({ op: Op.cancelTask, a: 6, b: 0, c: 0, d: 0, e: 0 });
  assert.equal(runtime.tasks.has(6), false);
});
