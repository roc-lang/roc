import { createMemoryViewCache } from "./wasm_memory_views.mjs";

export const Op = Object.freeze({
  resetDom: 1,
  createElement: 2,
  createText: 3,
  appendChild: 4,
  removeNode: 5,
  moveBefore: 6,
  setText: 7,
  setValue: 8,
  setChecked: 9,
  setDisabled: 10,
  setRole: 11,
  setLabel: 12,
  setTestId: 13,
  bindClick: 14,
  bindInput: 15,
  bindCheck: 16,
  clearEvent: 17,
  startInterval: 18,
  cancelInterval: 19,
  startTask: 20,
  cancelTask: 21,
  setClass: 22,
});

// RenderEventKind enum (render_commands.zig) -> DOM event name. The host emits
// `clearEvent` with the kind in operand `b`; JS maps it back to the listener it
// bound so it can run the matching cleanup.
const EventKind = Object.freeze({ click: 1, input: 2, check: 3 });
const domEventForKind = Object.freeze({
  [EventKind.click]: "click",
  [EventKind.input]: "input",
  [EventKind.check]: "change",
});

export const PayloadKind = Object.freeze({
  unit: 1,
  str: 2,
  bool: 3,
});

export const PayloadAccessor = Object.freeze({
  none: 1,
  targetValue: 2,
  targetChecked: 3,
});

const textDecoder = new TextDecoder();
const textEncoder = new TextEncoder();

export async function instantiateSignalsWasm(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}: ${response.status}`);
  }

  const bytes = await response.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return instance;
}

export async function mountSignalsApp({ wasmUrl, root, taskHandler }) {
  const instance = await instantiateSignalsWasm(wasmUrl);
  const runtime = new SignalsRuntime(instance.exports, root, { taskHandler });
  runtime.mount();
  return runtime;
}

export class SignalsRuntime {
  constructor(exports, root, options = {}) {
    this.exports = exports;
    this.root = root;
    this.views = createMemoryViewCache(exports.memory);
    this.nodes = new Map([[0, root]]);
    this.eventCleanups = new Map();
    this.intervals = new Map();
    this.tasks = new Map();
    this.taskHandler = options.taskHandler ?? null;
    // The patch stream is inspectable: `lastCommands` holds the records drained
    // by the most recent host call so guards can assert the per-event patch
    // budget (mirrors the native host's `patches_emitted` discipline).
    this.lastCommands = [];
  }

  liveHostValues() {
    return this.exports.roc_ui_live_host_values?.() ?? 0;
  }

  mount() {
    this.views.callHost(this.exports.roc_ui_mount);
    this.applyPendingCommands();
  }

  unmount() {
    this.views.callHost(this.exports.roc_ui_unmount);
    this.applyPendingCommands();
    this.clearDom();
  }

  dispatchUnit(eventId) {
    this.dispatch(eventId, PayloadKind.unit, 0, 0, 0);
  }

  dispatchBool(eventId, value) {
    this.dispatch(eventId, PayloadKind.bool, 0, 0, value ? 1 : 0);
  }

  dispatchString(eventId, value) {
    const bytes = textEncoder.encode(value);
    const ptr = this.views.callHost(this.exports.roc_alloc, bytes.length, 1).result;
    this.views.u8.set(bytes, ptr);
    this.dispatch(eventId, PayloadKind.str, ptr, bytes.length, 0);
    this.views.callHost(this.exports.roc_dealloc, ptr, 1);
  }

  dispatch(eventId, payloadKind, payloadPtr, payloadLen, boolValue) {
    this.views.callHost(
      this.exports.roc_ui_event,
      eventId,
      payloadKind,
      payloadPtr,
      payloadLen,
      boolValue,
    );
    this.applyPendingCommands();
  }

  tickTimer(token) {
    this.views.callHost(this.exports.roc_ui_timer, token);
    this.applyPendingCommands();
  }

  resolveTask(requestId, value, failed = false) {
    const bytes = textEncoder.encode(value);
    const ptr = this.views.callHost(this.exports.roc_alloc, bytes.length, 1).result;
    this.views.u8.set(bytes, ptr);
    this.views.callHost(this.exports.roc_ui_resolve, requestId, ptr, bytes.length, failed ? 1 : 0);
    this.views.callHost(this.exports.roc_dealloc, ptr, 1);
    this.tasks.delete(requestId);
    this.applyPendingCommands();
  }

  readPendingCommands() {
    this.views.afterHostCall();
    const words = this.exports.roc_ui_command_record_words();
    const ptr = this.exports.roc_ui_command_buffer_ptr();
    const len = this.exports.roc_ui_command_buffer_len();
    if (ptr === 0 || len === 0) {
      return [];
    }

    const raw = new Uint32Array(this.exports.memory.buffer, ptr, len * words);
    const records = [];
    for (let index = 0; index < len; index += 1) {
      const offset = index * words;
      records.push({
        op: raw[offset],
        a: raw[offset + 1],
        b: raw[offset + 2],
        c: raw[offset + 3],
        d: raw[offset + 4],
        e: raw[offset + 5],
      });
    }
    return records;
  }

  readString(offset, length) {
    if (length === 0) {
      return "";
    }

    this.views.afterHostCall();
    const base = this.exports.roc_ui_string_buffer_ptr();
    const bytes = this.views.u8.subarray(base + offset, base + offset + length);
    return textDecoder.decode(bytes);
  }

  applyPendingCommands() {
    const records = this.readPendingCommands();
    this.lastCommands = records;
    for (const record of records) {
      this.applyCommand(record);
    }
    return records;
  }

  applyCommand(record) {
    switch (record.op) {
      case Op.resetDom:
        this.clearDom();
        return;

      case Op.createElement: {
        const elem = document.createElement(this.readString(record.b, record.c));
        this.nodes.set(record.a, elem);
        return;
      }

      case Op.createText: {
        const node = document.createTextNode(this.readString(record.b, record.c));
        this.nodes.set(record.a, node);
        return;
      }

      case Op.appendChild:
        this.node(record.a).appendChild(this.node(record.b));
        return;

      case Op.removeNode: {
        const node = this.node(record.a);
        node.parentNode?.removeChild(node);
        this.clearElemListeners(record.a);
        this.nodes.delete(record.a);
        return;
      }

      case Op.moveBefore: {
        const parent = this.node(record.a);
        const child = this.node(record.b);
        const before = record.c === 0 ? null : this.node(record.c);
        parent.insertBefore(child, before);
        return;
      }

      case Op.setText:
        setNodeText(this.node(record.a), this.readString(record.b, record.c));
        return;

      case Op.setValue:
        this.node(record.a).value = this.readString(record.b, record.c);
        return;

      case Op.setChecked:
        this.node(record.a).checked = record.b !== 0;
        return;

      case Op.setDisabled:
        this.node(record.a).disabled = record.b !== 0;
        return;

      case Op.setRole:
        setRole(this.node(record.a), this.readString(record.b, record.c));
        return;

      case Op.setLabel:
        this.node(record.a).setAttribute("aria-label", this.readString(record.b, record.c));
        return;

      case Op.setTestId:
        this.node(record.a).setAttribute("data-testid", this.readString(record.b, record.c));
        return;

      case Op.setClass:
        setClass(this.node(record.a), this.readString(record.b, record.c));
        return;

      case Op.bindClick:
        this.bindEvent(record.a, "click", record.b, record.c);
        return;

      case Op.bindInput:
        this.bindEvent(record.a, "input", record.b, record.c);
        return;

      case Op.bindCheck:
        this.bindEvent(record.a, "change", record.b, record.c);
        return;

      case Op.clearEvent: {
        const domEvent = domEventForKind[record.b];
        if (domEvent === undefined) {
          throw new Error(`unknown clear_event kind ${record.b}`);
        }
        this.clearEvent(record.a, domEvent);
        return;
      }

      case Op.startInterval:
        this.startInterval(record.a, record.b);
        return;

      case Op.cancelInterval:
        this.cancelInterval(record.a);
        return;

      case Op.startTask:
        this.startTask(
          record.a,
          this.readString(record.b, record.c),
          this.readString(record.d, record.e),
        );
        return;

      case Op.cancelTask:
        this.cancelTask(record.a);
        return;

      default:
        throw new Error(`unknown render op ${record.op}`);
    }
  }

  bindEvent(elemId, domEvent, eventId, payloadAccessor) {
    const key = `${elemId}:${domEvent}`;
    this.eventCleanups.get(key)?.();
    const elem = this.node(elemId);
    const listener = (event) => this.dispatchEventPayload(eventId, payloadAccessor, event);
    elem.addEventListener(domEvent, listener);
    this.eventCleanups.set(key, () => elem.removeEventListener(domEvent, listener));
    elem.dataset.rocEventId = String(eventId);
  }

  dispatchEventPayload(eventId, payloadAccessor, event) {
    switch (payloadAccessor) {
      case PayloadAccessor.none:
        this.dispatchUnit(eventId);
        return;

      case PayloadAccessor.targetValue:
        this.dispatchString(eventId, event.currentTarget.value);
        return;

      case PayloadAccessor.targetChecked:
        this.dispatchBool(eventId, event.currentTarget.checked);
        return;

      default:
        throw new Error(`unknown event payload accessor ${payloadAccessor}`);
    }
  }

  clearEvent(elemId, domEvent) {
    const key = `${elemId}:${domEvent}`;
    const cleanup = this.eventCleanups.get(key);
    if (!cleanup) {
      return;
    }
    cleanup();
    this.eventCleanups.delete(key);
    const elem = this.nodes.get(elemId);
    if (elem && elem.dataset) {
      delete elem.dataset.rocEventId;
    }
  }

  clearElemListeners(elemId) {
    const prefix = `${elemId}:`;
    for (const key of this.eventCleanups.keys()) {
      if (key.startsWith(prefix)) {
        this.eventCleanups.get(key)();
        this.eventCleanups.delete(key);
      }
    }
  }

  startInterval(token, periodMs) {
    this.cancelInterval(token);
    const id = setInterval(() => this.tickTimer(token), periodMs);
    this.intervals.set(token, id);
  }

  cancelInterval(token) {
    const id = this.intervals.get(token);
    if (id === undefined) {
      return;
    }
    clearInterval(id);
    this.intervals.delete(token);
  }

  startTask(requestId, name, request) {
    this.cancelTask(requestId);
    const controller = new AbortController();
    this.tasks.set(requestId, { name, request, controller });
    if (!this.taskHandler) {
      return;
    }

    Promise.resolve(this.taskHandler({ requestId, name, request, signal: controller.signal }))
      .then((value) => {
        if (!controller.signal.aborted && this.tasks.has(requestId)) {
          this.resolveTask(requestId, String(value), false);
        }
      })
      .catch((err) => {
        if (!controller.signal.aborted && this.tasks.has(requestId)) {
          this.resolveTask(requestId, String(err?.message ?? err), true);
        }
      });
  }

  cancelTask(requestId) {
    const task = this.tasks.get(requestId);
    if (!task) {
      return;
    }
    task.controller.abort();
    this.tasks.delete(requestId);
  }

  clearAsyncResources() {
    for (const token of [...this.intervals.keys()]) {
      this.cancelInterval(token);
    }
    for (const requestId of [...this.tasks.keys()]) {
      this.cancelTask(requestId);
    }
  }

  node(id) {
    const node = this.nodes.get(id);
    if (!node) {
      throw new Error(`unknown DOM node id ${id}`);
    }
    return node;
  }

  clearDom() {
    this.clearAsyncResources();
    for (const cleanup of this.eventCleanups.values()) {
      cleanup();
    }
    this.eventCleanups.clear();
    this.nodes.clear();
    this.nodes.set(0, this.root);
    this.root.replaceChildren();
  }
}

function setNodeText(node, value) {
  if (node.nodeType === Node.TEXT_NODE) {
    node.nodeValue = value;
  } else {
    node.textContent = value;
  }
}

function setRole(node, value) {
  node.setAttribute("role", value);
  if (node.tagName === "INPUT" && value === "checkbox") {
    node.type = "checkbox";
  }
}

function setClass(node, value) {
  if (value === "") {
    node.removeAttribute("class");
  } else {
    node.className = value;
  }
}
