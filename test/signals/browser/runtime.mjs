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
});

export const PayloadKind = Object.freeze({
  unit: 1,
  str: 2,
  bool: 3,
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

export async function mountSignalsApp({ wasmUrl, root }) {
  const instance = await instantiateSignalsWasm(wasmUrl);
  const runtime = new SignalsRuntime(instance.exports, root);
  runtime.mount();
  return runtime;
}

export class SignalsRuntime {
  constructor(exports, root) {
    this.exports = exports;
    this.root = root;
    this.views = createMemoryViewCache(exports.memory);
    this.nodes = new Map([[0, root]]);
    this.eventCleanups = new Map();
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

      case Op.bindClick:
        this.bindEvent(record.a, "click", record.b, () => this.dispatchUnit(record.b));
        return;

      case Op.bindInput:
        this.bindEvent(record.a, "input", record.b, (event) => {
          this.dispatchString(record.b, event.currentTarget.value);
        });
        return;

      case Op.bindCheck:
        this.bindEvent(record.a, "change", record.b, (event) => {
          this.dispatchBool(record.b, event.currentTarget.checked);
        });
        return;

      default:
        throw new Error(`unknown render op ${record.op}`);
    }
  }

  bindEvent(elemId, domEvent, eventId, listener) {
    const key = `${elemId}:${domEvent}`;
    this.eventCleanups.get(key)?.();
    const elem = this.node(elemId);
    elem.addEventListener(domEvent, listener);
    this.eventCleanups.set(key, () => elem.removeEventListener(domEvent, listener));
    elem.dataset.rocEventId = String(eventId);
  }

  node(id) {
    const node = this.nodes.get(id);
    if (!node) {
      throw new Error(`unknown DOM node id ${id}`);
    }
    return node;
  }

  clearDom() {
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
