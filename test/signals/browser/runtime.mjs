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
  bindPointerDown: 23,
  bindPointerUp: 24,
  bindPointerEnter: 25,
  bindPointerLeave: 26,
  extended: 27,
});

export const Protocol = Object.freeze({
  version: 1,
});

export const ProtocolFeature = Object.freeze({
  dynamicAttrs: 1 << 0,
});

const requiredProtocolFeatures = ProtocolFeature.dynamicAttrs;

export const DynamicOp = Object.freeze({
  setAttrText: 1,
  removeAttr: 2,
});

// RenderEventKind enum (render_commands.zig) -> DOM event name. The host emits
// `clearEvent` with the kind in operand `b`; JS maps it back to the listener it
// bound so it can run the matching cleanup.
const EventKind = Object.freeze({
  click: 1,
  input: 2,
  check: 3,
  pointerDown: 4,
  pointerUp: 5,
  pointerEnter: 6,
  pointerLeave: 7,
});
const domEventForKind = Object.freeze({
  [EventKind.click]: "click",
  [EventKind.input]: "input",
  [EventKind.check]: "change",
  [EventKind.pointerDown]: "pointerdown",
  [EventKind.pointerUp]: "pointerup",
  [EventKind.pointerEnter]: "pointerenter",
  [EventKind.pointerLeave]: "pointerleave",
});

const opNames = Object.freeze({
  [Op.resetDom]: "reset_dom",
  [Op.createElement]: "create_element",
  [Op.createText]: "create_text",
  [Op.appendChild]: "append_child",
  [Op.removeNode]: "remove_node",
  [Op.moveBefore]: "move_before",
  [Op.setText]: "set_text",
  [Op.setValue]: "set_value",
  [Op.setChecked]: "set_checked",
  [Op.setDisabled]: "set_disabled",
  [Op.setRole]: "set_role",
  [Op.setLabel]: "set_label",
  [Op.setTestId]: "set_test_id",
  [Op.bindClick]: "bind_click",
  [Op.bindInput]: "bind_input",
  [Op.bindCheck]: "bind_check",
  [Op.clearEvent]: "clear_event",
  [Op.startInterval]: "start_interval",
  [Op.cancelInterval]: "cancel_interval",
  [Op.startTask]: "start_task",
  [Op.cancelTask]: "cancel_task",
  [Op.setClass]: "set_class",
  [Op.bindPointerDown]: "bind_pointer_down",
  [Op.bindPointerUp]: "bind_pointer_up",
  [Op.bindPointerEnter]: "bind_pointer_enter",
  [Op.bindPointerLeave]: "bind_pointer_leave",
  [Op.extended]: "extended",
});

const dynamicOpNames = Object.freeze({
  [DynamicOp.setAttrText]: "set_attr_text",
  [DynamicOp.removeAttr]: "remove_attr",
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

const payloadKindNames = Object.freeze({
  [PayloadKind.unit]: "unit",
  [PayloadKind.str]: "str",
  [PayloadKind.bool]: "bool",
});

const payloadAccessorNames = Object.freeze({
  [PayloadAccessor.none]: "none",
  [PayloadAccessor.targetValue]: "target_value",
  [PayloadAccessor.targetChecked]: "target_checked",
});

const pointerProbeEvents = Object.freeze([
  "pointerdown",
  "pointermove",
  "pointerup",
  "pointercancel",
  "pointerover",
  "pointerout",
  "pointerenter",
  "pointerleave",
]);

export const HttpTextTask = Object.freeze({
  namePrefix: "http:get-text:",
  opsApiPaths: Object.freeze([
    "/api/ops/dashboard",
    "/api/ops/summary",
    "/api/ops/traffic",
    "/api/ops/jobs",
    "/api/ops/alerts",
    "/api/ops/health",
  ]),
});

const textDecoder = new TextDecoder();
const dynamicTextDecoder = new TextDecoder("utf-8", { fatal: true });
const textEncoder = new TextEncoder();

const opsApiTextPathSet = new Set(HttpTextTask.opsApiPaths);

export function opsApiTextTaskHandler({ name, request, signal }) {
  if (!name.startsWith(HttpTextTask.namePrefix)) {
    return null;
  }
  if (!opsApiTextPathSet.has(request)) {
    throw new Error(`unsupported ops API text endpoint: ${request}`);
  }

  return fetch(request, {
    signal,
    headers: { Accept: "text/plain" },
  }).then(async (response) => {
    if (!response.ok) {
      throw new Error(`GET ${request} failed: ${response.status}`);
    }
    return response.text();
  });
}

export async function instantiateSignalsWasm(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}: ${response.status}`);
  }

  const bytes = await response.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return instance;
}

export async function mountSignalsApp({ wasmUrl, root, taskHandler, onError, telemetry }) {
  const instance = await instantiateSignalsWasm(wasmUrl);
  const runtime = new SignalsRuntime(instance.exports, root, { taskHandler, onError, telemetry });
  runtime.mount();
  return runtime;
}

export class SignalsRuntime {
  constructor(exports, root, options = {}) {
    this.exports = exports;
    this.root = root;
    this.checkProtocol();
    this.views = createMemoryViewCache(exports.memory);
    this.nodes = new Map([[0, root]]);
    this.eventCleanups = new Map();
    this.intervals = new Map();
    this.tasks = new Map();
    this.taskHandler = options.taskHandler ?? null;
    this.telemetryLog = normalizeTelemetry(options.telemetry);
    this.telemetrySeq = 0;
    this.pointerProbeCleanups = [];
    this.onError = options.onError ?? ((err) => {
      setTimeout(() => {
        throw err;
      }, 0);
    });
    // The patch stream is inspectable: `lastCommands` holds the records drained
    // by the most recent host call so guards can assert the per-event patch
    // budget (mirrors the native host's `patches_emitted` discipline).
    this.lastCommands = [];
    if (this.telemetryLog) {
      this.installPointerProbe();
    }
  }

  checkProtocol() {
    if (typeof this.exports.roc_ui_protocol_version !== "function") {
      throw new Error("Signals wasm export roc_ui_protocol_version is missing");
    }
    if (typeof this.exports.roc_ui_protocol_features !== "function") {
      throw new Error("Signals wasm export roc_ui_protocol_features is missing");
    }
    if (typeof this.exports.roc_ui_dynamic_buffer_ptr !== "function") {
      throw new Error("Signals wasm export roc_ui_dynamic_buffer_ptr is missing");
    }
    if (typeof this.exports.roc_ui_dynamic_buffer_len !== "function") {
      throw new Error("Signals wasm export roc_ui_dynamic_buffer_len is missing");
    }
    const version = this.exports.roc_ui_protocol_version();
    if (version !== Protocol.version) {
      throw new Error(
        `Signals wire protocol version mismatch: runtime expects ${Protocol.version}, wasm exports ${version}`,
      );
    }
    const features = this.exports.roc_ui_protocol_features();
    if ((features & requiredProtocolFeatures) !== requiredProtocolFeatures) {
      throw new Error(
        `Signals wire protocol feature mismatch: runtime requires 0x${requiredProtocolFeatures.toString(16)}, wasm exports 0x${features.toString(16)}`,
      );
    }
  }

  liveHostValues() {
    return this.exports.roc_ui_live_host_values?.() ?? 0;
  }

  mount() {
    this.emitTelemetry("host_call", { call: "mount" });
    this.views.callHost(this.exports.roc_ui_mount);
    this.applyPendingCommands("mount");
  }

  unmount() {
    this.emitTelemetry("host_call", { call: "unmount" });
    this.views.callHost(this.exports.roc_ui_unmount);
    this.applyPendingCommands("unmount");
    this.clearPointerProbe();
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
    this.emitTelemetry("host_call", {
      call: "event",
      eventId,
      payloadKind: payloadKindName(payloadKind),
      payloadLen,
      boolValue: boolValue !== 0,
    });
    this.views.callHost(
      this.exports.roc_ui_event,
      eventId,
      payloadKind,
      payloadPtr,
      payloadLen,
      boolValue,
    );
    this.applyPendingCommands(`event:${eventId}`);
  }

  tickTimer(token) {
    this.emitTelemetry("host_call", { call: "timer", token });
    this.views.callHost(this.exports.roc_ui_timer, token);
    this.applyPendingCommands(`timer:${token}`);
  }

  resolveTask(requestId, value, failed = false) {
    const bytes = textEncoder.encode(value);
    const ptr = this.views.callHost(this.exports.roc_alloc, bytes.length, 1).result;
    try {
        this.views.u8.set(bytes, ptr);
        this.emitTelemetry("host_call", {
          call: "resolve_task",
          requestId,
          failed: failed !== false,
          payloadLen: bytes.length,
        });
        this.views.callHost(this.exports.roc_ui_resolve, requestId, ptr, bytes.length, failed ? 1 : 0);
    } catch (err) {
      throw this.runtimeError(err);
    } finally {
      this.views.callHost(this.exports.roc_dealloc, ptr, 1);
    }
    this.tasks.delete(requestId);
    this.applyPendingCommands(`resolve:${requestId}`);
  }

  runtimeError(err) {
    const hostMessage = this.lastHostError();
    if (hostMessage === "") {
      return err;
    }
    const message = err?.message ? `${hostMessage}: ${err.message}` : hostMessage;
    const wrapped = new Error(message);
    wrapped.cause = err;
    return wrapped;
  }

  lastHostError() {
    const ptr = this.exports.roc_ui_last_error_ptr?.() ?? 0;
    const len = this.exports.roc_ui_last_error_len?.() ?? 0;
    if (ptr === 0 || len === 0) {
      return "";
    }
    this.views.afterHostCall();
    return textDecoder.decode(this.views.u8.subarray(ptr, ptr + len));
  }

  reportError(err) {
    this.onError(err);
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

  readDynamicBytes(offset, length) {
    this.views.afterHostCall();
    const base = this.exports.roc_ui_dynamic_buffer_ptr();
    const available = this.exports.roc_ui_dynamic_buffer_len();
    if (length === 0) {
      return new Uint8Array(0);
    }
    if (base === 0) {
      throw new Error("dynamic render command referenced an empty dynamic buffer");
    }
    if (offset + length > available) {
      throw new Error(
        `dynamic render command slice ${offset}:${offset + length} exceeds dynamic buffer length ${available}`,
      );
    }
    return this.views.u8.subarray(base + offset, base + offset + length);
  }

  applyPendingCommands(phase = "host-call") {
    const records = this.readPendingCommands();
    this.lastCommands = records;
    this.emitCommandTelemetry(phase, records);
    for (const record of records) {
      this.applyCommand(record);
    }
    this.emitTelemetry("commands_applied", {
      phase,
      count: records.length,
      domNodes: this.nodes.size,
      eventListeners: this.eventCleanups.size,
      liveHostValues: this.liveHostValues(),
    });
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

      case Op.bindPointerDown:
        this.bindEvent(record.a, "pointerdown", record.b, record.c);
        return;

      case Op.bindPointerUp:
        this.bindEvent(record.a, "pointerup", record.b, record.c);
        return;

      case Op.bindPointerEnter:
        this.bindEvent(record.a, "pointerenter", record.b, record.c);
        return;

      case Op.bindPointerLeave:
        this.bindEvent(record.a, "pointerleave", record.b, record.c);
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

      case Op.extended:
        this.applyDynamicCommand(record.a, record.b);
        return;

      default:
        throw new Error(`unknown render op ${record.op}`);
    }
  }

  applyDynamicCommand(offset, length) {
    const command = this.decodeDynamicCommand(offset, length);
    switch (command.op) {
      case DynamicOp.setAttrText:
        setDynamicTextAttribute(this.node(command.elemId), command.name, command.value);
        return;

      case DynamicOp.removeAttr:
        removeDynamicAttribute(this.node(command.elemId), command.name);
        return;

      default:
        throw new Error(`unknown dynamic render op ${command.op}`);
    }
  }

  decodeDynamicCommand(offset, length) {
    const bytes = this.readDynamicBytes(offset, length);
    if (bytes.byteLength < 8) {
      throw new Error(
        `malformed dynamic render record at byte ${offset}: header needs 8 bytes, got ${bytes.byteLength}`,
      );
    }

    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    const op = view.getUint16(0, true);
    const flags = view.getUint16(2, true);
    const payloadLen = view.getUint32(4, true);
    const totalLen = 8 + align4(payloadLen);
    const opName = dynamicOpName(op);

    if (flags !== 0) {
      throw new Error(
        `malformed dynamic render record at byte ${offset}: ${opName} used unsupported flags 0x${flags.toString(16)}`,
      );
    }
    if (totalLen > bytes.byteLength) {
      throw new Error(
        `malformed dynamic render record at byte ${offset}: ${opName} payload_len ${payloadLen} extends beyond ${bytes.byteLength} bytes`,
      );
    }
    if (totalLen !== bytes.byteLength) {
      throw new Error(
        `malformed dynamic render record at byte ${offset}: ${opName} outer length ${bytes.byteLength} did not match payload_len ${payloadLen}`,
      );
    }

    const cursor = { offset: 8, limit: 8 + payloadLen, recordOffset: offset, opName };
    switch (op) {
      case DynamicOp.setAttrText: {
        const elemId = readDynamicU32(view, cursor, "elem_id");
        const name = readDynamicString(view, cursor, "name");
        const value = readDynamicString(view, cursor, "value");
        assertDynamicPayloadConsumed(cursor);
        return { op, elemId, name, value };
      }

      case DynamicOp.removeAttr: {
        const elemId = readDynamicU32(view, cursor, "elem_id");
        const name = readDynamicString(view, cursor, "name");
        assertDynamicPayloadConsumed(cursor);
        return { op, elemId, name };
      }

      default:
        throw new Error(`unknown dynamic render op ${op} at byte ${offset}`);
    }
  }

  bindEvent(elemId, domEvent, eventId, payloadAccessor) {
    const key = `${elemId}:${domEvent}`;
    this.eventCleanups.get(key)?.();
    const elem = this.node(elemId);
    const listener = (event) => {
      const preventedDefault = preventDefaultForRocEvent(domEvent, event);
      this.emitTelemetry("dom_event", {
        domEvent,
        eventId,
        payloadAccessor: payloadAccessorName(payloadAccessor),
        preventedDefault,
        currentTarget: describeDomNode(event.currentTarget, elemId),
        target: describeDomNode(event.target),
        pointer: describePointerEvent(event),
      });
      this.dispatchEventPayload(eventId, payloadAccessor, event);
    };
    elem.addEventListener(domEvent, listener);
    this.eventCleanups.set(key, () => elem.removeEventListener(domEvent, listener));
    elem.dataset.rocEventId = String(eventId);
    if (domEvent === "pointerdown") {
      elem.dataset.rocPointerDrag = "true";
      elem.draggable = false;
      if (elem.style) {
        elem.style.userSelect = "none";
        elem.style.webkitUserSelect = "none";
        elem.style.touchAction = "none";
      }
    }
    this.emitTelemetry("bind_event", {
      elemId,
      domEvent,
      eventId,
      payloadAccessor: payloadAccessorName(payloadAccessor),
      elem: describeDomNode(elem, elemId),
    });
  }

  dispatchEventPayload(eventId, payloadAccessor, event) {
    switch (payloadAccessor) {
      case PayloadAccessor.none:
        this.emitTelemetry("event_payload", {
          eventId,
          payloadKind: "unit",
          payloadAccessor: payloadAccessorName(payloadAccessor),
        });
        this.dispatchUnit(eventId);
        return;

      case PayloadAccessor.targetValue:
        this.emitTelemetry("event_payload", {
          eventId,
          payloadKind: "str",
          payloadAccessor: payloadAccessorName(payloadAccessor),
          value: event.currentTarget.value,
        });
        this.dispatchString(eventId, event.currentTarget.value);
        return;

      case PayloadAccessor.targetChecked:
        this.emitTelemetry("event_payload", {
          eventId,
          payloadKind: "bool",
          payloadAccessor: payloadAccessorName(payloadAccessor),
          value: event.currentTarget.checked,
        });
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
      if (domEvent === "pointerdown") {
        delete elem.dataset.rocPointerDrag;
      }
    }
    if (elem && domEvent === "pointerdown" && elem.style) {
      elem.style.userSelect = "";
      elem.style.webkitUserSelect = "";
      elem.style.touchAction = "";
    }
    this.emitTelemetry("clear_event", {
      elemId,
      domEvent,
      elem: describeDomNode(elem, elemId),
    });
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
    this.emitTelemetry("start_task", { requestId, name, request });
    if (!this.taskHandler) {
      return;
    }

    let handled;
    try {
      handled = this.taskHandler({ requestId, name, request, signal: controller.signal });
    } catch (err) {
      handled = Promise.reject(err);
    }
    if (handled === null || handled === undefined) {
      return;
    }

    Promise.resolve(handled).then(
      (value) => {
        if (!controller.signal.aborted && this.tasks.has(requestId)) {
          try {
            this.resolveTask(requestId, String(value), false);
          } catch (err) {
            this.reportError(err);
          }
        }
      },
      (err) => {
        if (!controller.signal.aborted && this.tasks.has(requestId)) {
          try {
            this.resolveTask(requestId, String(err?.message ?? err), true);
          } catch (resolveErr) {
            this.reportError(resolveErr);
          }
        }
      },
    );
  }

  cancelTask(requestId) {
    const task = this.tasks.get(requestId);
    if (!task) {
      return;
    }
    task.controller.abort();
    this.tasks.delete(requestId);
    this.emitTelemetry("cancel_task", {
      requestId,
      name: task.name,
      request: task.request,
    });
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
    this.emitTelemetry("clear_dom", {
      domNodes: this.nodes.size,
      eventListeners: this.eventCleanups.size,
      intervals: this.intervals.size,
      tasks: this.tasks.size,
    });
    this.clearAsyncResources();
    for (const cleanup of this.eventCleanups.values()) {
      cleanup();
    }
    this.eventCleanups.clear();
    this.nodes.clear();
    this.nodes.set(0, this.root);
    this.root.replaceChildren();
  }

  emitTelemetry(kind, detail = {}) {
    if (!this.telemetryLog) {
      return;
    }
    this.telemetryLog({
      source: "signals-runtime",
      seq: ++this.telemetrySeq,
      timeMs: Date.now(),
      kind,
      ...detail,
    });
  }

  emitCommandTelemetry(phase, records) {
    if (!this.telemetryLog) {
      return;
    }
    const commands = records.map((record) => this.describeCommand(record));
    const opCounts = {};
    for (const command of commands) {
      opCounts[command.op] = (opCounts[command.op] ?? 0) + 1;
    }
    this.emitTelemetry("commands", {
      phase,
      count: records.length,
      fixedRecordBytes: records.length * this.exports.roc_ui_command_record_words() * 4,
      fixedStringBytes: this.exports.roc_ui_string_buffer_len(),
      dynamicBytes: this.exports.roc_ui_dynamic_buffer_len(),
      opCounts,
      commands,
    });
  }

  describeCommand(record) {
    const op = opName(record.op);
    switch (record.op) {
      case Op.resetDom:
        return { op };

      case Op.createElement:
        return { op, elemId: record.a, tag: this.readString(record.b, record.c) };

      case Op.createText:
        return { op, nodeId: record.a, text: this.readString(record.b, record.c) };

      case Op.appendChild:
        return { op, parentId: record.a, childId: record.b };

      case Op.removeNode:
        return { op, nodeId: record.a, node: describeDomNode(this.nodes.get(record.a), record.a) };

      case Op.moveBefore:
        return { op, parentId: record.a, childId: record.b, beforeId: record.c };

      case Op.setText:
        return { op, nodeId: record.a, text: this.readString(record.b, record.c) };

      case Op.setValue:
        return { op, elemId: record.a, value: this.readString(record.b, record.c) };

      case Op.setChecked:
        return { op, elemId: record.a, checked: record.b !== 0 };

      case Op.setDisabled:
        return { op, elemId: record.a, disabled: record.b !== 0 };

      case Op.setRole:
        return { op, elemId: record.a, role: this.readString(record.b, record.c) };

      case Op.setLabel:
        return { op, elemId: record.a, label: this.readString(record.b, record.c) };

      case Op.setTestId:
        return { op, elemId: record.a, testId: this.readString(record.b, record.c) };

      case Op.setClass:
        return { op, elemId: record.a, className: this.readString(record.b, record.c) };

      case Op.bindClick:
        return this.describeBindCommand(op, record, "click");

      case Op.bindInput:
        return this.describeBindCommand(op, record, "input");

      case Op.bindCheck:
        return this.describeBindCommand(op, record, "change");

      case Op.bindPointerDown:
        return this.describeBindCommand(op, record, "pointerdown");

      case Op.bindPointerUp:
        return this.describeBindCommand(op, record, "pointerup");

      case Op.bindPointerEnter:
        return this.describeBindCommand(op, record, "pointerenter");

      case Op.bindPointerLeave:
        return this.describeBindCommand(op, record, "pointerleave");

      case Op.clearEvent:
        return {
          op,
          elemId: record.a,
          domEvent: domEventForKind[record.b],
          eventKind: record.b,
        };

      case Op.startInterval:
        return { op, token: record.a, periodMs: record.b };

      case Op.cancelInterval:
        return { op, token: record.a };

      case Op.startTask:
        return {
          op,
          requestId: record.a,
          name: this.readString(record.b, record.c),
          request: this.readString(record.d, record.e),
        };

      case Op.cancelTask:
        return { op, requestId: record.a };

      case Op.extended:
        return this.describeDynamicCommand(record.a, record.b);

      default:
        return { op, raw: { ...record } };
    }
  }

  describeDynamicCommand(offset, length) {
    const command = this.decodeDynamicCommand(offset, length);
    switch (command.op) {
      case DynamicOp.setAttrText:
        return {
          op: dynamicOpName(command.op),
          elemId: command.elemId,
          name: command.name,
          value: command.value,
        };

      case DynamicOp.removeAttr:
        return {
          op: dynamicOpName(command.op),
          elemId: command.elemId,
          name: command.name,
        };

      default:
        return { op: dynamicOpName(command.op), offset, length };
    }
  }

  describeBindCommand(op, record, domEvent) {
    return {
      op,
      elemId: record.a,
      domEvent,
      eventId: record.b,
      payloadAccessor: payloadAccessorName(record.c),
    };
  }

  installPointerProbe() {
    if (typeof globalThis.document?.addEventListener !== "function") {
      return;
    }
    for (const domEvent of pointerProbeEvents) {
      const listener = (event) => {
        this.emitTelemetry("pointer_probe", {
          domEvent,
          target: describeDomNode(event.target),
          currentTarget: describeDomNode(event.currentTarget),
          pointer: describePointerEvent(event),
        });
      };
      globalThis.document.addEventListener(domEvent, listener, true);
      this.pointerProbeCleanups.push(() =>
        globalThis.document.removeEventListener(domEvent, listener, true),
      );
    }
    this.emitTelemetry("pointer_probe_installed", { events: [...pointerProbeEvents] });
  }

  clearPointerProbe() {
    for (const cleanup of this.pointerProbeCleanups) {
      cleanup();
    }
    this.pointerProbeCleanups = [];
  }
}

function normalizeTelemetry(telemetry) {
  if (telemetry === undefined || telemetry === null || telemetry === false) {
    return null;
  }
  if (telemetry === true) {
    return consoleTelemetry;
  }
  if (typeof telemetry === "function") {
    return telemetry;
  }
  if (typeof telemetry.log === "function") {
    return (entry) => telemetry.log(entry);
  }
  throw new TypeError("SignalsRuntime telemetry must be true, a function, or an object with log(entry)");
}

function consoleTelemetry(entry) {
  console.log(`[signals] ${JSON.stringify(entry)}`);
}

function opName(op) {
  return opNames[op] ?? `unknown:${op}`;
}

function dynamicOpName(op) {
  return dynamicOpNames[op] ?? `unknown:${op}`;
}

function payloadKindName(kind) {
  return payloadKindNames[kind] ?? `unknown:${kind}`;
}

function payloadAccessorName(accessor) {
  return payloadAccessorNames[accessor] ?? `unknown:${accessor}`;
}

function preventDefaultForRocEvent(domEvent, event) {
  if (!domEvent.startsWith("pointer")) {
    return false;
  }
  if (typeof event?.preventDefault !== "function") {
    return false;
  }
  event.preventDefault();
  return true;
}

function setNodeText(node, value) {
  if (node.nodeType === Node.TEXT_NODE) {
    node.nodeValue = value;
  } else {
    node.textContent = value;
  }
}

function describeDomNode(node, id = undefined) {
  if (!node) {
    return null;
  }
  if (node.nodeType === Node.TEXT_NODE) {
    return compactObject({
      id,
      type: "text",
      text: compactText(node.nodeValue),
    });
  }

  return compactObject({
    id,
    type: "element",
    tag: node.tagName?.toLowerCase(),
    role: node.getAttribute?.("role"),
    label: node.getAttribute?.("aria-label"),
    testId: node.getAttribute?.("data-testid"),
    className: node.getAttribute?.("class"),
    rocEventId: node.dataset?.rocEventId,
    text: compactText(node.textContent),
  });
}

function describePointerEvent(event) {
  if (!event || !event.type?.startsWith("pointer")) {
    return null;
  }
  return compactObject({
    pointerId: event.pointerId,
    pointerType: event.pointerType,
    isPrimary: event.isPrimary,
    button: event.button,
    buttons: event.buttons,
    clientX: event.clientX,
    clientY: event.clientY,
    pageX: event.pageX,
    pageY: event.pageY,
  });
}

function compactObject(input) {
  const out = {};
  for (const [key, value] of Object.entries(input)) {
    if (value !== undefined && value !== null && value !== "") {
      out[key] = value;
    }
  }
  return out;
}

function compactText(value) {
  if (value === undefined || value === null) {
    return "";
  }
  const text = String(value).replace(/\s+/g, " ").trim();
  return text.length > 160 ? `${text.slice(0, 157)}...` : text;
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

function align4(value) {
  return (value + 3) & ~3;
}

function ensureDynamicAvailable(cursor, byteCount, field) {
  if (cursor.offset + byteCount <= cursor.limit) {
    return;
  }
  throw new Error(
    `malformed dynamic render record at byte ${cursor.recordOffset}: ${cursor.opName} operand ${field} extends beyond payload_len`,
  );
}

function readDynamicU32(view, cursor, field) {
  ensureDynamicAvailable(cursor, 4, field);
  const value = view.getUint32(cursor.offset, true);
  cursor.offset += 4;
  return value;
}

function readDynamicString(view, cursor, field) {
  const length = readDynamicU32(view, cursor, `${field}_len`);
  ensureDynamicAvailable(cursor, length, field);
  const bytes = new Uint8Array(view.buffer, view.byteOffset + cursor.offset, length);
  cursor.offset += length;
  try {
    return dynamicTextDecoder.decode(bytes);
  } catch (err) {
    throw new Error(
      `malformed dynamic render record at byte ${cursor.recordOffset}: ${cursor.opName} ${field} was not valid UTF-8`,
      { cause: err },
    );
  }
}

function assertDynamicPayloadConsumed(cursor) {
  if (cursor.offset === cursor.limit) {
    return;
  }
  throw new Error(
    `malformed dynamic render record at byte ${cursor.recordOffset}: ${cursor.opName} left ${cursor.limit - cursor.offset} trailing payload bytes`,
  );
}

function setDynamicTextAttribute(node, name, value) {
  if (name === "role") {
    setRole(node, value);
  } else if (name === "class") {
    setClass(node, value);
  } else {
    node.setAttribute(name, value);
  }
}

function removeDynamicAttribute(node, name) {
  if (name === "class") {
    setClass(node, "");
  } else {
    node.removeAttribute(name);
  }
}
