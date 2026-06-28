import test from "node:test";
import assert from "node:assert/strict";

import {
  DynamicOp,
  ListenerOptions,
  Op,
  PayloadAccessor,
  PayloadKind,
  Protocol,
  ProtocolFeature,
  SignalsRuntime,
  opsApiTextTaskHandler,
} from "./runtime.mjs";
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
const DYN_BASE = 24576;
const ERROR_BASE = 32768;
const DEFAULT_ALLOC_BASE = 40960;
const RECORD_WORDS = 6;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const PayloadSpecTag = Object.freeze({
  unit: 1,
  text: 2,
  bool: 3,
  record: 4,
});

const PayloadSpecSource = Object.freeze({
  event: 1,
  target: 2,
  currentTarget: 3,
});

const PayloadSpecLeaf = Object.freeze({
  key: 1,
  value: 2,
  checked: 3,
  shiftKey: 4,
});

const unitPayloadSpec = new Uint8Array([PayloadSpecTag.unit]);
const keyShiftPayloadSpec = concatBytes([
  new Uint8Array([PayloadSpecTag.record, 2]),
  fieldSpec("key", new Uint8Array([PayloadSpecTag.text, PayloadSpecSource.event, PayloadSpecLeaf.key])),
  fieldSpec(
    "shift_key",
    new Uint8Array([PayloadSpecTag.bool, PayloadSpecSource.event, PayloadSpecLeaf.shiftKey]),
  ),
]);

class MockHost {
  constructor({
    allocBase = DEFAULT_ALLOC_BASE,
    protocolVersion = Protocol.version,
    protocolFeatures = ProtocolFeature.dynamicAttrs | ProtocolFeature.dynamicEvents,
  } = {}) {
    this.memory = new WebAssembly.Memory({ initial: 1 });
    this.cmdLen = 0;
    this.strLen = 0;
    this.dynamicLen = 0;
    this.lastErrorLen = 0;
    this.allocPtr = allocBase;
    this.protocolVersion = protocolVersion;
    this.protocolFeatures = protocolFeatures;
    this.dispatches = [];
    this.timers = [];
    this.resolutions = [];
    this.resolveTrapMessage = null;
    this.mountScript = [];
    this.eventResponses = new Map();
    this.timerResponses = new Map();

    this.exports = {
      memory: this.memory,
      roc_ui_protocol_version: () => this.protocolVersion,
      roc_ui_protocol_features: () => this.protocolFeatures,
      roc_ui_command_record_words: () => RECORD_WORDS,
      roc_ui_command_buffer_ptr: () => (this.cmdLen === 0 ? 0 : CMD_BASE),
      roc_ui_command_buffer_len: () => this.cmdLen,
      roc_ui_string_buffer_ptr: () => STR_BASE,
      roc_ui_string_buffer_len: () => this.strLen,
      roc_ui_dynamic_buffer_ptr: () => (this.dynamicLen === 0 ? 0 : DYN_BASE),
      roc_ui_dynamic_buffer_len: () => this.dynamicLen,
      roc_ui_last_error_ptr: () => (this.lastErrorLen === 0 ? 0 : ERROR_BASE),
      roc_ui_last_error_len: () => this.lastErrorLen,
      roc_ui_live_host_values: () => 0,
      roc_alloc: (len) => this.alloc(len),
      roc_dealloc: () => {},
      roc_ui_mount: () => this.writeCommands(this.mountScript),
      roc_ui_unmount: () => {
        this.cmdLen = 0;
        this.strLen = 0;
        this.dynamicLen = 0;
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
        if (this.resolveTrapMessage !== null) {
          this.writeLastError(this.resolveTrapMessage);
          throw new WebAssembly.RuntimeError("unreachable");
        }
        this.writeCommands([]);
      },
      roc_ui_event: (eventId, kind, ptr, len, boolValue) => {
        const dispatch = { eventId, kind };
        if (kind === PayloadKind.str) {
          dispatch.payload = decoder.decode(new Uint8Array(this.memory.buffer, ptr, len));
        } else if (kind === PayloadKind.bool) {
          dispatch.payload = boolValue !== 0;
        } else if (kind === PayloadKind.bytes) {
          dispatch.payloadBytes = [...new Uint8Array(this.memory.buffer, ptr, len)];
        }
        this.dispatches.push(dispatch);
        const respond = this.eventResponses.get(eventId);
        this.writeCommands(respond ? respond(dispatch) : []);
      },
    };
  }

  writeLastError(message) {
    const bytes = encoder.encode(message);
    new Uint8Array(this.memory.buffer).set(bytes, ERROR_BASE);
    this.lastErrorLen = bytes.length;
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
    let dynamicOffset = 0;
    script.forEach((entry, index) => {
      let op = entry.op;
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
      if (entry.dynamic !== undefined || entry.dynamicBytes !== undefined) {
        const encoded =
          entry.dynamicBytes === undefined
            ? encodeDynamicRecord(entry.dynamic)
            : toUint8Array(entry.dynamicBytes);
        bytes.set(encoded, DYN_BASE + dynamicOffset);
        op = Op.extended;
        a = dynamicOffset;
        b = entry.dynamicLength ?? encoded.length;
        dynamicOffset += encoded.length;
      }
      const base = CMD_BASE + index * RECORD_WORDS * 4;
      view.setUint32(base, op, true);
      view.setUint32(base + 4, a, true);
      view.setUint32(base + 8, b, true);
      view.setUint32(base + 12, c, true);
      view.setUint32(base + 16, d, true);
      view.setUint32(base + 20, e, true);
    });
    this.cmdLen = script.length;
    this.strLen = strOffset;
    this.dynamicLen = dynamicOffset;
  }
}

function encodeDynamicRecord(spec) {
  const payload = spec.payloadBytes === undefined ? encodeDynamicPayload(spec) : toUint8Array(spec.payloadBytes);
  const totalLen = 8 + align4(payload.length);
  const out = new Uint8Array(totalLen);
  const view = new DataView(out.buffer);
  view.setUint16(0, spec.op, true);
  view.setUint16(2, spec.flags ?? 0, true);
  view.setUint32(4, payload.length, true);
  out.set(payload, 8);
  return out;
}

function encodeDynamicPayload(spec) {
  switch (spec.op) {
    case DynamicOp.setAttrText:
      return concatBytes([
        u32Bytes(spec.elemId),
        stringBytes(spec.name),
        stringBytes(spec.value),
      ]);

    case DynamicOp.removeAttr:
      return concatBytes([u32Bytes(spec.elemId), stringBytes(spec.name)]);

    case DynamicOp.bindEvent:
      return concatBytes([
        u32Bytes(spec.elemId),
        u32Bytes(spec.eventId),
        stringBytes(spec.eventName),
        u32Bytes(spec.options ?? 0),
        u32Bytes(spec.payloadKind),
        bytesField(spec.payloadSpec),
      ]);

    case DynamicOp.clearEvent:
      return concatBytes([u32Bytes(spec.elemId), stringBytes(spec.eventName)]);

    default:
      return new Uint8Array(0);
  }
}

function stringBytes(value) {
  const bytes = encoder.encode(value);
  return concatBytes([u32Bytes(bytes.length), bytes]);
}

function bytesField(value) {
  const bytes = toUint8Array(value);
  return concatBytes([u32Bytes(bytes.length), bytes]);
}

function fieldSpec(name, spec) {
  const nameBytes = encoder.encode(name);
  return concatBytes([new Uint8Array([nameBytes.length]), nameBytes, spec]);
}

function keyShiftBytes(key, shiftKey) {
  const keyBytes = encoder.encode(key);
  return [...concatBytes([u32Bytes(keyBytes.length), keyBytes, new Uint8Array([shiftKey ? 1 : 0])])];
}

function u32Bytes(value) {
  const out = new Uint8Array(4);
  new DataView(out.buffer).setUint32(0, value, true);
  return out;
}

function concatBytes(chunks) {
  const len = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
  const out = new Uint8Array(len);
  let offset = 0;
  for (const chunk of chunks) {
    out.set(chunk, offset);
    offset += chunk.length;
  }
  return out;
}

function align4(value) {
  return (value + 3) & ~3;
}

function toUint8Array(value) {
  return value instanceof Uint8Array ? value : new Uint8Array(value);
}

function mountWith(mountScript, options = {}) {
  const { taskHandler, onError, telemetry, ...hostOptions } = options;
  const host = new MockHost(hostOptions);
  host.mountScript = mountScript;
  const root = installDomDouble();
  const runtime = new SignalsRuntime(host.exports, root, { taskHandler, onError, telemetry });
  runtime.mount();
  return { host, root, runtime };
}

test("protocol checks reject incompatible wasm exports", () => {
  assert.throws(
    () => new SignalsRuntime(new MockHost({ protocolVersion: Protocol.version + 1 }).exports, installDomDouble()),
    /wire protocol version mismatch/,
  );
  assert.throws(
    () => new SignalsRuntime(new MockHost({ protocolFeatures: 0 }).exports, installDomDouble()),
    /wire protocol feature mismatch/,
  );

  const host = new MockHost();
  delete host.exports.roc_ui_protocol_features;
  assert.throws(
    () => new SignalsRuntime(host.exports, installDomDouble()),
    /roc_ui_protocol_features is missing/,
  );

  const missingDynamic = new MockHost();
  delete missingDynamic.exports.roc_ui_dynamic_buffer_ptr;
  assert.throws(
    () => new SignalsRuntime(missingDynamic.exports, installDomDouble()),
    /roc_ui_dynamic_buffer_ptr is missing/,
  );
});

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
    { op: Op.setClass, a: 2, s: "rounded-md border-zinc-300" },
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
  assert.equal(input.getAttribute("class"), "rounded-md border-zinc-300");
  assert.equal(findTextNode(root, "before"), null);
});

test("dynamic attribute commands set and remove DOM attributes", () => {
  const telemetry = [];
  const { host, root, runtime } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.createElement, a: 1, s: "button" },
      {
        dynamic: {
          op: DynamicOp.setAttrText,
          elemId: 1,
          name: "aria-label",
          value: "Save",
        },
      },
      {
        dynamic: {
          op: DynamicOp.setAttrText,
          elemId: 1,
          name: "data-mode",
          value: "primary",
        },
      },
      {
        dynamic: {
          op: DynamicOp.setAttrText,
          elemId: 1,
          name: "class",
          value: "toolbar",
        },
      },
      { op: Op.appendChild, a: 0, b: 1 },
    ],
    { telemetry: (entry) => telemetry.push(entry) },
  );

  const button = findNode(root, (node) => node.tagName === "BUTTON");
  assert.equal(button.getAttribute("aria-label"), "Save");
  assert.equal(button.getAttribute("data-mode"), "primary");
  assert.equal(button.getAttribute("class"), "toolbar");

  const mountCommands = telemetry.find((entry) => entry.kind === "commands" && entry.phase === "mount");
  assert.equal(mountCommands.opCounts.set_attr_text, 3);
  assert.equal(mountCommands.dynamicBytes, 104);
  assert.equal(mountCommands.fixedRecordBytes, 6 * RECORD_WORDS * 4);

  host.writeCommands([
    {
      dynamic: {
        op: DynamicOp.removeAttr,
        elemId: 1,
        name: "data-mode",
      },
    },
    {
      dynamic: {
        op: DynamicOp.removeAttr,
        elemId: 1,
        name: "class",
      },
    },
  ]);
  runtime.applyPendingCommands("dynamic-remove");

  assert.equal(button.getAttribute("aria-label"), "Save");
  assert.equal(button.getAttribute("data-mode"), null);
  assert.equal(button.getAttribute("class"), null);

  const removeCommands = telemetry.find(
    (entry) => entry.kind === "commands" && entry.phase === "dynamic-remove",
  );
  assert.equal(removeCommands.opCounts.remove_attr, 2);
  assert.equal(removeCommands.dynamicBytes, 52);
});

test("malformed dynamic command records fail closed", () => {
  const { host, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const record = (payloadBytes, overrides = {}) =>
    encodeDynamicRecord({
      op: overrides.op ?? DynamicOp.removeAttr,
      flags: overrides.flags ?? 0,
      payloadBytes,
    });
  const validEmptyNameRemove = record(concatBytes([u32Bytes(1), u32Bytes(0)]));
  const withExtraByte = new Uint8Array(validEmptyNameRemove.length + 4);
  withExtraByte.set(validEmptyNameRemove);

  const cases = [
    [{ dynamicBytes: new Uint8Array([1, 0, 0, 0]) }, /header needs 8 bytes/],
    [
      { dynamicBytes: encodeDynamicRecord({ op: DynamicOp.removeAttr, flags: 1, payloadBytes: [] }) },
      /unsupported flags/,
    ],
    [
      {
        dynamicBytes: record(concatBytes([u32Bytes(1), u32Bytes(99), encoder.encode("abc")])),
      },
      /operand name extends beyond payload_len/,
    ],
    [
      {
        dynamicBytes: record(concatBytes([u32Bytes(1), u32Bytes(1), new Uint8Array([0xff])])),
      },
      /name was not valid UTF-8/,
    ],
    [
      {
        dynamicBytes: record(concatBytes([u32Bytes(1), u32Bytes(0), new Uint8Array([1])])),
      },
      /left 1 trailing payload bytes/,
    ],
    [{ dynamicBytes: withExtraByte }, /outer length 20 did not match payload_len 8/],
    [
      { dynamicBytes: encodeDynamicRecord({ op: 65535, payloadBytes: [] }) },
      /unknown dynamic render op 65535/,
    ],
    [
      { dynamicBytes: new Uint8Array([1, 0, 0, 0]), dynamicLength: 12 },
      /exceeds dynamic buffer length/,
    ],
  ];

  for (const [entry, pattern] of cases) {
    host.writeCommands([entry]);
    assert.throws(() => runtime.applyPendingCommands("malformed-dynamic"), pattern);
  }
});

test("event payloads round-trip through the wasm memory boundary", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "click" },
    { op: Op.bindClick, a: 1, b: 10, c: PayloadAccessor.none },
    { op: Op.appendChild, a: 0, b: 1 },
    { op: Op.createElement, a: 2, s: "input" },
    { op: Op.bindInput, a: 2, b: 11, c: PayloadAccessor.targetValue },
    { op: Op.appendChild, a: 0, b: 2 },
    { op: Op.createElement, a: 3, s: "input" },
    { op: Op.setRole, a: 3, s: "checkbox" },
    { op: Op.bindCheck, a: 3, b: 12, c: PayloadAccessor.targetChecked },
    { op: Op.appendChild, a: 0, b: 3 },
    { op: Op.createElement, a: 4, s: "section" },
    { op: Op.setText, a: 4, s: "drop-zone" },
    { op: Op.bindPointerDown, a: 4, b: 13, c: PayloadAccessor.none },
    { op: Op.bindPointerEnter, a: 4, b: 14, c: PayloadAccessor.none },
    { op: Op.bindPointerUp, a: 4, b: 15, c: PayloadAccessor.none },
    { op: Op.bindPointerLeave, a: 4, b: 16, c: PayloadAccessor.none },
    { op: Op.appendChild, a: 0, b: 4 },
  ]);

  fireEvent(findByText(root, "button", "click"), "click");
  const textInput = findAll(root, (node) => node.tagName === "INPUT")[0];
  textInput.value = "typed text";
  fireEvent(textInput, "input");
  const checkbox = findAll(root, (node) => node.tagName === "INPUT")[1];
  checkbox.checked = true;
  fireEvent(checkbox, "change");
  const dropZone = findByText(root, "section", "drop-zone");
  assert.equal(dropZone.dataset.rocPointerDrag, "true");
  assert.equal(dropZone.draggable, false);
  assert.equal(dropZone.style.userSelect, "none");
  assert.equal(dropZone.style.touchAction, "none");
  assert.equal(fireEvent(dropZone, "pointerdown").defaultPrevented, true);
  assert.equal(fireEvent(dropZone, "pointerenter").defaultPrevented, true);
  assert.equal(fireEvent(dropZone, "pointerup").defaultPrevented, true);
  assert.equal(fireEvent(dropZone, "pointerleave").defaultPrevented, true);

  assert.deepEqual(host.dispatches, [
    { eventId: 10, kind: PayloadKind.unit },
    { eventId: 11, kind: PayloadKind.str, payload: "typed text" },
    { eventId: 12, kind: PayloadKind.bool, payload: true },
    { eventId: 13, kind: PayloadKind.unit },
    { eventId: 14, kind: PayloadKind.unit },
    { eventId: 15, kind: PayloadKind.unit },
    { eventId: 16, kind: PayloadKind.unit },
  ]);
});

test("dynamic keydown events dispatch explicit key shift byte payloads", () => {
  const { host, root } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "input" },
    {
      dynamic: {
        op: DynamicOp.bindEvent,
        elemId: 1,
        eventName: "keydown",
        eventId: 21,
        options: 0,
        payloadKind: PayloadKind.bytes,
        payloadSpec: keyShiftPayloadSpec,
      },
    },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  fireEvent(input, "keydown", { key: "K", shiftKey: true });

  assert.deepEqual(host.dispatches, [
    { eventId: 21, kind: PayloadKind.bytes, payloadBytes: keyShiftBytes("K", true) },
  ]);
});

test("dynamic submit events apply static prevent-default policy", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "form" },
    {
      dynamic: {
        op: DynamicOp.bindEvent,
        elemId: 1,
        eventName: "submit",
        eventId: 22,
        options: ListenerOptions.preventDefault,
        payloadKind: PayloadKind.unit,
        payloadSpec: unitPayloadSpec,
      },
    },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const form = findNode(root, (node) => node.tagName === "FORM");
  const event = fireEvent(form, "submit");
  assert.equal(event.defaultPrevented, true);
  assert.deepEqual(host.dispatches, [{ eventId: 22, kind: PayloadKind.unit }]);

  host.writeCommands([
    {
      dynamic: {
        op: DynamicOp.clearEvent,
        elemId: 1,
        eventName: "submit",
      },
    },
  ]);
  runtime.applyPendingCommands("clear-submit");
  fireEvent(form, "submit");
  assert.deepEqual(host.dispatches, [{ eventId: 22, kind: PayloadKind.unit }]);
});

test("malformed dynamic event payload descriptors fail closed", () => {
  const { host, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "input" },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const bind = (payloadSpec, overrides = {}) => ({
    dynamic: {
      op: DynamicOp.bindEvent,
      elemId: 1,
      eventName: "keydown",
      eventId: 31,
      options: overrides.options ?? 0,
      payloadKind: overrides.payloadKind ?? PayloadKind.bytes,
      payloadSpec,
    },
  });

  const duplicateFields = concatBytes([
    new Uint8Array([PayloadSpecTag.record, 2]),
    fieldSpec("key", new Uint8Array([PayloadSpecTag.text, PayloadSpecSource.event, PayloadSpecLeaf.key])),
    fieldSpec("key", new Uint8Array([PayloadSpecTag.text, PayloadSpecSource.event, PayloadSpecLeaf.key])),
  ]);

  const cases = [
    [bind(new Uint8Array([99])), /malformed event payload descriptor.*unknown descriptor tag 99/],
    [
      bind(new Uint8Array([PayloadSpecTag.record, 1, 3, 0x6b])),
      /malformed event payload descriptor.*record_field_name extends beyond descriptor length/,
    ],
    [bind(duplicateFields), /malformed event payload descriptor.*duplicated/],
    [
      bind(keyShiftPayloadSpec, { payloadKind: PayloadKind.unit }),
      /malformed event payload descriptor.*payload_kind unit did not match descriptor bytes/,
    ],
    [
      bind(keyShiftPayloadSpec, { options: 1 << 12 }),
      /unsupported listener option bits/,
    ],
  ];

  for (const [entry, pattern] of cases) {
    host.writeCommands([entry]);
    assert.throws(() => runtime.applyPendingCommands("bad-bind-event"), pattern);
  }
});

test("memory growth during byte payload allocation keeps response commands readable", () => {
  const { host, root } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.createElement, a: 1, s: "input" },
      {
        dynamic: {
          op: DynamicOp.bindEvent,
          elemId: 1,
          eventName: "keydown",
          eventId: 41,
          options: 0,
          payloadKind: PayloadKind.bytes,
          payloadSpec: keyShiftPayloadSpec,
        },
      },
      { op: Op.appendChild, a: 0, b: 1 },
      { op: Op.createText, a: 2, s: "start" },
      { op: Op.appendChild, a: 0, b: 2 },
    ],
    { allocBase: PAGE - 1 },
  );
  host.eventResponses.set(41, () => [
    { op: Op.setText, a: 2, s: "keyed" },
    {
      dynamic: {
        op: DynamicOp.setAttrText,
        elemId: 1,
        name: "data-last-key",
        value: "Enter",
      },
    },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  const before = host.memory.buffer.byteLength;
  fireEvent(input, "keydown", { key: "Enter", shiftKey: false });

  assert.ok(host.memory.buffer.byteLength > before);
  assert.deepEqual(host.dispatches, [
    { eventId: 41, kind: PayloadKind.bytes, payloadBytes: keyShiftBytes("Enter", false) },
  ]);
  assert.ok(findTextNode(root, "keyed"));
  assert.equal(input.getAttribute("data-last-key"), "Enter");
});

test("clear_event and remove_node release DOM listeners", () => {
  const { host, root, runtime } = mountWith([
    { op: Op.resetDom },
    { op: Op.createElement, a: 1, s: "button" },
    { op: Op.setText, a: 1, s: "click" },
    { op: Op.bindClick, a: 1, b: 1, c: PayloadAccessor.none },
    { op: Op.appendChild, a: 0, b: 1 },
  ]);

  const button = findByText(root, "button", "click");
  runtime.applyCommand({ op: Op.clearEvent, a: 1, b: 1, c: 0, d: 0, e: 0 });
  fireEvent(button, "click");
  assert.deepEqual(host.dispatches, []);

  runtime.applyCommand({
    op: Op.bindClick,
    a: 1,
    b: 2,
    c: PayloadAccessor.none,
    d: 0,
    e: 0,
  });
  runtime.applyCommand({ op: Op.removeNode, a: 1, b: 0, c: 0, d: 0, e: 0 });
  fireEvent(button, "click");
  assert.deepEqual(host.dispatches, []);
});

test("telemetry records command batches DOM events and event payload dispatches", () => {
  const telemetry = [];
  const { host, root } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.createElement, a: 1, s: "button" },
      { op: Op.setText, a: 1, s: "click" },
      { op: Op.bindClick, a: 1, b: 17, c: PayloadAccessor.none },
      { op: Op.appendChild, a: 0, b: 1 },
    ],
    { telemetry: (entry) => telemetry.push(entry) },
  );
  host.eventResponses.set(17, () => [{ op: Op.setText, a: 1, s: "clicked" }]);

  fireEvent(findByText(root, "button", "click"), "click");

  assert.equal(findByText(root, "button", "clicked").textContent, "clicked");
  assert.ok(
    telemetry.some(
      (entry) =>
        entry.kind === "commands" &&
        entry.phase === "mount" &&
        entry.opCounts.create_element === 1 &&
        entry.opCounts.bind_click === 1,
    ),
  );
  assert.ok(
    telemetry.some(
      (entry) =>
        entry.kind === "dom_event" &&
        entry.domEvent === "click" &&
        entry.eventId === 17 &&
        entry.currentTarget.tag === "button",
    ),
  );
  assert.ok(
    telemetry.some(
      (entry) =>
        entry.kind === "event_payload" &&
        entry.eventId === 17 &&
        entry.payloadKind === "unit" &&
        entry.payloadAccessor === "none",
    ),
  );
  assert.ok(
    telemetry.some(
      (entry) =>
        entry.kind === "commands" &&
        entry.phase === "event:17" &&
        entry.count === 1 &&
        entry.commands[0].op === "set_text" &&
        entry.commands[0].text === "clicked",
    ),
  );
});

test("memory growth during dispatch keeps the response command stream readable", () => {
  const { host, root } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.createElement, a: 1, s: "input" },
      { op: Op.bindInput, a: 1, b: 2, c: PayloadAccessor.targetValue },
      { op: Op.appendChild, a: 0, b: 1 },
      { op: Op.createText, a: 2, s: "start" },
      { op: Op.appendChild, a: 0, b: 2 },
    ],
    { allocBase: PAGE - 1 },
  );
  host.eventResponses.set(2, (dispatch) => [
    { op: Op.setText, a: 2, s: dispatch.payload },
    {
      dynamic: {
        op: DynamicOp.setAttrText,
        elemId: 1,
        name: "data-last-input",
        value: dispatch.payload,
      },
    },
  ]);

  const input = findNode(root, (node) => node.tagName === "INPUT");
  input.value = "after-grow";
  const before = host.memory.buffer.byteLength;
  fireEvent(input, "input");

  assert.ok(host.memory.buffer.byteLength > before);
  assert.deepEqual(host.dispatches, [
    { eventId: 2, kind: PayloadKind.str, payload: "after-grow" },
  ]);
  assert.ok(findTextNode(root, "after-grow"));
  assert.equal(input.getAttribute("data-last-input"), "after-grow");
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

test("task handler rejections resolve through the task failure path", async () => {
  const { host } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.startTask, a: 8, strings: ["lookup", "roc"] },
    ],
    {
      taskHandler: () => Promise.reject(new Error("offline")),
    },
  );

  await Promise.resolve();
  await Promise.resolve();

  assert.deepEqual(host.resolutions, [
    { requestId: 8, payload: "offline", failed: true },
  ]);
});

test("async task resolution traps report onError without retrying as task failure", async () => {
  const errors = [];
  const { host } = mountWith(
    [
      { op: Op.resetDom },
      { op: Op.startTask, a: 9, strings: ["lookup", "roc"] },
    ],
    {
      taskHandler: () => Promise.resolve("ready payload"),
      onError: (err) => errors.push(err),
    },
  );
  host.resolveTrapMessage = "roc_ui_resolve trapped while applying task result";

  await Promise.resolve();
  await Promise.resolve();

  assert.deepEqual(host.resolutions, [
    { requestId: 9, payload: "ready payload", failed: false },
  ]);
  assert.equal(errors.length, 1);
  assert.match(errors[0].message, /roc_ui_resolve trapped while applying task result/);
});

test("ops API text task handler fetches only documented endpoints", async () => {
  const originalFetch = globalThis.fetch;
  const calls = [];
  globalThis.fetch = async (url, options) => {
    calls.push({ url, accept: options.headers.Accept, aborted: options.signal.aborted });
    return {
      ok: true,
      text: async () => "summary rows",
    };
  };

  try {
    const signal = new AbortController().signal;
    assert.equal(
      opsApiTextTaskHandler({ name: "lookup", request: "roc", signal }),
      null,
    );

    const value = await opsApiTextTaskHandler({
      name: "http:get-text:summary",
      request: "/api/ops/summary",
      signal,
    });
    assert.equal(value, "summary rows");
    assert.deepEqual(calls, [
      { url: "/api/ops/summary", accept: "text/plain", aborted: false },
    ]);

    assert.throws(
      () =>
        opsApiTextTaskHandler({
          name: "http:get-text:private",
          request: "/api/private",
          signal,
        }),
      /unsupported ops API text endpoint/,
    );
  } finally {
    globalThis.fetch = originalFetch;
  }
});
