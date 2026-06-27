const align4 = (n) => (n + 3) & ~3;
const utf8Len = (text) => new TextEncoder().encode(text).length;

const FIXED_RECORD = 24;
const DYNAMIC_HEADER = 8; // u16 op, u16 flags, u32 payload_len
const U32 = 4;
const BOOL = 1;
const STR_LEN = 4; // first spike assumes per-string u32 byte lengths

function fixedRecord(strings = []) {
  return FIXED_RECORD + strings.reduce((sum, text) => sum + utf8Len(text), 0);
}

function dynamicPayload(...parts) {
  let bytes = 0;
  for (const part of parts) {
    if (part === "u32") {
      bytes += U32;
    } else if (part === "bool") {
      bytes += BOOL;
    } else if (part && part.string !== undefined) {
      bytes += STR_LEN + utf8Len(part.string);
    } else if (part && part.bytes !== undefined) {
      bytes += part.bytes;
    } else {
      throw new Error(`unknown dynamic part ${JSON.stringify(part)}`);
    }
  }
  return DYNAMIC_HEADER + align4(bytes);
}

function fixedRefsBatch({ commands, strings = [], specs = [] }) {
  const tableBytes =
    strings.reduce((sum, text) => sum + 8 + utf8Len(text), 0) +
    specs.reduce((sum, spec) => sum + 8 + spec.bytes, 0);
  return commands * FIXED_RECORD + tableBytes;
}

const commands = {
  reset: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload(),
  }),
  createElement: (tag) => ({
    fixed: fixedRecord([tag]),
    dynamic: dynamicPayload("u32", { string: tag }),
  }),
  createText: (text) => ({
    fixed: fixedRecord([text]),
    dynamic: dynamicPayload("u32", { string: text }),
  }),
  appendChild: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload("u32", "u32"),
  }),
  removeNode: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload("u32"),
  }),
  moveBefore: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload("u32", "u32", "u32"),
  }),
  setText: (text) => ({
    fixed: fixedRecord([text]),
    dynamic: dynamicPayload("u32", { string: text }),
  }),
  setBool: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload("u32", "bool"),
  }),
  bindHotEvent: () => ({
    fixed: fixedRecord(),
    dynamic: dynamicPayload("u32", "u32", "u32"),
  }),
  startTask: (name, request) => ({
    fixed: fixedRecord([name, request]),
    dynamic: dynamicPayload("u32", { string: name }, { string: request }),
  }),
  setAttr: (name, value) => ({
    fixed: fixedRecord([name, value]),
    dynamic: dynamicPayload("u32", { string: name }, { string: value }),
  }),
  bindEventSpec: (eventName, specBytes) => ({
    fixed: fixedRecord([eventName]) + specBytes,
    dynamic: dynamicPayload("u32", "u32", { string: eventName }, "u32", { bytes: specBytes }),
  }),
  httpStart: (method, url, headerName, headerValue, body) => ({
    fixed: fixedRecord([method, url, headerName, headerValue, body]),
    dynamic: dynamicPayload(
      "u32",
      { string: method },
      { string: url },
      { string: headerName },
      { string: headerValue },
      { string: body },
    ),
  }),
};

function sum(list, key) {
  return list.reduce((total, item) => total + item[key], 0);
}

const keyboardPayloadSpecBytes = 24; // record{ key: event.key text, shift_key: event.shiftKey bool }

const scenarios = [
  {
    name: "initial simple mount",
    ops: [
      commands.reset(),
      commands.createElement("div"),
      commands.appendChild(),
      commands.createElement("button"),
      commands.setText("Save"),
      commands.bindHotEvent(),
      commands.appendChild(),
      commands.createElement("input"),
      commands.setText("Ada"),
      commands.bindHotEvent(),
      commands.appendChild(),
    ],
    fixedRefs: fixedRefsBatch({ commands: 11, strings: ["div", "button", "Save", "input", "Ada"] }),
    hybridUsesDynamic: false,
  },
  {
    name: "text update",
    ops: [commands.setText("Count: 42")],
    fixedRefs: fixedRefsBatch({ commands: 1, strings: ["Count: 42"] }),
    hybridUsesDynamic: false,
  },
  {
    name: "class update",
    ops: [commands.setText("rounded-md border-zinc-300")],
    fixedRefs: fixedRefsBatch({ commands: 1, strings: ["rounded-md border-zinc-300"] }),
    hybridUsesDynamic: false,
  },
  {
    name: "keyed row insert",
    ops: [
      commands.createElement("li"),
      commands.createText("Alert 17"),
      commands.appendChild(),
      commands.appendChild(),
    ],
    fixedRefs: fixedRefsBatch({ commands: 4, strings: ["li", "Alert 17"] }),
    hybridUsesDynamic: false,
  },
  {
    name: "keyed row move",
    ops: [commands.moveBefore()],
    fixedRefs: fixedRefsBatch({ commands: 1 }),
    hybridUsesDynamic: false,
  },
  {
    name: "future SetAttr aria-label",
    ops: [commands.setAttr("aria-label", "Search")],
    fixedRefs: fixedRefsBatch({ commands: 1, strings: ["aria-label", "Search"] }),
    hybridUsesDynamic: true,
  },
  {
    name: "future BindEvent key payload",
    ops: [commands.bindEventSpec("keydown", keyboardPayloadSpecBytes)],
    fixedRefs: fixedRefsBatch({
      commands: 1,
      strings: ["keydown"],
      specs: [{ bytes: keyboardPayloadSpecBytes }],
    }),
    hybridUsesDynamic: true,
  },
  {
    name: "current StartTask text",
    ops: [commands.startTask("http:get-text:summary", "/api/ops/summary")],
    fixedRefs: fixedRefsBatch({ commands: 1, strings: ["http:get-text:summary", "/api/ops/summary"] }),
    hybridUsesDynamic: false,
  },
  {
    name: "future HTTP request",
    ops: [commands.httpStart("POST", "/api/orders", "content-type", "application/json", "{\"id\":17}")],
    fixedRefs: fixedRefsBatch({
      commands: 1,
      strings: ["POST", "/api/orders", "content-type", "application/json", "{\"id\":17}"],
    }),
    hybridUsesDynamic: true,
  },
];

const rows = scenarios.map((scenario) => {
  const fixed = sum(scenario.ops, "fixed");
  const dynamic = sum(scenario.ops, "dynamic");
  const hybrid = scenario.hybridUsesDynamic ? dynamic : fixed;
  return {
    scenario: scenario.name,
    currentFixed: fixed,
    fixedRefs: scenario.fixedRefs,
    dynamic,
    hybrid,
  };
});

console.log("| Scenario | Current fixed | Fixed + refs | Dynamic | Hybrid |");
console.log("| --- | ---: | ---: | ---: | ---: |");
for (const row of rows) {
  console.log(
    `| ${row.scenario} | ${row.currentFixed} | ${row.fixedRefs} | ${row.dynamic} | ${row.hybrid} |`,
  );
}
