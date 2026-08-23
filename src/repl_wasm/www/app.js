import {
  activeCell,
  advanceCell,
  completionDocumentRange,
  utf16ToUtf8Offset,
} from "./cells.js";

const source = document.querySelector("#source");
const transcript = document.querySelector("#transcript");
const runButton = document.querySelector("#run");
const stopButton = document.querySelector("#stop");
const clearButton = document.querySelector("#clear");
const detailsButton = document.querySelector("#toggle-details");
const details = document.querySelector("#details");
const workspace = document.querySelector("#workspace");
const workerState = document.querySelector("#worker-state");
const revisionLabel = document.querySelector("#revision");
const editorStatus = document.querySelector("#editor-status");
const completion = document.querySelector("#completion");
const definitions = document.querySelector("#definitions");
const modules = document.querySelector("#modules");
const toasts = document.querySelector("#toasts");

let worker;
let ready = false;
let evaluating = false;
let nextToken = 1;
let nextRequestId = 1;
let nextExecution = 1;
let pending = new Map();
let replayDefinitionSource = "";
let replayModules = [];
let completionState = null;
let completionTimer = null;
let completionVersion = 0;

function makeElement(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text != null) node.textContent = text;
  return node;
}

function setRevision(revision) {
  if (revision != null) revisionLabel.textContent = String(revision);
}

function syncRunButton() {
  runButton.disabled = !ready || evaluating;
}

function send(request) {
  return new Promise((resolve, reject) => {
    const token = nextToken++;
    pending.set(token, { resolve, reject });
    worker.postMessage({ type: "request", token, request });
  });
}

async function call(op, params) {
  const request = { protocol: 1, id: nextRequestId++, op };
  if (params !== undefined) request.params = params;
  const response = await send(request);
  if (!response.ok) {
    throw new Error(`${response.error?.code || "repl_error"}: ${response.error?.message || "Request failed"}`);
  }
  setRevision(response.result?.revision);
  return response.result;
}

function renderCollection(container, items, renderItem, emptyText) {
  container.replaceChildren();
  if (items.length === 0) {
    container.append(makeElement("div", "empty", emptyText));
    return;
  }
  for (const item of items) container.append(renderItem(item));
}

async function refreshSessionState() {
  const state = await call("get_state");
  replayDefinitionSource = state.definition_source;
  replayModules = state.modules.map((module) => ({ name: module.name, source: module.source }));
  renderCollection(definitions, state.definitions, (definition) => {
    const row = makeElement("div", "state-item");
    row.append(makeElement("span", "", definition.name), makeElement("span", "", definition.kind));
    return row;
  }, "No definitions yet");
  renderCollection(modules, state.modules, (module) => {
    const row = makeElement("div", "state-item");
    row.append(makeElement("span", "", module.name), makeElement("span", "", "module"));
    return row;
  }, "No virtual modules");
}

function definitionSummary(snippet) {
  switch (snippet.definition_kind) {
    case "value": return `${snippet.name}${snippet.type ? ` : ${snippet.type}` : ""}`;
    case "annotation": return `${snippet.name} (annotation stored)`;
    case "type": return `type ${snippet.name}`;
    case "import": return `import ${snippet.name}`;
    default: return snippet.name || "definition";
  }
}

function showToast(payload) {
  const toast = makeElement("div", "toast", payload);
  toasts.append(toast);
  setTimeout(() => toast.remove(), 4000);
}

const effectHandlers = new Map([
  ["log", (payload) => console.log(`[Roc REPL] ${payload}`)],
  ["toast", showToast],
]);

function renderEvent(event, runEffects) {
  if (event.kind !== "effect") {
    return makeElement("div", "event", `${event.kind}: ${event.message}`);
  }

  const handler = effectHandlers.get(event.name);
  let disposition = "unhandled";
  if (!runEffects) {
    disposition = "suppressed";
  } else if (handler) {
    handler(event.payload);
    disposition = "handled";
  }
  return makeElement("div", `event effect ${disposition}`, `effect ${event.name}: ${event.payload} (${disposition})`);
}

function renderSnippet(snippet) {
  const node = makeElement("div", "snippet");
  if (snippet.status === "ok" && snippet.kind === "definition") {
    node.append(makeElement("div", "definition", definitionSummary(snippet)));
  } else if (snippet.status === "ok" && snippet.kind === "expression") {
    node.append(makeElement("div", "value", snippet.value ?? ""));
    if (snippet.type) node.append(makeElement("div", "type", `: ${snippet.type}`));
  }

  for (const diagnostic of snippet.diagnostics || []) {
    node.append(makeElement("div", "diagnostic", `[${diagnostic.code}] ${diagnostic.message}`));
  }
  for (const event of snippet.events || []) {
    if (event.kind === "crashed") continue;
    node.append(renderEvent(event, snippet.status === "ok"));
  }
  if (snippet.status === "crashed") {
    node.append(makeElement(
      "div",
      "diagnostic",
      snippet.crash ? `crashed: ${snippet.crash.message}` : "[protocol_error] Crashed result omitted crash metadata.",
    ));
  }
  return node;
}

function renderEvaluation(input, result, execution) {
  transcript.querySelector(".empty-output")?.remove();
  const turn = makeElement("article", "turn");
  const heading = makeElement("div", "turn-heading", `Cell ${execution}`);
  const snapshot = makeElement("pre", "source-snapshot", input);
  turn.append(heading, snapshot);
  for (const snippet of result.snippets) turn.append(renderSnippet(snippet));
  if (!result.completed && result.committed_count > 0) {
    turn.append(makeElement("div", "notice", `${result.committed_count} earlier definition(s) remain committed.`));
  }
  transcript.append(turn);
  transcript.scrollTop = transcript.scrollHeight;
}

function renderRequestFailure(input, error, execution) {
  transcript.querySelector(".empty-output")?.remove();
  const turn = makeElement("article", "turn");
  turn.append(
    makeElement("div", "turn-heading", `Cell ${execution}`),
    makeElement("pre", "source-snapshot", input),
    makeElement("div", "diagnostic request-failure", String(error)),
  );
  transcript.append(turn);
}

async function replayState() {
  if (replayModules.length > 0) await call("set_modules", { modules: replayModules });
  if (!replayDefinitionSource.trim()) return;
  const result = await call("eval", { source: replayDefinitionSource });
  if (!result.completed) throw new Error(`State replay stopped because of ${result.stop_reason || "an error"}`);
}

function rejectPending(message) {
  for (const waiter of pending.values()) waiter.reject(new Error(message));
  pending.clear();
}

function startWorker({ replay = false } = {}) {
  ready = false;
  evaluating = false;
  syncRunButton();
  workerState.textContent = "loading";
  hideCompletion();

  const startedWorker = new Worker("worker.js", { type: "module" });
  worker = startedWorker;
  startedWorker.onmessage = async ({ data }) => {
    if (worker !== startedWorker) return;
    if (data.type === "ready") {
      try {
        if (replay) await replayState();
        await refreshSessionState();
        ready = true;
        workerState.textContent = "ready";
        editorStatus.textContent = replay ? "Session restored" : "Ready";
        syncRunButton();
        scheduleCompletion();
      } catch (error) {
        workerState.textContent = "replay failed";
        editorStatus.textContent = String(error);
      }
      return;
    }

    if (data.token != null && pending.has(data.token)) {
      const waiter = pending.get(data.token);
      pending.delete(data.token);
      if (data.type === "response") waiter.resolve(data.response);
      else waiter.reject(new Error(data.message));
    } else if (data.type === "failure") {
      workerState.textContent = "failed";
      editorStatus.textContent = data.message;
    }
  };
}

async function evaluateActiveCell() {
  if (!ready || evaluating) return;
  const cell = activeCell(source.value, source.selectionStart);
  const input = cell.source;
  const advanced = advanceCell(source.value, cell.index);
  source.value = advanced.text;
  source.setSelectionRange(advanced.cursor, advanced.cursor);
  source.focus();
  completionVersion += 1;
  hideCompletion();

  if (!input.trim()) {
    editorStatus.textContent = "Skipped empty cell";
    scheduleCompletion();
    return;
  }

  const execution = nextExecution++;
  evaluating = true;
  syncRunButton();
  workerState.textContent = "running";
  editorStatus.textContent = `Running cell ${execution}…`;

  try {
    const result = await call("eval", { source: input });
    renderEvaluation(input, result, execution);
    await refreshSessionState();
    editorStatus.textContent = result.completed ? `Cell ${execution} complete` : `Cell ${execution} stopped: ${result.stop_reason}`;
  } catch (error) {
    renderRequestFailure(input, error, execution);
    editorStatus.textContent = String(error);
  } finally {
    evaluating = false;
    syncRunButton();
    workerState.textContent = ready ? "ready" : "stopped";
    scheduleCompletion();
  }
}

function hideCompletion() {
  completionState = null;
  completion.hidden = true;
  completion.replaceChildren();
  source.setAttribute("aria-expanded", "false");
  source.removeAttribute("aria-activedescendant");
}

function renderCompletion() {
  completion.replaceChildren();
  if (!completionState || completionState.items.length === 0) {
    hideCompletion();
    return;
  }
  completionState.items.forEach((item, index) => {
    const button = makeElement("button", index === completionState.index ? "selected" : "");
    button.id = `completion-${index}`;
    button.type = "button";
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", String(index === completionState.index));
    button.append(makeElement("span", "", item.label), makeElement("small", "", item.detail || item.kind));
    button.addEventListener("mousedown", (event) => event.preventDefault());
    button.addEventListener("click", () => acceptCompletion(index));
    completion.append(button);
  });
  completion.hidden = false;
  source.setAttribute("aria-expanded", "true");
  source.setAttribute("aria-activedescendant", `completion-${completionState.index}`);
  completion.querySelector(".selected")?.scrollIntoView({ block: "nearest" });
}

function acceptCompletion(index = completionState?.index ?? 0) {
  if (!completionState || source.value !== completionState.document) return;
  const item = completionState.items[index];
  const range = completionDocumentRange(completionState.cell, completionState.replacement);
  source.setRangeText(item.insert_text, range.start, range.end, "end");
  hideCompletion();
  source.focus();
  scheduleCompletion();
}

async function requestCompletion({ force = false } = {}) {
  if (!ready || evaluating) return;
  const version = ++completionVersion;
  const documentSnapshot = source.value;
  const selection = source.selectionStart;
  const cell = activeCell(documentSnapshot, selection);
  const cursor = utf16ToUtf8Offset(cell.source, cell.localCursor);
  try {
    const result = await call("complete", { source: cell.source, cursor });
    if (version !== completionVersion || source.value !== documentSnapshot || source.selectionStart !== selection) return;
    if (result.items.length === 0 || (!force && result.prefix.length === 0)) {
      hideCompletion();
      editorStatus.textContent = result.items.length === 0 ? "No session completions" : "Ctrl/⌘ + Space for completions";
      return;
    }
    completionState = {
      items: result.items,
      index: 0,
      replacement: result.replacement,
      document: documentSnapshot,
      cell,
    };
    editorStatus.textContent = `${result.items.length} completion${result.items.length === 1 ? "" : "s"}`;
    renderCompletion();
  } catch (error) {
    if (version === completionVersion) editorStatus.textContent = String(error);
  }
}

function scheduleCompletion() {
  clearTimeout(completionTimer);
  completionTimer = setTimeout(() => requestCompletion(), 120);
}

runButton.addEventListener("click", evaluateActiveCell);
source.addEventListener("input", scheduleCompletion);
source.addEventListener("click", scheduleCompletion);
source.addEventListener("select", scheduleCompletion);
source.addEventListener("keydown", (event) => {
  if ((event.ctrlKey || event.metaKey) && event.key === "Enter") {
    event.preventDefault();
    evaluateActiveCell();
    return;
  }
  if ((event.ctrlKey || event.metaKey) && event.code === "Space") {
    event.preventDefault();
    requestCompletion({ force: true });
    return;
  }
  if (!completionState) return;
  if (event.key === "ArrowDown" || event.key === "ArrowUp") {
    event.preventDefault();
    const delta = event.key === "ArrowDown" ? 1 : -1;
    completionState.index = (completionState.index + delta + completionState.items.length) % completionState.items.length;
    renderCompletion();
  } else if (event.key === "Tab") {
    event.preventDefault();
    acceptCompletion();
  } else if (event.key === "Escape") {
    event.preventDefault();
    hideCompletion();
  }
});

stopButton.addEventListener("click", () => {
  worker?.terminate();
  rejectPending("Evaluation stopped");
  workerState.textContent = "restarting";
  editorStatus.textContent = "Restarting the module and replaying structured session state…";
  startWorker({ replay: true });
});

clearButton.addEventListener("click", async () => {
  if (!ready || evaluating) return;
  try {
    await call("clear");
    replayDefinitionSource = "";
    nextExecution = 1;
    transcript.replaceChildren(makeElement("div", "empty empty-output", "Run a cell to see its structured result."));
    await refreshSessionState();
    editorStatus.textContent = "Session and output cleared; source kept";
  } catch (error) {
    editorStatus.textContent = String(error);
  }
});

detailsButton.addEventListener("click", () => {
  const showing = details.hidden;
  details.hidden = !showing;
  workspace.classList.toggle("details-open", showing);
  detailsButton.setAttribute("aria-expanded", String(showing));
  detailsButton.textContent = showing ? "Hide details" : "Show details";
});

startWorker();
