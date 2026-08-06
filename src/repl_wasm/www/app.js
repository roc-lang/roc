const source = document.querySelector("#source");
const transcript = document.querySelector("#transcript");
const runButton = document.querySelector("#run");
const stopButton = document.querySelector("#stop");
const clearButton = document.querySelector("#clear");
const historyPrevButton = document.querySelector("#history-prev");
const historyNextButton = document.querySelector("#history-next");
const workerState = document.querySelector("#worker-state");
const revisionLabel = document.querySelector("#revision");
const editorStatus = document.querySelector("#editor-status");
const completion = document.querySelector("#completion");
const definitions = document.querySelector("#definitions");
const modules = document.querySelector("#modules");

const encoder = new TextEncoder();

let worker;
let ready = false;
let nextToken = 1;
let nextRequestId = 1;
let pending = new Map();
let replayDefinitionSource = "";
let replayModules = [];
let completionState = null;
let completionTimer = null;
let completionVersion = 0;
const submissionHistory = [];
let historyIndex = 0;
let historyDraft = "";

function makeElement(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text != null) node.textContent = text;
  return node;
}

function setRevision(revision) {
  if (revision != null) revisionLabel.textContent = String(revision);
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
  if (!response.ok) throw new Error(`${response.error?.code || "repl_error"}: ${response.error?.message || "Request failed"}`);
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
    node.append(makeElement("div", "event", `${event.kind}: ${event.message}`));
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

function renderEvaluation(input, result) {
  const turn = makeElement("article", "turn");
  turn.append(makeElement("pre", "", `› ${input}`));
  for (const snippet of result.snippets) turn.append(renderSnippet(snippet));
  if (!result.completed && result.committed_count > 0) {
    turn.append(makeElement("div", "notice", `${result.committed_count} earlier definition(s) remain committed.`));
  }
  transcript.append(turn);
  transcript.scrollTop = transcript.scrollHeight;
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
  runButton.disabled = true;
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
        runButton.disabled = false;
        workerState.textContent = "ready";
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

async function evaluate() {
  const input = source.value.trim();
  if (!ready || !input) return;
  completionVersion += 1;
  hideCompletion();
  runButton.disabled = true;
  workerState.textContent = "running";
  submissionHistory.push(input);
  historyIndex = submissionHistory.length;
  historyDraft = "";

  try {
    const result = await call("eval", { source: input });
    renderEvaluation(input, result);
    await refreshSessionState();
    source.value = "";
    editorStatus.textContent = result.completed ? "Complete" : `Stopped: ${result.stop_reason}`;
  } catch (error) {
    editorStatus.textContent = String(error);
  } finally {
    if (ready) runButton.disabled = false;
    workerState.textContent = ready ? "ready" : "stopped";
  }
}

function utf16ToUtf8Offset(text, utf16Index) {
  return encoder.encode(text.slice(0, utf16Index)).length;
}

function utf8ToUtf16Offset(text, utf8Offset) {
  let bytes = 0;
  let utf16 = 0;
  for (const scalar of text) {
    if (bytes === utf8Offset) return utf16;
    const scalarBytes = encoder.encode(scalar).length;
    if (bytes + scalarBytes > utf8Offset) throw new Error("Completion returned a non-boundary UTF-8 offset");
    bytes += scalarBytes;
    utf16 += scalar.length;
  }
  if (bytes === utf8Offset) return utf16;
  throw new Error("Completion returned an out-of-range UTF-8 offset");
}

function hideCompletion() {
  completionState = null;
  completion.hidden = true;
  completion.replaceChildren();
}

function renderCompletion() {
  completion.replaceChildren();
  if (!completionState || completionState.items.length === 0) {
    hideCompletion();
    return;
  }
  completionState.items.forEach((item, index) => {
    const button = makeElement("button", index === completionState.index ? "selected" : "");
    button.type = "button";
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", String(index === completionState.index));
    button.append(makeElement("span", "", item.label), makeElement("small", "", item.detail || item.kind));
    button.addEventListener("mousedown", (event) => event.preventDefault());
    button.addEventListener("click", () => acceptCompletion(index));
    completion.append(button);
  });
  completion.hidden = false;
  completion.querySelector(".selected")?.scrollIntoView({ block: "nearest" });
}

function acceptCompletion(index = completionState?.index ?? 0) {
  if (!completionState) return;
  const item = completionState.items[index];
  const start = utf8ToUtf16Offset(completionState.source, completionState.replacement.start);
  const end = utf8ToUtf16Offset(completionState.source, completionState.replacement.end);
  source.setRangeText(item.insert_text, start, end, "end");
  hideCompletion();
  source.focus();
  scheduleCompletion();
}

async function requestCompletion({ force = false } = {}) {
  if (!ready) return;
  const version = ++completionVersion;
  const text = source.value;
  const selection = source.selectionStart;
  const cursor = utf16ToUtf8Offset(text, selection);
  try {
    const result = await call("complete", { source: text, cursor });
    if (version !== completionVersion || source.value !== text || source.selectionStart !== selection) return;
    if (result.items.length === 0 || (!force && result.prefix.length === 0)) {
      hideCompletion();
      editorStatus.textContent = result.items.length === 0 ? "No session completions" : "Ctrl/⌘ + Space for completions";
      return;
    }
    completionState = { items: result.items, index: 0, replacement: result.replacement, source: text };
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

function navigateHistory(direction) {
  if (submissionHistory.length === 0) return;
  if (historyIndex === submissionHistory.length) historyDraft = source.value;
  historyIndex = Math.max(0, Math.min(submissionHistory.length, historyIndex + direction));
  source.value = historyIndex === submissionHistory.length ? historyDraft : submissionHistory[historyIndex];
  source.setSelectionRange(source.value.length, source.value.length);
  source.focus();
  scheduleCompletion();
}

runButton.addEventListener("click", evaluate);
source.addEventListener("input", scheduleCompletion);
source.addEventListener("click", scheduleCompletion);
source.addEventListener("keydown", (event) => {
  if ((event.ctrlKey || event.metaKey) && event.key === "Enter") {
    event.preventDefault();
    evaluate();
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
  if (!ready) return;
  try {
    await call("clear");
    replayDefinitionSource = "";
    transcript.replaceChildren();
    await refreshSessionState();
    editorStatus.textContent = "Session cleared";
  } catch (error) {
    editorStatus.textContent = String(error);
  }
});

historyPrevButton.addEventListener("click", () => navigateHistory(-1));
historyNextButton.addEventListener("click", () => navigateHistory(1));

startWorker();
