const STATUS = Object.freeze({
  wrote: "wrote",
  skipped: "skipped",
  deferred: "deferred",
});

const REASON = Object.freeze({
  equal: "equal",
  focused: "focused",
  composing: "composing",
  noPending: "no-pending",
});

export function createControlledInputState(initialValue = "") {
  return {
    value: String(initialValue),
    focused: false,
    composing: false,
    selectionStart: String(initialValue).length,
    selectionEnd: String(initialValue).length,
    pendingValue: null,
  };
}

export function focusInput(state, selectionStart = state.value.length, selectionEnd = selectionStart) {
  state.focused = true;
  state.selectionStart = selectionStart;
  state.selectionEnd = selectionEnd;
}

export function blurInput(state) {
  state.focused = false;
  state.selectionStart = state.value.length;
  state.selectionEnd = state.value.length;
  return flushPendingSetValue(state);
}

export function beginComposition(state) {
  state.composing = true;
}

export function endComposition(state) {
  state.composing = false;
  return flushPendingSetValue(state);
}

export function userInput(state, value, selectionStart = String(value).length, selectionEnd = selectionStart) {
  state.value = String(value);
  state.selectionStart = selectionStart;
  state.selectionEnd = selectionEnd;

  if (state.pendingValue === state.value) {
    state.pendingValue = null;
  }
}

export function applySetValue(state, value) {
  const next = String(value);

  if (state.value === next) {
    state.pendingValue = null;
    return result(STATUS.skipped, REASON.equal, state);
  }

  if (state.composing) {
    state.pendingValue = next;
    return result(STATUS.deferred, REASON.composing, state);
  }

  if (state.focused) {
    state.pendingValue = next;
    return result(STATUS.deferred, REASON.focused, state);
  }

  state.value = next;
  state.selectionStart = next.length;
  state.selectionEnd = next.length;
  state.pendingValue = null;
  return result(STATUS.wrote, null, state);
}

export function flushPendingSetValue(state) {
  if (state.pendingValue === null) {
    return result(STATUS.skipped, REASON.noPending, state);
  }

  return applySetValue(state, state.pendingValue);
}

export function attachControlledInputSpike(input, log = () => {}) {
  const state = createControlledInputState(input.value);

  const syncFromDom = () => {
    userInput(
      state,
      input.value,
      input.selectionStart ?? input.value.length,
      input.selectionEnd ?? input.value.length,
    );
  };

  const syncWriteToDom = (op) => {
    if (op.status === STATUS.wrote) {
      input.value = state.value;
    }
    log(op);
  };

  input.addEventListener("focus", () => {
    focusInput(
      state,
      input.selectionStart ?? input.value.length,
      input.selectionEnd ?? input.value.length,
    );
    log(result(STATUS.skipped, "focus", state));
  });

  input.addEventListener("blur", () => {
    syncWriteToDom(blurInput(state));
  });

  input.addEventListener("compositionstart", () => {
    syncFromDom();
    beginComposition(state);
    log(result(STATUS.skipped, "compositionstart", state));
  });

  input.addEventListener("compositionend", () => {
    syncFromDom();
    syncWriteToDom(endComposition(state));
  });

  input.addEventListener("input", () => {
    syncFromDom();
    log(result(STATUS.skipped, "input", state));
  });

  return {
    state,
    applySetValue(value) {
      const op = applySetValue(state, value);
      syncWriteToDom(op);
      return op;
    },
    flushPending() {
      const op = flushPendingSetValue(state);
      syncWriteToDom(op);
      return op;
    },
  };
}

function result(status, reason, state) {
  return {
    status,
    reason,
    value: state.value,
    pendingValue: state.pendingValue,
    focused: state.focused,
    composing: state.composing,
    selectionStart: state.selectionStart,
    selectionEnd: state.selectionEnd,
  };
}
