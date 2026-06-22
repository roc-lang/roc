import test from "node:test";
import assert from "node:assert/strict";

import {
  applySetValue,
  beginComposition,
  blurInput,
  createControlledInputState,
  endComposition,
  focusInput,
  userInput,
} from "./controlled_input_policy.mjs";

test("unfocused SetValue writes the DOM value immediately", () => {
  const state = createControlledInputState("old");

  const op = applySetValue(state, "new");

  assert.equal(op.status, "wrote");
  assert.equal(state.value, "new");
  assert.equal(state.pendingValue, null);
});

test("equal SetValue is a no-op and clears stale pending state", () => {
  const state = createControlledInputState("text");
  focusInput(state, 2, 2);

  const deferred = applySetValue(state, "server-text");
  assert.equal(deferred.status, "deferred");
  assert.equal(state.pendingValue, "server-text");

  userInput(state, "server-text", 11, 11);
  const echo = applySetValue(state, "server-text");

  assert.equal(echo.status, "skipped");
  assert.equal(echo.reason, "equal");
  assert.equal(state.pendingValue, null);
  assert.equal(state.selectionStart, 11);
  assert.equal(state.selectionEnd, 11);
});

test("focused differing SetValue is deferred and keeps caret state", () => {
  const state = createControlledInputState("abcdef");
  focusInput(state, 3, 3);

  const op = applySetValue(state, "abcXYZdef");

  assert.equal(op.status, "deferred");
  assert.equal(op.reason, "focused");
  assert.equal(state.value, "abcdef");
  assert.equal(state.pendingValue, "abcXYZdef");
  assert.equal(state.selectionStart, 3);
  assert.equal(state.selectionEnd, 3);
});

test("composition defers host writes until the control is no longer focused", () => {
  const state = createControlledInputState("");
  focusInput(state, 0, 0);
  beginComposition(state);
  userInput(state, "に", 1, 1);

  const composing = applySetValue(state, "host");
  assert.equal(composing.status, "deferred");
  assert.equal(composing.reason, "composing");
  assert.equal(state.value, "に");
  assert.equal(state.pendingValue, "host");

  const stillFocused = endComposition(state);
  assert.equal(stillFocused.status, "deferred");
  assert.equal(stillFocused.reason, "focused");
  assert.equal(state.value, "に");

  const blurred = blurInput(state);
  assert.equal(blurred.status, "wrote");
  assert.equal(state.value, "host");
  assert.equal(state.pendingValue, null);
});

test("typing the pending host value clears the deferred write", () => {
  const state = createControlledInputState("a");
  focusInput(state, 1, 1);

  const deferred = applySetValue(state, "abc");
  assert.equal(deferred.status, "deferred");
  assert.equal(state.pendingValue, "abc");

  userInput(state, "abc", 3, 3);
  const blurred = blurInput(state);

  assert.equal(blurred.status, "skipped");
  assert.equal(blurred.reason, "no-pending");
  assert.equal(state.value, "abc");
});
