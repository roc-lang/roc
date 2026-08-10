import assert from "node:assert/strict";
import test from "node:test";
import {
  activeCell,
  advanceCell,
  completionDocumentRange,
  findCells,
  utf16ToUtf8Offset,
  utf8ToUtf16Offset,
} from "../../src/repl_wasm/www/cells.js";

test("findCells recognizes only standalone delimiter lines", () => {
  const text = "first #%% stays\n  #%%  \nsecond\r\n#%%\r\nthird";
  assert.deepEqual(
    findCells(text).map(({ sourceStart, sourceEnd }) => text.slice(sourceStart, sourceEnd)),
    ["first #%% stays", "second", "third"],
  );
});

test("a cursor on a delimiter selects the following cell", () => {
  const text = "one\n#%%\ntwo";
  assert.equal(activeCell(text, 0).source, "one");
  assert.equal(activeCell(text, text.indexOf("#%%")).source, "two");
  assert.equal(activeCell(text, text.length).source, "two");
});

test("activeCell reports cell-local offsets", () => {
  const text = "one\n#%%\nalphabet";
  const cursor = text.indexOf("alpha") + 3;
  const cell = activeCell(text, cursor);
  assert.equal(cell.index, 1);
  assert.equal(cell.localCursor, 3);
  assert.equal(cell.sourceStart + cell.localCursor, cursor);
});

test("advanceCell selects an existing next cell", () => {
  const text = "one\n#%%\ntwo";
  const advanced = advanceCell(text, 0);
  assert.equal(advanced.text, text);
  assert.equal(advanced.cursor, text.indexOf("two"));
  assert.equal(advanced.created, false);
});

test("advanceCell appends a fresh cell after the last cell", () => {
  assert.deepEqual(advanceCell("one", 0), {
    text: "one\n#%%\n",
    cursor: "one\n#%%\n".length,
    created: true,
  });
  assert.deepEqual(advanceCell("one\n", 0), {
    text: "one\n#%%\n",
    cursor: "one\n#%%\n".length,
    created: true,
  });
});

test("empty and adjacent cells remain addressable", () => {
  const text = "#%%\n#%%\n";
  assert.equal(findCells(text).length, 3);
  assert.equal(activeCell(text, 0).index, 1);
  assert.equal(activeCell(text, text.lastIndexOf("#%%")).index, 2);
});

test("completion offsets stay cell-local across UTF-8 and notebook boundaries", () => {
  const text = "first\n#%%\néclair";
  const cell = activeCell(text, text.length);
  const cursor = utf16ToUtf8Offset(cell.source, cell.source.length);
  assert.equal(cursor, 7);
  assert.equal(utf8ToUtf16Offset(cell.source, cursor), 6);
  assert.deepEqual(completionDocumentRange(cell, { start: 2, end: 7 }), {
    start: text.indexOf("éclair") + 1,
    end: text.length,
  });
  assert.throws(() => utf8ToUtf16Offset(cell.source, 1), /scalar boundary/);
  assert.throws(() => utf16ToUtf8Offset("😀", 1), /surrogate pair/);
  assert.throws(() => completionDocumentRange(cell, { start: 7, end: 2 }), /reversed/);
});
