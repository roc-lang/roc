import test from "node:test";
import assert from "node:assert/strict";

import {
  createMemoryViewCache,
  instantiateMemoryGrowFixture,
} from "./wasm_memory_views.mjs";

test("memory views are rebuilt after a host call grows wasm memory", async () => {
  const instance = await instantiateMemoryGrowFixture();
  const exports = instance.exports;
  const views = createMemoryViewCache(exports.memory);
  const staleView = views.u8;

  assert.equal(exports.memory.buffer.byteLength, 65536);
  assert.equal(staleView.byteLength, 65536);

  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  const allocation = views.callHost(exports.roc_alloc, 32);

  assert.equal(allocation.result, 65520);
  assert.equal(allocation.grew, true);
  assert.equal(staleView.byteLength, 0);
  assert.equal(views.u8.byteLength, 131072);
  assert.equal(views.generation, 1);
});

test("refreshed views can write and wasm can read across the grow boundary", async () => {
  const instance = await instantiateMemoryGrowFixture();
  const exports = instance.exports;
  const views = createMemoryViewCache(exports.memory);

  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  const { result: ptr } = views.callHost(exports.roc_alloc, 32);
  const pattern = [
    [ptr, 0x11],
    [65535, 0x22],
    [65536, 0x33],
    [ptr + 31, 0x44],
  ];

  for (const [address, byte] of pattern) {
    views.u8[address] = byte;
  }

  for (const [address, byte] of pattern) {
    assert.equal(exports.read_byte(address), byte);
  }
});

test("host calls that do not grow memory keep the same cached views", async () => {
  const instance = await instantiateMemoryGrowFixture();
  const exports = instance.exports;
  const views = createMemoryViewCache(exports.memory);

  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  assert.equal(views.callHost(exports.roc_alloc, 8).grew, false);
  const first = views.callHost(exports.roc_alloc, 32);
  assert.equal(first.grew, true);
  const bufferAfterGrow = views.buffer;
  const generationAfterGrow = views.generation;

  const second = views.callHost(exports.roc_alloc, 8);

  assert.equal(second.grew, false);
  assert.equal(views.buffer, bufferAfterGrow);
  assert.equal(views.generation, generationAfterGrow);
});
