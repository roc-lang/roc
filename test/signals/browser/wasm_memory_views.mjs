export function createMemoryViewCache(memory) {
  let buffer = memory.buffer;
  let u8 = new Uint8Array(buffer);
  let i32 = new Int32Array(buffer);
  let dataView = new DataView(buffer);
  let generation = 0;

  const rebuild = () => {
    buffer = memory.buffer;
    u8 = new Uint8Array(buffer);
    i32 = new Int32Array(buffer);
    dataView = new DataView(buffer);
    generation += 1;
  };

  return {
    get generation() {
      return generation;
    },
    get buffer() {
      return buffer;
    },
    get u8() {
      return u8;
    },
    get i32() {
      return i32;
    },
    get dataView() {
      return dataView;
    },
    refresh() {
      if (memory.buffer === buffer) {
        return false;
      }

      rebuild();
      return true;
    },
    afterHostCall() {
      return this.refresh();
    },
    callHost(fn, ...args) {
      let result;
      let grew = false;
      try {
        result = fn(...args);
      } finally {
        grew = this.afterHostCall();
      }
      return { result, grew };
    },
  };
}

export async function instantiateMemoryGrowFixture() {
  const module = new WebAssembly.Module(memoryGrowFixtureBytes());
  return WebAssembly.instantiate(module, {});
}

function memoryGrowFixtureBytes() {
  const i32Type = 0x7f;
  const funcKind = 0x00;
  const memoryKind = 0x02;

  const types = section(1, vec([
    funcType([i32Type], [i32Type]),
    funcType([i32Type, i32Type], []),
    funcType([i32Type], [i32Type]),
  ]));

  const functions = section(3, vec([
    u32(0),
    u32(1),
    u32(2),
  ]));

  const memory = section(5, [
    ...u32(1),
    0x00,
    ...u32(1),
  ]);

  const globals = section(6, vec([
    [
      i32Type,
      0x01,
      0x41,
      ...u32(65504),
      0x0b,
    ],
  ]));

  const exports = section(7, vec([
    [...name("memory"), memoryKind, ...u32(0)],
    [...name("roc_alloc"), funcKind, ...u32(0)],
    [...name("write_byte"), funcKind, ...u32(1)],
    [...name("read_byte"), funcKind, ...u32(2)],
  ]));

  const code = section(10, vec([
    body([[1, i32Type]], [
      0x23, 0x00,
      0x21, 0x01,
      0x23, 0x00,
      0x20, 0x00,
      0x6a,
      0x24, 0x00,
      0x23, 0x00,
      0x3f, 0x00,
      0x41, 0x10,
      0x74,
      0x4b,
      0x04, 0x40,
      0x41, 0x01,
      0x40, 0x00,
      0x1a,
      0x0b,
      0x20, 0x01,
    ]),
    body([], [
      0x20, 0x00,
      0x20, 0x01,
      0x3a, 0x00, 0x00,
    ]),
    body([], [
      0x20, 0x00,
      0x2d, 0x00, 0x00,
    ]),
  ]));

  return new Uint8Array([
    0x00, 0x61, 0x73, 0x6d,
    0x01, 0x00, 0x00, 0x00,
    ...types,
    ...functions,
    ...memory,
    ...globals,
    ...exports,
    ...code,
  ]);
}

function funcType(params, results) {
  return [
    0x60,
    ...byteVec(params),
    ...byteVec(results),
  ];
}

function body(locals, ops) {
  const localDecls = vec(locals.map(([count, type]) => [...u32(count), type]));
  const bytes = [
    ...localDecls,
    ...ops,
    0x0b,
  ];
  return [
    ...u32(bytes.length),
    ...bytes,
  ];
}

function name(value) {
  const bytes = new TextEncoder().encode(value);
  return [
    ...u32(bytes.length),
    ...bytes,
  ];
}

function section(id, payload) {
  return [
    id,
    ...u32(payload.length),
    ...payload,
  ];
}

function vec(items) {
  return [
    ...u32(items.length),
    ...items.flat(),
  ];
}

function byteVec(bytes) {
  return [
    ...u32(bytes.length),
    ...bytes,
  ];
}

function u32(value) {
  const bytes = [];
  let remaining = value >>> 0;

  do {
    let byte = remaining & 0x7f;
    remaining >>>= 7;
    if (remaining !== 0) {
      byte |= 0x80;
    }
    bytes.push(byte);
  } while (remaining !== 0);

  return bytes;
}
