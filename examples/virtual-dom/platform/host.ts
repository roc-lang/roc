const nodes: Node[] = [];
const listeners: Array<[string, (e: Event) => void]> = [];
let memory8 = new Uint8Array(1024);
let memory32 = new Uint32Array(memory8.buffer);
const utf8Decoder = new TextDecoder();
const utf8Encoder = new TextEncoder();

const app = new WebAssembly.Instance(
  new WebAssembly.Module(new ArrayBuffer(1024))
); // TODO: make this actually work

type RocAlloc = (size: number, alignment: number) => number;
type RocDomEvent = (
  jsonListAddr: number,
  jsonListLength: number,
  handlerId: number
) => void;

// decode a Roc `Str` to a JavaScript string
const decodeRocStr = (strAddr8: number): string => {
  const lastByte = memory8[strAddr8 + 12];
  const isSmall = lastByte >= 0x80;

  if (isSmall) {
    const len = lastByte & 0x7f;
    const bytes = memory8.slice(strAddr8, strAddr8 + len);
    return utf8Decoder.decode(bytes);
  } else {
    return decodeRocListU8(strAddr8);
  }
};

// decode a Roc List of UTF-8 bytes to a JavaScript string
const decodeRocListU8 = (listAddr8: number): string => {
  const listIndex32 = listAddr8 >> 2;
  const bytesAddr8 = memory32[listIndex32];
  const len = memory32[listIndex32 + 1];
  const bytes = memory8.slice(bytesAddr8, bytesAddr8 + len);
  return utf8Decoder.decode(bytes);
};

const insertNode = (node: Node): number => {
  let i = 0;
  for (; i < nodes.length; i++) {
    if (!nodes[i]) break;
  }
  nodes[i] = node;
  return i;
};

// createElement : Str -> Effect NodeId
const createElement = (tagNameAddr: number): number => {
  const tagName = decodeRocStr(tagNameAddr);
  const node = document.createElement(tagName);
  return insertNode(node);
};

// createTextNode : Str -> Effect NodeId
const createTextNode = (contentAddr: number): number => {
  const content = decodeRocStr(contentAddr);
  const node = document.createTextNode(content);
  return insertNode(node);
};

// appendChild : NodeId, NodeId -> Effect {}
const appendChild = (parentId: number, childId: number): void => {
  const parent = nodes[parentId];
  const child = nodes[childId];
  parent.appendChild(child);
};

// removeNode : NodeId -> Effect {}
const removeNode = (id: number): void => {
  const node = nodes[id];
  node.parentElement?.removeChild(node);
};

// setAttribute : NodeId, Str, Str -> Effect {}
const setAttribute = (
  nodeId: number,
  nameAddr: number,
  valueAddr: number
): void => {
  const node = nodes[nodeId] as Element;
  const name = decodeRocStr(nameAddr);
  const value = decodeRocStr(valueAddr);
  node.setAttribute(name, value);
};

// removeAttribute : NodeId, Str -> Effect {}
const removeAttribute = (nodeId: number, nameAddr: number): void => {
  const node = nodes[nodeId] as Element;
  const name = decodeRocStr(nameAddr);
  node.removeAttribute(name);
};

// setProperty : NodeId, Str, List U8 -> Effect {}
const setProperty = (
  nodeId: number,
  propNameAddr: number,
  jsonAddr: number
): void => {
  const node = nodes[nodeId] as Element;
  const propName = decodeRocStr(propNameAddr);
  const json = decodeRocListU8(jsonAddr);
  const value = JSON.parse(json);
  node[propName] = value;
};

// removeProperty : NodeId, Str -> Effect {}
const removeProperty = (nodeId: number, propNameAddr: number): void => {
  const node = nodes[nodeId] as Element;
  const propName = decodeRocStr(propNameAddr);
  node[propName] = null;
};

type CyclicStructureAccessor =
  | { ObjectField: [string, CyclicStructureAccessor] }
  | {
      ArrayIndex: [number, CyclicStructureAccessor];
    }
  | {
      SerializableValue: undefined;
    };

const accessCyclicStructure = (
  accessor: CyclicStructureAccessor,
  structure: any
): string => {
  if ("SerializableValue" in accessor) {
    return JSON.stringify(accessor.SerializableValue);
  } else if ("ObjectField" in accessor) {
    const [field, childAccessor] = accessor.ObjectField;
    return accessCyclicStructure(childAccessor, structure[field]);
  } else if ("ArrayIndex" in accessor) {
    const [index, childAccessor] = accessor.ArrayIndex;
    return accessCyclicStructure(childAccessor, structure[index]);
  }
  throw new Error("Invalid CyclicStructureAccessor");
};

// setListener : NodeId, Str, List Accessor, EventHandlerId -> Effect {}
const setListener = (
  nodeId: number,
  eventTypeAddr: number,
  accessorsJsonAddr: number,
  handlerId: number
): void => {
  const node = nodes[nodeId];
  const eventType = decodeRocStr(eventTypeAddr);
  const accessorsJson = decodeRocStr(accessorsJsonAddr);
  const accessors: CyclicStructureAccessor[] = JSON.parse(accessorsJson);

  const rocEventListener = (ev: Event) => {
    const roc_alloc = app.exports.roc_alloc as RocAlloc;

    const outerListRcAddr = roc_alloc(4 + accessors.length * 12, 4);
    memory32[outerListRcAddr >> 2] = 1;
    const outerListBaseAddr = outerListRcAddr + 4;

    let outerListIndex32 = outerListBaseAddr >> 2;
    accessors.forEach((accessor) => {
      const json = accessCyclicStructure(accessor, ev);

      // Due to encoding overhead, there are rare chars that go from 2 bytes in UTF-16 to 3 bytes in UTF-8!
      const capacity = json.length * 3; // Extremely conservative, but simple. The allocation is short-lived.
      const rcAddr = roc_alloc(4 + capacity, 4);
      memory32[rcAddr >> 2] = 1;
      const baseAddr = rcAddr + 4;

      // Write JSON to the heap allocation of the inner `List U8`
      const allocation = memory8.subarray(baseAddr, baseAddr + capacity);
      const { written } = utf8Encoder.encodeInto(json, allocation);
      const length = written || 0; // TypeScript claims that `written` can be undefined, though I don't see this in the spec.

      // Write the fields of the inner `List U8` into the heap allocation of the outer List
      memory32[outerListIndex32++] = baseAddr;
      memory32[outerListIndex32++] = length;
      memory32[outerListIndex32++] = capacity;
    });

    const roc_dom_event = app.exports.roc_dom_event as RocDomEvent;
    roc_dom_event(outerListBaseAddr, accessors.length, handlerId);
  };

  listeners[handlerId] = [eventType, rocEventListener];
  node.addEventListener(eventType, rocEventListener);
};

// removeListener : NodeId, EventHandlerId -> Effect {}
const removeListener = (nodeId: number, handlerId: number): void => {
  const node = nodes[nodeId];
  const [eventType, handler] = listeners[handlerId];
  node.removeEventListener(eventType, handler);
};
