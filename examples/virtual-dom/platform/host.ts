type JsEventDispatcher = (e: Event) => void;
type Listener = [string, JsEventDispatcher];
type RocWasmExports = {
  roc_alloc: (size: number, alignment: number) => number;
  roc_dispatch_event: (
    jsonListAddr: number,
    jsonListLength: number,
    handlerId: number
  ) => void;
  main: () => number;
};
type CyclicStructureAccessor =
  | { ObjectField: [string, CyclicStructureAccessor] }
  | {
      ArrayIndex: [number, CyclicStructureAccessor];
    }
  | {
      SerializableValue: undefined;
    };

const nodes: Array<Node | null> = [];
const listeners: Array<Listener | null> = [];
const utf8Decoder = new TextDecoder();
const utf8Encoder = new TextEncoder();

export const init = async (wasmFilename: string) => {
  const effects = {
    // createElement : Str -> Effect NodeId
    createElement: (tagAddr: number): number => {
      const tagName = decodeRocStr(tagAddr);
      const node = document.createElement(tagName);
      return insertNode(node);
    },

    // createTextNode : Str -> Effect NodeId
    createTextNode: (contentAddr: number): number => {
      const content = decodeRocStr(contentAddr);
      const node = document.createTextNode(content);
      return insertNode(node);
    },

    // appendChild : NodeId, NodeId -> Effect {}
    appendChild: (parentId: number, childId: number): void => {
      const parent = nodes[parentId] as Element;
      const child = nodes[childId] as Node;
      parent.appendChild(child);
    },

    // removeNode : NodeId -> Effect {}
    removeNode: (id: number): void => {
      const node = nodes[id];
      nodes[id] = null;
      node?.parentElement?.removeChild(node);
    },

    // setAttribute : NodeId, Str, Str -> Effect {}
    setAttribute: (
      nodeId: number,
      typeAddr: number,
      valueAddr: number
    ): void => {
      const node = nodes[nodeId] as Element;
      const name = decodeRocStr(typeAddr);
      const value = decodeRocStr(valueAddr);
      node.setAttribute(name, value);
    },

    // removeAttribute : NodeId, Str -> Effect {}
    removeAttribute: (nodeId: number, typeAddr: number): void => {
      const node = nodes[nodeId] as Element;
      const name = decodeRocStr(typeAddr);
      node.removeAttribute(name);
    },

    // setProperty : NodeId, Str, List U8 -> Effect {}
    setProperty: (
      nodeId: number,
      propNameAddr: number,
      jsonAddr: number
    ): void => {
      const node = nodes[nodeId] as Element;
      const propName = decodeRocStr(propNameAddr);
      const json = decodeRocListUtf8(jsonAddr);
      const value = JSON.parse(json);
      node[propName] = value;
    },

    // removeProperty : NodeId, Str -> Effect {}
    removeProperty: (nodeId: number, propNameAddr: number): void => {
      const node = nodes[nodeId] as Element;
      const propName = decodeRocStr(propNameAddr);
      node[propName] = null;
    },

    // setListener : NodeId, Str, List Accessor, EventHandlerId -> Effect {}
    setListener: (
      nodeId: number,
      eventTypeAddr: number,
      accessorsJsonAddr: number,
      handlerId: number
    ): void => {
      const element = nodes[nodeId] as Element;
      const eventType = decodeRocStr(eventTypeAddr);
      const accessorsJson = decodeRocStr(accessorsJsonAddr);
      const accessors: CyclicStructureAccessor[] = JSON.parse(accessorsJson);

      // Dispatch a DOM event to the specified handler function in Roc
      const dispatchEvent = (ev: Event) => {
        const { roc_alloc, roc_dispatch_event } = app.exports as RocWasmExports;

        const outerListRcAddr = roc_alloc(4 + accessors.length * 12, 4);
        memory32[outerListRcAddr >> 2] = 1;
        const outerListBaseAddr = outerListRcAddr + 4;

        let outerListIndex32 = outerListBaseAddr >> 2;
        accessors.forEach((accessor) => {
          const json = accessCyclicStructure(accessor, ev);
          const length16 = json.length;

          // Due to UTF-8 encoding overhead, a few code points go from 2 bytes in UTF-16 to 3 bytes in UTF-8!
          const capacity8 = length16 * 3; // Extremely "worst-case", but simple, and the allocation is short-lived.
          const rcAddr = roc_alloc(4 + capacity8, 4);
          memory32[rcAddr >> 2] = 1;
          const baseAddr = rcAddr + 4;

          // Write JSON to the heap allocation of the inner `List U8`
          const allocation = memory8.subarray(baseAddr, baseAddr + capacity8);
          const { written } = utf8Encoder.encodeInto(json, allocation);
          const length = written || 0; // TypeScript claims that `written` can be undefined, though I don't see this in the spec.

          // Write the fields of the inner `List U8` into the heap allocation of the outer List
          memory32[outerListIndex32++] = baseAddr;
          memory32[outerListIndex32++] = length;
          memory32[outerListIndex32++] = capacity8;
        });

        roc_dispatch_event(outerListBaseAddr, accessors.length, handlerId);
      };

      // Make things easier to debug
      dispatchEvent.name = `dispatchEvent${handlerId}`;
      element.setAttribute("data-roc-event-handler-id", `${handlerId}`);

      listeners[handlerId] = [eventType, dispatchEvent];
      element.addEventListener(eventType, dispatchEvent);
    },

    // removeListener : NodeId, EventHandlerId -> Effect {}
    removeListener: (nodeId: number, handlerId: number): void => {
      const element = nodes[nodeId] as Element;
      const [eventType, dispatchEvent] = findListener(element, handlerId);
      listeners[handlerId] = null;
      element.removeAttribute("data-roc-event-handler-id");
      element.removeEventListener(eventType, dispatchEvent);
    },
  };

  // decode a Roc `Str` to a JavaScript string
  const decodeRocStr = (strAddr8: number): string => {
    const lastByte = memory8[strAddr8 + 12];
    const isSmall = lastByte >= 0x80;

    if (isSmall) {
      const len = lastByte & 0x7f;
      const bytes = memory8.slice(strAddr8, strAddr8 + len);
      return utf8Decoder.decode(bytes);
    } else {
      return decodeRocListUtf8(strAddr8);
    }
  };

  // decode a Roc List of UTF-8 bytes to a JavaScript string
  const decodeRocListUtf8 = (listAddr8: number): string => {
    const listIndex32 = listAddr8 >> 2;
    const bytesAddr8 = memory32[listIndex32];
    const len = memory32[listIndex32 + 1];
    const bytes = memory8.slice(bytesAddr8, bytesAddr8 + len);
    return utf8Decoder.decode(bytes);
  };

  const wasmImports = { effects };
  const promise = fetch(wasmFilename);
  const instanceAndModule = await WebAssembly.instantiateStreaming(
    promise,
    wasmImports
  );
  const app = instanceAndModule.instance;
  const memory = app.exports.memory as WebAssembly.Memory;
  const memory8 = new Uint8Array(memory.buffer);
  const memory32 = new Uint32Array(memory.buffer);

  const { main } = app.exports as RocWasmExports;
  const exitCode = main();
  if (exitCode) {
    throw new Error(`Roc exited with error code ${exitCode}`);
  }
};

const insertNode = (node: Node): number => {
  let i = 0;
  for (; i < nodes.length; i++) {
    if (!nodes[i]) break;
  }
  nodes[i] = node;
  return i;
};

const accessCyclicStructure = (
  accessor: CyclicStructureAccessor,
  structure: any
): string => {
  while (true) {
    if ("SerializableValue" in accessor) {
      return JSON.stringify(structure);
    } else if ("ObjectField" in accessor) {
      const [field, childAccessor] = accessor.ObjectField;
      structure = structure[field];
      accessor = childAccessor;
    } else if ("ArrayIndex" in accessor) {
      const [index, childAccessor] = accessor.ArrayIndex;
      structure = structure[index];
      accessor = childAccessor;
    }
    throw new Error("Invalid CyclicStructureAccessor");
  }
};

const findListener = (element: Element, handlerId: number) => {
  const listener = listeners[handlerId];
  if (listener) {
    return listener;
  } else {
    throw new Error(
      `Event listener #${handlerId} not found. This is a bug in virtual-dom, not your app!` +
        "It should have been on this node:\n" +
        element.outerHTML
    );
  }
};
