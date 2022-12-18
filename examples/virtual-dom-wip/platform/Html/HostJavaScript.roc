interface Html.HostJavaScript
    exposes [hostJavaScript]
    imports []

hostJavaScript : Str
hostJavaScript =
    """
    /** @typedef {(e: Event) => void} JsEventDispatcher */

    /** @typedef {[string, JsEventDispatcher]} Listener */

    /**
     * @typedef {Object} RocWasmExports
     * @property {(size: number, alignment: number) => number} roc_alloc
     * @property {(jsonListAddr: number, jsonListLength: number, handlerId: number) => void} roc_dispatch_event
     * @property {() => number} main
     */

    /**
     * @typedef {{ObjectField: [string, CyclicStructureAccessor]} | {ArrayIndex: [number, CyclicStructureAccessor]} | {SerializableValue: []}} CyclicStructureAccessor
     */

    /**
     * @param {string} initData
     * @param {string} wasmUrl
     */
    const init = async (initData, wasmUrl) => {
      /** @type {Array<Node | null>} */
      const nodes = [];

      /** @type {Array<Listener | null>} */
      const listeners = [];

      const utf8Decoder = new TextDecoder();
      const utf8Encoder = new TextEncoder();

      const effects = {
        /**
         * @param {number} tagAddr
         * @param {number} id
         */
        createElement: (tagAddr, id) => {
          const tagName = decodeRocStr(tagAddr);
          const node = document.createElement(tagName);
          nodes[id] = node;
        },

        /**
         * @param {number} contentAddr
         * @param {number} id
         */
        createTextNode: (contentAddr, id) => {
          const content = decodeRocStr(contentAddr);
          const node = document.createTextNode(content);
          nodes[id] = node;
        },

        /**
         * @param {number} nodeId
         * @param {number} contentAddr
         */
        updateTextNode: (nodeId, contentAddr) => {
          const node = nodes[nodeId];
          node.textContent = decodeRocStr(contentAddr);
        },

        /**
         * @param {number} parentId
         * @param {number} childId
         */
        appendChild: (parentId, childId) => {
          const parent = nodes[parentId];
          const child = nodes[childId];
          parent.appendChild(child);
        },

        /**
         * @param {number} id
         */
        removeNode: (id) => {
          const node = nodes[id];
          nodes[id] = null;
          node.parentElement.removeChild(node);
        },

        /**
         * @param {number} id
         */
        replaceNode: (oldId, newId) => {
          const oldNode = nodes[oldId];
          const newNode = nodes[newId];
          const parent = oldNode.parentElement;
          parent.replaceChild(newNode, oldNode);
          nodes[oldId] = null;
        },

        /**
         * @param {number} nodeId
         * @param {number} typeAddr
         * @param {number} valueAddr
         */
        setAttribute: (nodeId, typeAddr, valueAddr) => {
          const node = nodes[nodeId];
          const name = decodeRocStr(typeAddr);
          const value = decodeRocStr(valueAddr);
          node.setAttribute(name, value);
        },

        /**
         * @param {number} nodeId
         * @param {number} typeAddr
         */
        removeAttribute: (nodeId, typeAddr) => {
          const node = nodes[nodeId];
          const name = decodeRocStr(typeAddr);
          node.removeAttribute(name);
        },

        /**
         * @param {number} nodeId
         * @param {number} propNameAddr
         * @param {number} jsonAddr
         */
        setProperty: (nodeId, propNameAddr, jsonAddr) => {
          const node = nodes[nodeId];
          const propName = decodeRocStr(propNameAddr);
          const json = decodeRocListUtf8(jsonAddr);
          const value = JSON.parse(json);
          node[propName] = value;
        },

        /**
         * @param {number} nodeId
         * @param {number} propNameAddr
         */
        removeProperty: (nodeId, propNameAddr) => {
          const node = nodes[nodeId];
          const propName = decodeRocStr(propNameAddr);
          node[propName] = typeof node[propName] === "string" ? "" : null;
        },

        /**
         * @param {number} nodeId
         * @param {number} eventTypeAddr
         * @param {number} accessorsJsonAddr
         * @param {number} handlerId
         */
        setListener: (nodeId, eventTypeAddr, accessorsJsonAddr, handlerId) => {
          const element = nodes[nodeId];
          const eventType = decodeRocStr(eventTypeAddr);
          const accessorsJson = decodeRocStr(accessorsJsonAddr);
          const accessors = JSON.parse(accessorsJson);

          // Dispatch a DOM event to the specified handler function in Roc
          const dispatchEvent = (ev) => {
            const outerListRcAddr = roc_alloc(4 + accessors.length * 12, 4);
            memory32[outerListRcAddr >> 2] = 1;
            const outerListBaseAddr = outerListRcAddr + 4;
            let outerListIndex32 = outerListBaseAddr >> 2;
            accessors.forEach((accessor) => {
              const json = accessCyclicStructure(accessor, ev);
              const { pointer, length, capacity } = encodeJsString(json);
              // Write the fields of the inner `List U8` into the heap allocation of the outer List
              memory32[outerListIndex32++] = pointer;
              memory32[outerListIndex32++] = length;
              memory32[outerListIndex32++] = capacity;
            });

            roc_dispatch_event(outerListBaseAddr, accessors.length, handlerId);
          };

          // Make things easier to debug
          dispatchEvent.name = `dispatchEvent${handlerId}`;
          element.setAttribute("data-roc-event-handler-id", `${handlerId}`);

          listeners[handlerId] = [eventType, dispatchEvent];
          element.addEventListener(eventType, dispatchEvent);
        },

        /**
         * @param {number} nodeId
         * @param {number} handlerId
         */
        removeListener: (nodeId, handlerId) => {
          const element = nodes[nodeId];
          const [eventType, dispatchEvent] = findListener(element, handlerId);
          listeners[handlerId] = null;
          element.removeAttribute("data-roc-event-handler-id");
          element.removeEventListener(eventType, dispatchEvent);
        },
      };

      /**
       * Write a JS string into the Roc app as a `List U8`
       * @param {string} str
       */
      const encodeJsString = (str) => {
        const length16 = str.length;
        // Due to UTF-8 encoding overhead, a few code points go from 2 bytes in UTF-16 to 3 bytes in UTF-8!
        const capacity = length16 * 3; // Extremely "worst-case", but simple, and the allocation is short-lived.
        const rcAddr = roc_alloc(4 + capacity, 4);
        memory32[rcAddr >> 2] = 1;
        const pointer = rcAddr + 4;

        // Write to the heap allocation of the inner `List U8`
        const allocation = memory8.subarray(pointer, pointer + capacity);
        const { written } = utf8Encoder.encodeInto(json, allocation);
        const length = written || 0; // TypeScript claims that `written` can be undefined, though I don't see this in the spec.
        return {
          pointer,
          length,
          capacity,
        };
      };

      /**
       * decode a Roc `Str` to a JavaScript string
       * @param {number} strAddr8
       */
      const decodeRocStr = (strAddr8) => {
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

      /**
       * decode a Roc List of UTF-8 bytes to a JavaScript string
       * @param {number} listAddr8
       */
      const decodeRocListUtf8 = (listAddr8) => {
        const listIndex32 = listAddr8 >> 2;
        const bytesAddr8 = memory32[listIndex32];
        const len = memory32[listIndex32 + 1];
        const bytes = memory8.slice(bytesAddr8, bytesAddr8 + len);
        return utf8Decoder.decode(bytes);
      };

      /**
       * @param {CyclicStructureAccessor} accessor
       * @param {any} structure
       */
      const accessCyclicStructure = (accessor, structure) => {
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

      /**
       * @param {Element} element
       * @param {number} handlerId
       */
      const findListener = (element, handlerId) => {
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

      const wasmImports = { effects };
      const promise = fetch(wasmUrl);
      const instanceAndModule = await WebAssembly.instantiateStreaming(
        promise,
        wasmImports
      );
      const app = instanceAndModule.instance;
      const memory = app.exports.memory;
      const memory8 = new Uint8Array(memory.buffer);
      const memory32 = new Uint32Array(memory.buffer);

      const { roc_vdom_init, roc_alloc, roc_dispatch_event } = app.exports;
      const initList = encodeJsString(initData);
      roc_vdom_init(initList.pointer, initList.length, initList.capacity);
    };
    """
