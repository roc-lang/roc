// A dependency-free DOM double for driving `runtime.mjs` under `node --test`.
//
// It implements exactly the surface the executor touches — no more — so the
// guards exercise the real `SignalsRuntime` command/event path without pulling
// in jsdom. `textContent` follows real DOM semantics (assigning it replaces the
// children with a single text node) so a `set_text` patch on an element node
// and an aggregate text query both behave like a browser.

export const ELEMENT_NODE = 1;
export const TEXT_NODE = 3;

class FakeNode {
  constructor() {
    this.childNodes = [];
    this.parentNode = null;
    this.listeners = new Map();
  }

  appendChild(child) {
    child.parentNode?.removeChild(child);
    child.parentNode = this;
    this.childNodes.push(child);
    return child;
  }

  removeChild(child) {
    const index = this.childNodes.indexOf(child);
    if (index !== -1) {
      this.childNodes.splice(index, 1);
    }
    child.parentNode = null;
    return child;
  }

  insertBefore(child, reference) {
    child.parentNode?.removeChild(child);
    child.parentNode = this;
    if (reference == null) {
      this.childNodes.push(child);
      return child;
    }
    const index = this.childNodes.indexOf(reference);
    if (index === -1) {
      throw new Error("insertBefore reference is not a child");
    }
    this.childNodes.splice(index, 0, child);
    return child;
  }

  replaceChildren() {
    for (const child of this.childNodes) {
      child.parentNode = null;
    }
    this.childNodes = [];
  }

  addEventListener(type, handler) {
    const handlers = this.listeners.get(type) ?? [];
    handlers.push(handler);
    this.listeners.set(type, handlers);
  }

  removeEventListener(type, handler) {
    const handlers = this.listeners.get(type);
    if (!handlers) {
      return;
    }
    const index = handlers.indexOf(handler);
    if (index !== -1) {
      handlers.splice(index, 1);
    }
  }

  dispatch(type, event) {
    for (const handler of [...(this.listeners.get(type) ?? [])]) {
      handler(event);
    }
  }
}

export class FakeElement extends FakeNode {
  constructor(tag) {
    super();
    this.nodeType = ELEMENT_NODE;
    this.tagName = String(tag).toUpperCase();
    this.attributes = new Map();
    this.dataset = {};
    this.style = {};
    this.value = "";
    this.checked = false;
    this.disabled = false;
    this.type = "";
  }

  set className(value) {
    this.setAttribute("class", value);
  }

  get className() {
    return this.getAttribute("class") ?? "";
  }

  setAttribute(name, value) {
    this.attributes.set(name, String(value));
  }

  getAttribute(name) {
    return this.attributes.has(name) ? this.attributes.get(name) : null;
  }

  removeAttribute(name) {
    this.attributes.delete(name);
  }

  get textContent() {
    let out = "";
    for (const child of this.childNodes) {
      out += child.textContent;
    }
    return out;
  }

  set textContent(value) {
    this.replaceChildren();
    const text = String(value);
    if (text !== "") {
      this.appendChild(new FakeText(text));
    }
  }
}

export class FakeText extends FakeNode {
  constructor(data) {
    super();
    this.nodeType = TEXT_NODE;
    this.nodeValue = String(data);
  }

  get textContent() {
    return this.nodeValue;
  }
}

export function createDocument() {
  return {
    createElement: (tag) => new FakeElement(tag),
    createTextNode: (data) => new FakeText(data),
  };
}

// Installs the globals `runtime.mjs` reads (`document`, `Node`) and returns a
// fresh detached root element to mount into.
export function installDomDouble() {
  globalThis.document = createDocument();
  globalThis.Node = { ELEMENT_NODE, TEXT_NODE };
  return globalThis.document.createElement("div");
}

// Depth-first search for the first node matching `predicate`.
export function findNode(root, predicate) {
  if (predicate(root)) {
    return root;
  }
  for (const child of root.childNodes) {
    const found = findNode(child, predicate);
    if (found) {
      return found;
    }
  }
  return null;
}

export function findAll(root, predicate, out = []) {
  if (predicate(root)) {
    out.push(root);
  }
  for (const child of root.childNodes) {
    findAll(child, predicate, out);
  }
  return out;
}

// Find an element by tag name whose aggregate text content equals `text`.
export function findByText(root, tag, text) {
  const wanted = tag.toUpperCase();
  return findNode(
    root,
    (node) => node.nodeType === ELEMENT_NODE && node.tagName === wanted && node.textContent === text,
  );
}

// Find the text node whose value starts with `prefix`.
export function findTextNode(root, prefix) {
  return findNode(
    root,
    (node) => node.nodeType === TEXT_NODE && node.nodeValue.startsWith(prefix),
  );
}

// Dispatch a synthetic DOM event to the listeners bound on `node`.
export function fireEvent(node, type, init = {}) {
  const event = {
    type,
    target: node,
    currentTarget: node,
    defaultPrevented: false,
    preventDefault() {
      this.defaultPrevented = true;
    },
    stopPropagation() {},
    ...init,
  };
  node.dispatch(type, event);
  return event;
}
