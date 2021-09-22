/**
 * Node.js test file for hello-web example
 * We are not running this in CI currently, and Node.js is not a Roc dependency.
 * But if you happen to have it, you can run this.
 */

// Node doesn't have the fetch API
const fs = require("fs/promises");
global.fetch = (filename) =>
  fs.readFile(filename).then((buffer) => ({
    arrayBuffer() {
      return buffer;
    },
  }));

const { roc_web_platform_run } = require("./platform/host");

roc_web_platform_run("./hello-world.wasm", (string_from_roc) => {
  const expected = "Hello, World!";
  if (string_from_roc !== expected) {
    console.error(`Expected "${expected}", but got "${string_from_roc}"`);
    process.exit(1);
  }
  console.log("OK");
});
