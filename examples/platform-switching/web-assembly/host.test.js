/**
 * Node.js test file for helloWeb example
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

const { roc_web_platform_run } = require("./host");

roc_web_platform_run("./rocLovesWebAssembly.wasm", (string_from_roc) => {
  const expected = "Roc <3 Web Assembly!\n";
  if (string_from_roc !== expected) {
    console.error(`Expected "${expected}", but got "${string_from_roc}"`);
    process.exit(1);
  }
  console.log("OK");
});
