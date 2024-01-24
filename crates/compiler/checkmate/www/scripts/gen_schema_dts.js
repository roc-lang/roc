const {compileFromFile} = require("json-schema-to-typescript");
const fs = require("node:fs/promises");
const path = require("node:path");

const SCHEMA_PATH = path.resolve(
  __dirname,
  "..",
  "..",
  "schema.json"
);

const DTS_PATH = path.resolve(
  __dirname,
  "..",
  "src",
  "schema.d.ts"
);

async function main() {
    const result = await compileFromFile(SCHEMA_PATH);
    await fs.writeFile(DTS_PATH, result);
}

main().catch(console.error);
