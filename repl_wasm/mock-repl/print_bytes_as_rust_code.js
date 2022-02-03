#!/usr/bin/env node

const fs = require("fs");

const infile = process.argv[2];
if (!infile) {
  throw new Error("Need an input file argument")
}

const buffer = fs.readFileSync(infile);

console.log("pub const APP: &[u8] = &[")
buffer.forEach((byte) => {
  const hex = byte.toString(16).padStart(2, '0');
  console.log(`    0x${hex},`);
});
console.log(`];`)
