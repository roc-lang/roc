const os = require("os");
const fs = require("fs");
const path = require("path");

// Use the appropriate .d.ts file based on our system's architecture.
const src = path.join("platform", "glue", os.arch() + ".d.ts");
const dest = path.join("build", "Release", "addon.d.ts");

fs.copyFileSync(src, dest);
