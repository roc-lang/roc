# HTTP Server for `roc docs --serve`

## Summary

Successfully implemented the `--serve` flag for `roc docs` that starts a simple HTTP server to view generated documentation.

## Implementation Details

### 1. CLI Arguments (`src/cli/cli_args.zig`)

Added `serve: bool = false` field to `DocsArgs` struct and updated the parser to handle `--serve` flag.

**Help text:**
```
--serve          Start an HTTP server to view the documentation
```

### 2. HTTP Server (`src/cli/main.zig`)

Implemented four new functions:

#### `serveDocumentation(gpa, docs_dir)`
- Creates TCP listener on `127.0.0.1:8080`
- Prints startup message: `Visit http://localhost:8080 to view the docs at ./{docs_dir}/`
- Accepts connections in infinite loop

#### `handleConnection(gpa, connection, docs_dir)`
- Parses HTTP GET requests
- Routes to appropriate file handler
- Returns proper HTTP responses (200, 404, 405, 500)

#### `resolveFilePath(gpa, docs_dir, url_path)`
- Smart path resolution:
  - `/` → `{docs_dir}/index.html`
  - `/Foo` → `{docs_dir}/Foo/index.html` (no extension = directory)
  - `/Foo/` → `{docs_dir}/Foo/index.html` (trailing slash)
  - `/style.css` → `{docs_dir}/style.css` (has extension = file)

#### `getContentType(file_path)`
- Returns proper MIME types:
  - `.html` → `text/html; charset=utf-8`
  - `.css` → `text/css`
  - `.js` → `application/javascript`
  - `.json` → `application/json`
  - `.png`, `.jpg`, `.svg` → image types
  - default → `text/plain`

#### `sendResponse(stream, status, content_type, body)`
- Sends proper HTTP/1.1 responses with headers

## Usage

```bash
# Generate docs and start server
roc docs main.roc --serve

# With custom output directory
roc docs main.roc --output=my-docs --serve
```

## Testing

The server works with the `generated-docs/` folder created earlier:

```bash
# Start the server (press Ctrl+C to stop)
./zig-out/bin/roc docs --serve
```

Then visit:
- http://localhost:8080 - Main documentation page
- http://localhost:8080/Foo - Foo module page
- http://localhost:8080/pf/Stdout - Platform module page

## Features

✅ Serves index.html for directories (e.g., `/` and `/Foo`)
✅ Serves files directly when they have extensions
✅ Proper content-type headers
✅ Clean URL routing without `.html` extensions needed
✅ User-friendly startup message with URL
✅ Works with any output directory specified via `--output`
