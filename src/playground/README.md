# Roc WASM Playground Interface Control Document

## Overview

The Roc Playground provides a WebAssembly interface to the compiler, providing analysis of `.roc` code in web browsers and JavaScript environments. It implements a state machine architecture to support an initial compilation (analysis), then queries for information about the intermeditate compilation stages.

## State Machine

The playground operates as a finite state machine with three states:

```
START --(INIT)-> READY --(LOAD_SOURCE)-> LOADED
                   ^                       |
                   |                       |
                   +--------(RESET)--------+
```

### State Descriptions

- **START**: Initial state, awaiting initialization
- **READY**: Initialized and ready to compile source code
- **LOADED**: Source compiled, ready for queries about compilation stages

## WASM Exports

### Core Functions

```javascript
// Initialize the WASM module
init(): void

// Process a message and get response
processMessage(
    messagePtr: number,     // Pointer to JSON message
    messageLen: number,     // Length of message
    responsePtr: number,    // Pointer to response buffer
    responseBufferSize: number  // Size of response buffer
): number  // Returns actual response length

// Memory management
allocate(size: number): number | null    // Returns pointer or null
deallocate(ptr: number, size: number): void

// Utilities
getCurrentState(): number    // Returns current state as integer
cleanup(): void             // Clean up all resources
```

## Message Protocol

All communication uses JSON messages. Each message must have a `type` field.

### Message Types

#### 1. INIT
**State**: START → READY
**Purpose**: Initialize the compiler and get version information

**Request**:
```json
{"type": "INIT"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "message": "debug-813709ba"
}
```

#### 2. LOAD_SOURCE
**State**: READY → LOADED
**Purpose**: Compile Roc source code through all compiler stages

**Request**:
```json
{
  "type": "LOAD_SOURCE",
  "source": "module [foo]\nfoo = 42\nbar = \"baz\"\n"
}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "message": "LOADED",
  "diagnostics": {
    "summary": {"errors": 0, "warnings": 1},
    "debug_counts": { ... },
    "list": [
      {
        "severity": "warning",
        "message": "UNUSED_DEFINITION",
        "region": {
          "start_line": 1,
          "start_column": 1,
          "end_line": 1,
          "end_column": 5
        }
      }
    ],
    "html": "<div class=\"report warning\">...</div>"
  }
}
```

#### 3. QUERY_TOKENS
**State**: LOADED
**Purpose**: Get tokenization results as formatted HTML

**Request**:
```json
{"type": "QUERY_TOKENS"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "data": "<div class=\"token-list\">...</div>"
}
```

#### 4. QUERY_AST
**State**: LOADED
**Purpose**: Get Abstract Syntax Tree as formatted HTML

**Request**:
```json
{"type": "QUERY_AST"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "data": "<div class=\"ast-tree\">...</div>"
}
```

#### 5. QUERY_CIR
**State**: LOADED
**Purpose**: Get Canonical Intermediate Representation as formatted HTML

**Request**:
```json
{"type": "QUERY_CIR"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "data": "<div class=\"cir-display\">...</div>"
}
```

#### 6. QUERY_TYPES
**State**: LOADED
**Purpose**: Get type information as formatted HTML

**Request**:
```json
{"type": "QUERY_TYPES"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "data": "<div class=\"types-display\">...</div>"
}
```

#### 7. GET_TYPE_INFO
**State**: LOADED
**Purpose**: Get type information at a specific source position

**Request**:
```json
{
  "type": "GET_TYPE_INFO",
  "line": 1,
  "column": 5
}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "type": "Num *",
  "region": {
    "start_line": 1,
    "start_column": 1,
    "end_line": 1,
    "end_column": 7
  }
}
```

#### 8. RESET
**State**: LOADED → READY
**Purpose**: Clean up compilation state and return to READY

**Request**:
```json
{"type": "RESET"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "message": "debug-813709ba"
}
```

## Response Status Codes

- `SUCCESS`: Operation completed successfully
- `ERROR`: Operation failed due to internal error
- `INVALID_STATE`: Message not valid for current state
- `INVALID_MESSAGE`: Malformed or unrecognized message

## Diagnostic Information

The `LOAD_SOURCE` response includes comprehensive diagnostic information:

### Summary
- `errors`: Total number of compilation errors
- `warnings`: Total number of warnings

### Debug Counts
Per-stage diagnostic counts:
- `tokenize`: Tokenization errors/warnings
- `parse`: Parse errors/warnings
- `can`: Canonicalization errors/warnings
- `type`: Type checking errors/warnings

### Diagnostic List
Array of diagnostic objects with:
- `severity`: "error", "warning", or "info"
- `message`: Human-readable diagnostic message
- `region`: Source location with start/end line/column

### HTML Output
Pre-formatted HTML for displaying diagnostics in web interfaces.

## Integration Notes

1. **Initialization**: Always call `init()` before processing messages
2. **State Awareness**: Track the current state to send appropriate messages
3. **Memory Management**: Always pair `allocate` with `deallocate` calls
4. **Error Recovery**: Handle both WASM errors and compilation errors gracefully
5. **Buffer Sizing**: Ensure response buffers are large enough for expected output

## Build Requirements

Built with Zig 0.14.0 targeting WASM32:
```bash
zig build playground
```

Output: `zig-out/bin/playground.wasm`
