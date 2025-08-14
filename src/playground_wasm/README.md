# Roc WASM Playground Interface Control Document

## Overview

The Roc Playground provides a WebAssembly interface to the compiler, providing analysis of `.roc` code in web browsers and JavaScript environments. It implements a state machine architecture to support both single-file compilation analysis and interactive REPL (Read-Eval-Print Loop) sessions.

The playground supports:
- **Single-file compilation**: Load and analyze complete Roc source files
- **Interactive REPL**: Evaluate expressions and definitions incrementally
- **Compiler introspection**: Query tokens, AST, CIR, types, and hover information
- **Diagnostic reporting**: Comprehensive error and warning information
- **State management**: Clean separation between compilation and REPL modes

## State Machine

The playground operates as a finite state machine with four states:

```
START --(INIT)-> READY --(LOAD_SOURCE)-> LOADED
                   |                       |
                   |                       |
                   +--(INIT_REPL)--> REPL_ACTIVE
                   ^                       |
                   |                       |
                   +--------(RESET)--------+
```

### State Descriptions

- **START**: Initial state, awaiting initialization
- **READY**: Initialized and ready to compile source code or start REPL
- **LOADED**: Source compiled, ready for queries about compilation stages
- **REPL_ACTIVE**: REPL session active, ready for interactive evaluation

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

#### 7. GET_HOVER_INFO
**State**: LOADED or REPL_ACTIVE
**Purpose**: Get hover information for an identifier at a specific source position

**Request**:
```json
{
  "type": "GET_HOVER_INFO",
  "identifier": "x",
  "line": 1,
  "ch": 5
}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "hover_info": {
    "name": "x",
    "type_str": "Num *",
    "definition_region": {
      "start_line": 1,
      "start_column": 1,
      "end_line": 1,
      "end_column": 7
    },
    "docs": null
  }
}
```

**Note**: In REPL_ACTIVE state, this works with the last REPL evaluation's ModuleEnv.

#### 8. INIT_REPL
**State**: READY → REPL_ACTIVE
**Purpose**: Initialize a REPL session for interactive evaluation

**Request**:
```json
{"type": "INIT_REPL"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "message": "REPL initialized",
  "repl_info": {
    "compiler_version": "debug-813709ba",
    "state": "REPL_ACTIVE"
  }
}
```

#### 9. REPL_STEP
**State**: REPL_ACTIVE
**Purpose**: Submit a line of input to the REPL for evaluation

**Request**:
```json
{
  "type": "REPL_STEP",
  "input": "x = 42"
}
```

**Response (Definition)**:
```json
{
  "status": "SUCCESS",
  "result": {
    "output": "assigned `x`",
    "type": "definition",
    "compiler_available": true
  }
}
```

**Response (Expression)**:
```json
{
  "status": "SUCCESS",
  "result": {
    "output": "43",
    "type": "expression",
    "compiler_available": true
  }
}
```

**Response (Error)**:
```json
{
  "status": "SUCCESS",
  "result": {
    "output": "Evaluation error: error.ZeroSizedType",
    "type": "error",
    "error_stage": "evaluation",
    "error_details": "error.ZeroSizedType",
    "compiler_available": false
  }
}
```

**Result Types**:
- `"definition"`: Input was a variable assignment (e.g., `x = 42`)
- `"expression"`: Input was an expression that was evaluated
- `"error"`: Input caused a compilation or evaluation error

**Error Information** (when `type` is `"error"`):
- `error_stage`: Indicates which compiler stage produced the error
  - `"parse"`: Syntax/parsing error
  - `"canonicalize"`: Canonicalization error
  - `"typecheck"`: Type checking error
  - `"layout"`: Layout computation error
  - `"evaluation"`: Runtime evaluation error
  - `"interpreter"`: Interpreter initialization error
  - `"runtime"`: General runtime error
  - `"unknown"`: Error stage could not be determined
- `error_details`: Additional error details, typically the specific error name or message

**Compiler Availability**:
- `compiler_available: true`: Compiler queries (QUERY_CIR, QUERY_TYPES, GET_HOVER_INFO) are available
- `compiler_available: false`: Compiler queries are not available (usually due to errors)

#### 10. CLEAR_REPL
**State**: REPL_ACTIVE
**Purpose**: Clear all REPL definitions while keeping the REPL session active

**Request**:
```json
{"type": "CLEAR_REPL"}
```

**Response**:
```json
{
  "status": "SUCCESS",
  "message": "REPL cleared",
  "repl_info": {
    "compiler_version": "debug-813709ba",
    "state": "REPL_ACTIVE"
  }
}
```

#### 11. RESET
**State**: LOADED or REPL_ACTIVE → READY
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

## REPL Functionality

The playground now supports interactive REPL (Read-Eval-Print Loop) sessions that allow users to:

- **Define variables**: `x = 42` → "assigned `x`"
- **Evaluate expressions**: `x + 1` → "43"
- **Redefine variables**: Automatically updates dependent expressions
- **Error recovery**: Invalid input doesn't corrupt REPL state
- **Compiler integration**: Access compiler diagnostics and introspection

### REPL Features

- **State Persistence**: Definitions remain available across multiple evaluations
- **Dependency Tracking**: When a variable is redefined, dependent expressions automatically use the new value
- **Error Recovery**: Invalid input doesn't corrupt the REPL state
- **Compiler Integration**: Full access to compiler diagnostics and introspection
- **Clean State Management**: Clear separation between REPL and single-file compilation modes

### Compiler Queries in REPL Mode

When in `REPL_ACTIVE` state, the following compiler query messages work with the last REPL evaluation:

- `QUERY_CIR`: Returns CIR for the last REPL evaluation
- `QUERY_TYPES`: Returns types for the last REPL evaluation  
- `GET_HOVER_INFO`: Returns hover info for the last REPL evaluation

These queries are only available when `compiler_available: true` in the REPL response.

## Integration Notes

1. **Initialization**: Always call `init()` before processing messages
2. **State Awareness**: Track the current state to send appropriate messages
3. **Memory Management**: Always pair `allocate` with `deallocate` calls
4. **Error Recovery**: Handle both WASM errors and compilation errors gracefully
5. **Buffer Sizing**: Ensure response buffers are large enough for expected output
6. **REPL State Management**: Use `RESET` to switch between REPL and single-file modes

## Example Usage

### Basic REPL Session

```javascript
// Initialize REPL
const initResponse = await sendMessage({type: "INIT_REPL"});

// Add definition
const defResponse = await sendMessage({
    type: "REPL_STEP", 
    input: "x = 42"
});
// Response: {result: {output: "assigned `x`", type: "definition"}}

// Evaluate expression
const exprResponse = await sendMessage({
    type: "REPL_STEP", 
    input: "x + 1"
});
// Response: {result: {output: "43", type: "expression"}}

// Query CIR of last evaluation
const cirResponse = await sendMessage({type: "QUERY_CIR"});
// Response: CIR representation of the last evaluation
```

### Error Handling and Recovery

```javascript
// Submit invalid syntax - Parse error
const parseError = await sendMessage({
    type: "REPL_STEP", 
    input: "x = = 42"  // Invalid syntax
});
// Response: {
//   result: {
//     output: "Parse error: Expected expression",
//     type: "error",
//     error_stage: "parse",
//     error_details: "Expected expression",
//     compiler_available: false
//   }
// }

// Submit type error
const typeError = await sendMessage({
    type: "REPL_STEP",
    input: "\"hello\" + 42"  // Type mismatch
});
// Response: {
//   result: {
//     output: "Type check expr error: TypeMismatch",
//     type: "error",
//     error_stage: "typecheck",
//     error_details: "TypeMismatch",
//     compiler_available: false
//   }
// }

// Submit evaluation error
const evalError = await sendMessage({
    type: "REPL_STEP",
    input: "x +"  // Incomplete expression
});
// Response: {
//   result: {
//     output: "Evaluation error: error.ZeroSizedType",
//     type: "error",
//     error_stage: "evaluation",
//     error_details: "error.ZeroSizedType",
//     compiler_available: false
//   }
// }

// REPL state remains intact after errors, continue with valid input
const validResponse = await sendMessage({
    type: "REPL_STEP", 
    input: "x = 42"  // Should still work
});
// Response: {result: {output: "assigned `x`", type: "definition"}}
```

### Using Error Information for Better UX

```javascript
// Process REPL response with enhanced error handling
function handleReplResponse(response) {
    const result = response.result;
    
    if (result.type === "error") {
        // Use error_stage to provide stage-specific help
        switch (result.error_stage) {
            case "parse":
                console.error("Syntax error:", result.output);
                showSyntaxHighlight(result.error_details);
                break;
            
            case "typecheck":
                console.error("Type error:", result.output);
                showTypeHints(result.error_details);
                break;
            
            case "evaluation":
                console.error("Runtime error:", result.output);
                showDebugInfo(result.error_details);
                break;
            
            default:
                console.error("Error:", result.output);
        }
        
        // Check if compiler is still available for queries
        if (result.compiler_available) {
            // Can still query CIR, types, etc.
            enableCompilerQueries();
        } else {
            disableCompilerQueries();
        }
    } else if (result.type === "definition") {
        console.log("Defined:", result.output);
    } else if (result.type === "expression") {
        console.log("Result:", result.output);
    }
}
```

### Dependency Updates

```javascript
// Define variables
await sendMessage({type: "REPL_STEP", input: "x = 10"});
await sendMessage({type: "REPL_STEP", input: "y = x + 5"});
const result1 = await sendMessage({type: "REPL_STEP", input: "y"});
// Response: {result: {output: "15", type: "expression"}}

// Redefine x
await sendMessage({type: "REPL_STEP", input: "x = 20"});
const result2 = await sendMessage({type: "REPL_STEP", input: "y"});
// Response: {result: {output: "25", type: "expression"}}
// Shows that y automatically uses the new value of x
```

## Build Requirements

Built with Zig 0.14.0 targeting WASM32:
```bash
zig build playground
```

Output: `zig-out/bin/playground.wasm`
