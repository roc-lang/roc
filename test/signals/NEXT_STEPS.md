# Signals Platform - Next Steps

## Current Status

The signals platform is blocked by a compiler bug involving pattern matching on complex nested opaque types from lists. Once that bug is fixed, we can continue with the following steps.

## Immediate Next Steps

### 1. Verify the Platform Works

```bash
cd test/signals
roc app.roc -- test_counter.txt
```

Expected: The counter app should run without crashing, processing all UI elements (buttons, label) correctly.

### 2. Test the Signal Graph Execution

Once `Elem.walk!` completes successfully, verify that:
- Signal nodes are created correctly in the host
- Event nodes are created and connected
- The `bind_text!` and `bind_click!` host functions work
- The test harness can simulate clicks and verify signal updates

### 3. Implement Test Harness Commands

The test spec (`test_counter.txt`) should support:
- `click <element_id>` - Simulate a click event
- `expect_text <element_id> <expected_value>` - Verify displayed text
- `tick` - Advance the signal graph by one step

### 4. Complete the Host Implementation

Review `host.zig` to ensure:
- Signal graph evaluation works correctly
- Event propagation triggers signal updates
- Text bindings update when signals change

## Future Enhancements

### Signal Types
- [ ] Add more signal combinators (filter, debounce, throttle)
- [ ] Support for async signals (HTTP requests, timers)

### UI Elements
- [ ] Add more element types (input, checkbox, select)
- [ ] Support for element attributes/styles
- [ ] Conditional rendering

### Testing
- [ ] Add more test cases for edge cases
- [ ] Performance benchmarks for large signal graphs

## Files Overview

| File | Purpose |
|------|---------|
| `app.roc` | Counter demo app |
| `platform/Elem.roc` | UI element tree with walk! |
| `platform/Signal.roc` | Signal API for users |
| `platform/SignalNode.roc` | Internal signal graph representation |
| `platform/Event.roc` | Event API for users |
| `platform/EventNode.roc` | Internal event graph representation |
| `platform/NodeValue.roc` | Universal value type for host boundary |
| `platform/Host.roc` | Host effect declarations |
| `platform/host.zig` | Zig host implementation |
| `test_counter.txt` | Test specification |
