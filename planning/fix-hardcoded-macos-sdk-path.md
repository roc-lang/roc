# Fix Hardcoded macOS SDK Path

## Problem

The LLVM backend hardcodes the macOS SDK path to the Command Line Tools location. This will fail for users who have only Xcode installed (not Command Line Tools), or who have the SDK in a non-standard location.

## Location

`src/cli/llvm_eval.zig`, lines 107-110:

```zig
try args.append(try allocator.dupeZ(u8, "-lSystem"));
try args.append(try allocator.dupeZ(u8, "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"));
try args.append(try allocator.dupeZ(u8, "-syslibroot"));
try args.append(try allocator.dupeZ(u8, "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"));
```

## Possible SDK Locations

The macOS SDK can be in several places:

1. **Command Line Tools**: `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk`
2. **Xcode**: `/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk`
3. **Custom Xcode location**: Users can install Xcode anywhere
4. **Beta Xcode**: `/Applications/Xcode-beta.app/...`

## How to Find the SDK Path

### Option 1: Use `xcrun` (Recommended)

The `xcrun --show-sdk-path` command returns the correct SDK path for the currently selected Xcode/CLT:

```zig
fn getMacOSSdkPath(allocator: Allocator) ![]const u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xcrun", "--show-sdk-path" },
    }) catch return error.SdkNotFound;
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        allocator.free(result.stdout);
        return error.SdkNotFound;
    }

    // Trim trailing newline
    var path = result.stdout;
    if (path.len > 0 and path[path.len - 1] == '\n') {
        const trimmed = try allocator.dupe(u8, path[0 .. path.len - 1]);
        allocator.free(path);
        return trimmed;
    }
    return path;
}
```

### Option 2: Check Multiple Locations

If you want to avoid spawning a subprocess:

```zig
fn findMacOSSdk(allocator: Allocator) ![]const u8 {
    const candidates = [_][]const u8{
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk",
        "/Applications/Xcode-beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk",
    };

    for (candidates) |path| {
        // Check if the SDK exists by checking for a known file
        const sdk_settings = std.fmt.allocPrint(allocator, "{s}/SDKSettings.plist", .{path}) catch continue;
        defer allocator.free(sdk_settings);

        std.fs.accessAbsolute(sdk_settings, .{}) catch continue;
        return allocator.dupe(u8, path);
    }

    return error.SdkNotFound;
}
```

### Option 3: Use SDKROOT Environment Variable

Apple's tools respect the `SDKROOT` environment variable:

```zig
fn getMacOSSdkPath(allocator: Allocator) ![]const u8 {
    // First check SDKROOT environment variable
    if (std.process.getEnvVarOwned(allocator, "SDKROOT")) |path| {
        return path;
    } else |_| {}

    // Fall back to xcrun or path checking
    // ...
}
```

## Recommended Implementation

Combine the approaches with proper fallbacks:

```zig
fn getMacOSSdkPath(allocator: Allocator) ![]const u8 {
    // 1. Check SDKROOT environment variable
    if (std.process.getEnvVarOwned(allocator, "SDKROOT")) |path| {
        return path;
    } else |_| {}

    // 2. Try xcrun (most reliable)
    if (runXcrun(allocator)) |path| {
        return path;
    } else |_| {}

    // 3. Check common locations
    const candidates = [_][]const u8{
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk",
    };

    for (candidates) |path| {
        if (sdkExists(path)) {
            return allocator.dupe(u8, path);
        }
    }

    return error.SdkNotFound;
}
```

## What Needs to Change

Update the `linkWithLld` function in `src/cli/llvm_eval.zig`:

1. Replace hardcoded SDK path with dynamic lookup
2. Handle the case where no SDK is found with a helpful error message
3. Use the found SDK path for both `-L` (library path) and `-syslibroot` arguments

## Files to Modify

- `src/cli/llvm_eval.zig` - Update the macOS linking section

## Error Handling

When no SDK is found, provide a helpful error message suggesting the user:
1. Install Xcode Command Line Tools: `xcode-select --install`
2. Or set the `SDKROOT` environment variable manually

## Testing

- Test on a machine with only Command Line Tools installed
- Test on a machine with only Xcode installed
- Test with `SDKROOT` environment variable set to a custom path
- Verify that linking still works correctly after the change

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
