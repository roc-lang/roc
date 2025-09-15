# ShaRoc

**ShaRoc** is a pure and robust SHA-256 hashing library for the Roc programming language. It is designed to be easy to use and integrate into your Roc projects, providing a secure and reliable way to compute SHA-256 hashes. This library aims for compliance with FIPS 180-4 standards.

## Features

*   **Pure Roc Implementation**: `ShaRoc` is written entirely in Roc, ensuring seamless integration and leveraging Roc's performance and safety features.
*   **FIPS 180-4 Compliance**: The library is designed to follow the official SHA-256 standard, providing reliable and secure hashes.
*   **Zero Dependencies**: This library has no external dependencies, making it lightweight and easy to add to any Roc project.
*   **Flexible API**: Supports hashing for both raw bytes (`List U8`) and strings (`Str`), with outputs available as raw bytes or hexadecimal strings.

## Quick Start

To quickly get started, add `ShaRoc` to your project's dependencies. Then, you can use it as follows:

```roc
app "myApp"
    imports [
        roc-community.sha256.Sha256, # (Assuming this will be the future package name)
        pf.Stdout,
    ]
    provides [main] to pf

main =
    # Hash a string and get a hex output
    helloHash = Sha256.hashStrToHex "hello world"
    Stdout.line "SHA-256 of 'hello world': (helloHash)"!

    # Hash a list of bytes and get a raw byte output
    byteList = [0x68, 0x65, 0x6c, 0x6c, 0x6f] # "hello"
    rawHashBytes = Sha256.hash byteList
    # To see the raw bytes as hex, you can use hashToHex:
    helloBytesAsHex = Sha256.hashToHex byteList
    Stdout.line "SHA-256 of [0x68, 0x65, 0x6c, 0x6c, 0x6f]: (helloBytesAsHex)"!

```

## Public API

The `Sha256` module exposes the following primary functions:

*   `hash : List U8 -> List U8`
    *   Computes the SHA-256 hash of a list of bytes (`List U8`) and returns the 32-byte hash as a `List U8`.
*   `hashToHex : List U8 -> Str`
    *   Computes the SHA-256 hash of a list of bytes (`List U8`) and returns the hash as a 64-character lowercase hexadecimal string (`Str`).
*   `hashStrToHex : Str -> Str`
    *   Computes the SHA-256 hash of a Roc string (`Str`) (after converting it to UTF-8 bytes) and returns the hash as a 64-character lowercase hexadecimal string (`Str`).
*   `hashStrToBytes : Str -> List U8`
    *   Computes the SHA-256 hash of a Roc string (`Str`) (after converting it to UTF-8 bytes) and returns the 32-byte hash as a `List U8`.

## Usage Examples

Here's how you can use ShaRoc in your Roc application:

```roc
app "myApp"
    imports [
        roc-community.sha256.Sha256, # (Assuming this will be the future package name)
        pf.Stdout,
    ]
    provides [main] to pf

main =
    # Example using hashStrToHex
    helloHashStr = Sha256.hashStrToHex "hello world"
    # To print the hash, you'd convert it to UTF-8 for Stdout:
    # Stdout.line "SHA-256 of 'hello world': (Str.toUtf8 helloHashStr)"!

    # Example using hashToHex
    byteList = [0x68, 0x65, 0x6c, 0x6c, 0x6f] # "hello"
    helloBytesHashStr = Sha256.hashToHex byteList
    # To print the hash, you'd convert it to UTF-8 for Stdout:
    # Stdout.line "SHA-256 of [0x68, 0x65, 0x6c, 0x6c, 0x6f]: (Str.toUtf8 helloBytesHashStr)"!

    # Example using hash (raw byte output)
    rawHashBytes = Sha256.hash byteList
    # rawHashBytes is a List U8. You can use it for cryptographic purposes
    # or convert it to a hex string if needed for display.
    # If you need a hex string, using Sha256.hashToHex (as shown above) is more direct.
    # If you needed to print these specific raw bytes as hex, you would first
    # need a way to convert a List U8 to a hex Str. For example:
    # Stdout.line "Raw hash bytes (hex): (Str.toUtf8 (yourOwnBytesToHex rawHashBytes))"!
    # For this demo, we'll just re-use helloBytesHashStr to show what its hex form would be:
    Stdout.line "SHA-256 of [0x68, 0x65, 0x6c, 0x6c, 0x6f] (raw bytes, if converted to hex): (Str.toUtf8 helloBytesHashStr)"!


# Note: To run these examples, you would need the Sha256 library available
# and a platform that provides Stdout, like `basic-cli`.
# The exact import path `roc-community.sha256.Sha256` is a placeholder
# for how packages will be named and imported.
```

## Installation

To use the ShaRoc library in your Roc project, you'll need to add it as a dependency in your `Package.roc` file and then import the `Sha256` module in your Roc source files.

### 1. Add Dependency

Open your project's `Package.roc` file and add `"roc-community/sha256"` to your `dependencies` list. If you don't have a `dependencies` section yet, you'll need to add it.

For example, if your `Package.roc` looks like this:

```roc
package "your-username/your-project"
    version "1.0.0"
    exposes [MyModule]
```

You would modify it to include `ShaRoc` as a dependency:

```roc
package "your-username/your-project"
    version "1.0.0"
    exposes [MyModule]
    dependencies [
        "roc-community/sha256" # Add this line
    ]
```

**Note on Package Resolution:** Roc's package management system is still evolving. Currently, how Roc fetches this dependency might vary:
*   It might be fetched from a central Roc package registry (if available).
*   You might need to specify a Git URL directly in the future, or use a local path for development.
*   Please refer to the latest Roc documentation for the most up-to-date package management practices.

### 2. Import Module

Once the dependency is declared, you can import the `Sha256` module in your Roc files (e.g., `main.roc` or any other `.roc` file where you need hashing):

```roc
imports [
    roc-community.sha256.Sha256,
    # ... other imports
]

# Example usage:
myHash = Sha256.hashStrToHex "hello roc"
```

The usage examples in the "Usage Examples" section of this README also demonstrate this import.

## Running Tests

To run the tests for this library:

You can run all tests for the library using the following command in your terminal:

```sh
roc test
```

If you want to run tests for a specific file, you can specify the path:

```sh
roc test tests/TestSha256.roc
```
