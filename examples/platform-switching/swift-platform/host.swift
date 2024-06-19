// Build with latest Swift 6.0 using this command (adjusting for architecture):
// swiftc -target aarch64-none-none-elf -Ounchecked -parse-as-library -enable-experimental-feature Embedded -wmo -c host.swift -o linux-arm64.o

@main
enum Main {
    static func main() {
        var str = RocString()
        mainForHost(&str)
        
        print(str, terminator: "")
    }
}

// Swift assumes its structs are not representable in C so the type has to be hidden
// to get around a hardcoded safety check.
@_extern(c, "roc__mainForHost_1_exposed_generic")
func _mainForHost(_: UnsafeMutableRawPointer)
func mainForHost(_ string: UnsafeMutablePointer<RocString>) { _mainForHost(UnsafeMutableRawPointer(string)) }

/// NOTE: The struct layout of Swift is that of C, without padding between fields (if alignment allows)
/// There is no way to define this in a stable way yet.
///
/// Only the first three fields are stored to match RocStr memory layout
struct RocString: BitwiseCopyable {
    private let rawData: UnsafePointer<UInt8>?
    private let rawCount: UInt
    private let capacityOrAllocPointer: UInt // This is a union accessed through computed properties
    
    private var capacity: UInt { capacityOrAllocPointer }
    private var allocPointer: UnsafeMutableRawPointer { .init(bitPattern: capacityOrAllocPointer)! }
    
    private var isSmallString: Bool { Int(bitPattern: capacity) < 0 }
    
    private var count: Int {
        isSmallString // Small strings encode their count in the last byte of capacity
            ? withUnsafeBytes(of: capacity) { bytes in Int(bytes.last! ^ 0b1000_0000) }
            : Int(bitPattern: rawCount)
    }
    
    // SAFETY: Not valid for small strings
    private var data: UnsafeBufferPointer<UInt8> {
        assert(!isSmallString) // Assert this in debug builds
        return if isSmallString { fatalError() } else { .init(start: rawData!, count: count) }
    }
    
    @_transparent
    internal func toString() -> String {
        if isSmallString {
            withUnsafeBytes(of: self) { bytes in
                .init(unsafeUninitializedCapacity: count) { buffer in
                    for (offset, byte) in bytes.enumerated() {
                        buffer[offset] = byte
                    }
                    return count
                }
            }
        } else {
            .init(unsafeUninitializedCapacity: count) { buffer in
                for (offset, byte) in data.enumerated() {
                    buffer[offset] = byte
                }
                return count
            }
        }
    }
    
    /// Default initializer for imported C structs (unsafe zero initialisation)
    init() {
        self.rawData = nil
        self.rawCount = 0
        self.capacityOrAllocPointer = 0
    }
}

// RocString to String conversion
extension String {
    init(_ rocString: RocString) { self = rocString.toString() }
}

// Make RocString printable directly
extension RocString: CustomStringConvertible {
    var description: String { .init(self) }    
}

@_cdecl("roc_alloc")
func rocAlloc(size: Int, alignment: Int) -> UnsafeMutableRawPointer { .allocate(byteCount: size, alignment: alignment) }

@_cdecl("roc_dealloc")
func rocDealloc(pointer: UnsafeMutableRawPointer, alignment: Int) { pointer.deallocate() }

@_cdecl("roc_realloc")
func rocRealloc(pointer: UnsafeMutableRawPointer, oldSize: Int, newSize: Int, alignment: Int) -> UnsafeMutableRawPointer {
    let new = UnsafeMutableRawPointer.allocate(byteCount: newSize, alignment: alignment)
    new.copyMemory(from: pointer, byteCount: oldSize)
    pointer.deallocate()
    return new
}
