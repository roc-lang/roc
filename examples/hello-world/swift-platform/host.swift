import Foundation

@_cdecl("roc_alloc")
func rocAlloc(size: Int, _alignment: UInt) -> UInt  {
    guard let ptr = malloc(size) else {
        return 0
    }
    return UInt(bitPattern: ptr)
}

@_cdecl("roc_dealloc")
func rocDealloc(ptr: UInt, _alignment: UInt)  {
    free(UnsafeMutableRawPointer(bitPattern: ptr))
}

@_cdecl("roc_realloc")
func rocRealloc(ptr: UInt, _oldSize: Int, newSize: Int, _alignment: UInt) -> UInt {
    guard let ptr = realloc(UnsafeMutableRawPointer(bitPattern: ptr), newSize) else {
        return 0
    }
    return UInt(bitPattern: ptr)
}

extension RocStr {
    var isSmallString: Bool {
        capacity < 0
    }

    var length: Int {
        if isSmallString {
            // Small String length is last in the byte of capacity.
            var cap = capacity
            let count = MemoryLayout.size(ofValue: cap)
            let bytes = Data(bytes: &cap, count: count)
            let lastByte = bytes[count - 1]
            return Int(lastByte ^ 0b1000_0000)
        } else {
            return len
        }
    }

    var string: String {
        if isSmallString {
            let data: Data = withUnsafePointer(to: self) { ptr in
                Data(bytes: ptr, count: length)
            }
            return String(data: data, encoding: .utf8)!
        } else {
            let data = Data(bytes: bytes, count: len)
            return String(data: data, encoding: .utf8)!
        }
    }
}

@_cdecl("main")
func main() -> UInt8 {
    print(roc__mainForHost_1_exposed().string, terminator: "")
    return 0
}
