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
        len < 0
    }
    
    var string: String {
        if isSmallString {
            fatalError("TODO: Implement small RocString")
        } else {
            let data = Data(bytes: bytes, count: len)
            return String(data: data, encoding: .utf8)!
        }
    }
}

@_cdecl("main")
func main() {
    print(roc__mainForHost_1_exposed().string)
}