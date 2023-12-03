import Foundation
import SwiftUI

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

func isSmallString(rocStr: RocStr) -> Bool {
    return rocStr.capacity < 0
}

func getStrLen(rocStr: RocStr) -> Int {
    if isSmallString(rocStr: rocStr) {
        // Small String length is last in the byte of capacity.
        var cap = rocStr.capacity
        let count = MemoryLayout.size(ofValue: cap)
        let bytes = Data(bytes: &cap, count: count)
        let lastByte = bytes[count - 1]
        return Int(lastByte ^ 0b1000_0000)
    } else {
        return rocStr.len
    }
}

func getSwiftString(rocStr: RocStr) -> String {
    let length = getStrLen(rocStr: rocStr)
    
    if isSmallString(rocStr: rocStr) {
        let data: Data = withUnsafePointer(to: rocStr) { ptr in
            Data(bytes: ptr, count: length)
        }
        return String(data: data, encoding: .utf8)!
    } else {
        let data = Data(bytes: rocStr.bytes, count: length)
        return String(data: data, encoding: .utf8)!
    }
}

struct ContentView: View {
    var str: String
    
    init() {
        var rocStr = RocStr()
        roc__mainForHost_1_exposed_generic(&rocStr)
        self.str = getSwiftString(rocStr: rocStr)
    }

    var body: some View {
        Text(self.str)
            .padding()
    }
}

@main
struct RocTestApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}