fun roc_alloc(size : LibC::SizeT, alignment : UInt32) : Pointer(Void)
  LibC.malloc(size)
end

fun roc_realloc(c_ptr : Pointer(Void), new_size : LibC::SizeT, old_size : LibC::SizeT, alignment : UInt32) : Pointer(Void)
  LibC.realloc(c_ptr, new_size)
end

fun roc_dealloc(c_ptr : Pointer(Void), alignment : UInt32) : Nil
  LibC.free(c_ptr)
end

lib LibRoc
  fun main = roc__mainForHost_1_exposed : Str

  struct Str
    bytes : Pointer(UInt8)
    len : LibC::SizeT
  end
end

class String
  def self.from_roc(str : LibRoc::Str) : String
    if str.len.to_i64! < 0
      str_ptr = pointerof(str).as(Pointer(UInt8))
      last_byte = str_ptr + sizeof(LibRoc::Str) - 1
      len = last_byte.value ^ 0b1000_0000
      String.new(str_ptr, len)
    else
      String.new(str.bytes, str.len)
    end
  end
end

module Roc
  def self.main : String
    String.from_roc(LibRoc.main)
  end
end

puts Roc.main
