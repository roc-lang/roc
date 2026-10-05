#include "nocover.hh"

#include <iostream>
#include <cstdlib>

namespace kcov
{

enum FileType {
  zig = 0,
  c = 1,
  cpp = 2,
  unsupported = 3,
};

bool endsWith(const std::string& full_string, const std::string& ending) {
    if (full_string.length() >= ending.length()) {
        return (full_string.compare(full_string.length() - ending.length(), ending.length(), ending) == 0);
    } else {
        return false;
    }
}

FileType getFileExt(const std::string& file_name) {
  const std::string zig_ending = ".zig";
  const std::string c_ending = ".c";
  const std::string cpp_ending1 = ".cpp";
  const std::string cpp_ending2 = ".cc";

  if (endsWith(file_name, zig_ending)) {
    return zig;
  }

  if (endsWith(file_name, c_ending)) {
    return c;
  }

  if (endsWith(file_name, cpp_ending1) || endsWith(file_name, cpp_ending2)) {
    return cpp;
  }

  // For unsupported file types (assembly, headers, etc.), don't abort - just skip coverage
  return unsupported;
}

bool shouldCover(const std::string& line, const std::string& file_name) {
  FileType t = getFileExt(file_name);

  switch(t) {
    case zig: {
      const std::string zig_nocover_1 = "unreachable";
      const std::string zig_nocover_2 = "@panic";

      if (line.find(zig_nocover_1) != std::string::npos) {
        return false;
      }
      if (line.find(zig_nocover_2) != std::string::npos) {
        return false;
      }
      return true;
    }
    break;

    case c: {
      const std::string c_nocover = "/* no-cover */";
      if (line.find(c_nocover) != std::string::npos) {
        return false;
      }
      return true;
    }
    break;

    case cpp: {
      const std::string cpp_nocover = "/* no-cover */";
      if (line.find(cpp_nocover) != std::string::npos) {
        return false;
      }
      return true;
    }
    break;

    case unsupported:
      // Don't track coverage for unsupported file types (assembly, headers, etc.)
      return false;
  }

  return true;
}

}
