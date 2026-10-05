#pragma once

#include <stdint.h>

#include <functional>
#include <string>

namespace kcov
{
	static uint64_t getLineId(const std::string &fileName, unsigned int nr)
	{
		return (std::hash<std::string>()(fileName) & 0xffffffff) | ((uint64_t)nr << 32ULL);
	}
}
