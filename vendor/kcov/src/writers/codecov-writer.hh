#pragma once

#include <string>

namespace kcov
{
	class IFileParser;
	class IReporter;
	class IOutputHandler;

	IWriter &createCodecovWriter(IFileParser &elf, IReporter &reporter,
			const std::string &outFile);
}
