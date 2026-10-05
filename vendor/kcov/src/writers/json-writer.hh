#pragma once

namespace kcov
{
	class IFileParser;
	class IReporter;
	class IOutputHandler;

	IWriter &createJsonWriter(IFileParser &elf, IReporter &reporter,
			const std::string &outFile);
}
