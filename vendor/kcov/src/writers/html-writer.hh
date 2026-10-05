#pragma once

namespace kcov
{
	class IFileParser;
	class IReporter;
	class IOutputHandler;

	IWriter &createHtmlWriter(IFileParser &elf, IReporter &reporter,
			const std::string &indexDirectory,
			const std::string &outDirectory,
			const std::string &name,
			bool includeInTotals = true);
}
