#pragma once

#include <reporter.hh>

#include <string>

namespace kcov
{
	class IFileParser;

	class IMergeParser :
		public IFileParser,
		public IReporter::IListener,
		public IWriter,
		public ICollector
	{
	};

	IMergeParser &createMergeParser(IReporter &reporter,
			const std::string &baseDirectory,
			const std::string &outputDirectory,
			IFilter &filter);
}
