#pragma once

namespace kcov
{
	class IFileParser;
	class IReporter;

	IWriter &createCoverallsWriter(IFileParser &parser, IReporter &reporter);
}
