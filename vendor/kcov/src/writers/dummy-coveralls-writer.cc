#include <reporter.hh>
#include <file-parser.hh>
#include <writer.hh>

#include "writer-base.hh"

class DummyCoverallsWriter : public kcov::IWriter
{
public:
	void onStartup()
	{
	}

	void onStop()
	{
	}

	void write()
	{
	}
};

namespace kcov
{
	IWriter &createCoverallsWriter(IFileParser &parser, IReporter &reporter)
	{
		return *new DummyCoverallsWriter();
	}
}
