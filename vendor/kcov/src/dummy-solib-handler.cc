#include <solib-handler.hh>

using namespace kcov;

class DummySolibHandler : public ISolibHandler
{
public:
	virtual void startup()
	{
	}
};

void kcov::blockUntilSolibDataRead()
{
}

ISolibHandler &kcov::createSolibHandler(IFileParser &parser, ICollector &collector)
{
	return *new DummySolibHandler();
}
