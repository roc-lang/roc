#include <engine.hh>
#include <utils.hh>

#include <vector>

using namespace kcov;

class EngineFactory: public IEngineFactory
{
public:
	EngineFactory()
	{
	}

	void registerEngine(IEngineCreator &engine)
	{
		m_engines.push_back(&engine);
	}

	IEngineCreator &matchEngine(const std::string &fileName)
	{
		IEngineCreator *best = NULL;
		size_t sz;

		uint8_t *data = (uint8_t *) peek_file(&sz, "%s", fileName.c_str());

		for (EngineList_t::iterator it = m_engines.begin();
				it != m_engines.end(); ++it)
		{
			if (!best)
				best = *it;

			if ((*it)->matchFile(fileName, data, sz)
					> best->matchFile(fileName, data, sz))
				best = *it;
		}
		free((void *) data);

		if (!best)
			panic("No engines registered");

		return *best;
	}

private:
	typedef std::vector<IEngineCreator *> EngineList_t;

	EngineList_t m_engines;
};

IEngineFactory::IEngineCreator::IEngineCreator()
{
	IEngineFactory::getInstance().registerEngine(*this);
}

static EngineFactory *g_instance;
IEngineFactory &IEngineFactory::getInstance()
{
	if (!g_instance)
		g_instance = new EngineFactory();

	return *g_instance;
}
