#include <file-parser.hh>
#include <utils.hh>

#include <vector>

using namespace kcov;

class ParserManager: public IParserManager
{
public:
	ParserManager()
	{
	}

	void registerParser(IFileParser &parser)
	{
		m_parsers.push_back(&parser);
	}

	IFileParser *matchParser(const std::string &fileName)
	{
		IFileParser *best = NULL;
		size_t sz;

		uint8_t *data = (uint8_t *) peek_file(&sz, "%s", fileName.c_str());

		if (!data)
			return NULL;

		size_t preVal = -1;
		for (ParserList_t::const_iterator it = m_parsers.begin(); it != m_parsers.end(); ++it)
		{
			unsigned int myVal = (*it)->matchParser(fileName, data, sz);
			if (myVal == match_none)
				continue;

			if (!best || myVal > preVal) {
				best = *it;
				preVal = myVal;
			}
		}

		free((void *) data);

		return best;
	}

private:
	typedef std::vector<IFileParser *> ParserList_t;

	ParserList_t m_parsers;
};

static ParserManager *g_instance;
IParserManager &IParserManager::getInstance()
{
	if (!g_instance)
		g_instance = new ParserManager();

	return *g_instance;
}
