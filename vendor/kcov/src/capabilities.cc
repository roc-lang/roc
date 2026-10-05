#include <capabilities.hh>
#include <utils.hh>

#include <unordered_map>

using namespace kcov;

class Capabilities : public ICapabilities
{
public:
	Capabilities()
	{
		m_capabilities["handle-solibs"] = false;
	}

	void addCapability(const std::string &name)
	{
		validateCapability(name);
		m_capabilities[name] = true;
	}

	void removeCapability(const std::string &name)
	{
		validateCapability(name);
		m_capabilities[name] = false;
	}

	bool hasCapability(const std::string &name)
	{
		validateCapability(name);

		return m_capabilities[name];
	}

private:
	void validateCapability(const std::string &name)
	{
		panic_if(m_capabilities.find(name) == m_capabilities.end(),
				"Capability %s not present", name.c_str());
	}

	std::unordered_map<std::string, bool> m_capabilities;
};

static Capabilities g_capabilities;
ICapabilities &ICapabilities::getInstance()
{
	return g_capabilities;
}
