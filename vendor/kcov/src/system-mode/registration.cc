#include <system-mode/registration.hh>

#include <utils.hh>

static bool unmarshalProcessEntry(struct new_process_entry *p)
{
	// Native endian
	if (p->magic != NEW_PROCES_MAGIC)
	{
		return false;
	}

	if (p->version != NEW_PROCES_VERSION)
	{
		return false;
	}

	if (p->entry_size < sizeof(*p) + 1) // At least '\0' for the filename
	{
		return false;
	}

	// Must be NULL-terminated
	uint16_t filenameSize = p->entry_size - sizeof(*p);
	if (p->filename[filenameSize - 1] != '\0')
	{
		return false;
	}

	return true;
}

static void marshalProcessEntry(struct new_process_entry *p)
{
	// All native endian
}

bool parseProcessEntry(struct new_process_entry *p, uint16_t &outPid, std::string &outFilename)
{
	if (!unmarshalProcessEntry(p))
	{
		return false;
	}

	outPid = p->pid;
	outFilename = p->filename;

	return true;
}

struct new_process_entry *createProcessEntry(uint16_t pid, const std::string &filename)
{
	uint32_t size = sizeof(struct new_process_entry) + filename.size() + 1;
	struct new_process_entry *entry = (struct new_process_entry *)xmalloc(size);

	entry->magic = NEW_PROCES_MAGIC;
	entry->version = NEW_PROCES_VERSION;
	entry->entry_size = size;

	entry->pid = pid;
	strcpy(entry->filename, filename.c_str());

	marshalProcessEntry(entry);

	return entry;
}
