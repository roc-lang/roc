#include <elf.hh>

#include <configuration.hh>

#include <elfutils/libdw.h>

using namespace kcov;

class ElfImpl : public IElf
{
public:
	ElfImpl(void *data, size_t size) :
			m_debugLinkValid(false), m_fileData((char *) data), m_fileSize(size)
	{
		parse();
	}

	~ElfImpl()
	{
	}

	virtual const std::vector<std::string> &getGcovGcdaFiles()
	{
		return m_gcdaFiles;
	}

	virtual const std::vector<std::string> &getGcovGcnoFiles()
	{
		return m_gcnoFiles;
	}

	virtual const std::string &getBuildId()
	{
		return m_buildId;
	}

	virtual const std::pair<std::string, uint32_t> *getDebugLink()
	{
		if (m_debugLinkValid)
			return &m_debugLink;

		return NULL;
	}

	virtual const std::vector<Segment> &getSegments()
	{
		return m_segments;
	}

	virtual void *getRawData(size_t &sz)
	{
		sz = m_fileSize;
		return m_fileData;
	}

private:
	typedef std::vector<std::string> FileList_t;

	bool parse()
	{
		struct Elf *elf;
		bool elfIs32Bit = true;

		Elf_Scn *scn = NULL;
		size_t shstrndx;
		bool ret = false;
		bool doScanForGcda = IConfiguration::getInstance().keyAsInt("gcov");
		unsigned int i;
		char *raw;
		size_t sz;

		if (!(elf = elf_memory((char *) m_fileData, m_fileSize)))
		{
			error("elf_begin failed\n");
			return false;
		}

		if (elf_getshdrstrndx(elf, &shstrndx) < 0)
		{
			error("elf_getshstrndx failed\n");
			goto out_elf_begin;
		}

		raw = elf_getident(elf, &sz);

		if (raw && sz > EI_CLASS)
			elfIs32Bit = raw[EI_CLASS] == ELFCLASS32;

		while ((scn = elf_nextscn(elf, scn)) != NULL)
		{
			uint64_t sh_type;
			uint64_t sh_addr;
			uint64_t sh_size;
			uint64_t sh_flags;
			uint64_t sh_name;
			uint64_t sh_offset;
			uint64_t n_namesz;
			uint64_t n_descsz;
			uint64_t n_type;
			char *n_data;
			char *name;

			if (elfIs32Bit)
			{
				Elf32_Shdr *shdr32 = elf32_getshdr(scn);

				sh_type = shdr32->sh_type;
				sh_addr = shdr32->sh_addr;
				sh_size = shdr32->sh_size;
				sh_flags = shdr32->sh_flags;
				sh_name = shdr32->sh_name;
				sh_offset = shdr32->sh_offset;
			}
			else
			{
				Elf64_Shdr *shdr64 = elf64_getshdr(scn);

				sh_type = shdr64->sh_type;
				sh_addr = shdr64->sh_addr;
				sh_size = shdr64->sh_size;
				sh_flags = shdr64->sh_flags;
				sh_name = shdr64->sh_name;
				sh_offset = shdr64->sh_offset;
			}

			Elf_Data *data = elf_getdata(scn, NULL);

			// Nothing there?
			if (!data)
				continue;

			if (!data->d_buf)
				continue;

			name = elf_strptr(elf, shstrndx, sh_name);
			if (!data)
			{
				error("elf_getdata failed on section %s\n", name);
				goto out_elf_begin;
			}

			// Parse rodata to find gcda files
			if (doScanForGcda && strcmp(name, ".rodata") == 0)
			{
				const char *dataPtr = (const char *) data->d_buf;

				for (size_t i = 0; i < data->d_size - 5; i++)
				{
					const char *p = &dataPtr[i];

					if (memcmp(p, (const void *) "gcda\0", 5) != 0)
						continue;

					const char *gcda = p;

					// Rewind to start of string
					while (gcda != dataPtr && *gcda != '\0')
						gcda--;

					// Rewound until start of rodata?
					if (gcda == dataPtr)
						continue;

					std::string file(gcda + 1);

					m_gcdaFiles.push_back(file);
				}
			}

			if (sh_type == SHT_NOTE)
			{
				if (elfIs32Bit)
				{
					Elf32_Nhdr *nhdr32 = (Elf32_Nhdr *) data->d_buf;

					n_namesz = nhdr32->n_namesz;
					n_descsz = nhdr32->n_descsz;
					n_type = nhdr32->n_type;
					n_data = (char *) data->d_buf + sizeof(Elf32_Nhdr);
				}
				else
				{
					Elf64_Nhdr *nhdr64 = (Elf64_Nhdr *) data->d_buf;

					n_namesz = nhdr64->n_namesz;
					n_descsz = nhdr64->n_descsz;
					n_type = nhdr64->n_type;
					n_data = (char *) data->d_buf + sizeof(Elf64_Nhdr);
				}

				if (::strcmp(n_data, ELF_NOTE_GNU) == 0 && n_type == NT_GNU_BUILD_ID)
				{
					const char *hex_digits = "0123456789abcdef";
					unsigned char *build_id;

					build_id = (unsigned char *) n_data + n_namesz;
					for (i = 0; i < n_descsz; i++)
					{
						m_buildId.push_back(hex_digits[(build_id[i] >> 4) & 0xf]);
						m_buildId.push_back(hex_digits[(build_id[i] >> 0) & 0xf]);
					}
				}
			}

			// Check for debug links
			if (strcmp(name, ".gnu_debuglink") == 0)
			{
				const char *p = (const char *) data->d_buf;
				m_debugLink.first.append(p);
				const char *endOfString = p + strlen(p) + 1;

				// Align address for the CRC32
				unsigned long addr = (unsigned long) (endOfString - p);
				unsigned long offs = 0;

				if ((addr & 3) != 0)
					offs = 4 - (addr & 3);
				// ... and read out the CRC32
				memcpy((void *) &m_debugLink.second, endOfString + offs, sizeof(m_debugLink.second));
				m_debugLinkValid = true;
			}

			if ((sh_flags & (SHF_EXECINSTR | SHF_ALLOC)) != (SHF_EXECINSTR | SHF_ALLOC))
				continue;

			Segment seg(m_fileData + sh_offset, sh_addr, sh_addr, sh_size);

			m_segments.push_back(seg);
		}

		// If we have gcda files, try to find the corresponding gcno dittos
		for (FileList_t::iterator it = m_gcdaFiles.begin(); it != m_gcdaFiles.end(); ++it)
		{
			std::string &gcno = *it; // Modify in-place
			size_t sz = gcno.size();

			// .gcda -> .gcno
			gcno[sz - 2] = 'n';
			gcno[sz - 1] = 'o';

			if (file_exists(gcno))
				m_gcnoFiles.push_back(gcno);
		}

		ret = true;

out_elf_begin:
		elf_end(elf);

		return ret;
	}

	std::vector<std::string> m_gcnoFiles;
	std::vector<std::string> m_gcdaFiles;
	std::string m_buildId;
	bool m_debugLinkValid;
	std::pair<std::string, uint32_t> m_debugLink;
	std::vector<Segment> m_segments;

	char *m_fileData;
	size_t m_fileSize;
};

IElf *IElf::create(const std::string &filename)
{
	size_t sz;
	void *data;

	// The data is freed by the parser
	data = read_file(&sz, "%s", filename.c_str());
	if (!data)
		return NULL;

	return new ElfImpl(data, sz);
}
