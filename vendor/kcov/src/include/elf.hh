#pragma once

#include <utils.hh>

#include <vector>
#include <utility>
#include <string>

// Add symbols missing from FreeBSD's elfutils.
#ifndef ELF_NOTE_GNU
#define ELF_NOTE_GNU "GNU"
#endif
#ifndef NT_GNU_BUILD_ID
#define NT_GNU_BUILD_ID 3
#endif

namespace kcov
{

	/**
	 * Holder class for address segments
	 */
	class Segment
	{
	public:
		Segment(const void *data, uint64_t paddr, uint64_t vaddr, uint64_t size) :
			m_data(NULL), m_paddr(paddr), m_vaddr(vaddr), m_size(size)
		{
			if (data)
			{
				m_data = xmalloc(size);
				memcpy(m_data, data, size);
			}
		}

		Segment(const Segment &other) :
			m_data(NULL), m_paddr(other.m_paddr), m_vaddr(other.m_vaddr), m_size(other.m_size)
		{
			if (other.m_data)
			{
				m_data = xmalloc(other.m_size);
				memcpy(m_data, other.m_data, other.m_size);
			}
		}

		~Segment()
		{
			free(m_data);
		}

		/**
		 * Check if an address is contained within this segment
		 *
		 * @param addr the address to check
		 *
		 * @return true if valid
		 */
		bool addressIsWithinSegment(uint64_t addr) const
		{
			return addr >= m_paddr && addr < m_paddr + m_size;
		}

		/**
		 * Adjust an address with the segment.
		 *
		 * @param addr the address to adjust
		 *
		 * @return the new address
		 */
		uint64_t adjustAddress(uint64_t addr) const
		{
			if (addressIsWithinSegment(addr))
				return addr - m_paddr + m_vaddr;

			return addr;
		}

		uint64_t getBase() const
		{
			return m_vaddr;
		}

		const void *getData() const
		{
			return m_data;
		}

		size_t getSize() const
		{
			return m_size;
		}

	private:
		void *m_data;

		// Should really be const, but GCC 4.6 doesn't like that
		uint64_t m_paddr;
		uint64_t m_vaddr;
		size_t m_size;
	};


	class IElf
	{
	public:
		virtual ~IElf()
		{
		}

		virtual const std::vector<std::string> &getGcovGcnoFiles() = 0;

		virtual const std::vector<std::string> &getGcovGcdaFiles() = 0;

		virtual const std::string &getBuildId() = 0;

		/**
		 * Return the debug link string and the CRC
		 *
		 * @return the debug link or NULL if there are none
		 */
		virtual const std::pair<std::string, uint32_t> *getDebugLink() = 0;

		virtual const std::vector<Segment> &getSegments() = 0;

		virtual void *getRawData(size_t &sz) = 0;

		static IElf *create(const std::string &filename);
	};
}
