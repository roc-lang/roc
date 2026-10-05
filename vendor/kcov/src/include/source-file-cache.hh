#pragma once

#include <vector>
#include <string>

#include <stdint.h>

namespace kcov
{
	/**
	 * Cache class for source code
	 */
	class ISourceFileCache
	{
	public:
		virtual ~ISourceFileCache()
		{
		}

		/**
		 * Get the source lines of a file
		 *
		 * @param filePath the file to lookup
		 *
		 * @return A reference to the source lines (possibly empty)
		 */
		virtual const std::vector<std::string> &getLines(const std::string &filePath) = 0;

		/**
		 * Get the checksum for a file
		 *
		 * @return the CRC32 of the file
		 */
		virtual uint32_t getCrc(const std::string &filePath) = 0;

		virtual bool fileExists(const std::string &filePath) = 0;

		static ISourceFileCache &getInstance();
	};
}
