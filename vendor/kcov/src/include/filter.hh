#pragma once

#include <string>

namespace kcov
{
	/**
	 * Class for filtering output data.
	 *
	 * Used for the --include-path and --include-pattern command line options.
	 */
	class IFilter
	{
	public:
		class Handler
		{
		public:
			virtual bool includeFile(const std::string &file) = 0;
		};

		/**
		 * Run filters on @a path.
		 *
		 * @param path the path to check
		 *
		 * @return true if this path should be included in the output, false otherwise.
		 */
		virtual bool runFilters(const std::string &path) = 0;

		/**
		 * Run filters on file/line pair.
		 *
		 * @param path the path to check
		 *
		 * @return true if this path should be included in the output, false otherwise.
		 */
		virtual bool runLineFilters(const std::string &filePath,
				unsigned int lineNr,
				const std::string &line) = 0;

		/**
		 * Convert source path to a real path and (if configured) run replacement
		 * on parts of the path (if the source has moved).
		 *
		 * @param path the path to mangle
		 *
		 * @return the mangled path
		 */
		virtual std::string mangleSourcePath(const std::string &path) = 0;

		static IFilter &create();

		static IFilter &createBasic();


		virtual ~IFilter() {}
	};
}
