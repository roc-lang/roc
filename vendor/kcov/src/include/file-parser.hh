#pragma once

#include <manager.hh>

#include <sys/types.h>
#include <stdint.h>
#include <string.h>

#include <string>
#include <vector>

#include <utils.hh>

struct phdr_data_entry;

namespace kcov
{
	class IFilter;

	class IFileParser
	{
	public:
		enum FileFlags
		{
			FLG_NONE = 0,
			FLG_TYPE_SOLIB = 1,
			FLG_TYPE_COVERAGE_DATA = 2, //< Typically gcov data files
		};

		enum PossibleHits
		{
			HITS_SINGLE,     //< Yes/no (merge-parser)
			HITS_LIMITED,    //< E.g., multiple branches
			HITS_UNLIMITED,  //< Accumulated (Python/bash)
		};

		/**
		 * Holder class for files (e.g., ELF binaries)
		 */
		class File
		{
		public:
			File(const std::string &filename, const enum FileFlags flags = FLG_NONE) :
				m_filename(filename), m_flags(flags)
			{
			}

			const std::string m_filename;
			const enum FileFlags m_flags;
		};

		virtual ~IFileParser() {}

		/**
		 * Listener for lines (lines in source files)
		 *
		 * This is the main way the (file,lineNr) -> address map is handled.
		 */
		class ILineListener
		{
		public:
			virtual void onLine(const std::string &file, unsigned int lineNr,
					uint64_t addr) = 0;
		};

		/**
		 * Listener for added files (typically an ELF binary)
		 */
		class IFileListener
		{
		public:
			virtual void onFile(const File &file) = 0;
		};

		/**
		 * Add a file to the parser
		 *
		 * @param filename the filename to add
		 * @param phdr_data base address data for solibs
		 *
		 * @return true if the file could be added, false otherwise
		 */
		virtual bool addFile(const std::string &filename, struct phdr_data_entry *phdr_data = 0) = 0;

		/**
		 * Set the relocation of the main file for the position-independent executable (PIE) support
		 *
		 * PIEs have, like shared objects, an unknown load address which can only be
		 * retrieved from the solib handler. Because of this the parser may defer
		 * onFile and onLine notifications before this call.
		 *
		 * This method, if any, should be called after the initial parse call and
		 * beefore the following addFile calls for solibs.
		 *
		 * @param relocation relocation offset
		 *
		 * @return true if the relocation is properly set, false otherwise
		 */
		virtual bool setMainFileRelocation(unsigned long relocation) = 0;

		/**
		 * Register a listener for source lines.
		 *
		 * Will be called when new source file/line pairs are found
		 *
		 * @param listener the listener
		 */
		virtual void registerLineListener(ILineListener &listener) = 0;

		/**
		 * Register a listener for coveree files.
		 *
		 * Will be called when new ELF binary (typically) is added
		 *
		 * @param listener the listener
		 */
		virtual void registerFileListener(IFileListener &listener) = 0;

		/**
		 * Parse the added files
		 *
		 * @return true if parsing could be done
		 */
		virtual bool parse() = 0;

		/**
		 * Get the checksum of the main file (not solibs)
		 *
		 * @return the file checksum
		 */
		virtual uint64_t getChecksum() = 0;

		/**
		 * Get the name of the parser
		 *
		 * @return the name
		 */
		virtual std::string getParserType() = 0;


		/**
		 * Return if this parser is of the multiple type (i.e., relying on
		 * breakpoints which are cleared after hit, but can have branches),
		 * or if every address can occur multiple times, or if only
		 * covered/non-covered is possible.
		 *
		 * @return the possible hits of this parser
		 */
		virtual enum PossibleHits maxPossibleHits() = 0;

		/**
		 * See if a particular file can be matched with this parser.
		 *
		 * Should return how well this parser fits, the higher the better
		 *
		 * @param filename the name of the file
		 * @param data the first few bytes of the file
		 * @param dataSize the size of @a data
		 */
		virtual unsigned int matchParser(const std::string &filename, uint8_t *data, size_t dataSize) = 0;

		// Setup once the parser has been chosen
		virtual void setupParser(IFilter *filter) = 0;
	};


	/**
	 * Manager class for getting one of multiple parsers, which can
	 * match different file types.
	 */
	class IParserManager
	{
	public:
		virtual ~IParserManager()
		{
		}

		virtual void registerParser(IFileParser &parser) = 0;

		virtual IFileParser *matchParser(const std::string &file) = 0;

		static IParserManager &getInstance();
	};
}
