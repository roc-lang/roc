#pragma once

#include <string>

#include <stdint.h>

namespace kcov
{
	class IFileParser;
	class IEngine;
	class IFilter;

	/**
	 * Run a program and collect coverage for it.
	 */
	class ICollector
	{
	public:
		class IListener
		{
		public:
			/**
			 * Called when an address is hit.
			 *
			 * @param addr the address which just got executed
			 * @param hits the number of hits for the address
			 */
			virtual void onAddressHit(uint64_t addr, unsigned long hits) = 0;
		};

		class IEventTickListener
		{
		public:
			virtual void onTick() = 0;
		};

		virtual ~ICollector() {};

		/**
		 * Register a listener for breakpoint address hits
		 *
		 * @param listener the listener
		 */
		virtual void registerListener(IListener &listener) = 0;

		/**
		 * Register a listener for events (called on each breakpoint)
		 */
		virtual void registerEventTickListener(IEventTickListener &listener) = 0;

		/**
		 * Run a program and collect coverage data
		 *
		 * @param filename the program to run
		 */
		virtual int run(const std::string &filename) = 0;



		static ICollector &create(IFileParser &elf, IEngine &engine, IFilter &filter);
	};
}
