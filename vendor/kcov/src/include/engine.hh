#pragma once

#include <manager.hh>

#include <stdint.h>
#include <stdlib.h>

#include <string>

namespace kcov
{
	class IFileParser;

	enum event_type
	{
		ev_error       = -1,
		ev_breakpoint  =  1,
		ev_signal      =  2,
		ev_exit        =  3,
		ev_exit_first_process = 4,
		ev_signal_exit =  5,
	};

	/**
	 * An "engine" which can run programs and collect events (e.g., breakpoints)
	 */
	class IEngine
	{
	public:
		class Event
		{
		public:
			Event(enum event_type type = ev_signal, int data = 0, uint64_t address = 0) :
				type(type),
				data(data),
				addr(address)
			{
			}

			enum event_type type;

			int data; // Typically the breakpoint
			uint64_t addr;
		};

		class IEventListener
		{
		public:
			virtual void onEvent(const Event &ev) = 0;
		};


		virtual ~IEngine() {}

		/**
		 * Set a breakpoint
		 *
		 * @param addr the address to set the breakpoint on
		 *
		 * @return the ID of the breakpoint, or -1 on failure
		 */
		virtual int registerBreakpoint(unsigned long addr) = 0;

		/**
		 * Fork a new process and attach to it
		 *
		 * @return true if OK, false otherwise
		 */
		virtual bool start(IEventListener &listener, const std::string &executable) = 0;

		/**
		 * Kill child with signal
		 *
		 * @param sig the signal to send to the child
		 */
		virtual void kill(int sig) = 0;

		/**
		 * Continue execution with an event
		 *
		 * @return true if the process should continue, false otherwise
		 */
		virtual bool continueExecution() = 0;
	};

	/**
	 * Factory class for getting one of multiple engines, which can
	 * match different file types.
	 */
	class IEngineFactory
	{
	public:
		class IEngineCreator
		{
		public:
			IEngineCreator();

			virtual IEngine *create() = 0;

			/**
			 * See if a particular file can be matched with this engine.
			 *
			 * Should return how well this engine fits, the higher the better
			 *
			 * @param filename the name of the file
			 * @param data the first few bytes of the file
			 * @param dataSize the size of @a data
			 */
			virtual unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize) = 0;
		};

		typedef IEngine *(*EngineCreator_t)(IFileParser &parser);

		virtual ~IEngineFactory()
		{
		}

		virtual void registerEngine(IEngineCreator &creator) = 0;

		virtual IEngineCreator &matchEngine(const std::string &file) = 0;

		static IEngineFactory &getInstance();
	};
}
