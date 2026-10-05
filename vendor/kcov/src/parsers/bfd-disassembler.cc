#include <disassembler.hh>
#include <utils.hh>

#include <unordered_map>
#include <set>
#include <map>
#include <algorithm>
#include <stdexcept>

#ifndef ATTRIBUTE_FPTR_PRINTF_2
# define ATTRIBUTE_FPTR_PRINTF_2
#endif

#include <bfd.h>
#include <dis-asm.h>

#include <elf.h>

using namespace kcov;

const uint64_t BT_INVALID = 0xfffffffffffffffeull;

const std::set<std::string> x86BranchInstructions =
{
		"ja",
		"jae",
		"jbe",
		"jc",
		"je",
		"jg",
		"jge",
		"jl",
		"jle",
		"jna",
		"jnae",
		"jnb",
		"jnbe",
		"jnc",
		"jne",
		"jng",
		"jnge",
		"jnl",
		"jnle",
		"jno",
		"jnp",
		"jns",
		"jnz",
		"jo",
		"jp",
		"jpe",
		"jpo",
		"js",
		"jz",
		"jczx",
		"jezx",
		"loop",
		"jmp",
		"call",
		"ret",
};

/*
 * Since binutils >= 2.29 or so, print_insn_i386 is no longer defined in
 * dis-asm.h. I'm not sure what the correct and backwards compatible way
 * of handling this is, so define it here.
 */
extern "C" int print_insn_i386              (bfd_vma, disassemble_info *);

class BfdDisassembler : public IDisassembler
{
public:
	BfdDisassembler()
	{
		memset(&m_info, 0, sizeof(m_info));
#if KCOV_LIBFD_DISASM_STYLED
		init_disassemble_info(&m_info, (void *)this, BfdDisassembler::opcodesFprintFuncStatic, BfdDisassembler::opcodesFprintStyledFuncStatic);
#else
		init_disassemble_info(&m_info, (void *)this, BfdDisassembler::opcodesFprintFuncStatic);
#endif
		m_disassembler = print_insn_i386;

		m_info.arch = bfd_arch_i386;

		disassemble_init_for_target(&m_info);
		m_info.application_data = (void *)this;
	}

	virtual void setup(const void *header, size_t headerSize)
	{
		// New ELF file, clear any state from the previous instance
		m_cache.clear();
		m_instructions.clear();
		m_orderedInstructions.clear();
		m_bbs.clear();

		const uint8_t *data = (const uint8_t *)header;

		panic_if(headerSize <= EI_CLASS,
				"Header size must be at least %u", EI_CLASS);

		if (data[EI_CLASS] == ELFCLASS64)
			m_info.mach = bfd_mach_x86_64;
		else
			m_info.mach = bfd_mach_i386_i386;
	}

	void addSection(const void *sectionData, size_t sectionSize, uint64_t baseAddress)
	{
		SectionCache_t::iterator it = m_cache.find(baseAddress);

		// Not visited before
		if (it == m_cache.end())
			m_cache[baseAddress] = new Section(sectionData, sectionSize, baseAddress);
	}

	bool verify(uint64_t address)
	{
		Section *p = lookupSection(address);

		if (!p)
			return true;

		if (!p->isDisassembled())
			p->disassemble(*this, m_info, m_disassembler);

		// The address is valid there is an instruction starting at it
		return getInstruction(address) != NULL;
	}

	const std::vector<uint64_t> &getBasicBlock(uint64_t address)
	{
		if (m_bbs.empty())
			setupBasicBlocks();

		if (m_instructions.find(address) == m_instructions.end())
			return m_empty;

		return m_instructions[address].getBasicBlock()->getInstructionAddresses();
	}

private:
	class BasicBlock
	{
	public:
		BasicBlock()
		{
		}

		const std::vector<uint64_t> &getInstructionAddresses() const
		{
			return m_instructionAddresses;
		}

		void addInstructionAddress(uint64_t address)
		{
			m_instructionAddresses.push_back(address);
		}

	private:
		std::vector<uint64_t> m_instructionAddresses;
	};

	class Instruction
	{
	public:
		Instruction(uint64_t branchTarget = 0) :
			m_branchTarget(branchTarget),
			m_leader(false),
			m_bb(NULL)
		{
		}

		uint64_t getBranchTarget() const
		{
			return m_branchTarget;
		}

		bool isBranch() const
		{
			return m_branchTarget != BT_INVALID;
		}

		bool isLeader() const
		{
			return m_leader;
		}

		void makeLeader()
		{
			m_leader = true;
		}

		const BasicBlock *getBasicBlock() const
		{
			return m_bb;
		}

		void setBasicBlock(const BasicBlock *bb)
		{
			m_bb = bb;
		}

	private:
		uint64_t m_branchTarget;
		bool m_leader;
		const BasicBlock *m_bb;
	};

	class Section
	{
	public:
		Section(const void *data, size_t size, uint64_t startAddress) :
			m_size(size),
			m_startAddress(startAddress),
			m_disassembled(false)
		{
			m_data = xmalloc(size);
			memcpy(m_data, data, size);
		}

		uint64_t getBase() const
		{
			return m_startAddress;
		}

		size_t getSize() const
		{
			return m_size;
		}

		bool isDisassembled() const
		{
			return m_disassembled;
		}

		void disassemble(BfdDisassembler &target, struct disassemble_info info, disassembler_ftype disassembler)
		{
			if (m_disassembled)
					return;

			m_disassembled = true;

			info.buffer_vma = 0;
			info.buffer_length = m_size;
			info.buffer = (bfd_byte *)m_data;
			info.stream = (void *)&target;

			uint64_t pc = 0;
			int count;
			do
			{
				target.m_instructionVector.clear();
				count = disassembler(pc, &info);

				if (count < 0)
					break;

				target.m_instructions[pc + m_startAddress] = target.instructionFactory(pc, target.m_instructionVector);
				// Point back into the other map
				target.m_orderedInstructions[pc + m_startAddress] = &target.m_instructions[pc + m_startAddress];

				pc += count;
			} while (count > 0 && pc < m_size);
		}

	private:
		void *m_data;
		const size_t m_size;
		const uint64_t m_startAddress;

		bool m_disassembled; // Lazy disassembly once it's used
	};

	// Implementation taken from EmilPRO, https://github.com/SimonKagstrom/emilpro
	Instruction instructionFactory(uint64_t pc, const std::vector<std::string> &vec)
	{
		uint64_t branchTarget = BT_INVALID; // Invalid

		// No encoding???
		if (vec.size() < 1)
			return Instruction();

		std::set<std::string>::const_iterator it = x86BranchInstructions.find(vec[0]);

		// Address?
		if (it != x86BranchInstructions.end() &&
				vec.size() >= 2 && string_is_integer(vec[1]))
		{
			int64_t offset = string_to_integer(vec[1]);

			// FIXME! Check if this is correct
			if ( (offset & (1 << 31)) )
				offset |= 0xffffffff00000000ULL;

			branchTarget = pc + offset;
		}

		return Instruction(branchTarget);
	}

	Section *lookupSection(uint64_t address)
	{
		// Iterator to the lowest-addressed section that starts at or above
		// `address`
		SectionCache_t::iterator it = m_cache.lower_bound(address);

		// Rare case, the searched for address might be exactly the base address of
		// this section
		if (it != m_cache.end() && it->first == address) {
			return it->second;
		}

		// Go back one section (this might be too far, but we'll check later)
		--it;

		// If there was no previous section, the address isn't in a section (it's
		// before the lowest section's baseAddress)
		if (it == m_cache.end())
			return NULL;

		// We've found the highest-starting section that's lower than the target
		// address, check it actually contains the address
		Section *cur = it->second;
		uint64_t end = cur->getBase() + cur->getSize() - 1;

		// Address is below the section's baseAddress (impossible from the above
		// logic)
		if (address < cur->getBase())
			throw std::logic_error("Section search invariant broken");

		// Address is above section's end
		if (address > end)
			return NULL;

		// Just right!
		return cur;
	}

	void setupBasicBlocks()
	{
		for (SectionCache_t::iterator it = m_cache.begin();
				it != m_cache.end();
				++it)
			it->second->disassemble(*this, m_info, m_disassembler);

		InstructionOrderedMap_t::iterator cur = m_orderedInstructions.begin();

		// The no-instructions program. Uncommon.
		if (cur == m_orderedInstructions.end())
			return;

		InstructionOrderedMap_t::iterator next = cur;
		next++;

		// The first instruction is always a leader
		cur->second->makeLeader();

		// The one-instructions program. Also uncommon.
		if (next == m_orderedInstructions.end())
			return;

		// Iterate the instruction pairs
		for (; next != m_orderedInstructions.end(); ++cur, ++next)
		{
			// Mark branch targets as leaders, as well as the instruction after that
			if (cur->second->isBranch())
			{
				m_instructions[cur->second->getBranchTarget()].makeLeader();
				next->second->makeLeader();
			}
		}

		// Create and populate basic blocks
		BasicBlock *bb = NULL;

		m_bbs.push_back(bb);
		for (InstructionOrderedMap_t::iterator it = m_orderedInstructions.begin();
				it != m_orderedInstructions.end();
				++it)
		{
			Instruction *cur = it->second;

			if (cur->isLeader())
			{
				bb = new BasicBlock();
				m_bbs.push_back(bb);
			}

			cur->setBasicBlock(bb);
			bb->addInstructionAddress(it->first);
		}
	}

	Instruction *getInstruction(uint64_t address)
	{
		if (m_instructions.find(address) == m_instructions.end())
			return NULL;

		return &m_instructions[address];
	}

	void opcodesFprintFunc(const char *str)
	{
		std::string stdStr(str);

		m_instructionVector.push_back(trim_string(stdStr));
	}

	static int opcodesFprintFuncStatic(void *info, const char *fmt, ...)
	{
		BfdDisassembler *pThis = (BfdDisassembler *)info;
		char str[64];
		int out;

		va_list args;
		va_start (args, fmt);
		out = vsnprintf( str, sizeof(str) - 1, fmt, args );
		va_end (args);

		pThis->opcodesFprintFunc(str);

		return out;
	}

#if KCOV_LIBFD_DISASM_STYLED
	static int opcodesFprintStyledFuncStatic(void *info, enum disassembler_style style, const char *fmt, ...)
	{
		(void)style;
		BfdDisassembler *pThis = (BfdDisassembler *)info;
		char str[64];
		int out;

		va_list args;
		va_start (args, fmt);
		out = vsnprintf( str, sizeof(str) - 1, fmt, args );
		va_end (args);

		pThis->opcodesFprintFunc(str);

		return out;
	}
#endif

	typedef std::map<uint64_t, Section *> SectionCache_t;
	typedef std::unordered_map<uint64_t, Instruction> InstructionAddressMap_t;
	typedef std::map<uint64_t, Instruction *> InstructionOrderedMap_t;

	struct disassemble_info m_info;
	disassembler_ftype m_disassembler;

	std::vector<std::string> m_instructionVector;

	SectionCache_t m_cache;
	InstructionAddressMap_t m_instructions;
	InstructionOrderedMap_t m_orderedInstructions;

	std::vector<BasicBlock *> m_bbs;
	std::vector<uint64_t> m_empty;
};

IDisassembler &IDisassembler::getInstance()
{
	static BfdDisassembler *g_p;

	if (!g_p)
	{
		bfd_init();
		g_p = new BfdDisassembler();
	}

	return *g_p;
}
