#pragma once

#include <stdint.h>

namespace kcov
{
	class GeneratedData
	{
	public:
		GeneratedData(const uint8_t *p, size_t size) :
			m_data(p), m_size(size)
		{
		}

		const uint8_t *data() const
		{
			return m_data;
		}

		const size_t size() const
		{
			return m_size;
		}

	private:
		const uint8_t *m_data;
		size_t m_size;
	};
}
