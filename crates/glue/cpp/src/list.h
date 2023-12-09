#pragma once

#include "internal.h"

namespace Roc
{
    template <typename T>
    class List : Value
    {
        T *m_elements;
        size_t m_length;
        size_t m_capacity;

        static size_t alloc_alignment()
        {
            return (alignof(T) > alignof(size_t)) ? alignof(T) : alignof(size_t);
        }

        char *allocation() const
        {
            return m_elements
                       ? (char *)m_elements - List<T>::alloc_alignment()
                       : NULL;
        }

        size_t allocation_size() const
        {
            return (m_capacity * sizeof(T)) + List<T>::alloc_alignment();
        }

        ptrdiff_t *refcount_ptr() const
        {
            return (ptrdiff_t *)m_elements - 1;
        }

        void inline create_new_allocation(size_t cap)
        {
            size_t alignment = List<T>::alloc_alignment();
            size_t rc_space = alignment;
            size_t rc_offset = rc_space - sizeof(size_t);
            size_t elems_offset = rc_space;
            size_t alloc_size = rc_space + cap * sizeof(T);
            char *alloc = (char *)roc_alloc(alloc_size, (uint32_t)alignment);
            ptrdiff_t *refcount = (ptrdiff_t *)(alloc + rc_offset);
            *refcount = REFCOUNT_ONE;

            m_elements = (T *)(alloc + elems_offset);
            m_capacity = cap;
        }

    public:
        List(const T *elems = NULL, size_t len = 0, size_t cap = 0)
        {
            if (elems == NULL)
                len = 0;
            if (len > cap)
                cap = len;

            if (cap == 0)
            {
                m_elements = NULL;
            }
            else
            {
                create_new_allocation(cap);
                if (elems != NULL)
                {
                    memcpy(m_elements, elems, len * sizeof(T));
                }
            }

            m_length = len;
            m_capacity = cap;
        }

        ~List()
        {
            if (m_elements != NULL)
            {
                for (size_t i = 0; i < m_length; i++)
                {
                    m_elements[i].~T();
                }
                roc_dealloc(allocation(), (uint32_t)List<T>::alloc_alignment());
            }
        }

        size_t length() const
        {
            return m_length;
        }

        size_t capacity() const
        {
            return m_capacity;
        }

        T *elements() const
        {
            return m_elements;
        }

        T *get(size_t index) const
        {
            if (index >= m_length)
                return NULL;
            return &m_elements[index];
        }

        void set(size_t index, const T &val)
        {
            if (index >= m_length)
            {
                roc_panic("Attempted to set List element out of bounds", 0);
            }
            m_elements[index] = val;
        }

        void push(const T &val)
        {
            if (m_length == m_capacity)
            {
                reserve(1);
            }
            m_elements[m_length] = val;
            m_length += 1;
        }

        bool rc_unique() const
        {
            return m_elements == NULL || *refcount_ptr() == REFCOUNT_ONE;
        }

        void rc_increment()
        {
            if (m_elements == NULL)
            {
                roc_panic("Attempted to increment refcount of freed allocation", 0);
                return;
            }
            ptrdiff_t *refcount = refcount_ptr();
            *refcount += 1;
            for (size_t i = 0; i < m_length; i++)
            {
                Value *elem = &m_elements[i];
                elem->rc_increment();
            }
        }

        void rc_decrement()
        {
            if (m_elements == NULL)
            {
                roc_panic("Attempted to decrement refcount of freed allocation", 0);
                return;
            }

            for (size_t i = 0; i < m_length; i++)
            {
                Value *elem = &m_elements[i];
                elem->rc_decrement();
            }

            ptrdiff_t *refcount = refcount_ptr();
            if (*refcount == REFCOUNT_ONE)
            {
                roc_dealloc(allocation(), List<T>::alloc_alignment());
                m_elements = NULL;
                m_length = 0;
                m_capacity = 0;
            }
            else if (*refcount > REFCOUNT_ONE)
            {
                // There are other owners of this allocation, so we need to reduce the refcount.
                // We don't need to do anything else, as we're not removing any elements.
                *refcount -= 1;
            }
        }

        /**
         * Increase capacity by at least the requested number of elements (possibly more).
         */
        void reserve(size_t num_extra_elems)
        {
            if (m_elements == NULL)
            {
                create_new_allocation(num_extra_elems);
            }
            else if (rc_unique())
            {
                char *old_alloc = allocation();
                size_t old_alloc_size = allocation_size();
                char *new_alloc = (char *)roc_realloc(
                    old_alloc,
                    old_alloc_size + num_extra_elems * sizeof(T),
                    old_alloc_size,
                    List<T>::alloc_alignment());
                if (new_alloc == old_alloc)
                {
                    // We've allocated in place!
                    m_capacity += num_extra_elems;
                }
                else
                {
                    // We got back a different allocation; copy the existing elements
                    // into it. We don't need to change their refcounts because we're
                    // just swapping one reference for another.
                    memcpy(new_alloc, old_alloc, old_alloc_size);
                    roc_dealloc(old_alloc, List<T>::alloc_alignment());
                    m_elements = (T *)((char *)new_alloc + List<T>::alloc_alignment());
                    m_capacity += num_extra_elems;
                }
            }
            else
            {
                // This allocation is shared. We need a new, bigger one.

                // Reduce refcount of the old allocation. This List isn't going to use it any more.
                ptrdiff_t *refcount = refcount_ptr();
                *refcount -= 1;

                // Allocate a new one
                T *old_elems = m_elements;
                create_new_allocation(m_capacity + num_extra_elems);

                // Copy the elements into the new allocation.
                // The element refcounts don't change, we're just swapping one ref for another.
                memcpy(m_elements, old_elems, m_length * sizeof(T));
            }
        }
    };

    // Specialization for zero-sized Roc types
    // C++ does not natively support zero-sized types, so we need to handle them specially.
    // The logic for length and capacity is the same as normal, but we never allocate.
    template <>
    class List<void> : Value
    {
        void *m_elements;
        size_t m_length;
        size_t m_capacity;

    public:
        List(void *elems = NULL, size_t len = 0, size_t cap = 0)
            : m_elements(elems), m_length(len), m_capacity(cap)
        {
            if (m_length > m_capacity)
                m_capacity = m_length;
        }
        ~List() {}
        size_t length() const { return m_length; }
        size_t capacity() const { return m_capacity; }
        void *elements() const { return m_elements; }
        void *get(size_t index) const { return NULL; }
        void set(size_t index, const void *val)
        {
            if (index >= m_length)
                roc_panic("Attempted to set List element out of bounds", 0);
        }
        void push(const void *)
        {
            m_length += 1;
            if (m_length > m_capacity)
                m_capacity = m_length;
        }
        bool rc_unique() const { return true; }
        void rc_increment() {}
        void rc_decrement() {}
        void reserve(size_t extra_elems) {
            m_capacity += extra_elems;
        }
    };

};
