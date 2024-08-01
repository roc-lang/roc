#pragma once

#include "internal.h"

namespace Roc
{
    template <typename T>
    class Box : Value
    {
        T *contents;

        static size_t alloc_alignment()
        {
            return (alignof(T) > alignof(size_t)) ? alignof(T) : alignof(size_t);
        }

        char *allocation() const
        {
            return contents
                       ? (char *)contents - Box<T>::alloc_alignment()
                       : NULL;
        }

        size_t allocation_size() const
        {
            return Box<T>::alloc_alignment() + sizeof(T);
        }

        ptrdiff_t *refcount_ptr() const
        {
            return (ptrdiff_t *)contents - 1;
        }

    public:
        // Note: Constructor doesn't increment the refcount of the value
        // User is responsible for that
        Box(const T &val)
        {
            char *allocation = (char *)roc_alloc(allocation_size(), Box<T>::alloc_alignment());

            ptrdiff_t *refcount = (ptrdiff_t *)allocation;
            *refcount = REFCOUNT_ONE;

            contents = (T *)(allocation + Box<T>::alloc_alignment());
            *contents = val;
        }

        ~Box()
        {
            Value *v = (Value *)contents;
            v->rc_decrement();
            roc_dealloc(allocation(), Box<T>::alloc_alignment());
        }

        T &operator*() const
        {
            return *contents;
        }

        T *operator->() const
        {
            return contents;
        }

        bool rc_unique() const
        {
            return *refcount_ptr() == REFCOUNT_ONE;
        };

        void rc_increment()
        {
            Value *v = (Value *)contents;
            v->rc_increment();
            *refcount_ptr() += 1;
        };

        void rc_decrement()
        {
            if (rc_unique())
            {
                this->~Box();
            }
            else
            {
                Value *v = (Value *)contents;
                v->rc_decrement();
                *refcount_ptr() -= 1;
            }
        };
    };
};