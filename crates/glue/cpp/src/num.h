#pragma once

#include "internal.h"

namespace Roc
{
    template <typename T>
    class Num : public Value
    {
    public:
        T value;
        Num() : value(0) {}
        Num(T value) : value(value) {}

        bool operator== (const Num<T> &other) const
        {
            return value == other.value;
        }
    };

    typedef Num<int64_t> I64;
    typedef Num<int32_t> I32;
    typedef Num<int16_t> I16;
    typedef Num<int8_t> I8;
    typedef Num<uint64_t> U64;
    typedef Num<uint32_t> U32;
    typedef Num<uint16_t> U16;
    typedef Num<uint8_t> U8;
    typedef Num<float> F32;
    typedef Num<double> F64;
};
