#pragma once

#include "internal.h"
#include "list.h"

namespace Roc
{

    template <typename K, typename V>
    struct DictItemKeyFirst
    {
        K key;
        V value;
    };

    template <typename K, typename V>
    struct DictItemValueFirst
    {
        V value;
        K key;
    };

    template <typename K, typename V>
    const bool DICT_KEY_FIRST = alignof(K) >= alignof(V);

    /*
     * Roc is constructing these values according to its memory layout rules.
     * Specifically:
     *
     * 1. fields with the highest alignment go first
     * 2. then fields are sorted alphabetically
     *
     * Taken together, these mean that if we have a value with higher alignment
     * than the key, it'll be first in memory. Otherwise, the key will be first.
     * Fortunately, the total amount of memory doesn't change, so we can use a
     * union and disambiguate by examining the alignment of the key and value.
     *
     * However, note that this only makes sense while we're storing KV pairs
     * contiguously in memory. If we separate them at some point, we'll need to
     * change this implementation drastically!
     */
    template <typename K, typename V>
    union DictItem
    {
        DictItemKeyFirst<K, V> key_first;
        DictItemValueFirst<K, V> value_first;

        DictItem(K key, V value)
        {
            if (DICT_KEY_FIRST<K, V>)
            {
                key_first = {
                    .key = key,
                    .value = value,
                };
            }
            else
            {
                value_first = {
                    .value = value,
                    .key = key,
                };
            }
        }

        ~DictItem()
        {
            if (DICT_KEY_FIRST<K, V>)
            {
                key_first.key.~K();
                key_first.value.~V();
            }
            else
            {
                value_first.value.~V();
                value_first.key.~K();
            }
        }

        K *key()
        {
            return DICT_KEY_FIRST<K, V>
                       ? &key_first.key
                       : &value_first.key;
        }

        V *value()
        {
            return DICT_KEY_FIRST<K, V>
                       ? &key_first.value
                       : &value_first.value;
        }
    };

    /*
     * At the moment, Roc's Dict is just an association list. Its lookups are O(n) but
     * we haven't grown such big programs that it's a problem yet!
     *
     * We use a union for [`DictItem`] instead of just a struct. See the
     * comment on that data structure for why.
     */
    template <typename K, typename V>
    class Dict
    {
        List<DictItem<K, V>> list;

    public:
        Dict(size_t capacity = 0) : list(NULL, 0, capacity) {}

        V *get(const K *key)
        {
            DictItem<K, V> *items = list.elements();
            for (size_t i = 0; i < list.length(); ++i)
            {
                DictItem<K, V> *item = items + i;
                K *k = item->key();
                if (*k == *key)
                    return item->value();
            }
            return NULL;
        }

        void insert(K key, V value)
        {
            DictItem<K, V> item = DictItem<K, V>(key, value);
            V *existing_value = get(&key);
            if (existing_value)
                *existing_value = value;
            else
                list.push(item);
        }

        size_t length() const
        {
            return list.length();
        }

        size_t capacity() const
        {
            return list.capacity();
        }
    };
};