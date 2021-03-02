#include "h3_nif_memory.h"
#include <erl_nif.h>
#include <stdatomic.h>

#ifdef H3_NIF_USE_BEAM_ALLOC
#include <string.h>
#else
#include <stdlib.h>
#endif

// A monotonic count of bytes allocated outside this module by libh3.
atomic_uint_least64_t libh3_mem_allocated = 0;
// Number of times memory was allocated outside this module by libh3.
atomic_uint libh3_mem_allocations = 0;
// A monotonic count of bytes freed outside this module by libh3.
atomic_uint_least64_t libh3_mem_freed = 0;
// Number of times memory was freed outside this module by libh3.
atomic_uint libh3_mem_frees = 0;
// Number of times memory was reallocated outside this module by libh3.
atomic_uint libh3_mem_reallocations = 0;
// A monotonic count of bytes allocated by this module.
atomic_uint_least64_t h3_nif_mem_allocated = 0;
// Number of times memory was allocated.
atomic_uint h3_nif_mem_allocations = 0;
// A monotonic count of bytes freed by this module.
atomic_uint_least64_t h3_nif_mem_freed = 0;
// Number of times memory was freed.
atomic_uint h3_nif_mem_frees = 0;

ERL_NIF_TERM
erl_meminfo(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM keys[9] = {
        enif_make_atom(env, "libh3_bytes_allocated"),
        enif_make_atom(env, "libh3_allocations"),
        enif_make_atom(env, "libh3_bytes_freed"),
        enif_make_atom(env, "libh3_frees"),
        enif_make_atom(env, "libh3_reallocations"),
        enif_make_atom(env, "h3_nif_bytes_allocated"),
        enif_make_atom(env, "h3_nif_allocations"),
        enif_make_atom(env, "h3_nif_bytes_freed"),
        enif_make_atom(env, "h3_nif_frees"),
    };

    ERL_NIF_TERM values[9] = {
        enif_make_uint(env,
                       atomic_load_explicit(&libh3_mem_allocated,
                                            memory_order_relaxed)),
        enif_make_uint64(env,
                         atomic_load_explicit(&libh3_mem_allocations,
                                              memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&libh3_mem_freed,
                                            memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&libh3_mem_frees,
                                            memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&libh3_mem_reallocations,
                                            memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&h3_nif_mem_allocated,
                                            memory_order_relaxed)),
        enif_make_uint64(env,
                         atomic_load_explicit(&h3_nif_mem_allocations,
                                              memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&h3_nif_mem_freed,
                                            memory_order_relaxed)),
        enif_make_uint(env,
                       atomic_load_explicit(&h3_nif_mem_frees,
                                            memory_order_relaxed)),
    };

    ERL_NIF_TERM meminfo;
    enif_make_map_from_arrays(env, keys, values, 9, &meminfo);
    return meminfo;
}


void *
h3_nif_calloc(size_t count, size_t size)
{
    void * ptr;
#ifdef H3_NIF_USE_BEAM_ALLOC
    ptr = 1 + (size_t *)enif_alloc(sizeof(size_t) + (count * size));
    if (ptr)
    {
        size_t * size_prefix = ((size_t *)ptr - 1);
        *size_prefix         = sizeof(size_t) + count * size;
        memset(ptr, 0, count * size);
        atomic_fetch_add_explicit(&h3_nif_mem_allocated,
                                  sizeof(size_t) + (count * size),
                                  memory_order_relaxed);
        atomic_fetch_add_explicit(&h3_nif_mem_allocations, 1, memory_order_relaxed);
    }
#else
    ptr = calloc(count, size);
#endif
    return ptr;
}

void
h3_nif_free(void * ptr)
{
#ifdef H3_NIF_USE_BEAM_ALLOC
    size_t size = *((size_t *)ptr - 1);
    enif_free((size_t *)ptr - 1);
    atomic_fetch_add_explicit(&h3_nif_mem_frees, 1, memory_order_relaxed);
    atomic_fetch_add_explicit(&h3_nif_mem_freed, size, memory_order_relaxed);
#else
    free(ptr);
#endif
}

__attribute__((used)) void *
libh3_malloc(size_t size)
{
    void * ptr;
#ifdef H3_NIF_USE_BEAM_ALLOC
    ptr = 1 + (size_t *)enif_alloc(sizeof(size_t) + size);
    if (ptr)
    {
        size_t * size_prefix = ((size_t *)ptr - 1);
        *size_prefix         = sizeof(size_t) + size;
        atomic_fetch_add_explicit(&libh3_mem_allocated,
                                  sizeof(size_t) + size,
                                  memory_order_relaxed);
        atomic_fetch_add_explicit(&libh3_mem_allocations, 1, memory_order_relaxed);
    }
#else
    ptr     = malloc(size);
#endif
    return ptr;
}

__attribute__((used)) void *
libh3_calloc(size_t count, size_t size)
{
    void * ptr;
#ifdef H3_NIF_USE_BEAM_ALLOC
    ptr = 1 + (size_t *)enif_alloc(sizeof(size_t) + (count * size));
    if (ptr)
    {
        size_t * size_prefix = ((size_t *)ptr - 1);
        *size_prefix         = sizeof(size_t) + count * size;
        memset(ptr, 0, count * size);
        atomic_fetch_add_explicit(&libh3_mem_allocated,
                                  sizeof(size_t) + (count * size),
                                  memory_order_relaxed);
        atomic_fetch_add_explicit(&libh3_mem_allocations, 1, memory_order_relaxed);
    }
#else
    ptr     = calloc(count, size);
#endif
    return ptr;
}

__attribute__((used)) void *
libh3_realloc(void * curr_ptr, size_t new_size)
{
    void * new_ptr;
#ifdef H3_NIF_USE_BEAM_ALLOC
    atomic_fetch_add_explicit(&libh3_mem_reallocations, 1, memory_order_relaxed);

    size_t curr_size = *((size_t *)curr_ptr - 1);
    if ((curr_size - sizeof(size_t)) == new_size)
    {
        return curr_ptr;
    }

    new_ptr = 1
              + (size_t *)enif_realloc((size_t *)curr_ptr - 1,
                                       sizeof(size_t) + new_size);
    size_t * size_prefix = ((size_t *)new_ptr - 1);
    *size_prefix         = sizeof(size_t) + new_size;

    if (new_size > curr_size)
    {
        atomic_fetch_add_explicit(&libh3_mem_allocated,
                                  (new_size + sizeof(size_t)) - curr_size,
                                  memory_order_relaxed);
    }
    else
    {
        atomic_fetch_add_explicit(&libh3_mem_freed,
                                  curr_size - (new_size + sizeof(size_t)),
                                  memory_order_relaxed);
    }
#else
    new_ptr = realloc(curr_ptr, new_size);
#endif
    return new_ptr;
}

__attribute__((used)) void
libh3_free(void * ptr)
{
#ifdef H3_NIF_USE_BEAM_ALLOC
    size_t size = *((size_t *)ptr - 1);
    enif_free((size_t *)ptr - 1);
    atomic_fetch_add_explicit(&libh3_mem_frees, 1, memory_order_relaxed);
    atomic_fetch_add_explicit(&libh3_mem_freed, size, memory_order_relaxed);
#else
    free(ptr);
#endif
}
