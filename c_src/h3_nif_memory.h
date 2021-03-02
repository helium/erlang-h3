#ifndef H3_NIF_MEMORY_9787A6AA
#define H3_NIF_MEMORY_9787A6AA

#include <erl_nif.h>
#include <stddef.h>

ERL_NIF_TERM
erl_meminfo(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

// Wraps `calloc` when ASan is enabled, otherwise wraps `enif_alloc` +
// `memset`.
void *
h3_nif_calloc(size_t count, size_t size);

// Wraps `free` when ASan is enabled, otherwise wraps `enif_free`.
void
h3_nif_free(void * ptr);

#endif /* H3_NIF_MEMORY_9787A6AA */
