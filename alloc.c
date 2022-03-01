#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ezo.h"

static void *xmalloc(size_t size);
static void *xrealloc(void *p, size_t size);

static char *string_allocs = NULL;
static size_t string_cap = 1024;
static size_t string_ind = 0;

static void **pointer_allocs = NULL;
static size_t pointers_cap = 32;
static size_t pointers_ind = 0;

void ezo_alloc_init(void) {
	string_allocs = xmalloc(string_cap);
	pointer_allocs = xmalloc(pointers_cap * sizeof(void *));
}

char *ezo_alloc_str(int size) {
	char *s;
	
	if (string_ind + size >= string_cap) {
		string_cap *= 2;
		string_allocs = xrealloc(string_allocs, string_cap);
	}
	
	s = string_allocs + string_ind;
	string_ind += size;
	return s;
}

void *ezo_alloc_bytes(int bytes) {
	void *p;

	if (pointers_ind >= pointers_cap) {
		pointers_cap *= 2;
		pointer_allocs = xrealloc(pointer_allocs, pointers_cap * sizeof(void *));
	}

	p = xmalloc(bytes);
	pointer_allocs[pointers_ind++] = p;
	return p;
}

void ezo_alloc_free(void) {
	int i;

	free(string_allocs);

	for (i = 0; i < pointers_ind; ++i) {
		free(pointer_allocs[i]);
	}
	free(pointer_allocs);
}

void *xmalloc(size_t size) {
	void *p;

	if (!(p = malloc(size)))
		fatal("malloc: %s\n", strerror(errno));

	return p;
}

void *xrealloc(void *p, size_t size) {
	void *temp = p;

	if ((temp = realloc(p, size)) == NULL)
		fatal("realloc: %s\n", strerror(errno));

	return temp;
}