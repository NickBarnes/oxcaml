/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MINOR_GC_H
#define CAML_MINOR_GC_H

#include "address_class.h"
#include "misc.h"
#include "config.h"
#include "weak.h"

#define caml_young_end Caml_state->young_end
#define caml_young_ptr Caml_state->young_ptr
#define caml_young_start Caml_state->young_start
#define caml_young_limit Caml_state->young_limit
#define caml_young_alloc_start Caml_state->young_start
#define caml_young_alloc_end Caml_state->young_end
#define caml_minor_heap_wsz Caml_state->minor_heap_wsz
#define caml_in_minor_collection Caml_state->in_minor_collection


#define CAML_TABLE_STRUCT(t) { \
  t *base;                     \
  t *end;                      \
  t *threshold;                \
  t *ptr;                      \
  t *limit;                    \
  asize_t size;                \
  asize_t reserve;             \
}

/* Count of the total number of minor collections performed by the program */
CAMLextern atomic_uintnat caml_minor_collections_count;

/* The epoch number for major slice. Used to trigger major slices.
   Always, [caml_major_slice_epoch <= caml_minor_collections_count] */
CAMLextern atomic_uintnat caml_major_slice_epoch;

struct caml_ref_table CAML_TABLE_STRUCT(value *);

struct caml_ephe_ref_elt {
  value ephe;      /* an ephemeron in major heap */
  mlsize_t offset; /* the offset that points in the minor heap  */
  value stash;     /* copy of the field if it requires minor-GC cleaning */
};
struct caml_ephe_ref_table CAML_TABLE_STRUCT(struct caml_ephe_ref_elt);

struct caml_custom_elt {
  value block;     /* The finalized block in the minor heap. */
};
struct caml_custom_table CAML_TABLE_STRUCT(struct caml_custom_elt);

struct caml_dependent_elt {
  value block;     /* The finalized block in the minor heap. */
  mlsize_t mem;    /* The size in bytes of the dependent memory. */
};
struct caml_dependent_table CAML_TABLE_STRUCT(struct caml_dependent_elt);

struct caml_minor_tables {
  struct caml_ref_table major_ref;
  struct caml_ephe_ref_table ephe_ref;
  struct caml_custom_table custom;
  struct caml_dependent_table dependent;
};

/* Setting this to a different value can be useful for debugging
 * although may cause some slowdown. It must be a header value not
 * otherwise found on the minor heap, not using Infix_tag, and not
 * equal to In_progress_hd.  I suggest Make_header(0 ,0, 0x200). */
#define Promoted_hd (Make_header(0, 0, 0))
#define Is_promoted_hd(hd) ((hd) == Promoted_hd)

CAMLextern void caml_minor_collection (void);

#ifdef CAML_INTERNALS
extern void caml_set_minor_heap_size (asize_t); /* size in bytes */
extern void caml_empty_minor_heap_no_major_slice_from_stw
  (caml_domain_state* domain, void* unused, int participating_count,
    caml_domain_state** participating); /* in STW */
extern int caml_try_empty_minor_heap_on_all_domains(void); /* out STW */
extern void caml_empty_minor_heaps_once(void); /* out STW */
void caml_alloc_small_dispatch (caml_domain_state* domain,
                                intnat wosize, int flags,
                                int nallocs, unsigned char* encoded_alloc_lens);
void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv);
extern void caml_realloc_ref_table (struct caml_ref_table *);
extern void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *);
extern void caml_realloc_custom_table (struct caml_custom_table *);
extern void caml_realloc_dependent_table (struct caml_dependent_table *);
struct caml_minor_tables* caml_alloc_minor_tables(void);
void caml_free_minor_tables(struct caml_minor_tables*);
void caml_empty_minor_heap_setup(caml_domain_state* domain, void*);

#ifdef DEBUG
extern int caml_debug_is_minor(value val);
extern int caml_debug_is_major(value val);
#endif

#define Ref_table_add(ref_table, x) do {                                \
    struct caml_ref_table* ref = (ref_table);                           \
    if (ref->ptr >= ref->limit) {                                       \
      CAMLassert (ref->ptr == ref->limit);                              \
      caml_realloc_ref_table (ref);                                     \
    }                                                                   \
    *ref->ptr++ = (value*)(x);                                          \
  } while (0)

Caml_inline void add_to_ephe_ref_table (struct caml_ephe_ref_table *tbl,
                                        value ar, mlsize_t offset)
{
  struct caml_ephe_ref_elt *ephe_ref;
  if (tbl->ptr >= tbl->limit){
    CAMLassert (tbl->ptr == tbl->limit);
    caml_realloc_ephe_ref_table (tbl);
  }
  ephe_ref = tbl->ptr++;
  ephe_ref->ephe = ar;
  ephe_ref->offset = offset;
  ephe_ref->stash = caml_ephe_none;
  CAMLassert(ephe_ref->offset < Wosize_val(ephe_ref->ephe));
}

Caml_inline void add_to_custom_table (struct caml_custom_table *tbl, value v)
{
  struct caml_custom_elt *elt;
  if (tbl->ptr >= tbl->limit){
    CAMLassert (tbl->ptr == tbl->limit);
    caml_realloc_custom_table (tbl);
  }
  elt = tbl->ptr++;
  elt->block = v;
}

Caml_inline void add_to_dependent_table (struct caml_dependent_table *tbl,
                                         value v,
                                         mlsize_t mem)
{
  struct caml_dependent_elt *elt;
  if (tbl->ptr >= tbl->limit){
    CAMLassert (tbl->ptr == tbl->limit);
    caml_realloc_dependent_table (tbl);
  }
  elt = tbl->ptr++;
  elt->block = v;
  elt->mem = mem;
}

Caml_inline uintnat caml_minor_words_allocated(void)
{
  return (Caml_state->stat_minor_words
          + (Wsize_bsize((uintnat)Caml_state->young_end -
                         (uintnat)Caml_state->young_ptr)));
}

#endif /* CAML_INTERNALS */

#endif /* CAML_MINOR_GC_H */
