/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1700
/* Microsoft introduced a regression in Visual Studio 2005 (technically it's
   not present in the Windows Server 2003 SDK which has a pre-release version)
   and the abort function ceased to be declared __declspec(noreturn). This was
   fixed in Visual Studio 2012. Trick stdlib.h into not defining abort (this
   means exit and _exit are not defined either, but they aren't required). */
#define _CRT_TERMINATE_DEFINED
__declspec(noreturn) void __cdecl abort(void);
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/version.h"

caml_timing_hook caml_major_slice_begin_hook = NULL;
caml_timing_hook caml_major_slice_end_hook = NULL;
caml_timing_hook caml_minor_gc_begin_hook = NULL;
caml_timing_hook caml_minor_gc_end_hook = NULL;
caml_timing_hook caml_finalise_begin_hook = NULL;
caml_timing_hook caml_finalise_end_hook = NULL;

#ifdef DEBUG

void caml_failed_assert (char * expr, char_os * file_os, int line)
{
  char* file = caml_stat_strdup_of_os(file_os);
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  caml_stat_free(file);
  abort();
}

void caml_set_fields (value v, uintnat start, uintnat filler)
{
  mlsize_t i;
  /* We use Wosize_val instead of Scannable_wosize_val because it's fine to set
     even unscannable fields. */
  for (i = start; i < Wosize_val (v); i++){
    Field (v, i) = (value) filler;
  }
}

#endif /* DEBUG */

uintnat caml_verb_gc = 0;

void caml_gc_message (int level, char *msg, ...)
{
  if ((caml_verb_gc & level) != 0){
    va_list ap;
    va_start(ap, msg);
    if (caml_verb_gc & 0x1000) {
      caml_print_timestamp(stderr, caml_verb_gc & 0x2000);
    }
    vfprintf (stderr, msg, ap);
    va_end(ap);
    fflush (stderr);
  }
}

void (*caml_fatal_error_hook) (char *msg, va_list args) = NULL;

CAMLexport void caml_fatal_error (char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if(caml_fatal_error_hook != NULL) {
    caml_fatal_error_hook(msg, ap);
  } else {
    fprintf (stderr, "Fatal error: ");
    vfprintf (stderr, msg, ap);
    fprintf (stderr, "\n");
  }
  va_end(ap);
  abort();
}

void caml_fatal_out_of_memory(void)
{
  caml_fatal_error("Out of memory");
}

void caml_ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = caml_stat_alloc(sizeof(void *) * init_capa);
}

int caml_ext_table_add(struct ext_table * tbl, caml_stat_block data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      caml_stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void caml_ext_table_remove(struct ext_table * tbl, caml_stat_block data)
{
  int i;
  for (i = 0; i < tbl->size; i++) {
    if (tbl->contents[i] == data) {
      caml_stat_free(tbl->contents[i]);
      memmove(&tbl->contents[i], &tbl->contents[i + 1],
              (tbl->size - i - 1) * sizeof(void *));
      tbl->size--;
    }
  }
}

void caml_ext_table_clear(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries) {
    for (i = 0; i < tbl->size; i++) caml_stat_free(tbl->contents[i]);
  }
  tbl->size = 0;
}

void caml_ext_table_free(struct ext_table * tbl, int free_entries)
{
  caml_ext_table_clear(tbl, free_entries);
  caml_stat_free(tbl->contents);
}

/* Integer arithmetic with overflow detection */

#if ! (__GNUC__ >= 5 || Caml_has_builtin(__builtin_mul_overflow))
CAMLexport int caml_umul_overflow(uintnat a, uintnat b, uintnat * res)
{
#define HALF_SIZE (sizeof(uintnat) * 4)
#define HALF_MASK (((uintnat)1 << HALF_SIZE) - 1)
#define LOW_HALF(x) ((x) & HALF_MASK)
#define HIGH_HALF(x) ((x) >> HALF_SIZE)
  /* Cut in half words */
  uintnat al = LOW_HALF(a);
  uintnat ah = HIGH_HALF(a);
  uintnat bl = LOW_HALF(b);
  uintnat bh = HIGH_HALF(b);
  /* Exact product is:
              al * bl
           +  ah * bl  << HALF_SIZE
           +  al * bh  << HALF_SIZE
           +  ah * bh  << 2*HALF_SIZE
     Overflow occurs if:
        ah * bh is not 0, i.e. ah != 0 and bh != 0
     OR ah * bl has high half != 0
     OR al * bh has high half != 0
     OR the sum al * bl + LOW_HALF(ah * bl) << HALF_SIZE
                        + LOW_HALF(al * bh) << HALF_SIZE overflows.
     This sum is equal to p = (a * b) modulo word size. */
  uintnat p = a * b;
  uintnat p1 = al * bh;
  uintnat p2 = ah * bl;
  *res = p;
  if (ah == 0 && bh == 0) return 0;
  if (ah != 0 && bh != 0) return 1;
  if (HIGH_HALF(p1) != 0 || HIGH_HALF(p2) != 0) return 1;
  p1 <<= HALF_SIZE;
  p2 <<= HALF_SIZE;
  p1 += p2;
  if (p < p1 || p1 < p2) return 1; /* overflow in sums */
  return 0;
#undef HALF_SIZE
#undef HALF_MASK
#undef LOW_HALF
#undef HIGH_HALF
}
#endif

/* Runtime warnings */

uintnat caml_runtime_warnings = 0;
static int caml_runtime_warnings_first = 1;

int caml_runtime_warnings_active(void)
{
  if (!caml_runtime_warnings) return 0;
  if (caml_runtime_warnings_first) {
    fprintf(stderr, "[ocaml] (use Sys.enable_runtime_warnings to control "
                    "these warnings)\n");
    caml_runtime_warnings_first = 0;
  }
  return 1;
}

/* Flambda 2 invalid term markers */

CAMLnoreturn_start
void caml_flambda2_invalid (value message)
CAMLnoreturn_end;

void caml_flambda2_invalid (value message)
{
  fprintf (stderr, "[ocaml] [flambda2] Invalid code:\n%s\n\n",
    String_val(message));
  fprintf (stderr,
    "This might have arisen from a wrong use of [%%identity],\n");
  fprintf (stderr,
    "for example confusing arrays and records, or non-mixed and\n");
  fprintf (stderr,
    "mixed blocks.\n");
  fprintf (stderr,
    "Consider using [Obj.magic], [Obj.repr] and/or [Obj.obj].\n");
  abort ();
}

/* Fake atomic operations - runtime4 is single threaded, but we need to
   provide these symbols for compatibility with the 5 Stdlib updates. */

CAMLprim value caml_atomic_make(value v)
{
  CAMLparam1(v);
  value ref = caml_alloc_small(1, 0);
  Field(ref, 0) = v;
  CAMLreturn(ref);
}


CAMLprim value caml_atomic_load_field(value ref, value vfield)
{
  return Field(ref, Long_val(vfield));
}


CAMLprim value caml_atomic_load(value ref)
{
  return caml_atomic_load_field(ref, Val_long(0));
}


CAMLprim value caml_atomic_set_field(value ref, value vfield, value v)
{
  caml_modify_local(ref, Long_val(vfield), v);
  return Val_unit;
}

CAMLprim value caml_atomic_set(value ref, value v)
{
  return caml_atomic_set_field(ref, Val_long(0), v);
}

CAMLprim value caml_atomic_compare_exchange_field(value ref, value vfield, value oldv, value newv)
{
  intnat field = Long_val(vfield);
  value p = Field(ref, field);
  if (p == oldv) {
    caml_modify_local(ref, field, newv);
    return oldv;
  } else {
    return p;
  }
}

CAMLprim value caml_atomic_compare_exchange(value ref, value oldv, value newv) {
  return caml_atomic_compare_exchange_field(ref, Val_long(0), oldv, newv);
}

CAMLprim value caml_atomic_cas_field(value ref, value vfield, value oldv, value newv)
{
  if (caml_atomic_compare_exchange_field(ref, vfield, oldv, newv) == oldv) {
    return Val_true;
  } else {
    return Val_false;
  }
}

CAMLprim value caml_atomic_cas(value ref, value oldv, value newv) {
  return caml_atomic_cas_field(ref, Val_long(0), oldv, newv);
}

CAMLprim value caml_atomic_exchange_field(value ref, value vfield, value v)
{
  value ret = Field(ref, Long_val(vfield));
  caml_modify_local(ref, Long_val(vfield), v);
  return ret;
}

CAMLprim value caml_atomic_exchange(value ref, value v) {
  return caml_atomic_exchange_field(ref, Val_long(0), v);
}

CAMLprim value caml_atomic_fetch_add_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value ret;
  value* p = &Op_val(ref)[field];
  ret = *p;
  CAMLassert(Is_long(ret));
  *p = Val_long(Long_val(ret) + Long_val(incr));
  return ret;
}

CAMLprim value caml_atomic_fetch_add(value ref, value incr) {
  return caml_atomic_fetch_add_field(ref, Val_long(0), incr);
}

CAMLprim value caml_atomic_add_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value* p = &Op_val(ref)[field];
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) + Long_val(incr));
  return Val_unit;
}

CAMLprim value caml_atomic_add(value ref, value incr) {
  return caml_atomic_add_field(ref, Val_long(0), incr);
}

CAMLprim value caml_atomic_sub_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value* p = &Op_val(ref)[field];
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) - Long_val(incr));
  return Val_unit;
}

CAMLprim value caml_atomic_sub(value ref, value incr) {
  return caml_atomic_sub_field(ref, Val_long(0), incr);
}

CAMLprim value caml_atomic_land_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value* p = &Op_val(ref)[field];
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) & Long_val(incr));
  return Val_unit;
}

CAMLprim value caml_atomic_land(value ref, value incr) {
  return caml_atomic_land_field(ref, Val_long(0), incr);
}

CAMLprim value caml_atomic_lor_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value* p = &Op_val(ref)[field];
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) | Long_val(incr));
  return Val_unit;
}

CAMLprim value caml_atomic_lor(value ref, value incr) {
  return caml_atomic_lor_field(ref, Val_long(0), incr);
}

CAMLprim value caml_atomic_lxor_field(value ref, value vfield, value incr)
{
  intnat field = Long_val(vfield);
  value* p = &Op_val(ref)[field];
  CAMLassert(Is_long(*p));
  *p = Val_long(Long_val(*p) ^ Long_val(incr));
  return Val_unit;
}

CAMLprim value caml_atomic_lxor(value ref, value incr) {
  return caml_atomic_lxor_field(ref, Val_long(0), incr);
}

// Dummy implementations so effect.ml can compile

CAMLprim value caml_continuation_use_noexc(void)
{
  caml_fatal_error("Effects not implemented in runtime4");
}

CAMLprim value caml_alloc_stack(void)
{
  caml_fatal_error("Effects not implemented in runtime4");
}

CAMLprim value caml_get_continuation_callstack(void)
{
  caml_fatal_error("Effects not implemented in runtime4");
}

CAMLprim value caml_continuation_use_and_update_handler_noexc(void)
{
  caml_fatal_error("Effects not implemented in runtime4");
}

CAMLprim value caml_domain_dls_compare_and_set(void)
{
  caml_fatal_error("Domains not implemented in runtime4");
}

CAMLprim value caml_no_bytecode_impl(void)
{
  caml_failwith("No bytecode implementation provided for this external");
}
