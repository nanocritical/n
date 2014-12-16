#include "nodes.h"

#include "parser.h"
#include "passes.h"
#include "types.h"
#include "topdeps.h"

#include <stdarg.h>

struct statit_mempool statit_mempool = { 0 };

void print_statit_mempool(void) {
  printf("\nstatit_mempool\n");
  printf("\t%zu	size\n", statit_mempool.size);
  printf("\n");
  for (size_t n = 0; n < ARRAY_SIZE(statit_mempool.size_by_type); ++n) {
    printf("\t%zu	count_by_type.%zu\n", statit_mempool.count_by_type[n], n);
    printf("\t%zu	size_by_type.%zu\n", statit_mempool.size_by_type[n], n);
  }
}

IMPLEMENT_VECTOR(, vecsize, size_t);
IMPLEMENT_VECTOR(, vecstr, char *);

EXAMPLE(data_structure_size_stats) {
  // It is a good idea to keep track of what is responsible for the size of
  // 'union node_as'. In other words, where to look first to shrink 'struct
  // node'.
  assert(sizeof(struct node_deffun) == sizeof(union node_as));

  // If not, we'll need to store step masks differently.
  assert(NODE__NUM <= 64);
}

EXAMPLE(vecnode) {
  struct node a[8] = { 0 };
  struct vecnode v = { 0 };
  assert(0 == vecnode_count(&v));
  for (size_t n = 0; n < 8; ++n) {
    vecnode_push(&v, &a[n]);
    assert(1 + n == vecnode_count(&v));
    for (size_t m = 0; m < vecnode_count(&v); ++m) {
      assert(&a[m] == *vecnode_get(&v, m));
    }
  }
  for (size_t n = 8; n > 3; --n) {
    assert(&a[n - 1] == vecnode_pop(&v));
    assert(n - 1 == vecnode_count(&v));
  }
  assert(3 == vecnode_count(&v));
  vecnode_push(&v, &a[3]);
  assert(4 == vecnode_count(&v));
  assert(&a[3] == vecnode_pop(&v));
  assert(3 == vecnode_count(&v));

  vecnode_destroy(&v);
  assert(0 == vecnode_count(&v));
  for (size_t n = 0; n < 8; ++n) {
    vecnode_push(&v, &a[n]);
    assert(1 + n == vecnode_count(&v));
    for (size_t m = 0; m < vecnode_count(&v); ++m) {
      assert(&a[m] == *vecnode_get(&v, m));
    }
  }
  for (size_t n = 8; n > 0; --n) {
    assert(&a[n - 1] == vecnode_pop(&v));
    assert(n - 1 == vecnode_count(&v));
  }

  vecnode_destroy(&v);
  assert(0 == vecnode_count(&v));

  struct vecnode w = { 0 };
  vecnode_resize(&w, 8);
  for (size_t n = 0; n < 8; ++n) {
    vecnode_push(&v, &a[n]);
  }

  vecnode_copy(&w, &v);
  for (size_t n = 8; n > 0; --n) {
    assert(*vecnode_get(&v, n - 1) == *vecnode_get(&w, n - 1));
    assert(&a[n - 1] == *vecnode_get(&w, n - 1));
  }

  vecnode_destroy(&w);
  vecnode_destroy(&v);
}

IMPLEMENT_VECTOR(, vecancestor, struct ancestor);

#define MEMPOOL_CHUNK (64*1024)

#if CONFIG_MEMPOOL_JUST_MALLOC == 1
noinline__ void *mempool_calloc(struct module *mod, size_t nmemb, size_t size) {
  (void) mod;
  return calloc(nmemb, size);
}
#else
noinline__ void *mempool_calloc(struct module *mod, size_t nmemb, size_t size) {
  STATIT statit_mempool.size += nmemb * size;

  struct mempool *mempool = mod->mempool;

  const size_t sz = nmemb * size;
  assert(likely(sz <= MEMPOOL_CHUNK && sz != 0));

  void *f = mempool->free;
  mempool->free += sz;
  if (likely((uintptr_t)mempool->free <= (uintptr_t)mempool->end)) {
    return f;
  }

  mempool->free -= sz;

  PUSH_STATE(mod->mempool);
  uint8_t *g = calloc(1, MEMPOOL_CHUNK);
  mod->mempool->free = g + sz;
  mod->mempool->end = mod->mempool->free + (MEMPOOL_CHUNK - sz);

  return g;
}
#endif

noinline__ void mempool_free(struct module *mod, void *p) {
  // noop. Function only here so we can start keeping track of where frees
  // should happen.
}

const char *node_which_strings[] = {
  [0] = "(none)",
  [NIL] = "NIL",
  [IDENT] = "IDENT",
  [NUMBER] = "NUMBER",
  [BOOL] = "BOOL",
  [STRING] = "STRING",
  [SIZEOF] = "SIZEOF",
  [ALIGNOF] = "ALIGNOF",
  [BIN] = "BIN",
  [UN] = "UN",
  [TUPLE] = "TUPLE",
  [CALL] = "CALL",
  [CALLNAMEDARG] = "CALLNAMEDARG",
  [INIT] = "INIT",
  [RETURN] = "RETURN",
  [BLOCK] = "BLOCK",
  [LAMBDA] = "LAMBDA",
  [FOR] = "FOR",
  [WHILE] = "WHILE",
  [BREAK] = "BREAK",
  [CONTINUE] = "CONTINUE",
  [NOOP] = "NOOP",
  [IF] = "IF",
  [MATCH] = "MATCH",
  [TRY] = "TRY",
  [CATCH] = "CATCH",
  [EXCEP] = "EXCEP",
  [THROW] = "THROW",
  [JUMP] = "JUMP",
  [PHI] = "PHI",
  [TYPECONSTRAINT] = "TYPECONSTRAINT",
  [DYN] = "DYN",
  [DEFFUN] = "DEFFUN",
  [DEFTYPE] = "DEFTYPE",
  [DEFINCOMPLETE] = "DEFINCOMPLETE",
  [DEFMETHOD] = "DEFMETHOD",
  [DEFINTF] = "DEFINTF",
  [DEFALIAS] = "DEFALIAS",
  [DEFNAME] = "DEFNAME",
  [DEFPATTERN] = "DEFPATTERN",
  [FUNARGS] = "FUNARGS",
  [DEFARG] = "DEFARG",
  [GENARGS] = "GENARGS",
  [DEFGENARG] = "DEFGENARG",
  [SETGENARG] = "SETGENARG",
  [LET] = "LET",
  [DEFFIELD] = "DEFFIELD",
  [DEFCHOICE] = "DEFCHOICE",
  [ASSERT] = "ASSERT",
  [PRE] = "PRE",
  [POST] = "POST",
  [INVARIANT] = "INVARIANT",
  [WITHIN] = "WITHIN",
  [ISALIST] = "ISALIST",
  [ISA] = "ISA",
  [IMPORT] = "IMPORT",
  [MODULE] = "MODULE",
  [MODULE_BODY] = "MODULE_BODY",
  [ROOT_OF_ALL] = "ROOT_OF_ALL",
  [DIRECTDEF] = "DIRECTDEF",
};

static uint32_t module_ptr_hash(const struct module **mod) {
  return hash32_hsieh(mod, sizeof(*mod));
}

static int module_ptr_cmp(const struct module **a, const struct module **b) {
  return memcmp(a, b, sizeof(*a));
}

IMPLEMENT_HTABLE_SPARSE(, importmap, struct node *, struct module *,
                        module_ptr_hash, module_ptr_cmp);

struct node *module_find_import(const struct module *mod, const struct module *other) {
  struct node **r = importmap_get(&CONST_CAST(mod)->importmap, other);
  return r == NULL ? NULL : *r;
}

static struct node *do_node_module_owner(struct node *node) {
  if (node == NULL) {
    return NULL;
  }
  assert(node->which != ROOT_OF_ALL);
  if (node->which == MODULE) {
    return node;
  } else {
    if (parent(node) == NULL) {
      return NULL;
    }
    return do_node_module_owner(parent(node));
  }
}

struct module *node_module_owner(struct node *node) {
  struct node *n = do_node_module_owner(node);
  assert(n != NULL);
  assert(n->which == MODULE);
  return n->as.MODULE.mod;
}

const struct module *node_module_owner_const(const struct node *node) {
  struct node *n = do_node_module_owner(CONST_CAST(node));
  assert(n != NULL);
  assert(n->which == MODULE);
  return n->as.MODULE.mod;
}

const struct module *try_node_module_owner_const(const struct module *mod,
                                                 const struct node *node) {
  struct node *n = do_node_module_owner(CONST_CAST(node));
  if (n == NULL) {
    return mod;
  } else {
    return n->as.MODULE.mod;
  }
}

struct node *node_statement_owner(struct node *node) {
  if (node->which == MODULE_BODY) {
    return NULL;
  } else if (node_is_statement(node)) {
    return node;
  } else {
    return node_statement_owner(parent(node));
  }
}

bool node_is_prototype(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (node->which == MODULE) {
    return node->as.MODULE.is_placeholder;
  } else if (toplevel == NULL) {
    return false;
  } else {
    return toplevel->flags & TOP_IS_PROTOTYPE;
  }
}

bool node_is_inline(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return false;
  } else {
    return toplevel->flags & TOP_IS_INLINE;
  }
}

bool node_is_opaque(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return false;
  } else {
    return toplevel->flags & TOP_IS_OPAQUE;
  }
}

bool node_is_export(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  assert(toplevel != NULL);
  return toplevel->flags & TOP_IS_EXPORT;
}

bool node_is_extern(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return false;
  } else {
    return toplevel->flags & TOP_IS_EXTERN;
  }
}

bool node_is_def(const struct node *node) {
  switch (node->which) {
  case MODULE:
  case DEFNAME:
  case DEFARG:
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFFIELD:
  case DEFINTF:
  case DEFCHOICE:
    return true;
  default:
    return false;
  }
}

bool node_is_statement(const struct node *node) {
  return parent_const(node) != NULL && parent_const(node)->which == BLOCK;
}

bool node_is_at_top(const struct node *node) {
  if (parent_const(node) == NULL) {
    return false;
  } else {
    return parent_const(node)->which == MODULE_BODY;
  }
}

bool node_is_rvalue(const struct node *node) {
  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      return false;
    }
    return true;
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_DEREF) {
      return false;
    }
    return true;
  case BOOL:
  case STRING:
  case NUMBER:
  case NIL:
  case TUPLE:
  case INIT:
  case CALL:
    return true;
  case TYPECONSTRAINT:
    return node_is_rvalue(subs_first_const(node));
  case BLOCK:
    return subs_count_atleast(node, 1)
      && node_is_rvalue(subs_last_const(node));
  case IF:
  case TRY:
  case MATCH:
    // Actually not considered to be rvalues, as we always insert a
    // temporary such that
    //   let tmp
    //     tmp = block-like
    // and we then move the 'tmp =' within the block-like, and then we
    // consider issues of rvalues/temporaries on these assignments within
    // the block-like.

    // Fallthrough.
  default:
    return false;
  }
}

bool node_is_name_of_globalenv(const struct node *node) {
  const struct node *par = parent_const(node);
  return par->which == DEFNAME && par->as.DEFNAME.is_globalenv;
}

void module_retval_set(struct module *mod, const struct node *retval) {
  mod->state->fun_state->retval = retval;
}

const struct node *module_retval_get(struct module *mod) {
  return mod->state->fun_state->retval;
}

void module_excepts_open_try(struct module *mod, struct node *tryy) {
  PUSH_STATE(mod->state->try_state);
  mod->state->try_state->tryy = tryy;
}

void module_excepts_push(struct module *mod, struct node *excep_node) {
  struct try_state *st = mod->state->try_state;
  vecnode_push(&st->excepts, excep_node);
}

struct try_state *module_excepts_get(struct module *mod) {
  return mod->state->try_state;
}

void module_excepts_close_try(struct module *mod) {
  struct try_state *st = mod->state->try_state;
  vecnode_destroy(&st->excepts);
  POP_STATE(mod->state->try_state);
}

void node_invariant(const struct node *node) {
  assert(subs_count(node) != 1 || node->subs_first == node->subs_last);
  assert(node->subs_first != node && node->subs_last != node);

  assert(!(NM(node->which) & NM(IDENT))
         || parent_const(node) == NULL
         || next_const(node) != NULL
         || prev_const(node) != NULL
         || !subs_count_atleast(parent_const(node), 2));

  assert(node->which < NODE__NUM);

  ssize_t n = 0;
  const struct node *p = NULL;
  FOREACH_SUB_CONST(s, node) {
    assert(s->which < NODE__NUM);

    assert(s->parent == node);
    if (n == 0) {
      assert(s->prev == NULL);
      assert(s == node->subs_first);
    } else {
      assert(s->prev != NULL);
      assert(s->prev == p);
      assert(s != node->subs_first);
    }
    if (s->next == NULL) {
      assert(s == node->subs_last);
    } else {
      assert(s != node->subs_last);
    }
    n += 1;
    p = s;
  }

  switch (node->which) {
  case NOOP:
  case NUMBER:
  case STRING:
  case BOOL:
  case NIL:
    assert(!subs_count_atleast(node, 1));
    break;
  case IF:
    assert(subs_count_atleast(node, 2));
    break;
  case CALL:
    assert(subs_count_atleast(node, 1));
    break;
  case UN:
    assert(subs_count(node) == 1);
    break;
  case LET:
    assert(subs_count_atleast(node, 1));
    assert(NM(subs_first_const(node)->which)
           & (NM(DEFNAME) | NM(DEFPATTERN) | NM(DEFALIAS)));
    break;
  case BIN:
    assert(subs_count(node) == 2);
    break;
  case DEFNAME:
    assert(subs_count(node) == 2);
    assert(node->as.DEFNAME.ssa_user == NULL
           || parent(node->as.DEFNAME.ssa_user) != NULL);
    break;
  default:
    break;
  }

  if (node->parent != NULL && !(node->flags & NODE__DETACHED)) {
    bool found = false;
    FOREACH_SUB_CONST(s, node->parent) {
      if (s == node) {
        found = true;
        break;
      }
    }
    assert(found);
  }

  assert(parent_const(node) != node);
}

void node_move_content(struct node *dst, struct node *src) {
  INVARIANT_NODE(src);

  struct node copy = *src;
  struct node *saved_dst_parent = parent(dst);

  if (src->typ != NULL) {
    unset_typ(&src->typ);
  }
  if (dst->typ != NULL) {
    unset_typ(&dst->typ);
  }

  memset(src, 0, sizeof(*src));

  // The source is emptied, but is left in its original position in the
  // tree.
  src->parent = parent(&copy);
  src->prev = prev(&copy);
  src->next = next(&copy);
  src->codeloc = copy.codeloc;

  struct node *prv = prev(dst);
  struct node *nxt = next(dst);

  *dst = copy;
  dst->parent = saved_dst_parent;

  dst->prev = prv;
  dst->next = nxt;
  dst->subs_first = subs_first(&copy);
  dst->subs_last = subs_last(&copy);
  dst->excepted = 0;

  FOREACH_SUB(s, dst) {
    s->parent = dst;
  }

  INVARIANT_NODE(dst);
}

struct node *node_new_subnode(struct module *mod, struct node *node) {
  STATIT {
    statit_mempool.count_by_type[0] += 1;
    statit_mempool.size_by_type[0] += sizeof(struct node);
  }

  struct node *r = mempool_calloc(mod, 1, sizeof(struct node));
  node_subs_append(node, r);

  if (mod->parser.codeloc.pos >= mod->parser.len) {
    // It's a node inserted after parsing.
    r->codeloc = node->codeloc;
  } else {
    r->codeloc = mod->parser.codeloc;
  }

  return r;
}

EXAMPLE(node_subs) {
  struct node n = { 0 };
  struct node a = { 0 };
  assert(subs_count_atleast(&n, 0));
  assert(!subs_count_atleast(&n, 1));
  node_subs_append(&n, &a);
  assert(subs_count(&n) == 1);
  assert(subs_count_atleast(&n, 0));
  assert(subs_count_atleast(&n, 1));
  assert(!subs_count_atleast(&n, 2));
  assert(&a == subs_first(&n));
  assert(&a == subs_last(&n));
  assert(NULL == next(&a));
  assert(NULL == prev(&a));

  struct node b = { 0 };
  node_subs_append(&n, &b);
  assert(subs_count(&n) == 2);
  assert(subs_count_atleast(&n, 0));
  assert(subs_count_atleast(&n, 1));
  assert(subs_count_atleast(&n, 2));
  assert(!subs_count_atleast(&n, 3));
  assert(&a == subs_first(&n));
  assert(&b == subs_last(&n));
  assert(&b == next(&a));
  assert(NULL == prev(&a));
  assert(NULL == next(&b));
  assert(&a == prev(&b));

  struct node c = { 0 };
  node_subs_replace(&n, &a, &c);
  assert(next(&a) == NULL);
  assert(prev(&a) == NULL);
  assert(subs_count(&n) == 2);
  assert(subs_count_atleast(&n, 0));
  assert(subs_count_atleast(&n, 1));
  assert(subs_count_atleast(&n, 2));
  assert(!subs_count_atleast(&n, 3));
  assert(&c == subs_first(&n));
  assert(&b == subs_last(&n));
  assert(&b == next(&c));
  assert(NULL == prev(&c));
  assert(NULL == next(&b));
  assert(&c == prev(&b));

  node_subs_insert_after(&n, &c, &a);
  assert(subs_count(&n) == 3);
  assert(&c == subs_first(&n));
  assert(&b == subs_last(&n));
  assert(&a == next(&c));
  assert(&b == next(&a));
  assert(NULL == next(&b));
  assert(NULL == prev(&c));
  assert(&c == prev(&a));
  assert(&a == prev(&b));

  node_subs_remove(&n, &b);
  assert(subs_count(&n) == 2);
  assert(&c == subs_first(&n));
  assert(&a == subs_last(&n));
  assert(&a == next(&c));
  assert(NULL == prev(&c));
  assert(NULL == next(&a));
  assert(&c == prev(&a));

  node_subs_insert_before(&n, &a, &b);
  assert(subs_count(&n) == 3);
  assert(&c == subs_first(&n));
  assert(&a == subs_last(&n));
  assert(&b == next(&c));
  assert(&a == next(&b));
  assert(NULL == next(&a));
  assert(NULL == prev(&c));
  assert(&c == prev(&b));
  assert(&b == prev(&a));
}

struct node *nparent(struct node *node, size_t nth) {
  for (size_t n = 0; n < nth; ++n) {
    node = node->parent;
  }
  return node;
}

const struct node *nparent_const(const struct node *node, size_t nth) {
  for (size_t n = 0; n < nth; ++n) {
    node = node->parent;
  }
  return node;
}

bool node_has_tail_block(const struct node *node) {
  return subs_count_atleast(node, 1)
    && subs_last_const(node)->which == BLOCK;
}

bool node_is_fun(const struct node *node) {
  return node->which == DEFFUN || node->which == DEFMETHOD;
}

size_t node_fun_all_args_count(const struct node *def) {
  return subs_count(subs_at_const(def, IDX_FUNARGS)) - 1;
}

size_t node_fun_min_args_count(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.min_args;
  case DEFMETHOD:
    return def->as.DEFMETHOD.min_args;
  default:
    assert(false);
    return -1;
  }
}

size_t node_fun_max_args_count(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.max_args;
  case DEFMETHOD:
    return def->as.DEFMETHOD.max_args;
  default:
    assert(false);
    return -1;
  }
}

ssize_t node_fun_first_vararg(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.first_vararg;
  case DEFMETHOD:
    return def->as.DEFMETHOD.first_vararg;
  default:
    assert(false);
    return -1;
  }
}

const struct node *node_fun_retval_const(const struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  const struct node *funargs = subs_at_const(def, IDX_FUNARGS);
  return subs_last_const(funargs);
}

struct node *node_fun_retval(struct node *def) {
  return CONST_CAST(node_fun_retval_const(def));
}

const struct node *node_defchoice_external_payload(const struct node *node) {
  assert(node->which == DEFCHOICE);
  if (subs_count(node) != IDX_CH_FIRST_PAYLOAD+1) {
    return NULL;
  }

  const struct node *p = subs_at_const(node, IDX_CH_FIRST_PAYLOAD);
  if (NM(p->which) & (NM(DEFFIELD) | NM(DEFCHOICE))) {
    return NULL;
  }

  return p;
}

struct node *node_get_member(struct node *node, ident id) {
  assert(NM(node->which) & (NM(DEFTYPE) | NM(DEFCHOICE) | NM(DEFINTF) | NM(DEFINCOMPLETE)));
  struct node *m = NULL;
  error null_on_error = scope_lookup_ident_immediate(&m, node, NULL, &node->scope, id, true);
  (void) null_on_error;
  return m;
}

const struct node *node_get_member_const(const struct node *node, ident id) {
  return node_get_member(CONST_CAST(node), id);
}

size_t node_branching_exhaustive_branch_count(struct node *node) {
  const size_t n = subs_count(node);
  switch (node->which) {
  case IF:
    // Always include the else case, even if implicit:
    return n / 2 + 1;
  case WHILE:
    return 1;
  case FOR:
    return 1;
  case MATCH:
    return n / 2;
  case TRY:
    {
      struct node *elet = subs_first(node);
      struct node *eblock = subs_at(elet, 1);
      return subs_count(eblock);
    }
  default:
    assert(false);
    return 0;
  }
}

struct node *mk_node(struct module *mod, struct node *par, enum node_which kind) {
  struct node *n = node_new_subnode(mod, par);
  node_set_which(n, kind);
  return n;
}

struct node *defincomplete_create(struct module *mod, const struct node *trigger) {

  struct node *dinc = node_new_subnode(mod, mod->body);

  dinc->codeloc = trigger->codeloc;
  node_set_which(dinc, DEFINCOMPLETE);
  dinc->as.DEFINCOMPLETE.trigger_mod = mod;
  struct node *dinc_name = mk_node(mod, dinc, IDENT);
  dinc_name->as.IDENT.name = gensym(mod);
  (void)mk_node(mod, dinc, GENARGS);
  (void)mk_node(mod, dinc, ISALIST);

  return dinc;
}

void defincomplete_set_ident(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident name) {
  assert(dinc->which == DEFINCOMPLETE);
  assert(name != ID__NONE && name != ID_ANONYMOUS);
  vecident_push(&dinc->as.DEFINCOMPLETE.idents, name);
  if (dinc->as.DEFINCOMPLETE.idents_for_error == NULL) {
    dinc->as.DEFINCOMPLETE.idents_for_error
      = calloc(1, sizeof(*dinc->as.DEFINCOMPLETE.idents_for_error));
  }
  vecnode_push(dinc->as.DEFINCOMPLETE.idents_for_error, CONST_CAST(for_error));
}

void defincomplete_add_field(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident field, struct typ *t) {
  assert(dinc->which == DEFINCOMPLETE);
  struct node *f = mk_node(mod, dinc, DEFFIELD);
  f->codeloc = for_error->codeloc;
  struct node *n = mk_node(mod, f, IDENT);
  n->as.IDENT.name = field;
  struct node *d = mk_node(mod, f, DIRECTDEF);
  set_typ(&d->as.DIRECTDEF.typ, t);
  d->as.DIRECTDEF.flags = NODE_IS_TYPE;
}

void defincomplete_add_isa(struct module *mod, const struct node *for_error,
                           struct node *dinc, struct typ *tisa) {
  assert(!typ_is_tentative(tisa) && "as tisa would typically be immediately"
         " linked to dinc->typ, creating an infinite loop. First create"
         " a typ_as_non_tentative() version of tisa.");
  struct node *isalist = subs_at(dinc, IDX_ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = true;
  isa->codeloc = for_error->codeloc;
  struct node *dd = mk_node(mod, isa, DIRECTDEF);
  set_typ(&dd->as.DIRECTDEF.typ, tisa);
}

error defincomplete_catchup(struct module *mod, struct node *dinc) {
  assert(dinc->which == DEFINCOMPLETE);
  const struct node *isalist = subs_at_const(dinc, IDX_ISALIST);
  dinc->as.DEFINCOMPLETE.is_isalist_literal
    = subs_count_atleast(isalist, 1) && !subs_count_atleast(dinc, IDX_ISALIST + 2);

  bool is_literal = false;
  FOREACH_SUB_CONST(isa, isalist) {
    const struct node *d = subs_first_const(isa);
    assert(d->which == DIRECTDEF);
    if (typ_is_literal(d->as.DIRECTDEF.typ)) {
      is_literal = true;
      break;
    }
  }

  const bool is_tentative = is_literal || !dinc->as.DEFINCOMPLETE.is_isalist_literal;
  error e = catchup_instantiation(mod, mod, dinc, is_tentative);
  EXCEPT(e);
  assert(!is_tentative || typ_is_tentative(dinc->typ));

  return 0;
}

int snprint_defincomplete(char *s, size_t len,
                          const struct module *mod, const struct node *dinc) {
  assert(dinc->which == DEFINCOMPLETE);
  size_t pos = 0;

  pos += snprint_codeloc(s+pos, len-pos, mod, dinc);
  pos += snprintf(s+pos, len-pos, "\n");

  struct node *mdinc = CONST_CAST(dinc);
  for (size_t n = 0, count = vecident_count(&mdinc->as.DEFINCOMPLETE.idents);
       n < count; ++n) {
    pos += snprintf(s+pos, len-pos, "  ");
    pos += snprint_codeloc(s+pos, len-pos, mod,
                           *vecnode_get(mdinc->as.DEFINCOMPLETE.idents_for_error, n));
    pos += snprintf(s+pos, len-pos,
                    "for ident '%s'\n",
                    idents_value(mod->gctx,
                                 *vecident_get(&mdinc->as.DEFINCOMPLETE.idents, n)));
  }

  const struct node *isalist = subs_at_const(dinc, IDX_ISALIST);
  if (subs_count_atleast(isalist, 1)) {
    FOREACH_SUB_CONST(isa, isalist) {
      pos += snprintf(s+pos, len-pos, "  ");
      pos += snprint_codeloc(s+pos, len-pos, mod, isa);
      pos += snprintf(s+pos, len-pos,
                      "with constraint '%s'\n",
                      pptyp(mod, isa->typ));
    }
  }

  FOREACH_SUB_CONST(f, dinc) {
    if (f->which == DEFFIELD) {
      pos += snprintf(s+pos, len-pos, "  ");
      pos += snprint_codeloc(s+pos, len-pos, mod, f);
      pos += snprintf(s+pos, len-pos,
                      "with field '%s', constrained by '%s'\n",
                      idents_value(mod->gctx, node_ident(f)),
                      pptyp(mod, f->typ));
    }
  }

  return pos;
}

static void do_node_deepcopy(struct module *mod, struct node *dst,
                             const struct node *src, bool omit_tail_block) {
  INVARIANT_NODE(src);

  struct node *par = dst->parent;
  struct node *prev = dst->prev;
  struct node *next = dst->next;

  node_set_which(dst, src->which);
  *dst = *src;
  memset(&dst->scope, 0, sizeof(dst->scope));
  dst->parent = par;
  dst->prev = prev;
  dst->next = next;
  dst->subs_count = 0;
  dst->subs_first = NULL;
  dst->subs_last = NULL;
  dst->typ = NULL;
  dst->constraint = NULL;
  dst->excepted = 0;

  struct toplevel *dtoplevel = node_toplevel(dst);
  if (dtoplevel != NULL) {
    dtoplevel->generic = NULL;
    dtoplevel->topdeps = NULL;
  }

  FOREACH_SUB_CONST(s, src) {
    if (omit_tail_block && next_const(s) == NULL && s->which == BLOCK) {
      break;
    }

    struct node *cpy = node_new_subnode(mod, dst);
    do_node_deepcopy(mod, cpy, s, omit_tail_block);
  }
}

void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src) {
  do_node_deepcopy(mod, dst, src, false);
}

void node_deepcopy_omit_tail_block(struct module *mod, struct node *dst,
                                   const struct node *src) {
  do_node_deepcopy(mod, dst, src, true);
}

int snprint_codeloc(char *s, size_t len,
                    const struct module *mod, const struct node *node) {
  if (node == NULL) {
    return snprintf(s, len, "(unknown): ");
  }

  const struct module *actual_mod = try_node_module_owner_const(mod, node);
  const char *fn = module_component_filename_at(actual_mod, node->codeloc.pos);

  return snprintf(s, len, "%s:%d:%d: ",
                  fn, node->codeloc.line, node->codeloc.column);
}

error mk_except(const struct module *mod, const struct node *node,
                const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(s);

  pos += snprint_codeloc(s+pos, len-pos, mod, node);
  pos += vsnprintf(s+pos, len-pos, fmt, ap);

  error e = 0;
  GOTO_THROWF(EINVAL, "%s", s);

except:
  va_end(ap);
  return e;
}

error mk_except_type(const struct module *mod, const struct node *node,
                     const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(s);

  pos += snprint_codeloc(s+pos, len-pos, mod, node);
  pos += snprintf(s+pos, len-pos, "type: ");
  pos += vsnprintf(s+pos, len-pos, fmt, ap);

  error e = 0;
  GOTO_THROWF(EINVAL, "%s", s);

except:
  va_end(ap);
  return e;
}

error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct typ *tfun, bool implicit_self,
                                size_t given) {
  const size_t minus = implicit_self ? 1 : 0;
  error e = mk_except_type(mod, node,
                           "invalid number of arguments:"
                           " between %zu and %zu expected, but %zu given",
                           typ_function_min_arity(tfun) - minus,
                           typ_function_max_arity(tfun) - minus,
                           given);
  THROW(e);
}
