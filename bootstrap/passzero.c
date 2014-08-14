#include "passzero.h"

#include "scope.h"
#include "lir.h"
#include "ssa.h"
#include "types.h"

#include "passbody.h"

static STEP_NM(step_do_rewrite_prototype_wildcards,
               NM(UN));
static ERROR step_do_rewrite_prototype_wildcards(struct module *mod, struct node *node,
                                                 void *user, bool *stop) {
  const bool within_self = *((bool *) user);

  DSTEP(mod, node);
  if (node->as.UN.operator == TREFWILDCARD
      || node->as.UN.operator == TNULREFWILDCARD) {
    // FIXME The proper solution is to use
    //   (intf t:Any) `nullable r:(`ref t) = (`any_ref t)
    // instead of `nullable_ref, `nullable_mutable_ref, and `nullable_mercurial_ref.
    // and use (`nullable __wildcard_ref_arg__) here.
    assert(node->as.UN.operator != TNULREFWILDCARD && "FIXME: Unsupported");

    node_set_which(node, CALL);
    struct node *d = mk_node(mod, node, IDENT);
    d->as.IDENT.name = within_self ? ID_WILDCARD_REF_ARG_SELF : ID_WILDCARD_REF_ARG;
    node_subs_remove(node, d);
    node_subs_insert_before(node, subs_first(node), d);
  }
  return 0;
}

static ERROR pass_rewrite_wildcards(struct module *mod, struct node *root,
                                    void *user, ssize_t shallow_last_up) {
  PASS(, UP_STEP(step_do_rewrite_prototype_wildcards), );
  return 0;
}

static STEP_NM(step_rewrite_prototype_wildcards,
               NM(DEFFUN) | NM(DEFMETHOD));
static ERROR step_rewrite_prototype_wildcards(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *funargs = subs_at(node, IDX_FUNARGS);
  FOREACH_SUB(arg, funargs) {
    PUSH_STATE(mod->state->step_state);
    bool within_self = node->which == DEFMETHOD && prev_const(arg) == NULL;
    error e = pass_rewrite_wildcards(mod, arg, &within_self, -1);
    EXCEPT(e);
    POP_STATE(mod->state->step_state);
  }

  return 0;
}

static void try_add_generic(struct node *node) {
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->generic == NULL) {
    toplevel->generic = calloc(1, sizeof(*toplevel->generic));
    instances_init(node);
  }
}

struct node *create_instance_deepcopy_from_pristine(struct module *mod,
                                                    struct node *node,
                                                    struct node *pristine,
                                                    bool tentative) {
  BEGTIMEIT(TIMEIT_CREATE_INSTANCE_DEEPCOPY);

  struct node *instance = calloc(1, sizeof(struct node));
  instance->parent = parent(node);
  if (tentative) {
    node_deepcopy_tentative(mod, instance, pristine);
  } else {
    node_deepcopy(mod, instance, pristine);
  }
  instance->flags |= NODE__DETACHED;
  node_toplevel(instance)->scope_name = 0;

  try_add_generic(node);
  try_add_generic(instance);

  ENDTIMEIT(true, TIMEIT_CREATE_INSTANCE_DEEPCOPY);
  return instance;
}

static STEP_NM(step_init_toplevel,
               STEP_NM_HAS_TOPLEVEL);
static ERROR step_init_toplevel(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  toplevel->passing = -1;
  toplevel->passed = -1;
  return 0;
}

static STEP_NM(step_generics_pristine_copy,
               NM(DEFTYPE) | NM(DEFINTF) | NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFINCOMPLETE));
static ERROR step_generics_pristine_copy(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  const bool is_instance = node_toplevel(node)->generic != NULL
    && node_toplevel(node)->generic->our_generic_functor_typ != NULL;
  if (is_instance) {
    return 0;
  }

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFFUN:
  case DEFMETHOD:
  case DEFINCOMPLETE:
    {
      struct node *pristine = create_instance_deepcopy_from_pristine(mod, node,
                                                                     node, false);
      node_toplevel(node)->generic->pristine = pristine;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

static STEP_NM(step_detect_prototypes,
               NM(DEFFUN) | NM(DEFMETHOD));
static ERROR step_detect_prototypes(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->builtingen == BG__NOT && !node_has_tail_block(node)) {
    toplevel->flags |= TOP_IS_PROTOTYPE;
  }

  return 0;
}

static ERROR do_check_deftype_kind(struct module *mod, struct node *deft,
                                   struct node *node) {
  error e;
  enum deftype_kind k = deft->as.DEFTYPE.kind;
  FOREACH_SUB(f, node) {
    switch (f->which) {
    case DEFFIELD:
      if (k == DEFTYPE_ENUM) {
        e = mk_except_type(mod, f, "enum may not contain a field declaration");
        THROW(e);
      }
      break;
    case DEFCHOICE:
      if (k == DEFTYPE_STRUCT) {
        e = mk_except_type(mod, f, "struct may not contain a choice declaration");
        THROW(e);
      }

      e = do_check_deftype_kind(mod, deft, f);
      EXCEPT(e);
      break;
    default:
      break;
    }
  }
  return 0;
}

static STEP_NM(step_check_deftype_kind,
               NM(DEFTYPE));
// Must be run before builtins are added.
static ERROR step_check_deftype_kind(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);
  error e = do_check_deftype_kind(mod, node, node);
  EXCEPT(e);
  return 0;
}

static void do_assign_defchoice_tag(struct module *mod,
                                    struct node *node) {
  struct node *par = parent(node);

  node->as.DEFCHOICE.is_leaf = true;
  if (node->as.DEFCHOICE.has_tag) {
    struct node *tag = subs_last(node);
    if (tag->which == NUMBER && strcmp(tag->as.NUMBER.value, "0") == 0) {
      struct node *top = par;
      while (top->which != DEFTYPE) {
        top = parent(top);
      }
      top->as.DEFTYPE.default_choice = tag;
    }
    return;
  }

  struct node *pre = prev(node);
  if (pre->which != DEFCHOICE) {
    pre = NULL;
  }

  struct node *tag;
  if (pre == NULL && par->which == DEFTYPE) {
    tag = mk_node(mod, node, NUMBER);
    tag->as.NUMBER.value = "1";
  } else {
    if (pre != NULL) {
      struct node *pred;
      if (pre->which == DEFCHOICE && !pre->as.DEFCHOICE.is_leaf) {
        struct node *pch = NULL;
        REVERSE_FOREACH_SUB(sub, pre) {
          if (sub->which == DEFCHOICE) {
            pch = sub;
            break;
          }
        }
        pred = subs_at(pch, IDX_CH_TAG_FIRST);
      } else {
        pred = subs_at(pre, IDX_CH_TAG_FIRST);
      }

      tag = mk_node(mod, node, BIN);
      tag->as.BIN.operator = TPLUS;
      struct node *left = node_new_subnode(mod, tag);
      node_deepcopy(mod, left, pred);
      struct node *one = mk_node(mod, tag, NUMBER);
      one->as.NUMBER.value = "1";
    } else if (par->which == DEFCHOICE) {
      tag = node_new_subnode(mod, node);
      node_deepcopy(mod, tag, subs_at(par, IDX_CH_TAG_FIRST));
    } else {
      return;
    }

  }

  node_subs_remove(node, tag);
  node_subs_insert_after(node, subs_at(node, IDX_CH_TAG_FIRST-1), tag);

  node->as.DEFCHOICE.has_tag = true;
  if (par->which == DEFCHOICE) {
    par->as.DEFCHOICE.is_leaf = false;
  }
}

static STEP_NM(step_assign_defchoice_tag_down,
               NM(DEFCHOICE));
static ERROR step_assign_defchoice_tag_down(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  do_assign_defchoice_tag(mod, node);

  return 0;
}

static STEP_NM(step_assign_defchoice_tag_up,
               NM(DEFCHOICE));
static ERROR step_assign_defchoice_tag_up(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFCHOICE.is_leaf) {
    struct node *dummy = mk_node(mod, node, NUMBER);
    dummy->as.NUMBER.value = "0";
    node_subs_remove(node, dummy);
    node_subs_insert_after(node, subs_at(node, IDX_CH_TAG_FIRST), dummy);
    return 0;
  }

  REVERSE_FOREACH_SUB(last, node) {
    if (last->which == DEFCHOICE) {
      struct node *last_tag = node_new_subnode(mod, node);
      node_deepcopy(mod, last_tag, subs_at(last, IDX_CH_TAG_FIRST));
      node_subs_remove(node, last_tag);
      node_subs_insert_after(node, subs_at(node, IDX_CH_TAG_FIRST), last_tag);
      break;
    }
  }

  return 0;
}

STEP_NM(step_stop_submodules,
        NM(MODULE));
error step_stop_submodules(struct module *mod, struct node *node,
                           void *user, bool *stop) {
  DSTEP(mod, node);

  int *module_depth = user;
  *module_depth += 1;

  if (*module_depth > 1) {
    assert(node->which == MODULE);
    *stop = true;
  }
  return 0;
}

static ERROR passzero0(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_init_toplevel);
    DOWN_STEP(step_generics_pristine_copy);
    DOWN_STEP(step_lir_conversion_down);
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_add_sequence_points);
    ,
    UP_STEP(step_lir_conversion_up);
    UP_STEP(step_ssa_convert_shallow_catchup);
    UP_STEP(step_ssa_convert);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static ERROR passzero1(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_rewrite_prototype_wildcards);
    DOWN_STEP(step_detect_prototypes);
    DOWN_STEP(step_check_deftype_kind);
    DOWN_STEP(step_assign_defchoice_tag_down);
    ,
    UP_STEP(step_assign_defchoice_tag_up);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

a_pass passzero[] = { passzero0, passzero1 };
