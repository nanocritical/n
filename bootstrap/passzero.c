#include "passzero.h"

#include "scope.h"

static STEP_FILTER(step_do_rewrite_prototype_wildcards,
                   SF(UN));
static error step_do_rewrite_prototype_wildcards(struct module *mod, struct node *node,
                                                 void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.UN.operator == TREFWILDCARD
      || node->as.UN.operator == TNULREFWILDCARD) {
    // FIXME The proper solution is to use
    //   (intf t:Any) i_nullable r:(i_ref t) = (i_any_ref t)
    // instead of i_nullable_ref, i_nullable_mutable_ref, and i_nullable_mercurial_ref.
    // and use (i_nullable __wildcard_ref_arg__) here.
    assert(node->as.UN.operator != TNULREFWILDCARD && "FIXME: Unsupported");

    node_set_which(node, CALL);
    struct node *d = mk_node(mod, node, IDENT);
    d->as.IDENT.name = ID_WILDCARD_REF_ARG;
    node_subs_remove(node, d);
    node_subs_insert_before(node, node_subs_first(node), d);
  }
  return 0;
}

static error pass_rewrite_wildcards(struct module *mod, struct node *root,
                                    void *user, ssize_t shallow_last_up) {
  PASS(, UP_STEP(step_do_rewrite_prototype_wildcards));
  return 0;
}

static STEP_FILTER(step_rewrite_prototype_wildcards,
                   SF(DEFFUN) | SF(DEFMETHOD));
static error step_rewrite_prototype_wildcards(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *funargs = node_subs_at(node, IDX_FUNARGS);
  FOREACH_SUB(arg, funargs) {
    PUSH_STATE(mod->state->step_state);
    error e = pass_rewrite_wildcards(mod, arg, NULL, -1);
    EXCEPT(e);
    POP_STATE(mod->state->step_state);
  }

  return 0;
}

struct node *add_instance_deepcopy_from_pristine(struct module *mod,
                                                 struct node *node,
                                                 struct node *pristine,
                                                 bool tentative) {
  struct node *instance = calloc(1, sizeof(struct node));
  node_deepcopy(mod, instance, pristine);

  if (!tentative) {
    struct toplevel *toplevel = node_toplevel(node);
    const size_t idx = toplevel->instances_count;
    toplevel->instances_count += 1;
    toplevel->instances = realloc(toplevel->instances,
                                  toplevel->instances_count * sizeof(*toplevel->instances));
    toplevel->instances[idx] = instance;
  }

  if (instance->which == DEFTYPE) {
    instance->as.DEFTYPE.members_count = pristine->as.DEFTYPE.members_count,
      instance->as.DEFTYPE.members = calloc(instance->as.DEFTYPE.members_count,
                                            sizeof(*instance->as.DEFTYPE.members));

    for (size_t n = 0; n < pristine->as.DEFTYPE.members_count; ++n) {
      instance->as.DEFTYPE.members[n] = calloc(1, sizeof(**instance->as.DEFTYPE.members));
      node_deepcopy(mod, instance->as.DEFTYPE.members[n], pristine->as.DEFTYPE.members[n]);
    }
  }

  return instance;
}

static STEP_FILTER(step_generics_pristine_copy,
                   SF(DEFTYPE) | SF(DEFINTF) | SF(DEFFUN) | SF(DEFMETHOD));
static error step_generics_pristine_copy(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *genargs = node_subs_at(node, IDX_GENARGS);
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    if (node_subs_count_atleast(genargs, 1)
        && node_subs_first(genargs)->which == DEFGENARG) {
      (void) add_instance_deepcopy_from_pristine(mod, node, node, FALSE);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    // Always needed because the method/fun could be part of a generic
    // DEFTYPE, and we cannot know that yet.
    (void) add_instance_deepcopy_from_pristine(mod, node, node, FALSE);
    break;
  default:
    assert(FALSE && "Unreached");
    break;
  }

  return 0;
}

static STEP_FILTER(step_detect_prototypes,
                   SF(DEFFUN) | SF(DEFMETHOD));
static error step_detect_prototypes(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  toplevel->is_prototype = toplevel->builtingen == BG__NOT
    && !node_has_tail_block(node);

  return 0;
}

static STEP_FILTER(step_detect_deftype_kind,
                   SF(DEFTYPE));
// Must be run before builtins are added.
static error step_detect_deftype_kind(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  struct node *for_error = NULL;
  enum deftype_kind k = DEFTYPE_PROTOTYPE;
  FOREACH_SUB(f, node) {
    switch (f->which) {
    case DEFFIELD:
      if (k == DEFTYPE_ENUM || k == DEFTYPE_SUM) {
        for_error = f;
        goto field_and_sum;
      }
      k = DEFTYPE_STRUCT;
      break;
    case DEFCHOICE:
      if (k == DEFTYPE_STRUCT) {
        for_error = f;
        goto field_and_sum;
      }
      if (k != DEFTYPE_SUM) {
        k = DEFTYPE_ENUM;
      }
      if ((!f->as.DEFCHOICE.has_value
           && node_ident(node_subs_at_const(f, IDX_CH_PAYLOAD-1)) != ID_TBI_VOID)
          || (f->as.DEFCHOICE.has_value
              && node_ident(node_subs_at_const(f, IDX_CH_PAYLOAD)) != ID_TBI_VOID)) {
        k = DEFTYPE_SUM;
      }
      break;
    default:
      break;
    }
  }

  node->as.DEFTYPE.kind = k;
  return 0;

field_and_sum:
  e = mk_except_type(mod, for_error, "type contains both fields and choices");
  THROW(e);
}

static STEP_FILTER(step_assign_deftype_which_values,
                   SF(DEFTYPE));
static error step_assign_deftype_which_values(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind != DEFTYPE_ENUM
      && node->as.DEFTYPE.kind != DEFTYPE_SUM) {
    return 0;
  }

  struct node *prev = NULL;
  FOREACH_SUB(d, node) {
    if (d->which != DEFCHOICE) {
      continue;
    }

    if (d->as.DEFCHOICE.has_value) {
      prev = d;
      continue;
    }

    d->as.DEFCHOICE.has_value = TRUE;
    struct node *val;
    if (prev == NULL) {
      val = mk_node(mod, d, NUMBER);
      val->as.NUMBER.value = "0";
    } else {
      val = mk_node(mod, d, BIN);
      val->as.BIN.operator = TPLUS;
      struct node *left = node_new_subnode(mod, val);
      node_deepcopy(mod, left, node_subs_at(prev, IDX_CH_VALUE));
      struct node *right = mk_node(mod, val, NUMBER);
      right->as.NUMBER.value = "1";
    }

    node_subs_insert_after(d, node_subs_at(d, IDX_CH_VALUE-1), val);

    prev = d;
  }

  return 0;
}

STEP_FILTER(step_add_scopes,
            -1);
error step_add_scopes(struct module *mod, struct node *node,
                      void *user, bool *stop) {
  DSTEP(mod, node);

  FOREACH_SUB(s, node) {
    s->scope.parent = &node->scope;
  }

  return 0;
}

STEP_FILTER(step_stop_submodules,
            SF(MODULE));
error step_stop_submodules(struct module *mod, struct node *node,
                           void *user, bool *stop) {
  DSTEP(mod, node);

  int *module_depth = user;
  *module_depth += 1;

  if (*module_depth > 1) {
    *stop = TRUE;
  }
  return 0;
}

static error passzero0(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_rewrite_prototype_wildcards);
    DOWN_STEP(step_generics_pristine_copy);
    DOWN_STEP(step_detect_prototypes);
    DOWN_STEP(step_detect_deftype_kind);
    DOWN_STEP(step_assign_deftype_which_values);
    ,
    UP_STEP(step_add_scopes);
    );
  return 0;
}

a_pass passzero[] = { passzero0 };
