#include "passzero.h"

#include "scope.h"

static error step_do_rewrite_prototype_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case UN:
    if (node->as.UN.operator == TREFWILDCARD
        || node->as.UN.operator == TNULREFWILDCARD) {
      // FIXME The proper solution is to use
      //   (intf t:Any) i_nullable r:(i_ref t) = (i_any_ref t)
      // instead of i_nullable_ref, i_nullable_mutable_ref, and i_nullable_mercurial_ref.
      // and use (i_nullable __wildcard_ref_arg__) here.
      assert(node->as.UN.operator != TNULREFWILDCARD && "Unsupported yet");

      node->which = CALL;
      struct node *d = mk_node(mod, node, IDENT);
      d->as.IDENT.name = ID_WILDCARD_REF_ARG;
      rew_insert_last_at(node, 0);
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_rewrite_prototype_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  static const step down[] = {
    NULL,
  };

  static const step up[] = {
    step_do_rewrite_prototype_wildcards,
    NULL,
  };

  struct node *funargs;
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    funargs = node->subs[IDX_FUNARGS];
    for (size_t n = 0; n < funargs->subs_count; ++n) {
      struct node *arg = funargs->subs[n];
      if (arg->which == BLOCK) {
        break;
      }

      PUSH_STATE(mod->state->step_state);
      error e = pass(mod, arg, down, up, -1, NULL);
      EXCEPT(e);
      POP_STATE(mod->state->step_state);
    }
    break;
  default:
    break;
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

static error step_generics_pristine_copy(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    if (node->subs[IDX_GENARGS]->subs_count > 0
        && node->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {
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
    break;
  }

  return 0;
}

static error step_detect_prototypes(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct toplevel *toplevel = node_toplevel(node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    toplevel->is_prototype = toplevel->builtingen == BG__NOT
      && !node_has_tail_block(node);
    break;
  default:
    break;
  }
  return 0;
}

// Must be run before builtins are added.
static error step_detect_deftype_kind(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }

  error e;
  struct node *f = NULL;
  enum deftype_kind k = DEFTYPE_PROTOTYPE;
  for (size_t n = 0; n < node->subs_count; ++n) {
    f = node->subs[n];

    switch (f->which) {
    case DEFFIELD:
      if (k == DEFTYPE_ENUM || k == DEFTYPE_SUM) {
        goto field_and_sum;
      }
      k = DEFTYPE_STRUCT;
      break;
    case DEFCHOICE:
      if (k == DEFTYPE_STRUCT) {
        goto field_and_sum;
      }
      if (k != DEFTYPE_SUM) {
        k = DEFTYPE_ENUM;
      }
      if ((!f->as.DEFCHOICE.has_value && node_ident(f->subs[IDX_CH_PAYLOAD-1]) != ID_TBI_VOID)
          || (f->as.DEFCHOICE.has_value && node_ident(f->subs[IDX_CH_PAYLOAD]) != ID_TBI_VOID)) {
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
  e = mk_except_type(mod, f, "type contains both fields and choices");
  THROW(e);
}

static error step_assign_deftype_which_values(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE
      || (node->as.DEFTYPE.kind != DEFTYPE_ENUM
          && node->as.DEFTYPE.kind != DEFTYPE_SUM)) {
    return 0;
  }

  struct node *prev = NULL;
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *d = node->subs[n];
    if (d->which != DEFCHOICE) {
      continue;
    }

    if (d->as.DEFCHOICE.has_value) {
      prev = d;
      continue;
    }

    d->as.DEFCHOICE.has_value = TRUE;
    if (prev == NULL) {
      struct node *val = mk_node(mod, d, NUMBER);
      val->as.NUMBER.value = "0";
    } else {
      struct node *val = mk_node(mod, d, BIN);
      val->as.BIN.operator = TPLUS;
      struct node *left = node_new_subnode(mod, val);
      node_deepcopy(mod, left, prev->subs[IDX_CH_VALUE]);
      struct node *right = mk_node(mod, val, NUMBER);
      right->as.NUMBER.value = "1";
    }

    rew_insert_last_at(d, IDX_CH_VALUE);

    prev = d;
  }

  return 0;
}

error step_add_scopes(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != MODULE) {
    node->scope = scope_new(node);
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->scope != NULL) {
      s->scope->parent = node->scope;
    }
  }

  return 0;
}

error step_stop_submodules(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != MODULE) {
    return 0;
  }

  int *module_depth = user;
  *module_depth += 1;

  if (*module_depth > 1) {
    *stop = TRUE;
  }
  return 0;
}

const struct pass passzero[] = {
  {
    PASS_ZERO, "zero",
    {
      step_rewrite_prototype_wildcards,
      step_generics_pristine_copy,
      step_detect_prototypes,
      step_detect_deftype_kind,
      step_assign_deftype_which_values,
      NULL,
    },
    {
      step_add_scopes,
      NULL,
    }
  }
};
