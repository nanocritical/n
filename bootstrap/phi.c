#include "phi.h"

#include "passes.h"
#include "parser.h"
#include "types.h"

struct phi_tracker_state *get_phi_tracker(struct node *def) {
  switch (def->which) {
  case DEFNAME:
    return def->as.DEFNAME.phi_state;
  case DEFARG:
    return def->as.DEFARG.phi_state;
  default:
    assert(false);
    return NULL;
  }
}

#define OR_ELSE(p, def) ( (p) != NULL ? (p) : (def) )

static struct node *scoped_block_surrounding(struct node *node) {
  struct node *n = node;
  do {
    n = parent(n);
  } while (n->which != BLOCK || n->as.BLOCK.is_scopeless);
  return n;
}

static error branching_block_foreach_scoped_ident(struct module *mod,
                                                  struct node *branching,
                                                  struct node *block,
                                                  scope_each each) {
  error e;
  struct node *fun = mod->state->top_state->top;
  assert(fun->which == DEFFUN || fun->which == DEFMETHOD);
  struct node *funargs = subs_at(fun, IDX_FUNARGS);
  FOREACH_SUB(arg, funargs) {
    e = each(mod, arg, block);
    EXCEPT(e);
  }

  struct node *top_block = subs_last(fun);
  struct node *b = branching;
  do {
    b = scoped_block_surrounding(b);

    e = scope_foreach(mod, &b->scope, each, block);
    EXCEPT(e);
  } while (b != top_block);

  return 0;
}

static struct node *insert_conditioned_phi(struct module *mod,
                                           struct branch_state *br_st,
                                           struct node *br_block,
                                           struct node *pre_branch_use) {
  if (br_st->branching->which == TRY) {
    // FIXME: unsupported in try/catch
    return pre_branch_use;
  }

  assert(br_block->which == BLOCK);
  struct node *phi = mk_node(mod, br_block, PHI);
  phi->as.PHI.is_conditioned = true;
  switch (pre_branch_use->which) {
  case PHI:
    phi->as.PHI.def = pre_branch_use->as.PHI.def;
    break;
  case IDENT:
    phi->as.PHI.def = pre_branch_use->as.IDENT.def;
    break;
  case DEFARG:
  case DEFNAME:
    phi->as.PHI.def = pre_branch_use;
    break;
  default:
    assert(false);
  }

  struct ancestor ancestor = { .prev = pre_branch_use, 0 };
  if (br_st->prev != NULL) {
    ancestor.cond = br_st->prev->cond;
    ancestor.reversed = br_st->prev->reversed;
  }
  vecancestor_push(&phi->as.PHI.ancestors, ancestor);

  node_subs_remove(br_block, phi);
  node_subs_insert_before(br_block, subs_first(br_block), phi);

  error e = catchup(mod, NULL, phi, CATCHUP_AFTER_CURRENT);
  assert(!e);

  return phi;
}

// Scopes are defined over an entire BLOCK, but a name becomes defined only
// in the statements following its DEFNAME or DEFALIAS within the block.
static bool local_name_is_scoped(struct module *mod, const struct node *node) {
  switch (node->which) {
  case DEFARG:
    return true;
  case DEFNAME:
    return node->as.DEFNAME.passed >= mod->stage->state->passing;
  case DEFALIAS:
    return node->as.DEFALIAS.passed >= mod->stage->state->passing;
  default:
    assert(false);
    return false;
  }
}

static error insert_all_possible_conditioned_phi_each(struct module *mod,
                                                      struct node *def,
                                                      void *user) {
  struct node *br_block = user;
  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  struct branch_state *br_st = mod->state->branch_state;
  struct phi_tracker_state *phi_st = get_phi_tracker(def);

  struct node *prev = phi_st->prev != NULL
    ? OR_ELSE(phi_st->prev->last, def) : def;
  struct node *phi = insert_conditioned_phi(mod, br_st, br_block, prev);
  phi_st->last = phi;
  return 0;
}

static error insert_all_possible_conditioned_phi(struct module *mod,
                                                 struct node *block) {
  error e = branching_block_foreach_scoped_ident(
    mod, parent(block), block, insert_all_possible_conditioned_phi_each);
  EXCEPT(e);
  return 0;
}

static error init_phi_trackers_for_branching_each(struct module *mod,
                                                  struct node *def,
                                                  void *user) {
  struct node *br_block = user;
  assert(br_block == NULL);

  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  switch (def->which) {
  case DEFNAME:
    PUSH_STATE(def->as.DEFNAME.phi_state);
    break;
  case DEFARG:
    PUSH_STATE(def->as.DEFARG.phi_state);
    break;
  default:
    assert(false);
  }

  return 0;
}

static void init_phi_trackers_for_branching(struct module *mod) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = br_st->branching;

  error e = branching_block_foreach_scoped_ident(
    mod, branching, NULL, init_phi_trackers_for_branching_each);
  assert(!e);
}

static error uninit_phi_trackers_for_branching_each(struct module *mod,
                                                    struct node *def,
                                                    void *user) {
  struct node *br_block = user;
  assert(br_block == NULL);

  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  switch (def->which) {
  case DEFNAME:
    POP_STATE(def->as.DEFNAME.phi_state);
    break;
  case DEFARG:
    POP_STATE(def->as.DEFARG.phi_state);
    break;
  default:
    assert(false);
  }

  return 0;
}

static void uninit_phi_trackers_for_branching(struct module *mod) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = br_st->branching;

  error e = branching_block_foreach_scoped_ident(
    mod, branching, NULL, uninit_phi_trackers_for_branching_each);
  assert(!e);
}

STEP_NM(step_branching_down,
        STEP_NM_BRANCHING);
error step_branching_down(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);

  PUSH_STATE(mod->state->branch_state);
  struct branch_state *st = mod->state->branch_state;
  st->branching = node;
  st->cond = NULL;
  st->reversed = false;

  return 0;
}

static size_t find_in_parent(const struct node *par, const struct node *node) {
  size_t n = 0;
  FOREACH_SUB_CONST(s, par) {
    if (s == node) {
      return n;
    }
    n += 1;
  }
  assert(false);
  return 0;
}

STEP_NM(step_branching_block_down,
        NM(BLOCK));
error step_branching_block_down(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *par = parent(node);
  const size_t nth_sub = find_in_parent(par, node);
  struct branch_state *st = mod->state->branch_state;

  switch (par->which) {
  case WHILE:
    if (nth_sub == 0) {
      st->cond = NULL;
      return 0;
    } else {
      st->cond = subs_first(par);
    }
    break;
  case IF:
    if (nth_sub == 0) {
      st->cond = NULL;
      return 0;
    } else if (nth_sub == 1) {
      st->cond = subs_first(par);
      st->reversed = false;
    } else if (nth_sub == 2) {
      // Else branch.
      st->cond = subs_first(par);
      st->reversed = true;
    } else {
      assert(false);
    }
    break;
  case MATCH:
    if (nth_sub == 0) {
      st->cond = NULL;
    } else {
      st->cond = subs_first(par);
    }
    break;
  case TRY:
    return 0;
  default:
    return 0;
  }

  assert(st->branching == par);

  return 0;
}

STEP_NM(step_branching_block_down_phi,
        NM(BLOCK));
error step_branching_block_down_phi(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  const struct node *par = parent_const(node);
  if (!(NM(par->which) & STEP_NM_BRANCHING)) {
    return 0;
  }

  struct branch_state *br_st = mod->state->branch_state;
  assert(br_st->branching == par);
  if (br_st->cond == NULL) {
    return 0;
  }

  init_phi_trackers_for_branching(mod);
  insert_all_possible_conditioned_phi(mod, node);

  return 0;
}

static void mark_conditioned_phi_chain_used(struct node *node) {
  if (node->which == PHI && !node->as.PHI.is_used) {
    node->as.PHI.is_used = true;

    if (node->as.PHI.is_conditioned) {
      struct node *ancestor = vecancestor_get(&node->as.PHI.ancestors, 0)->prev;
      mark_conditioned_phi_chain_used(ancestor);
    }
  }
}

static error track_ident_use(struct module *mod, struct node *node) {
  assert(node->which == IDENT);
  struct node *def = node->as.IDENT.def;

  switch (def->which) {
  case WITHIN:
    // FIXME: WITHIN should probably also have its own phi_tracker and be
    // handled like DEFNAME and DEFARG below.
  case DEFGENARG:
  case SETGENARG:
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
  case DEFALIAS:
  case DEFCHOICE:
    node->as.IDENT.prev_use = def;
    assert(node->as.IDENT.prev_use != node);
    return 0;
  case IMPORT:
    return 0;

  case DEFNAME:
    if (parent_const(parent_const(def))->which == MODULE_BODY) {
      return 0;
    }
    break;
  case DEFARG:
    break;

  default:
    assert(false);
    return 0;
  }

  if (node->as.IDENT.non_local_scope == NULL && !local_name_is_scoped(mod, def)) {
    error e = mk_except(mod, node, "identifier '%s' used before its definition",
                        idents_value(mod->gctx, node_ident(node)));
    THROW(e);
  }

  struct phi_tracker_state *phi_st = get_phi_tracker(def);

  node->as.IDENT.prev_use = OR_ELSE(phi_st->last, def);
  assert(node->as.IDENT.prev_use != node);

  if (phi_st->last != NULL && phi_st->last->which == IDENT) {
    phi_st->last->as.IDENT.next_use = node;
  }

  mark_conditioned_phi_chain_used(node->as.IDENT.prev_use);

  phi_st->last = node;
  return 0;
}

static void phi_insertion(struct module *mod, struct node *br_block) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = parent(br_block);
  assert(NM(branching->which) & STEP_NM_BRANCHING);
  struct node *where_phi = parent(branching);
  struct node *maybe_first_phi = next(branching);

  struct node *cphi = subs_first(br_block);
  while (cphi->which == PHI) {
    assert(cphi->as.PHI.is_conditioned);
    if (!cphi->as.PHI.is_used) {
      struct node *to_remove = cphi;
      cphi = next(cphi);
      node_subs_remove(br_block, to_remove);
      continue;
    }

    struct node *def = cphi->as.PHI.def;

    // Look for a suitable existing PHI. Slow-ish, quadratic-ish.
    struct node *phi = maybe_first_phi;
    while (phi != NULL && phi->which == PHI && node_ident(phi) != node_ident(cphi)) {
      phi = next(phi);
    }

    if (phi == NULL || phi->which != PHI) {
      struct node *nxt = phi;
      phi = mk_node(mod, where_phi, PHI);
      phi->as.PHI.def = cphi->as.PHI.def;
      phi->typ = TBI__NOT_TYPEABLE;

      if (nxt != NULL) {
        node_subs_remove(where_phi, phi);
        node_subs_insert_before(where_phi, nxt, phi);
      }

      struct ancestor *pre_branch = vecancestor_get(&cphi->as.PHI.ancestors, 0);
      vecancestor_push(&phi->as.PHI.ancestors, *pre_branch);
    }

    assert(phi->as.PHI.def == def);
    struct phi_tracker_state *phi_st = get_phi_tracker(def);

    struct ancestor ancestor = {
      .prev = phi_st->last,
      .cond = br_st->cond,
      .reversed = br_st->reversed,
    };
    vecancestor_push(&phi->as.PHI.ancestors, ancestor);

    cphi = next(cphi);
  }
}

STEP_NM(step_branching_block_up_phi,
        NM(BLOCK));
error step_branching_block_up_phi(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  const struct node *par = parent_const(node);
  if (!(NM(par->which) & STEP_NM_BRANCHING)) {
    return 0;
  }

  struct branch_state *br_st = mod->state->branch_state;
  assert(br_st->branching == par);
  if (br_st->cond == NULL) {
    return 0;
  }

  phi_insertion(mod, node);
  uninit_phi_trackers_for_branching(mod);
  return 0;
}

STEP_NM(step_branching_up,
        STEP_NM_BRANCHING);
error step_branching_up(struct module *mod, struct node *node,
                        void *user, bool *stop) {
  DSTEP(mod, node);

  POP_STATE(mod->state->branch_state);
  return 0;
}

// FIXME: should we be removing these before inferring dyn?
STEP_NM(step_remove_typeconstraints,
        NM(TYPECONSTRAINT));
error step_remove_typeconstraints(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  struct typ *saved = node->typ;
  struct node *sub = subs_first(node);
  node_move_content(node, sub);
  unset_typ(&node->typ);
  set_typ(&node->typ, saved);

  if (node->which == IDENT) {
    error e = track_ident_use(mod, node);
    EXCEPT(e);
  }

  return 0;
}

STEP_NM(step_ident_non_local_scope,
        NM(IDENT));
error step_ident_non_local_scope(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  struct scope *non_local_scope = node->as.IDENT.non_local_scope;
  const struct node *d = typ_definition_const(node->typ);

  if (non_local_scope != NULL
      && scope_node(non_local_scope)->which == DEFINCOMPLETE
      && d->which == DEFTYPE
      && (d->as.DEFTYPE.kind == DEFTYPE_ENUM || d->as.DEFTYPE.kind == DEFTYPE_UNION)) {
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &d->scope,
                                           node_ident(node), false);
    assert(!e);

    node->as.IDENT.def = def;
    node->as.IDENT.non_local_scope = &parent(def)->scope;

    e = track_ident_use(mod, node);
    assert(!e);
  }
  return 0;
}

STEP_NM(step_track_ident_use,
        NM(IDENT) | NM(PHI));
error step_track_ident_use(struct module *mod, struct node *node,
                           void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_name_of_globalenv(node)
      || node_ident(node) == ID_OTHERWISE
      || (node->which == IDENT && typ_equal(node->typ, TBI__NOT_TYPEABLE))) {
    return 0;
  }

  error e;

  switch (node->which) {
  case IDENT:
    e = track_ident_use(mod, node);
    EXCEPT(e);
    break;
  case PHI:
    {
      struct node *def = node->as.PHI.def;
      switch (def->which) {
      case DEFNAME:
        def->as.DEFNAME.phi_state->last = node;
        break;
      case DEFARG:
        def->as.DEFARG.phi_state->last = node;
        break;
      default:
        break;
      }
    }
    break;
  default:
    break;
  }

  return 0;
}
