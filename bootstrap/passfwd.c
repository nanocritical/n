#include "passfwd.h"

#include "table.h"
#include "types.h"
#include "constraints.h"
#include "scope.h"
#include "unify.h"
#include "import.h"

#include "passes.h"
#include "passzero.h"
#include "passbody.h"

static STEP_NM(step_codeloc_for_generated,
               -1);
static error step_codeloc_for_generated(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  if (node->codeloc.pos == 0) {
    node->codeloc = parent(node)->codeloc;
  }

  return 0;
}

static STEP_NM(step_export_pre_post_invariant,
               NM(PRE) | NM(POST) | NM(INVARIANT));
static error step_export_pre_post_invariant(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  const struct node *par = parent_const(node);
  if (par->which == DEFTYPE || par->which == DEFINTF) {
    node_toplevel(node)->flags |= node_toplevel_const(par)->flags & TOP_IS_EXPORT;
    return 0;
  }

  if (par->which == BLOCK) {
    const struct node *pparent = parent_const(par);
    if (pparent->which == DEFFUN || pparent->which == DEFMETHOD) {
      node_toplevel(node)->flags |= node_toplevel_const(pparent)->flags & TOP_IS_EXPORT;
    }
  }

  return 0;
}

STEP_NM(step_stop_already_morningtypepass,
        NM(LET) | NM(ISA) | NM(GENARGS) | NM(FUNARGS) |
        NM(DEFGENARG) | NM(SETGENARG) | NM(DEFFIELD) | NM(DEFCHOICE) |
        NM(WITHIN));
error step_stop_already_morningtypepass(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  *stop = node->typ != NULL;

  return 0;
}

static error pass_early_typing(struct module *mod, struct node *root,
                               void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_block);
    DOWN_STEP(step_stop_already_morningtypepass);
    DOWN_STEP(step_type_destruct_mark);
    ,
    UP_STEP(step_type_inference);
    UP_STEP(step_constraint_inference);
    UP_STEP(step_remove_typeconstraints);
    );
  return 0;
}

static error morningtypepass(struct module *mod, struct node *node) {
  PUSH_STATE(mod->state->step_state);
  bool tentatively_saved = mod->state->tentatively;
  if (mod->state->prev != NULL) {
    mod->state->tentatively |= mod->state->prev->tentatively;
  }

  error e = pass_early_typing(mod, node, NULL, -1);
  EXCEPT(e);

  mod->state->tentatively = tentatively_saved;
  POP_STATE(mod->state->step_state);

  return 0;
}

static STEP_NM(step_type_definitions,
               STEP_NM_DEFS);
static error step_type_definitions(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  ident id = node_ident(subs_first(node));

  if (subs_count_atleast(subs_at(node, IDX_GENARGS), 1)
      && node_toplevel(node)->generic->our_generic_functor_typ != NULL) {
    set_typ(&node->typ, typ_create(NULL, node));
  } else if (mod->path[0] == ID_NLANG
             && (id >= ID_TBI__FIRST && id <= ID_TBI__LAST)) {
    // FIXME Effectively reserving these idents for builtin types, but
    // that's a temporary trick to avoid having to look up the current
    // module path.
    set_typ(&node->typ, typ_create(mod->gctx->builtin_typs_by_name[id], node));
  } else {
    set_typ(&node->typ, typ_create(NULL, node));
  }
  node->flags = NODE_IS_TYPE;

  step_constraint_inference(mod, node, NULL, NULL);

  return 0;
}

static struct node *do_move_detached_member(struct module *mod,
                                            struct node *par,
                                            struct node *node) {
  struct node *nxt = next(node);
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->scope_name == 0) {
    return nxt;
  }
  if (toplevel->generic->our_generic_functor_typ != NULL) {
    assert(parent(node)->which == DEFTYPE);
    return nxt;
  }

  struct node *container = NULL;
  error e = scope_lookup_ident_wontimport(&container, node, mod,
                                          &parent(node)->scope,
                                          toplevel->scope_name, false);
  assert(!e);
  assert(container->which == DEFTYPE);

  toplevel->scope_name = 0;

  node_subs_remove(par, node);
  node_subs_append(container, node);

  struct toplevel *container_toplevel = node_toplevel(container);
  if (container_toplevel->generic != NULL) {
    struct node *copy = node_new_subnode(
      mod, container_toplevel->generic->instances[0]);
    node_deepcopy(mod, copy, toplevel->generic->instances[0]);
  } else {
    const struct node *genargs = subs_at_const(node, IDX_GENARGS);
    if (!subs_count_atleast(genargs, 1)) {
      // Remove uneeded 'generic', added in step_generics_pristine_copy().
      free(toplevel->generic->instances);
      free(toplevel->generic);
      toplevel->generic = NULL;
    }
  }

  if (toplevel->generic != NULL
      || container_toplevel->generic != NULL) {
    // FIXME: This is not strictly correct, but for now, it is necessary to
    // assume that all methods and functions in a generic type are 'inline'.
    toplevel->flags |= TOP_IS_INLINE;
    if (toplevel->generic != NULL) {
      node_toplevel(toplevel->generic->instances[0])->flags |= TOP_IS_INLINE;
    }
  }

  return nxt;
}

static STEP_NM(step_move_detached_members,
               NM(MODULE_BODY));
static error step_move_detached_members(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  for (struct node *s = subs_first(node); s != NULL;) {
    switch (s->which) {
    case DEFFUN:
    case DEFMETHOD:
      s = do_move_detached_member(mod, node, s);
      break;
    default:
      s = next(s);
      break;
    }
  }

  return 0;
}

static STEP_NM(step_lexical_import,
               NM(IMPORT));
static error step_lexical_import(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node_is_at_top(node)) {
    e = lexical_import(&mod->body->scope, mod, node, node);
    EXCEPT(e);
  }

  return 0;
}

static error lexical_retval(struct module *mod, struct node *fun, struct node *retval) {
  error e;

  switch (retval->which) {
  case BIN:
  case UN:
  case IDENT:
  case CALL:
    break;
  case DEFARG:
    e = scope_define(mod, &fun->scope, subs_first(retval), retval);
    EXCEPT(e);
    if (subs_last(retval)->which == TUPLE) {
      e = lexical_retval(mod, fun, subs_last(retval));
      EXCEPT(e);
    }
    break;
  case TUPLE:
    FOREACH_SUB(r, retval) {
      e = lexical_retval(mod, fun, r);
      EXCEPT(e);
    }
    break;
  default:
    e = mk_except(mod, retval, "return value type expression not supported");
    EXCEPT(e);
  }

  return 0;
}

static struct scope *find_scope_for_name(struct node *node) {
  assert(node->which == DEFNAME || node->which == DEFALIAS);

  if (node_ident(node) == ID_OTHERWISE) {
    return NULL;
  } else if (node->as.DEFNAME.is_globalenv) {
    assert(parent(parent(node))->which == MODULE_BODY);
    return &parent(parent(node))->as.MODULE_BODY.globalenv_scope;
  } else if (NM(parent(parent(node))->which)
             & (STEP_NM_DEFS_NO_FUNS | NM(MODULE_BODY))) {
    return &parent(parent(node))->scope;
  } else {
    struct node *p = node;
    do {
      p = parent(p);
    } while (p->which != BLOCK || p->as.BLOCK.is_scopeless);
    return &p->scope;
  }
}

static STEP_NM(step_lexical_scoping,
               NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(DEFINTF) |
               NM(DEFFIELD) | NM(DEFCHOICE) | NM(DEFALIAS) | NM(DEFNAME) |
               NM(WITHIN));
static error step_lexical_scoping(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *id = NULL;
  struct scope *sc = NULL;
  const struct toplevel *toplevel = NULL;
  error e;
  struct node *par = parent(node);

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    id = subs_first(node);
    if (id->which != IDENT) {
      assert(id->which == BIN);
      id = subs_last(id);
    }

    toplevel = node_toplevel_const(node);
    if (toplevel->generic->our_generic_functor_typ != NULL) {
      // See comment below for DEFTYPE/DEFINTF. To get the same instance
      // than the current function, you have to explicitly instantiate it
      // (as there is no 'thisfun').
      sc = NULL;
    } else if (toplevel->scope_name != 0) {
      struct node *container = NULL;
      error e = scope_lookup_ident_wontimport(&container, node, mod,
                                              &parent(node)->scope,
                                              toplevel->scope_name, false);
      EXCEPT(e);
      assert(container->which == DEFTYPE);

      sc = &container->scope;
    } else {
      sc = &par->scope;
    }
    break;
  case DEFTYPE:
  case DEFINTF:
    toplevel = node_toplevel_const(node);
    if (toplevel->generic != NULL
        && toplevel->generic->our_generic_functor_typ != NULL) {
      // For generic instances, do not define the name, as we want the type
      // name to point to the generic functor (e.g. in '(vector u8)', allow
      // 'vector' to be used in '(vector u16)'. To get the current instance,
      // use this or final.
      sc = NULL;
    } else {
      id = subs_first(node);
      sc = &parent(node)->scope;
    }
    break;
  case DEFFIELD:
    id = subs_first(node);
    sc = &parent(node)->scope;
    break;
  case DEFCHOICE:
    id = subs_first(node);
    struct node *p = par;
    while (p->which == DEFCHOICE) {
      p = parent(p);
    }
    sc = &p->scope;
    break;
  case DEFNAME:
  case DEFALIAS:
    id = subs_first(node);
    sc = find_scope_for_name(node);
    break;
  case WITHIN:
    if (subs_count_atleast(node, 1)
        && subs_first(node)->which != WITHIN) {
      struct node *n = subs_first(node);
      if (n->which == BIN) {
        assert(n->as.BIN.operator == TDOT);
        id = subs_last(n);
      } else {
        id = n;
      }

      struct node *pparent = parent(par);
      if (pparent->which == MODULE_BODY) {
        sc = &pparent->scope;
      } else {
        assert(pparent->which == DEFFUN || pparent->which == DEFMETHOD);
        sc = &subs_last(parent(par))->scope;
      }
    }
    break;
  default:
    assert(false && "Unreached");
    return 0;
  }

  if (sc != NULL) {
    e = scope_define(mod, sc, id, node);
    EXCEPT(e);
  }

  struct node *genargs = NULL;
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    genargs = subs_at(node, IDX_GENARGS);
    FOREACH_SUB(ga, genargs) {
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, subs_first(ga), ga);
      EXCEPT(e);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    genargs = subs_at(node, IDX_GENARGS);
    FOREACH_SUB(ga, genargs) {
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, subs_first(ga), ga);
      EXCEPT(e);
    }

    struct node *funargs = subs_at(node, IDX_FUNARGS);
    FOREACH_SUB(arg, funargs) {
      if (next_const(arg) == NULL) {
        break;
      }
      assert(arg->which == DEFARG);
      e = scope_define(mod, &node->scope, subs_first(arg), arg);
      EXCEPT(e);
    }

    e = lexical_retval(mod, node, node_fun_retval(node));
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error define_builtin_alias(struct module *mod, struct node *node,
                                  ident name, struct typ *t) {
  struct node *let = mk_node(mod, node, LET);
  node_subs_remove(node, let);
  node_subs_insert_after(node, subs_at(node, 2), let);
  let->flags = NODE_IS_GLOBAL_LET;
  struct node *defa = mk_node(mod, let, DEFALIAS);
  struct node *n = mk_node(mod, defa, IDENT);
  n->as.IDENT.name = name;
  struct node *expr = mk_node(mod, defa, DIRECTDEF);
  set_typ(&expr->as.DIRECTDEF.typ, t);
  expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

  error e = catchup(mod, NULL, let, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);
  return 0;
}

static STEP_NM(step_add_builtin_members,
               NM(DEFTYPE) | NM(DEFINTF));
static error step_add_builtin_members(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  if (typ_is_pseudo_builtin(node->typ)) {
    return 0;
  }

  define_builtin_alias(mod, node, ID_THIS, node->typ);
  define_builtin_alias(mod, node, ID_FINAL, node->typ);

  return 0;
}

static STEP_NM(step_add_builtin_members_enum_union,
               NM(DEFTYPE));
static error step_add_builtin_members_enum_union(struct module *mod, struct node *node,
                                                 void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->as.DEFTYPE.kind != DEFTYPE_ENUM
      && node->as.DEFTYPE.kind != DEFTYPE_UNION) {
    return 0;
  }

  define_builtin_alias(mod, node, ID_TAG_TYPE, node->as.DEFTYPE.tag_typ);

  return 0;
}

static STEP_NM(step_type_inference_genargs,
               STEP_NM_DEFS);
static error step_type_inference_genargs(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  struct node *genargs = subs_at(node, IDX_GENARGS);
  e = morningtypepass(mod, genargs);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_type_aliases,
               NM(LET));
static error step_type_aliases(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *par = parent(node);
  if ((node_is_at_top(node) || node_is_at_top(par))
      && subs_first(node)->which == DEFALIAS) {
    error e = morningtypepass(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_type_create_update,
               STEP_NM_DEFS);
static error step_type_create_update(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  typ_create_update_genargs(node->typ);
  typ_create_update_hash(node->typ);

  return 0;
}

static STEP_NM(step_type_inference_isalist,
               NM(ISA));
static error step_type_inference_isalist(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  error e = morningtypepass(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_type_update_quickisa,
               STEP_NM_DEFS_NO_FUNS);
static error step_type_update_quickisa(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);

  typ_create_update_quickisa(node->typ);

  return 0;
}

static error validate_genarg_types(struct module *mod, struct node *node) {
  struct node *genargs = subs_at(node, IDX_GENARGS);
  if (!subs_count_atleast(genargs, 1)) {
    return 0;
  }

  const struct typ *t0 = node_toplevel(node)->generic->our_generic_functor_typ;

  size_t n = 0;
  FOREACH_SUB(ga, genargs) {
    if (ga->which == SETGENARG) {
      error e = typ_check_isa(mod, ga->as.SETGENARG.for_error,
                              ga->typ, typ_generic_arg_const(t0, n));
      EXCEPT(e);
      n += 1;
    }
  }

  return 0;
}

static STEP_NM(step_validate_genargs,
               STEP_NM_DEFS);
static error step_validate_genargs(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  // We couldn't do this before -- we needed to wait until all genargs were
  // typed across the compilation unit.
  error e = validate_genarg_types(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_type_lets,
               NM(LET));
static error step_type_lets(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *par = parent(node);
  if ((node_is_at_top(node) || node_is_at_top(par))
      && subs_first(node)->which == DEFNAME) {
    error e = morningtypepass(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_type_deffields,
               NM(DEFCHOICE) | NM(DEFFIELD));
static error step_type_deffields(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);

  error e = morningtypepass(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_type_defchoices,
               NM(DEFTYPE) | NM(DEFCHOICE));
static error step_type_defchoices(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFTYPE) {
    switch (node->as.DEFTYPE.kind) {
    case DEFTYPE_ENUM:
    case DEFTYPE_UNION:
      break;
    default:
      return 0;
    }
  }

  set_typ(&node->as.DEFTYPE.tag_typ,
          typ_create_tentative(TBI_LITERALS_INTEGER));

  error e;
  FOREACH_SUB(ch, node) {
    if (ch->which != DEFCHOICE) {
      continue;
    }

    e = unify(mod, ch, node->as.DEFTYPE.tag_typ,
              subs_at(ch, IDX_CH_TAG_FIRST)->typ);
    EXCEPT(e);
    e = unify(mod, ch, node->as.DEFTYPE.tag_typ,
              subs_at(ch, IDX_CH_TAG_LAST)->typ);
    EXCEPT(e);

    ch->flags |= NODE_IS_DEFCHOICE;
  }

  if (typ_definition_const(node->as.DEFTYPE.tag_typ)->which == DEFINTF
      || typ_equal(node->as.DEFTYPE.tag_typ, TBI_LITERALS_INTEGER)) {
    e = unify(mod, node, node->as.DEFTYPE.tag_typ, TBI_U32);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_type_deffuns,
               NM(DEFMETHOD) | NM(DEFFUN));
static error step_type_deffuns(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);

  error e = morningtypepass(mod, node);
  EXCEPT(e);

  return 0;
}

static void do_mk_expr_abspath(struct module *mod, struct node *node, const char *path, ssize_t len) {
  assert(node->which == BIN);
  node->as.BIN.operator = TDOT;

  for (ssize_t i = len-1; i >= 0; --i) {
    if (i == 0) {
      assert(len > 1);
      ident id = idents_add_string(mod->gctx, path, len - i);

      struct node *root = mk_node(mod, node, DIRECTDEF);
      set_typ(&root->as.DIRECTDEF.typ, mod->gctx->modules_root.typ);
      root->as.DIRECTDEF.flags = NODE_IS_TYPE;
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      break;
    } else if (path[i] == '.') {
      assert(len - i > 1);
      ident id = idents_add_string(mod->gctx, path + i + 1, len - i - 1);

      struct node *down = mk_node(mod, node, BIN);
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      do_mk_expr_abspath(mod, down, path, i);
      break;
    }
  }
}

static struct node *mk_expr_abspath(struct module *mod, struct node *node, const char *path) {
  if (strstr(path, ".") == NULL) {
    struct node *n = mk_node(mod, node, IDENT);
    n->as.IDENT.name = idents_add_string(mod->gctx, path, strlen(path));
    return n;
  }

  struct node *n = mk_node(mod, node, BIN);
  do_mk_expr_abspath(mod, n, path, strlen(path));
  return n;
}

static void add_inferred_isa(struct module *mod, struct node *deft, const char *path) {
  struct node *isalist = subs_at(deft, IDX_ISALIST);
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_is_inline(deft);
  (void)mk_expr_abspath(mod, isa, path);

  error e = catchup(mod, NULL, isa, CATCHUP_BELOW_CURRENT);
  assert(!e);

  typ_create_update_quickisa(deft->typ);
}

static STEP_NM(step_add_builtin_enum_intf,
               NM(DEFTYPE));
static error step_add_builtin_enum_intf(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind != DEFTYPE_ENUM) {
    return 0;
  }

  add_inferred_isa(mod, node, "nlang.builtins.`trivial_copy");
  add_inferred_isa(mod, node, "nlang.builtins.`trivial_dtor");
  add_inferred_isa(mod, node, "nlang.builtins.`enum");

  return 0;
}

static STEP_NM(step_add_builtin_detect_ctor_intf,
               NM(DEFTYPE));
static error step_add_builtin_detect_ctor_intf(struct module *mod, struct node *node,
                                               void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM
      || node->as.DEFTYPE.kind == DEFTYPE_UNION) {
    return 0;
  }

  struct node *proxy = node;
  struct node *ctor = node_get_member(mod, proxy, ID_CTOR);
  if (ctor != NULL) {
    if (node_fun_max_args_count(ctor) == 0) {
      add_inferred_isa(mod, node, "nlang.builtins.`default_ctor");
    } else if (node_fun_max_args_count(ctor) == 1) {
      add_inferred_isa(mod, node, "nlang.builtins.`ctor_with");
    }
  } else {
    // see step_infer_trivial_intfs
  }

  return 0;
}

struct trivials {
  bool ctor, dtor, copy, equality, order, return_by_copy;
};

static void have_trivials(struct trivials *trivials, const struct node *node) {
  if (trivials->ctor && !typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    trivials->ctor = false;
  }
  if (trivials->dtor && !typ_isa(node->typ, TBI_TRIVIAL_DTOR)) {
    trivials->dtor = false;
  }
  if (trivials->copy && !typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
    trivials->copy = false;
  }
  if (trivials->equality && !typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    trivials->equality = false;
  }
  if (trivials->order && !typ_isa(node->typ, TBI_TRIVIAL_ORDER)) {
    trivials->order = false;
  }
  if (trivials->return_by_copy && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    trivials->return_by_copy = false;
  }
}

static STEP_NM(step_infer_trivial_intfs,
               NM(DEFTYPE));
static error step_infer_trivial_intfs(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  struct trivials trivials = { true, true, true, true, true, true };
  FOREACH_SUB_CONST(f, node) {
    if (!(NM(f->which) & (NM(DEFFIELD) | NM(DEFCHOICE)))) {
      continue;
    }

    have_trivials(&trivials, f);
  }

  if (trivials.ctor && node_get_member_const(mod, node, ID_CTOR) == NULL) {
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_ctor");
  }
  if (trivials.dtor && node_get_member_const(mod, node, ID_DTOR) == NULL) {
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_dtor");
  }
  if (trivials.copy && node_get_member_const(mod, node, ID_COPY_CTOR) == NULL) {
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_copy");
  } else {
    trivials.return_by_copy = false;
  }
  if (trivials.equality
      && node_get_member_const(mod, node, ID_OPERATOR_EQ) == NULL
      && node_get_member_const(mod, node, ID_OPERATOR_NE) == NULL) {
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_equality");
  }
  if (trivials.order
      && node_get_member_const(mod, node, ID_OPERATOR_LE) == NULL
      && node_get_member_const(mod, node, ID_OPERATOR_LT) == NULL
      && node_get_member_const(mod, node, ID_OPERATOR_GE) == NULL
      && node_get_member_const(mod, node, ID_OPERATOR_GT) == NULL) {
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_order");
  }
  if (trivials.return_by_copy) {
    add_inferred_isa(mod, node, "nlang.builtins.`return_by_copy");
  }

  return 0;
}

struct intf_proto_rewrite_state {
  struct typ *thi;
  const struct node *proto_parent;
};

static STEP_NM(step_rewrite_final_this,
               NM(IDENT));
static error step_rewrite_final_this(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  struct typ *thi = ((struct intf_proto_rewrite_state *)user)->thi;
  ident id = node_ident(node);
  if (id == ID_THIS) {
    node_set_which(node, DIRECTDEF);
    set_typ(&node->as.DIRECTDEF.typ, thi);
    node->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }
  return 0;
}

static STEP_NM(step_rewrite_local_idents,
               NM(DEFARG) | NM(IDENT));
static error step_rewrite_local_idents(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  const struct node *proto_parent =
    ((struct intf_proto_rewrite_state *)user)->proto_parent;
  if (proto_parent == NULL) {
    return 0;
  }

  if (node->which == DEFARG) {
    subs_first(node)->typ = TBI__NOT_TYPEABLE;
    return 0;
  }
  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  const ident id = node_ident(node);
  if (id == ID_THIS || id == ID_FINAL) {
    return 0;
  }

  struct node *d = NULL;
  error e = scope_lookup_ident_immediate(&d, node, mod, &proto_parent->scope,
                                         id, true);
  if (e == EINVAL) {
    return 0;
  } else if (e) {
    assert(!e);
  }

  node_set_which(node, DIRECTDEF);
  set_typ(&node->as.DIRECTDEF.typ, d->typ);

  return 0;
}

static error pass_rewrite_proto(struct module *mod, struct node *root,
                                void *user, ssize_t shallow_last_up) {
  PASS(DOWN_STEP(step_rewrite_final_this);
       DOWN_STEP(step_rewrite_local_idents),);
  return 0;
}

static void intf_proto_deepcopy(struct module *mod,
                                struct node *dst, struct node *src,
                                struct typ *thi,
                                const struct node *proto_parent) {
  node_deepcopy(mod, dst, src);

  struct intf_proto_rewrite_state st = {
    .thi = thi,
    .proto_parent = proto_parent,
  };

  PUSH_STATE(mod->state->step_state);
  error e = pass_rewrite_proto(mod, dst, &st, -1);
  assert(!e);
  POP_STATE(mod->state->step_state);

  if (node_toplevel(dst) != NULL) {
    node_toplevel(dst)->yet_to_pass = 0;
  }
}

static void define_builtin(struct module *mod, struct node *deft,
                           enum builtingen bg,
                           const struct node *proto_parent) {
  error e;
  struct node *proto;
  if (proto_parent != NULL) {
    const char *n = builtingen_abspath[bg];
    const ident id = idents_add_string(mod->gctx, n, strlen(n));
    e = scope_lookup_ident_immediate(&proto, deft, mod,
                                     &proto_parent->scope,
                                     id, false);
    assert(!e);
  } else {
    e = scope_lookup_abspath(&proto, deft, mod, builtingen_abspath[bg]);
    assert(!e);
  }

  struct node *existing = node_get_member(mod, deft, node_ident(proto));
  if (existing != NULL) {
    return;
  }

  struct node *d = node_new_subnode(mod, deft);
  intf_proto_deepcopy(mod, d, proto, parent(proto)->typ, proto_parent);
  struct node *full_name = mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  node_subs_remove(d, full_name);
  node_subs_replace(d, subs_first(d), full_name);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->builtingen = bg;
  toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  e = catchup(mod, NULL, d, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static STEP_NM(step_add_builtin_ctor,
               NM(DEFTYPE));
static error step_add_builtin_ctor(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  return 0;
}

static STEP_NM(step_add_builtin_dtor,
               NM(DEFTYPE));
static error step_add_builtin_dtor(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  return 0;
}

static error define_auto(struct module *mod, struct node *deft,
                         enum builtingen bg) {
  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, deft, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *existing = node_get_member(mod, deft, node_ident(proto));
  if (existing != NULL) {
    return 0;
  }

  struct node *ctor = node_get_member(mod, deft, ID_CTOR);
  if (ctor == NULL) {
    // FIXME This should be narrower and only in the case the type cannot be
    // given an automatically generated ctor.
    e = mk_except_type(mod, deft, "type '%s' is not `trivial_ctor and has no 'ctor'",
                       typ_pretty_name(mod, deft->typ));
    THROW(e);
  }

  struct node *d = node_new_subnode(mod, deft);
  intf_proto_deepcopy(mod, d, proto, parent(proto)->typ, NULL);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->builtingen = bg;
  toplevel->flags |= node_toplevel(ctor)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  struct node *ctor_funargs = subs_at(ctor, IDX_FUNARGS);
  struct node *d_funargs = subs_at(d, IDX_FUNARGS);
  struct node *d_retval = subs_last(d_funargs);
  FOREACH_SUB_EVERY(arg, ctor_funargs, 1, 1) {
    if (next_const(arg) == NULL) {
      // Skip self.
      break;
    }
    struct node *cpy = node_new_subnode(mod, d_funargs);
    node_subs_remove(d_funargs, cpy);
    node_subs_insert_before(d_funargs, d_retval, cpy);
    intf_proto_deepcopy(mod, cpy, arg, parent(proto)->typ, NULL);
  }

  assert(node_toplevel(d)->yet_to_pass == 0);
  e = catchup(mod, NULL, d, CATCHUP_BELOW_CURRENT);
  assert(!e);

  return 0;
}

static STEP_NM(step_add_builtin_mk_new,
               NM(DEFTYPE));
static error step_add_builtin_mk_new(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_UNION
      || node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    define_builtin(mod, node, BG_TRIVIAL_CTOR_CTOR, NULL);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_MK, NULL);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_NEW, NULL);
  } else if (typ_isa(node->typ, TBI_DEFAULT_CTOR)) {
    define_builtin(mod, node, BG_DEFAULT_CTOR_MK, NULL);
    define_builtin(mod, node, BG_DEFAULT_CTOR_NEW, NULL);
  } else if (typ_isa(node->typ, TBI_ANY_TUPLE)) {
    // noop
  } else {
    error e = define_auto(mod, node, BG_AUTO_MK);
    EXCEPT(e);
    e = define_auto(mod, node, BG_AUTO_NEW);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_add_builtin_mkv_newv,
               NM(DEFTYPE));
static error step_add_builtin_mkv_newv(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_ARRAY_CTOR)) {
    define_builtin(mod, node, BG_AUTO_MKV, NULL);
    define_builtin(mod, node, BG_AUTO_NEWV, NULL);
  }

  return 0;
}

static STEP_NM(step_add_trivials,
               NM(DEFTYPE));
static error step_add_trivials(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);

  // FIXME: We should check that the fields/defchoice do indeed support
  // these trivial interfaces. It must be safe to declare them.
  // Same thing for trivial ctor, dtor.

  if (typ_is_pseudo_builtin(node->typ)
      || typ_is_reference(node->typ)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
    define_builtin(mod, node, BG_TRIVIAL_COPY_COPY_CTOR, NULL);
  }
  if (typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_EQ, NULL);
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_NE, NULL);
  }

  return 0;
}

static error add_environment_builtins_eachisalist(struct module *mod,
                                                  struct typ *t,
                                                  struct typ *intf,
                                                  bool *stop,
                                                  void *user) {
  struct node *deft = user;
  if (typ_generic_arity(intf) == 0
      || !typ_equal(typ_generic_functor_const(intf), TBI_ENVIRONMENT)) {
    return 0;
  }

  const struct node *dintf = typ_definition_const(intf);
  define_builtin(mod, deft, BG_ENVIRONMENT_PARENT, dintf);
  define_builtin(mod, deft, BG_ENVIRONMENT_INSTALL, dintf);
  define_builtin(mod, deft, BG_ENVIRONMENT_UNINSTALL, dintf);
  return 0;
}

static STEP_NM(step_add_environment_builtins,
               NM(DEFTYPE));
static error step_add_environment_builtins(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);

  if (!typ_isa(node->typ, TBI_ANY_ENVIRONMENT)) {
    return 0;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                add_environment_builtins_eachisalist, node);
  EXCEPT(e);
  return 0;
}

static STEP_NM(step_rewrite_def_return_through_ref,
               NM(DEFFUN) | NM(DEFMETHOD));
static error step_rewrite_def_return_through_ref(struct module *mod, struct node *node,
                                                 void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *retval = node_fun_retval(node);
  if (typ_isa(retval->typ, TBI_RETURN_BY_COPY)) {
    return 0;
  }

  if (retval->which == DEFARG) {
    return 0;
  }

  struct node *funargs = subs_at(node, IDX_FUNARGS);
  struct node *named = mk_node(mod, funargs, DEFARG);
  named->as.DEFARG.is_retval = true;
  struct node *name = mk_node(mod, named, IDENT);
  name->as.IDENT.name = ID_NRETVAL;
  node_subs_remove(funargs, named);
  node_subs_replace(funargs, retval, named);
  node_subs_append(named, retval);

  error e = lexical_retval(mod, node, named);
  EXCEPT(e);

  const struct node *except[] = { retval, NULL };
  e = catchup(mod, except, named, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  set_typ(&name->typ, TBI__NOT_TYPEABLE);
  set_typ(&named->typ, retval->typ);

  return 0;
}

static error passfwd0(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // scoping_deftypes
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_codeloc_for_generated);
    DOWN_STEP(step_export_pre_post_invariant);
    DOWN_STEP(step_lexical_scoping);
    ,
    UP_STEP(step_type_definitions);
    UP_STEP(step_complete_instantiation);
    UP_STEP(step_move_detached_members);
    );
  return 0;
}

static error passfwd1(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // imports
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_lexical_import);
    DOWN_STEP(step_add_builtin_members);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd2(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_genargs
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_type_inference_genargs);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_type_aliases);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd3(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_isalist
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_type_create_update);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_type_inference_isalist);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd4(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_complete_create
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_validate_genargs);
    DOWN_STEP(step_type_update_quickisa);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    ,
    );
  return 0;
}

static error passfwd5(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_def_lets
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_type_lets);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd6(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_deffields
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_type_deffields);
    UP_STEP(step_type_defchoices);
    UP_STEP(step_add_builtin_members_enum_union);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd7(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_add_builtin_intf
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_add_builtin_enum_intf);
    UP_STEP(step_add_builtin_detect_ctor_intf);
    UP_STEP(step_infer_trivial_intfs);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd8(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_deffuns
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_push_top_state);
    ,
    UP_STEP(step_type_deffuns);
    UP_STEP(step_add_builtin_ctor);
    UP_STEP(step_add_builtin_dtor);
    UP_STEP(step_add_builtin_mk_new);
    UP_STEP(step_add_builtin_mkv_newv);
    UP_STEP(step_add_trivials);
    UP_STEP(step_add_environment_builtins);
    UP_STEP(step_rewrite_def_return_through_ref);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

a_pass passfwd[] = {
  passfwd0,
  passfwd1,
  passfwd2,
  passfwd3,
  passfwd4,
  passfwd5,
  passfwd6,
  passfwd7,
  passfwd8,
};
