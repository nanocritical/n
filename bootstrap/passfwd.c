#include "passfwd.h"

#include "table.h"
#include "types.h"
#include "scope.h"
#include "unify.h"
#include "import.h"

#include "passes.h"
#include "passzero.h"
#include "passbody.h"

static STEP_FILTER(step_codeloc_for_generated,
                   -1);
static error step_codeloc_for_generated(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  if (node->codeloc == 0
      && node->scope.parent != NULL) {
    node->codeloc = node_parent(node)->codeloc;
  }

  return 0;
}

static STEP_FILTER(step_export_pre_post_invariant,
                   SF(PRE) | SF(POST) | SF(INVARIANT));
static error step_export_pre_post_invariant(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  const struct node *parent = node_parent_const(node);
  if (parent->which == DEFTYPE || parent->which == DEFINTF) {
    node_toplevel(node)->flags |= node_toplevel_const(parent)->flags & TOP_IS_EXPORT;
    return 0;
  }

  if (parent->which == BLOCK) {
    const struct node *pparent = node_parent_const(parent);
    if (pparent->which == DEFFUN || pparent->which == DEFMETHOD) {
      node_toplevel(node)->flags |= node_toplevel_const(pparent)->flags & TOP_IS_EXPORT;
    }
  }

  return 0;
}

STEP_FILTER(step_stop_already_morningtypepass,
            SF(LET) | SF(ISA) | SF(GENARGS) | SF(FUNARGS) | SF(DEFGENARG) |
            SF(SETGENARG) | SF(DEFFIELD) | SF(DEFCHOICE));
error step_stop_already_morningtypepass(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == LET) {
    if (node_is_at_top(node) || node_is_at_top(node_parent_const(node))) {
      *stop = node->typ != NULL;
    }
    return 0;
  }

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

static STEP_FILTER(step_type_definitions,
                   STEP_FILTER_DEFS);
static error step_type_definitions(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  ident id = node_ident(node_subs_first(node));

  if (node_subs_count_atleast(node_subs_at(node, IDX_GENARGS), 1)
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

  return 0;
}

static struct node *do_move_detached_member(struct module *mod,
                                            struct node *parent,
                                            struct node *node) {
  struct node *next = node_next(node);
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->scope_name == 0) {
    return next;
  }
  if (toplevel->generic->our_generic_functor_typ != NULL) {
    assert(node_parent(node)->which == DEFTYPE);
    return next;
  }

  struct node *container = NULL;
  error e = scope_lookup_ident_wontimport(&container, node, mod, node->scope.parent,
                                          toplevel->scope_name, FALSE);
  assert(!e);
  assert(container->which == DEFTYPE);

  toplevel->scope_name = 0;

  node_subs_remove(parent, node);
  node_subs_append(container, node);
  node->scope.parent = &container->scope;

  struct toplevel *container_toplevel = node_toplevel(container);
  if (container_toplevel->generic != NULL
      && container_toplevel->generic->instances_count > 0) {
    struct node *copy = node_new_subnode(
      mod, container_toplevel->generic->instances[0]);
    node_deepcopy(mod, copy, toplevel->generic->instances[0]);
  }

  return next;
}

static STEP_FILTER(step_move_detached_members,
                   SF(MODULE_BODY));
static error step_move_detached_members(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  for (struct node *s = node_subs_first(node); s != NULL;) {
    switch (s->which) {
    case DEFFUN:
    case DEFMETHOD:
      s = do_move_detached_member(mod, node, s);
      break;
    default:
      s = node_next(s);
      break;
    }
  }

  return 0;
}

static STEP_FILTER(step_lexical_import,
                   SF(IMPORT));
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
    e = scope_define(mod, &fun->scope, node_subs_first(retval), retval);
    EXCEPT(e);
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

static error insert_tupleextract(struct module *mod, size_t arity, struct node *expr) {
  struct scope *parent_scope = expr->scope.parent;

  struct node *value = node_new_subnode(mod, expr);
  node_subs_remove(expr, value);
  node_move_content(value, expr);

  node_set_which(expr, TUPLEEXTRACT);

  for (size_t n = 0; n < arity; ++n) {
    struct node *nth = mk_node(mod, expr, TUPLENTH);
    nth->as.TUPLENTH.nth = n;
  }

  node_subs_append(expr, value);

  const struct node *except[] = { value, NULL };
  error e = catchup(mod, except, expr, parent_scope, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error extract_defnames_in_pattern(struct module *mod, struct node *defpattern,
                                         struct node *pattern, struct node *expr) {
  struct node *def;
  error e;

  if (expr != NULL
      && pattern->which != expr->which
      && pattern->which != IDENT
      && pattern->which != EXCEP) {
    if (pattern->which == TUPLE) {
      e = insert_tupleextract(mod, node_subs_count(pattern), expr);
      EXCEPT(e);
    } else {
      e = mk_except(mod, pattern, "value destruct not supported");
      THROW(e);
    }
  }

#define UNLESS_NULL(n, sub) ( (n) != NULL ? (sub) : NULL )

  switch (pattern->which) {
  case EXCEP:
    {
      struct node *parent = node_parent(pattern);

      struct node *label_ident = NULL;
      if (node_subs_count_atleast(pattern, 1)) {
        label_ident = node_subs_first(pattern);
      }

      struct node *pattern_id = mk_node(mod, parent, IDENT);
      pattern_id->as.IDENT.name = gensym(mod);
      node_subs_remove(parent, pattern_id);
      node_subs_replace(parent, pattern, pattern_id);

      e = catchup(mod, NULL, pattern_id, &defpattern->scope, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);

      def = mk_node(mod, defpattern, DEFNAME);
      def->as.DEFNAME.pattern = pattern_id;
      def->as.DEFNAME.expr = expr;
      def->as.DEFNAME.is_excep = TRUE;
      def->as.DEFNAME.excep_label_ident = label_ident;

      struct node *for_test = mk_node(mod, def, IDENT);
      for_test->as.IDENT.name = node_ident(pattern_id);

      e = catchup(mod, NULL, def, &defpattern->scope, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);

      return 0;
    }

  case IDENT:
    def = mk_node(mod, defpattern, DEFNAME);
    def->as.DEFNAME.pattern = pattern;
    def->as.DEFNAME.expr = expr;

    e = catchup(mod, NULL, def, &defpattern->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);

    return 0;

  case UN:
    assert(FALSE && "FIXME: Unsupported");
    e = extract_defnames_in_pattern(mod, defpattern, node_subs_first(pattern),
                                    UNLESS_NULL(expr, node_subs_first(expr)));
    EXCEPT(e);
    break;
  case TUPLE:
    for (struct node *p = node_subs_first(pattern),
         *ex = UNLESS_NULL(expr, node_subs_first(expr)); p != NULL;
         p = node_next(p), ex = UNLESS_NULL(expr, node_next(ex))) {
      e = extract_defnames_in_pattern(mod, defpattern, p,
                                      UNLESS_NULL(expr, ex));
      EXCEPT(e);
    }
    break;
  case TYPECONSTRAINT:
    pattern->as.TYPECONSTRAINT.in_pattern = TRUE;
    e = extract_defnames_in_pattern(mod, defpattern, node_subs_first(pattern),
                                    UNLESS_NULL(expr, node_subs_first(expr)));
    EXCEPT(e);
    break;
  default:
    e = mk_except(mod, pattern, "invalid construct in pattern");
    THROW(e);
  }
#undef UNLESS_NULL

  return 0;
}

static STEP_FILTER(step_defpattern_extract_defname,
                   SF(DEFPATTERN));
static error step_defpattern_extract_defname(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *expr = NULL;
  if (node_subs_count_atleast(node, 2)) {
    expr = node_subs_at(node, 1);
  }

  error e = extract_defnames_in_pattern(mod, node, node_subs_first(node), expr);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_lexical_scoping,
                   SF(DEFFUN) | SF(DEFMETHOD) | SF(DEFTYPE) | SF(DEFINTF) |
                   SF(DEFFIELD) | SF(DEFCHOICE) | SF(DEFNAME) | SF(CATCH));
static error step_lexical_scoping(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *id = NULL;
  struct scope *sc = NULL;
  const struct toplevel *toplevel = NULL;
  error e;

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    id = node_subs_first(node);
    if (id->which != IDENT) {
      assert(id->which == BIN);
      id = node_subs_last(id);
    }

    toplevel = node_toplevel_const(node);
    if (toplevel->generic->our_generic_functor_typ != NULL) {
      // See comment below for DEFTYPE/DEFINTF. To get the same instance
      // than the current function, you have to explicitly instantiate it
      // (as there is no 'thisfun').
      sc = NULL;
    } else if (toplevel->scope_name != 0) {
      struct node *container = NULL;
      error e = scope_lookup_ident_wontimport(&container, node, mod, node->scope.parent,
                                              toplevel->scope_name, FALSE);
      EXCEPT(e);
      assert(container->which == DEFTYPE);

      sc = &container->scope;
    } else {
      sc = &node_parent(node)->scope;
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
      id = node_subs_first(node);
      sc = node->scope.parent;
    }
    break;
  case DEFFIELD:
  case DEFCHOICE:
    id = node_subs_first(node);
    sc = node->scope.parent;
    break;
  case DEFNAME:
    if (node_parent(node)->as.DEFPATTERN.is_globalenv) {
      id = node->as.DEFNAME.pattern;
      assert(node_parent(node_parent(node_parent(node)))->which == MODULE_BODY);
      sc = &node_parent(node_parent(node_parent(node)))
        ->as.MODULE_BODY.globalenv_scope;
    } else if (node_ident(node) != ID_OTHERWISE) {
      id = node->as.DEFNAME.pattern;
      sc = node->scope.parent->parent->parent;
    }
    break;
  case CATCH:
    break;
  default:
    assert(FALSE && "Unreached");
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
    genargs = node_subs_at(node, IDX_GENARGS);
    FOREACH_SUB(ga, genargs) {
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, node_subs_first(ga), ga);
      EXCEPT(e);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    genargs = node_subs_at(node, IDX_GENARGS);
    FOREACH_SUB(ga, genargs) {
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, node_subs_first(ga), ga);
      EXCEPT(e);
    }

    struct node *funargs = node_subs_at(node, IDX_FUNARGS);
    FOREACH_SUB(arg, funargs) {
      if (node_next_const(arg) == NULL) {
        break;
      }
      assert(arg->which == DEFARG);
      e = scope_define(mod, &node->scope, node_subs_first(arg), arg);
      EXCEPT(e);
    }

    e = lexical_retval(mod, node, node_fun_retval(node));
    EXCEPT(e);
    break;
  case CATCH:
    if (node->as.CATCH.is_user_label) {
      e = scope_define_ident(mod, node->scope.parent, node->as.CATCH.label, node);
      EXCEPT(e);
    }
    break;
  default:
    break;
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_members,
                   SF(DEFTYPE) | SF(DEFINTF));
static error step_add_builtin_members(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  if (typ_is_pseudo_builtin(node->typ)) {
    return 0;
  }

  {
    struct node *let = mk_node(mod, node, LET);
    node_subs_remove(node, let);
    node_subs_insert_after(node, node_subs_at(node, 2), let);
    let->flags = NODE_IS_GLOBAL_LET;
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_THIS;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    error e = catchup(mod, NULL, let, &node->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  {
    struct node *let = mk_node(mod, node, LET);
    node_subs_remove(node, let);
    node_subs_insert_after(node, node_subs_at(node, 3), let);
    let->flags = NODE_IS_GLOBAL_LET;
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_FINAL;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    error e = catchup(mod, NULL, let, &node->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  return 0;
}

static STEP_FILTER(step_type_inference_genargs,
                   STEP_FILTER_DEFS);
static error step_type_inference_genargs(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  struct node *genargs = node_subs_at(node, IDX_GENARGS);
  e = morningtypepass(mod, genargs);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_type_create_update,
                   STEP_FILTER_DEFS);
static error step_type_create_update(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  typ_create_update_genargs(node->typ);
  typ_create_update_hash(node->typ);

  return 0;
}

static STEP_FILTER(step_type_inference_isalist,
                   SF(ISA));
static error step_type_inference_isalist(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  error e = morningtypepass(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_type_update_quickisa,
                   STEP_FILTER_DEFS_NO_FUNS);
static error step_type_update_quickisa(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);

  typ_create_update_quickisa(node->typ);

  return 0;
}

static STEP_FILTER(step_type_lets,
                   SF(LET));
static error step_type_lets(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *parent = node_parent(node);
  if (node_is_at_top(node) || node_is_at_top(parent)) {
    error e = morningtypepass(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_FILTER(step_type_deffields,
                   SF(DEFCHOICE) | SF(DEFFIELD));
static error step_type_deffields(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);

  error e = morningtypepass(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_type_defchoices,
                   SF(DEFTYPE));
static error step_type_defchoices(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_ENUM:
  case DEFTYPE_UNION:
    {
      set_typ(&node->as.DEFTYPE.choice_typ,
              typ_create_tentative(TBI_LITERALS_INTEGER));
      struct typ *u = node->as.DEFTYPE.choice_typ;

      FOREACH_SUB(ch, node) {
        if (ch->which != DEFCHOICE) {
          continue;
        }

        e = unify(mod, ch, u, node_subs_at(ch, IDX_CH_VALUE)->typ);
        EXCEPT(e);

        ch->flags |= NODE_IS_DEFCHOICE;
      }

      if (typ_equal(u, TBI_LITERALS_INTEGER)) {
        typ_link_tentative(TBI_U32, u);
      }
    }
    break;
  default:
    break;
  }

  return 0;
}

static STEP_FILTER(step_type_deffuns,
                   SF(DEFMETHOD) | SF(DEFFUN));
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
  struct node *isalist = node_subs_at(deft, IDX_ISALIST);
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_is_inline(deft);
  (void)mk_expr_abspath(mod, isa, path);

  error e = catchup(mod, NULL, isa, &isalist->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);

  typ_create_update_quickisa(deft->typ);
}

static STEP_FILTER(step_add_builtin_enum_intf,
                   SF(DEFTYPE));
static error step_add_builtin_enum_intf(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind != DEFTYPE_ENUM) {
    return 0;
  }

  add_inferred_isa(mod, node, "nlang.builtins.`trivial_copy");
  add_inferred_isa(mod, node, "nlang.builtins.`trivial_dtor");

  return 0;
}

static STEP_FILTER(step_add_builtin_detect_ctor_intf,
                   SF(DEFTYPE));
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
    add_inferred_isa(mod, node, "nlang.builtins.`trivial_ctor");
  }

  return 0;
}

struct intf_proto_rewrite_state {
  struct typ *thi;
  const struct node *proto_parent;
};

static STEP_FILTER(step_rewrite_final_this,
                   SF(IDENT));
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

static STEP_FILTER(step_rewrite_local_idents,
                   SF(DEFARG) | SF(IDENT));
static error step_rewrite_local_idents(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  const struct node *proto_parent =
    ((struct intf_proto_rewrite_state *)user)->proto_parent;
  if (proto_parent == NULL) {
    return 0;
  }

  if (node->which == DEFARG) {
    node_subs_first(node)->typ = TBI__NOT_TYPEABLE;
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
                                         id, TRUE);
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
                                     id, FALSE);
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
  intf_proto_deepcopy(mod, d, proto, node_parent(proto)->typ, proto_parent);
  struct node *full_name = mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  node_subs_remove(d, full_name);
  node_subs_replace(d, node_subs_first(d), full_name);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->builtingen = bg;
  toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  e = catchup(mod, NULL, d, &deft->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void define_defchoice_builtin(struct module *mod, struct node *ch,
                                     enum builtingen bg) {
  struct node *deft = node_parent(ch);

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, ch, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = node_new_subnode(mod, ch);
  intf_proto_deepcopy(mod, d, proto, node_parent(proto)->typ, NULL);
  struct node *full_name = mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  node_subs_remove(d, full_name);
  node_subs_replace(d, node_subs_first(d), full_name);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->builtingen = bg;
  toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  if (bg == BG_UNION_CTOR_WITH_CTOR
      || bg == BG_UNION_CTOR_WITH_MK
      || bg == BG_UNION_CTOR_WITH_NEW) {
    struct node *funargs = node_subs_at(d, IDX_FUNARGS);
    struct node *arg = mk_node(mod, funargs, DEFARG);
    struct node *name = mk_node(mod, arg, IDENT);
    name->as.IDENT.name = ID_C;
    struct node *typename = mk_node(mod, arg, DIRECTDEF);
    set_typ(&typename->as.DIRECTDEF.typ, node_subs_at(ch, IDX_CH_PAYLOAD)->typ);
    typename->as.DIRECTDEF.flags = NODE_IS_TYPE;

    node_subs_remove(funargs, arg);
    node_subs_insert_before(
      funargs, node_subs_at(funargs, (d->which == DEFMETHOD) ? 1 : 0), arg);
  }

  e = catchup(mod, NULL, d, &ch->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static STEP_FILTER(step_add_builtin_defchoice_constructors,
                   SF(DEFCHOICE));
static error step_add_builtin_defchoice_constructors(struct module *mod, struct node *node,
                                                     void *user, bool *stop) {
  DSTEP(mod, node);

  const struct node *deft = node_parent(node);
  if (deft->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  const struct node *arg = node_subs_at(node, IDX_CH_PAYLOAD);
  error e = typ_check_isa(mod, arg, arg->typ, TBI_COPYABLE);
  EXCEPT(e);

  define_defchoice_builtin(mod, node, BG_UNION_CTOR_WITH_CTOR);

  return 0;
}

static STEP_FILTER(step_add_builtin_ctor,
                   SF(DEFTYPE));
static error step_add_builtin_ctor(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_dtor,
                   SF(DEFTYPE));
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

  struct node *ctor = NULL;
  e = scope_lookup_ident_immediate(&ctor, deft, mod, &deft->scope,
                                   ID_CTOR, TRUE);
  if (e) {
    // FIXME This should be narrower and only in the case the type cannot be
    // given an automatically generated ctor.
    e = mk_except_type(mod, deft, "type '%s' is not `trivial_ctor and has no 'ctor'",
                       typ_pretty_name(mod, deft->typ));
    THROW(e);
  }

  struct node *d = node_new_subnode(mod, deft);
  intf_proto_deepcopy(mod, d, proto, node_parent(proto)->typ, NULL);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->builtingen = bg;
  toplevel->flags |= node_toplevel(ctor)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  struct node *ctor_funargs = node_subs_at(ctor, IDX_FUNARGS);
  struct node *d_funargs = node_subs_at(d, IDX_FUNARGS);
  struct node *d_retval = node_subs_last(d_funargs);
  FOREACH_SUB_EVERY(arg, ctor_funargs, 1, 1) {
    if (node_next_const(arg) == NULL) {
      // Skip self.
      break;
    }
    struct node *cpy = node_new_subnode(mod, d_funargs);
    node_subs_remove(d_funargs, cpy);
    node_subs_insert_before(d_funargs, d_retval, cpy);
    intf_proto_deepcopy(mod, cpy, arg, node_parent(proto)->typ, NULL);
  }

  assert(node_toplevel(d)->yet_to_pass == 0);
  e = catchup(mod, NULL, d, &deft->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);

  return 0;
}

static STEP_FILTER(step_add_builtin_mk_new,
                   SF(DEFTYPE));
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
  } else if (typ_isa(node->typ, TBI_CTOR_WITH)) {
    define_builtin(mod, node, BG_CTOR_WITH_MK, NULL);
    define_builtin(mod, node, BG_CTOR_WITH_NEW, NULL);
  } else {
    error e = define_auto(mod, node, BG_AUTO_MK);
    EXCEPT(e);
    e = define_auto(mod, node, BG_AUTO_NEW);
    EXCEPT(e);
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_mkv_newv,
                   SF(DEFTYPE));
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

static STEP_FILTER(step_add_builtin_defchoice_mk_new,
                   SF(DEFCHOICE));
static error step_add_builtin_defchoice_mk_new(struct module *mod, struct node *node,
                                               void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFCHOICE) {
    return 0;
  }

  struct node *deft = node_parent(node);
  assert(deft->which == DEFTYPE);
  if (deft->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_MK);
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_NEW);
  } else if (deft->as.DEFTYPE.kind == DEFTYPE_UNION) {
    define_defchoice_builtin(mod, node, BG_UNION_CTOR_WITH_MK);
    define_defchoice_builtin(mod, node, BG_UNION_CTOR_WITH_NEW);
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_operators,
                   SF(DEFTYPE));
static error step_add_builtin_operators(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_is_extern(node)) {
    return 0;
  }

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
    break;
  case DEFTYPE_STRUCT:
    break;
  case DEFTYPE_UNION:
    break;
  case DEFTYPE_ENUM:
    if (!typ_isa(node->as.DEFTYPE.choice_typ, TBI_NATIVE_INTEGER)) {
      define_builtin(mod, node, BG_ENUM_EQ, NULL);
      define_builtin(mod, node, BG_ENUM_NE, NULL);
    }
    break;
  }

  return 0;
}

static STEP_FILTER(step_add_trivials,
                   SF(DEFTYPE));
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

static void define_dispatch(struct module *mod, struct node *deft, struct typ *tintf) {
  struct node *intf = typ_definition(tintf);

  struct node *modbody = node_parent(deft);

  FOREACH_SUB(proto, intf) {
    if (proto->which != DEFMETHOD && proto->which != DEFFUN) {
      continue;
    }
    if (node_toplevel_const(proto)->flags & TOP_IS_NOT_DYN) {
      continue;
    }

    struct node *existing = node_get_member(mod, deft, node_ident(proto));
    if (existing != NULL) {
      return;
    }

    struct node *d = node_new_subnode(mod, modbody);
    node_subs_remove(modbody, d);
    node_subs_insert_after(modbody, deft, d);
    intf_proto_deepcopy(mod, d, proto, node_parent(proto)->typ, NULL);
    char *abspath = scope_name(mod, &proto->scope);
    struct node *full_name = mk_expr_abspath(mod, d, abspath);
    node_subs_remove(d, full_name);
    node_subs_replace(d, node_subs_first(d), full_name);

    struct toplevel *toplevel = node_toplevel(d);
    toplevel->builtingen = BG_UNION_DISPATCH;
    toplevel->flags &= ~TOP_IS_PROTOTYPE;
    toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

    error e = catchup(mod, NULL, d, &deft->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

static error union_choice_with_intf(struct module *mod, struct typ *t,
                                  struct typ *intf, bool *stop, void *user) {
  struct node *node = user;

  struct typ *to_check = intf;
  if (typ_equal(intf, TBI_UNION_COPY)) {
    to_check = TBI_COPYABLE;
  } else if (typ_equal(intf, TBI_UNION_EQUALITY)) {
    to_check = TBI_HAS_EQUALITY;
  } else if (typ_equal(intf, TBI_UNION_ORDER)) {
    to_check = TBI_ORDERED;
  }

  FOREACH_SUB(ch, node) {
    if (ch->which != DEFCHOICE) {
      continue;
    }

    struct typ *tch = NULL;
    if (typ_equal(node_subs_at(ch, IDX_CH_PAYLOAD)->typ, TBI_VOID)) {
      tch = node->as.DEFTYPE.choice_typ;
    } else {
      tch = node_subs_at(ch, IDX_CH_PAYLOAD)->typ;
    }

    error e = typ_check_isa(mod, ch, tch, to_check);
    EXCEPT(e);
  }

  if (typ_equal(intf, TBI_UNION_COPY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
      define_builtin(mod, node, BG_UNION_COPY, NULL);
    }
  } else if (typ_equal(intf, TBI_UNION_EQUALITY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
      define_builtin(mod, node, BG_UNION_EQUALITY_EQ, NULL);
      define_builtin(mod, node, BG_UNION_EQUALITY_NE, NULL);
    }
  } else if (typ_equal(intf, TBI_UNION_ORDER)) {
    define_builtin(mod, node, BG_UNION_ORDER_LE, NULL);
    define_builtin(mod, node, BG_UNION_ORDER_LT, NULL);
    define_builtin(mod, node, BG_UNION_ORDER_GT, NULL);
    define_builtin(mod, node, BG_UNION_ORDER_GE, NULL);
  } else {
    define_dispatch(mod, node, intf);
  }

  return 0;
}

static STEP_FILTER(step_add_union_dispatch,
                   SF(DEFTYPE));
static error step_add_union_dispatch(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
  case DEFTYPE_ENUM:
  case DEFTYPE_STRUCT:
    return 0;
  case DEFTYPE_UNION:
    break;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                union_choice_with_intf, node);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_rewrite_def_return_through_ref,
                   SF(DEFFUN) | SF(DEFMETHOD));
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

  struct node *funargs = node_subs_at(node, IDX_FUNARGS);
  struct node *named = mk_node(mod, funargs, DEFARG);
  named->as.DEFARG.is_retval = TRUE;
  struct node *name = mk_node(mod, named, IDENT);
  name->as.IDENT.name = ID_NRETVAL;
  node_subs_remove(funargs, named);
  node_subs_replace(funargs, retval, named);
  node_subs_append(named, retval);

  error e = lexical_retval(mod, node, named);
  EXCEPT(e);

  const struct node *except[] = { retval, NULL };
  e = catchup(mod, except, named, &node->scope, CATCHUP_BELOW_CURRENT);
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
    DOWN_STEP(step_defpattern_extract_defname);
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
    DOWN_STEP(step_type_update_quickisa);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_funblock);
    ,
    );
  return 0;
}

static error passfwd5(struct module *mod, struct node *root,
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
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passfwd6(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // type_lets
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

static error passfwd7(struct module *mod, struct node *root,
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
    UP_STEP(step_add_builtin_defchoice_mk_new);
    UP_STEP(step_add_builtin_defchoice_constructors);
    UP_STEP(step_add_builtin_ctor);
    UP_STEP(step_add_builtin_dtor);
    UP_STEP(step_add_builtin_mk_new);
    UP_STEP(step_add_builtin_mkv_newv);
    UP_STEP(step_add_builtin_operators);
    UP_STEP(step_add_trivials);
    UP_STEP(step_add_union_dispatch);
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
