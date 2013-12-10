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
    UP_STEP(step_type_inference));
  return 0;
}

static error morningtypepass(struct module *mod, struct node *node) {
  PUSH_STATE(mod->state->step_state);
  if (mod->state->prev != NULL) {
    mod->state->tentatively |= mod->state->prev->tentatively;
  }

  error e = pass_early_typing(mod, node, NULL, -1);
  EXCEPT(e);

  POP_STATE(mod->state->step_state);

  return 0;
}

static STEP_FILTER(step_type_definitions,
                   STEP_FILTER_DEFS);
static error step_type_definitions(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  ident id = node_ident(node->subs[0]);

  if (node->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel(node)->our_generic_functor_typ != NULL) {
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
    e = scope_define(mod, &fun->scope, retval->subs[0], retval);
    EXCEPT(e);
    break;
  case TUPLE:
    for (size_t n = 0; n < retval->subs_count; ++n) {
      struct node *r = retval->subs[n];
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
  struct node copy = *expr;

  memset(expr, 0, sizeof(*expr));
  expr->which = TUPLEEXTRACT;
  for (size_t n = 0; n < arity; ++n) {
    struct node *nth = mk_node(mod, expr, TUPLENTH);
    nth->as.TUPLENTH.nth = n;
  }
  struct node *value = node_new_subnode(mod, expr);
  *value = copy;
  fix_scopes_after_move(value);
  assert(value->typ == NULL);

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
      e = insert_tupleextract(mod, pattern->subs_count, expr);
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
      const size_t where = rew_find_subnode_in_parent(parent, pattern);

      struct node *label_ident = NULL;
      if (pattern->subs_count > 0) {
        label_ident = pattern->subs[0];
      }

      pattern = mk_node(mod, parent, IDENT);
      pattern->as.IDENT.name = gensym(mod);
      rew_move_last_over(parent, where, FALSE);

      e = catchup(mod, NULL, pattern, &defpattern->scope, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);

      def = mk_node(mod, defpattern, DEFNAME);
      def->as.DEFNAME.pattern = pattern;
      def->as.DEFNAME.expr = expr;
      def->as.DEFNAME.is_excep = TRUE;
      def->as.DEFNAME.excep_label_ident = label_ident;

      struct node *test = mk_node(mod, def, IDENT);
      test->as.IDENT.name = node_ident(pattern);

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
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
    EXCEPT(e);
    break;
  case TUPLE:
    for (size_t n = 0; n < pattern->subs_count; ++n) {
      e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[n],
                                      UNLESS_NULL(expr, expr->subs[n]));
      EXCEPT(e);
    }
    break;
  case TYPECONSTRAINT:
    pattern->as.TYPECONSTRAINT.in_pattern = TRUE;
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
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
  if (node->subs_count >= 2) {
    expr = node->subs[1];
  }

  error e = extract_defnames_in_pattern(mod, node, node->subs[0], expr);
  EXCEPT(e);

  return 0;
}

static void append_member(struct node *deft, struct node *m) {
  assert(deft->which == DEFTYPE);

  deft->as.DEFTYPE.members_count += 1;
  deft->as.DEFTYPE.members = realloc(
    deft->as.DEFTYPE.members,
    deft->as.DEFTYPE.members_count * sizeof(*deft->as.DEFTYPE.members));

  deft->as.DEFTYPE.members[deft->as.DEFTYPE.members_count - 1] = m;
}

static void add_deftype_pristine_external_member(struct module *mod, struct node *deft,
                                                 struct node *member) {
  assert(deft->which == DEFTYPE);
  struct node *deft_pristine = node_toplevel(deft)->instances[0];
  struct node *member_pristine = node_toplevel(member)->instances[0];

  append_member(deft_pristine, member_pristine);
}

static STEP_FILTER(step_lexical_scoping,
                   SF(DEFFUN) | SF(DEFMETHOD) | SF(DEFTYPE) | SF(DEFINTF) |
                   SF(DEFFIELD) | SF(DEFCHOICE) | SF(DEFNAME) | SF(CATCH));
static error step_lexical_scoping(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *id = NULL;
  struct scope *sc = NULL;
  error e;

  struct node *container = NULL;
  const struct toplevel *toplevel = NULL;

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    if (node->subs[0]->which == IDENT) {
      id = node->subs[0];
    } else {
      id = node->subs[0]->subs[1];
    }

    toplevel = node_toplevel_const(node);
    if (toplevel->our_generic_functor_typ != NULL) {
      // For generic instances, do not define the name, as we want the type
      // name to point to the generic functor (e.g. in (vector u8), allow
      // vector to be used in (vector u16). To get the current instance,
      // use this or final.
      sc = NULL;
    } else if (toplevel->scope_name == 0
               || node_parent(node)->which == DEFINTF) {
      sc = node->scope.parent;
    } else {
      if (node_parent(node)->which == DEFTYPE) {
        // Generic instance *members* already have the 'right' parent.
        container = node_parent(node);
        sc = &container->scope;
      } else {
        e = scope_lookup_ident_wontimport(&container, node, mod, node->scope.parent,
                                          toplevel->scope_name, FALSE);
        EXCEPT(e);
        sc = &container->scope;
        node->scope.parent = sc;
      }

      const struct toplevel *ctoplevel = node_toplevel_const(container);
      if (toplevel->builtingen == BG__NOT // otherwise, will be re-generated
          && ctoplevel != NULL
          && ctoplevel->instances != NULL
          && ctoplevel->our_generic_functor_typ == NULL
          && container->subs[IDX_GENARGS]->subs_count > 0
          && container->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {
        add_deftype_pristine_external_member(mod, container, node);
      }
    }
    break;
  case DEFTYPE:
  case DEFINTF:
    if (node_toplevel_const(node)->our_generic_functor_typ != NULL) {
      sc = NULL;

      // For generic instances, define the name in its own scope, to make
      // sure lookups inside the instance resolve to the instance itself
      // (e.g. the definition of this).
      e = scope_define(mod, &node->scope, node->subs[0], node);
      EXCEPT(e);
    } else {
      id = node->subs[0];
      sc = node->scope.parent;
    }
    break;
  case DEFFIELD:
  case DEFCHOICE:
    id = node->subs[0];
    sc = node->scope.parent;
    break;
  case DEFNAME:
    if (node_ident(node) != ID_OTHERWISE) {
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

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    for (size_t n = 0; n < node->subs[IDX_GENARGS]->subs_count; ++n) {
      struct node *ga = node->subs[IDX_GENARGS]->subs[n];
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, ga->subs[0], ga);
      EXCEPT(e);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    for (size_t n = 0; n < node->subs[IDX_GENARGS]->subs_count; ++n) {
      struct node *ga = node->subs[IDX_GENARGS]->subs[n];
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, &node->scope, ga->subs[0], ga);
      EXCEPT(e);
    }

    struct node *funargs = node->subs[IDX_FUNARGS];
    for (size_t n = 0; n < node_fun_all_args_count(node); ++n) {
      struct node *arg = funargs->subs[n];
      assert(arg->which == DEFARG);
      e = scope_define(mod, &node->scope, arg->subs[0], arg);
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
    let->flags = NODE_IS_GLOBAL_LET;
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_THIS;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(node, 3);

    error e = catchup(mod, NULL, let, &node->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  {
    struct node *let = mk_node(mod, node, LET);
    let->flags = NODE_IS_GLOBAL_LET;
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_FINAL;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(node, 4);

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

  struct node *genargs = node->subs[IDX_GENARGS];
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
  case DEFTYPE_SUM:
    {
      set_typ(&node->as.DEFTYPE.choice_typ,
              typ_create_tentative(TBI_LITERALS_INTEGER));
      struct typ *u = node->as.DEFTYPE.choice_typ;

      for (size_t n = 0; n < node->subs_count; ++n) {
        struct node *ch = node->subs[n];
        if (ch->which != DEFCHOICE) {
          continue;
        }

        e = unify(mod, ch, u, ch->subs[IDX_CH_VALUE]->typ);
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
  struct node *isalist = deft->subs[IDX_ISALIST];
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_toplevel(deft)->is_inline;
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

  add_inferred_isa(mod, node, "nlang.builtins.i_trivial_copy");
  add_inferred_isa(mod, node, "nlang.builtins.i_trivial_dtor");

  return 0;
}

static STEP_FILTER(step_add_builtin_detect_ctor_intf,
                   SF(DEFTYPE));
static error step_add_builtin_detect_ctor_intf(struct module *mod, struct node *node,
                                               void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM
      || node->as.DEFTYPE.kind == DEFTYPE_SUM) {
    return 0;
  }

  struct node *proxy = node;
  struct node *ctor = node_get_member(mod, proxy, ID_CTOR);
  if (ctor != NULL) {
    if (node_fun_explicit_args_count(ctor) == 0) {
      add_inferred_isa(mod, node, "nlang.builtins.i_default_ctor");
    } else if (node_fun_explicit_args_count(ctor) == 1) {
      add_inferred_isa(mod, node, "nlang.builtins.i_ctor_with");
    }
  } else {
    add_inferred_isa(mod, node, "nlang.builtins.i_trivial_ctor");
  }

  return 0;
}

static STEP_FILTER(step_rewrite_final_this,
                   SF(IDENT));
static error step_rewrite_final_this(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  struct typ *thi = user;
  ident id = node_ident(node);
  if (id == ID_THIS) {
    node->which = DIRECTDEF;
    set_typ(&node->as.DIRECTDEF.typ, thi);
    node->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }
  return 0;
}

static error pass_rewrite_final_this(struct module *mod, struct node *root,
                                     void *user, ssize_t shallow_last_up) {
  PASS(DOWN_STEP(step_rewrite_final_this),);
  return 0;
}

static void intf_proto_deepcopy(struct module *mod, struct typ *thi,
                                struct node *dst, struct node *src) {
  node_deepcopy(mod, dst, src);

  PUSH_STATE(mod->state->step_state);
  error e = pass_rewrite_final_this(mod, dst, thi, -1);
  assert(!e);
  POP_STATE(mod->state->step_state);

  if (node_toplevel(dst) != NULL) {
    node_toplevel(dst)->yet_to_pass = 0;
  }
}

static void define_builtin(struct module *mod, struct node *deft,
                           enum builtingen bg) {
  struct node *modbody;
  ssize_t insert_pos;
  if (deft->subs[IDX_GENARGS]->subs_count > 0) {
    modbody = NULL;
    insert_pos = -1;
  } else {
    modbody = node_parent(deft);
    insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;
  }

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, deft, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *existing = node_get_member(mod, deft, node_ident(proto));
  if (existing != NULL) {
    return;
  }

  struct node *d;
  if (insert_pos >= 0) {
    d = node_new_subnode(mod, modbody);
  } else {
    d = calloc(1, sizeof(*d));
  }
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(deft)->is_export;
  toplevel->is_inline = node_toplevel(deft)->is_inline;

  enum catchup_for how;
  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
    how = CATCHUP_AFTER_CURRENT;
  } else {
    append_member(deft, d);
    how = CATCHUP_BELOW_CURRENT;
  }

  e = catchup(mod, NULL, d, &deft->scope, how);
  assert(!e);
}

static void define_defchoice_builtin(struct module *mod, struct node *ch,
                                     enum builtingen bg, enum node_which which) {
  struct node *deft = node_parent(ch);

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, ch, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = mk_node(mod, ch, which);
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(ch);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(deft)->is_export;
  toplevel->is_inline = node_toplevel(deft)->is_inline;

  if (bg == BG_SUM_CTOR_WITH_CTOR
      || bg == BG_SUM_CTOR_WITH_MK
      || bg == BG_SUM_CTOR_WITH_NEW) {
    struct node *funargs = d->subs[IDX_FUNARGS];
    struct node *arg = mk_node(mod, funargs, DEFARG);
    struct node *name = mk_node(mod, arg, IDENT);
    name->as.IDENT.name = ID_C;
    struct node *typename = mk_node(mod, arg, DIRECTDEF);
    set_typ(&typename->as.DIRECTDEF.typ, ch->subs[IDX_CH_PAYLOAD]->typ);
    typename->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(funargs, (d->which == DEFMETHOD) ? 1 : 0);
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

  const struct typ *targ = node->subs[IDX_CH_PAYLOAD]->typ;
  error e = typ_check_isa(mod, node->subs[IDX_CH_PAYLOAD],
                          targ, TBI_COPYABLE);
  EXCEPT(e);

  define_defchoice_builtin(
    mod, node, BG_SUM_CTOR_WITH_CTOR, DEFMETHOD);

  return 0;
}

static STEP_FILTER(step_add_builtin_ctor,
                   SF(DEFTYPE));
static error step_add_builtin_ctor(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_dtor,
                   SF(DEFTYPE));
static error step_add_builtin_dtor(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_toplevel_const(node)->is_extern) {
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
    e = mk_except_type(mod, deft, "type '%s' is not i_trivial_ctor and has no 'ctor'",
                       typ_pretty_name(mod, deft->typ));
    THROW(e);
  }

  struct node *modbody;
  ssize_t insert_pos;
  if (deft->subs[IDX_GENARGS]->subs_count > 0) {
    modbody = NULL;
    insert_pos = -1;
  } else {
    modbody = node_parent(deft);
    insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;
  }

  struct node *d;
  if (insert_pos >= 0) {
    d = node_new_subnode(mod, modbody);
  } else {
    d = calloc(1, sizeof(*d));
  }
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(ctor)->is_export;
  toplevel->is_inline = node_toplevel(ctor)->is_inline;

  struct node *ctor_funargs = ctor->subs[IDX_FUNARGS];
  struct node *d_funargs = d->subs[IDX_FUNARGS];
  // (Skip self.)
  for (size_t n = 1; n < node_fun_all_args_count(ctor); ++n) {
    struct node *arg = ctor_funargs->subs[n];
    struct node *cpy = node_new_subnode(mod, d_funargs);
    intf_proto_deepcopy(mod, node_parent(proto)->typ, cpy, arg);
    rew_insert_last_at(d_funargs, n-1);
  }

  enum catchup_for how;
  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
    how = CATCHUP_AFTER_CURRENT;
  } else {
    append_member(deft, d);
    how = CATCHUP_BELOW_CURRENT;
  }

  assert(node_toplevel(d)->yet_to_pass == 0);
  e = catchup(mod, NULL, d, &deft->scope, how);
  assert(!e);

  return 0;
}

static STEP_FILTER(step_add_builtin_mk_new,
                   SF(DEFTYPE));
static error step_add_builtin_mk_new(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_SUM
      || node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    define_builtin(mod, node, BG_TRIVIAL_CTOR_CTOR);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_MK);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_NEW);
  } else if (typ_isa(node->typ, TBI_DEFAULT_CTOR)) {
    define_builtin(mod, node, BG_DEFAULT_CTOR_MK);
    define_builtin(mod, node, BG_DEFAULT_CTOR_NEW);
  } else if (typ_isa(node->typ, TBI_CTOR_WITH)) {
    define_builtin(mod, node, BG_CTOR_WITH_MK);
    define_builtin(mod, node, BG_CTOR_WITH_NEW);
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

  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_ARRAY_CTOR)) {
    define_builtin(mod, node, BG_AUTO_MKV);
    define_builtin(mod, node, BG_AUTO_NEWV);
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
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_NEW, DEFFUN);
  } else if (deft->as.DEFTYPE.kind == DEFTYPE_SUM) {
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_NEW, DEFFUN);
  }

  return 0;
}

static STEP_FILTER(step_add_builtin_operators,
                   SF(DEFTYPE));
static error step_add_builtin_operators(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
    break;
  case DEFTYPE_STRUCT:
    break;
  case DEFTYPE_SUM:
    break;
  case DEFTYPE_ENUM:
    if (!typ_isa(node->as.DEFTYPE.choice_typ, TBI_NATIVE_INTEGER)) {
      define_builtin(mod, node, BG_ENUM_EQ);
      define_builtin(mod, node, BG_ENUM_NE);
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
    define_builtin(mod, node, BG_TRIVIAL_COPY_COPY_CTOR);
  }
  if (typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_EQ);
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_NE);
  }

  return 0;
}

static void define_dispatch(struct module *mod, struct node *deft, struct typ *tintf) {
  struct node *intf = typ_definition(tintf);

  struct node *modbody = node_parent(deft);
  size_t insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;

  for (size_t n = 0; n < intf->subs_count; ++n) {
    struct node *proto = intf->subs[n];
    if (proto->which != DEFMETHOD && proto->which != DEFFUN) {
      continue;
    }
    if (node_toplevel_const(proto)->is_not_dyn) {
      continue;
    }

    struct node *existing = node_get_member(mod, deft, node_ident(proto));
    if (existing != NULL) {
      return;
    }

    struct node *d = mk_node(mod, modbody, proto->which);
    intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
    char *abspath = scope_name(mod, &proto->scope);
    mk_expr_abspath(mod, d, abspath);
    rew_move_last_over(d, 0, FALSE);

    struct toplevel *toplevel = node_toplevel(d);
    toplevel->scope_name = node_ident(deft);
    toplevel->builtingen = BG_SUM_DISPATCH;
    toplevel->is_prototype = FALSE;
    toplevel->is_export = node_toplevel(deft)->is_export;
    toplevel->is_inline = node_toplevel(deft)->is_inline;

    rew_insert_last_at(modbody, insert_pos);

    error e = catchup(mod, NULL, d, &deft->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

static error sum_choice_with_intf(struct module *mod, struct typ *t,
                                  struct typ *intf, bool *stop, void *user) {
  struct node *node = user;

  struct typ *to_check = intf;
  if (typ_equal(intf, TBI_SUM_COPY)) {
    to_check = TBI_COPYABLE;
  } else if (typ_equal(intf, TBI_SUM_EQUALITY)) {
    to_check = TBI_HAS_EQUALITY;
  } else if (typ_equal(intf, TBI_SUM_ORDER)) {
    to_check = TBI_ORDERED;
  }

  for (size_t c = 0; c < node->subs_count; ++c) {
    struct node *ch = node->subs[c];
    if (ch->which != DEFCHOICE) {
      continue;
    }

    struct typ *tch = NULL;
    if (typ_equal(ch->subs[IDX_CH_PAYLOAD]->typ, TBI_VOID)) {
      tch = node->as.DEFTYPE.choice_typ;
    } else {
      tch = ch->subs[IDX_CH_PAYLOAD]->typ;
    }

    error e = typ_check_isa(mod, ch, tch, to_check);
    EXCEPT(e);
  }

  if (typ_equal(intf, TBI_SUM_COPY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
      define_builtin(mod, node, BG_SUM_COPY);
    }
  } else if (typ_equal(intf, TBI_SUM_EQUALITY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
      define_builtin(mod, node, BG_SUM_EQUALITY_EQ);
      define_builtin(mod, node, BG_SUM_EQUALITY_NE);
    }
  } else if (typ_equal(intf, TBI_SUM_ORDER)) {
    define_builtin(mod, node, BG_SUM_ORDER_LE);
    define_builtin(mod, node, BG_SUM_ORDER_LT);
    define_builtin(mod, node, BG_SUM_ORDER_GT);
    define_builtin(mod, node, BG_SUM_ORDER_GE);
  } else {
    define_dispatch(mod, node, intf);
  }

  return 0;
}

static STEP_FILTER(step_add_sum_dispatch,
                   SF(DEFTYPE));
static error step_add_sum_dispatch(struct module *mod, struct node *node,
                                   void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
  case DEFTYPE_ENUM:
  case DEFTYPE_STRUCT:
    return 0;
  case DEFTYPE_SUM:
    break;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                sum_choice_with_intf, node);
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

  struct node *funargs = node->subs[IDX_FUNARGS];
  const size_t where = rew_find_subnode_in_parent(funargs, retval);
  struct node *named = mk_node(mod, funargs, DEFARG);
  named->as.DEFARG.is_retval = TRUE;
  struct node *name = mk_node(mod, named, IDENT);
  name->as.IDENT.name = ID_NRETVAL;
  rew_append(named, retval);
  rew_move_last_over(funargs, where, TRUE);

  error e = lexical_retval(mod, node, named);
  EXCEPT(e);

  const struct node *except[] = { retval, NULL };
  e = catchup(mod, except, named, &node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  // We need to force the typing of 'named' by hand.
  e = step_type_inference(mod, named, NULL, NULL);
  EXCEPT(e);

  return 0;
}

static error passfwd0(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  // scoping_deftypes
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_codeloc_for_generated);
    DOWN_STEP(step_defpattern_extract_defname);
    DOWN_STEP(step_lexical_scoping);
    ,
    UP_STEP(step_type_definitions);
    UP_STEP(step_gather_final_instantiations);
    UP_STEP(step_complete_instantiation);
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
    UP_STEP(step_gather_final_instantiations);
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
    ,
    UP_STEP(step_gather_final_instantiations);
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
    ,
    UP_STEP(step_type_inference_isalist);
    UP_STEP(step_gather_final_instantiations);
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
    UP_STEP(step_gather_final_instantiations);
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
    ,
    UP_STEP(step_add_builtin_enum_intf);
    UP_STEP(step_add_builtin_detect_ctor_intf);
    UP_STEP(step_gather_final_instantiations);
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
    ,
    UP_STEP(step_type_lets);
    UP_STEP(step_gather_final_instantiations);
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
    ,
    UP_STEP(step_type_deffields);
    UP_STEP(step_type_defchoices);
    UP_STEP(step_gather_final_instantiations);
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
    UP_STEP(step_add_sum_dispatch);
    UP_STEP(step_rewrite_def_return_through_ref);
    UP_STEP(step_gather_final_instantiations);
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
