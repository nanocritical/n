#include "autointf.h"

#include "types.h"
#include "passes.h"
#include "parser.h"

struct intf_proto_rewrite_state {
  struct typ *thi;
  const struct typ *proto_parent;
};

static STEP_NM(step_rewrite_this,
               NM(IDENT));
static ERROR step_rewrite_this(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  struct typ *thi = user;
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
static ERROR step_rewrite_local_idents(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  const struct typ *proto_parent = user;
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

  if (node->which == IDENT && parent_const(node)->which == BIN) {
    return 0;
  }

  const ident id = node_ident(node);
  if (id == ID_THIS || id == ID_FINAL) {
    return 0;
  }

  struct tit *m = typ_definition_one_member(proto_parent, id);
  if (m == NULL) {
    return 0;
  }

  if (NM(tit_which(m)) & (NM(DEFMETHOD) | NM(DEFFUN))) {
    return 0;
  }

  node_set_which(node, DIRECTDEF);
  set_typ(&node->as.DIRECTDEF.typ, tit_typ(m));

  tit_next(m);
  return 0;
}

static ERROR pass_rewrite_proto(struct module *mod, struct node *root,
                                void *user, ssize_t shallow_last_up) {
  PASS(DOWN_STEP(step_rewrite_this);
       DOWN_STEP(step_rewrite_local_idents);
       ,,);
  return 0;
}

static void intf_proto_deepcopy(struct module *mod,
                                struct node *dst, const struct tit *src,
                                const struct typ *thi) {
  node_deepcopy(mod, dst, tit_node_ignore_any_overlay(src));

  PUSH_STATE(mod->state->step_state);
  error e = pass_rewrite_proto(mod, dst, (void *)thi, -1);
  assert(!e);
  POP_STATE(mod->state->step_state);

  if (node_toplevel(dst) != NULL) {
    node_toplevel(dst)->passing = -1;
    node_toplevel(dst)->passed = -1;
  }
}

static struct node *define_builtin_start(struct module *mod, struct node *deft,
                                         const struct typ *intf,
                                         const struct tit *mi) {
  struct node *d = node_new_subnode(mod, deft);
  intf_proto_deepcopy(mod, d, mi, intf);
  d->codeloc = deft->codeloc;

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->flags &= ~TOP_IS_PROTOTYPE;
  toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  return d;
}

static void define_builtin_catchup(struct module *mod, struct node *d) {
  error e = catchup(mod, NULL, d, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static bool arg_is_ref(const struct node *arg) {
  assert(arg->which == DEFARG);
  const struct node *tspec = subs_last_const(arg);
  return tspec->which == UN;
}

static enum token_type arg_ref_operator(const struct node *arg) {
  assert(arg->which == DEFARG);
  const struct node *tspec = subs_last_const(arg);
  assert(tspec->which == UN);
  return tspec->as.UN.operator;
}

static enum token_type arg_ref_accessor(const struct node *arg) {
  switch (arg_ref_operator(arg)) {
  case TREFDOT: return TDOT;
  case TREFBANG: return TBANG;
  case TREFSHARP: return TSHARP;
  default:
                  assert(false);
                  return 0;
  }
}

static void like_arg(struct module *mod, struct node *call,
                     const struct node *arg, ident f) {
  GSTART();
  if (arg_is_ref(arg)) {
    G0(r, call, UN,
       r->as.UN.operator = arg_ref_operator(arg);
       G(b, BIN,
         b->as.BIN.operator = arg_ref_accessor(arg);
         G(base, IDENT,
           base->as.IDENT.name = node_ident(arg));
         G(a, IDENT,
           a->as.IDENT.name = f)));
  } else {
    G0(b, call, BIN,
       b->as.BIN.operator = arg_ref_accessor(arg);
       G(base, IDENT,
         base->as.IDENT.name = node_ident(arg));
       G(a, IDENT,
         a->as.IDENT.name = f));
  }
}

static void gen_on_choices_and_fields(struct module *mod,
                                      struct node *deft,
                                      struct node *ch,
                                      struct node *m,
                                      struct node *body) {
  GSTART();
  assert(deft->which == DEFTYPE);
  if (deft->as.DEFTYPE.kind == DEFTYPE_UNION) {
    // FIXME: unsupported
    G0(noop, body, NOOP);
    return;
  } else {
    assert(ch == NULL);
  }

  const struct node *funargs = subs_at_const(m, IDX_FUNARGS);
  size_t n = 0;
  FOREACH_SUB_CONST(f, deft) {
    if (f->which != DEFFIELD) {
      continue;
    }

    const struct node *arg_self = subs_first_const(funargs);
    G0(call, body, CALL,
       G(fun, BIN,
         fun->as.BIN.operator = TDOT;
         like_arg(mod, fun, arg_self, node_ident(f));
         G(name, IDENT,
           name->as.IDENT.name = node_ident(m))));

    if (subs_count_atleast(funargs, 3)) {
      const struct node *arg_other = next_const(arg_self);
      like_arg(mod, call, arg_other, node_ident(f));
    }

    n += 1;
  }

  if (n == 0) {
    G0(noop, body, NOOP);
  }
}

static void gen_on_choices_and_fields_lexicographic(struct module *mod,
                                                    struct node *deft,
                                                    struct node *ch,
                                                    struct node *m,
                                                    struct node *body) {
  assert(deft->which == DEFTYPE);
  if (deft->as.DEFTYPE.kind == DEFTYPE_UNION) {
    // FIXME: unsupported
    return;
  } else {
    assert(ch == NULL);
  }

  const struct node *funargs = subs_at_const(m, IDX_FUNARGS);
  GSTART();
  struct node *par = body;
  size_t n = 0;
  FOREACH_SUB_CONST(f, deft) {
    if (f->which != DEFFIELD) {
      continue;
    }

    const struct node *arg_self = subs_first_const(funargs);
    const ident g = gensym(mod);
    G0(let, par, LET,
       G(defp, DEFPATTERN,
         G(var, IDENT,
           var->as.IDENT.name = g);
         G(call, CALL,
           G(fun, BIN,
             fun->as.BIN.operator = TDOT;
             like_arg(mod, fun, arg_self, node_ident(f));
             G(name, IDENT,
               name->as.IDENT.name = node_ident(m))))));
    if (subs_count_atleast(funargs, 3)) {
      const struct node *arg_other = next_const(arg_self);
      like_arg(mod, call, arg_other, node_ident(f));
    }

    G0(cond, par, IF,
       G(test, BIN,
         test->as.BIN.operator = TNE;
         G(var2, IDENT,
           var2->as.IDENT.name = g);
         G(zero, NUMBER,
           zero->as.NUMBER.value = "0"));
       G(yes, BLOCK,
         G(ret, RETURN,
           G(var3, IDENT,
             var3->as.IDENT.name = g)));
       G(no, BLOCK));

    par = no;
    n += 1;
  }

  G0(ret, par, RETURN,
     G(zero, NUMBER,
       zero->as.NUMBER.value = "0"));
}

static void gen_by_compare(struct module *mod, struct node *deft,
                           struct node *m, struct node *body) {
  enum token_type op = 0;
  switch (node_ident(m)) {
  case ID_OPERATOR_EQ: op = TEQ; break;
  case ID_OPERATOR_NE: op = TNE; break;
  case ID_OPERATOR_LE: op = TLE; break;
  case ID_OPERATOR_LT: op = TLT; break;
  case ID_OPERATOR_GT: op = TGT; break;
  case ID_OPERATOR_GE: op = TGE; break;
  default:
                       assert(false);
  }

  GSTART();
  G0(ret, body, RETURN,
     G(test, BIN,
       test->as.BIN.operator = op;
       G(zero, NUMBER,
         zero->as.NUMBER.value = "0");
       G(call, CALL,
         G(fun, BIN,
           fun->as.BIN.operator = TDOT;
           G_IDENT(self, "self");
           G_IDENT(name, "Operator_compare"));
         G_IDENT(other, "other"))));
}

static void gen_enum_show_choices(struct module *mod, struct node *deft,
                             struct node *par, const struct node *node) {
  assert(NM(node->which) & (NM(DEFCHOICE) | NM(DEFTYPE)));

  if (node->which == DEFCHOICE && node->as.DEFCHOICE.is_leaf) {
    const char *n = idents_value(mod->gctx, node_ident(node));
    const size_t len = strlen(n);
    char *v = calloc(len + 3, sizeof(char));
    v[0] = '"';
    strcpy(v+1, n);
    v[len+1] = '"';

    GSTART();
    G0(pat, par, IDENT,
       pat->as.IDENT.name = node_ident(node));
    G0(bl, par, BLOCK,
       G(ignlet, LET,
         G(igndefp, DEFPATTERN,
           G(ign, IDENT,
             ign->as.IDENT.name = ID_OTHERWISE);
           G(append, CALL,
             G(fun, BIN,
               fun->as.BIN.operator = TSHARP;
               G_IDENT(st, "st");
               G_IDENT(app, "Write"));
             G(b, BIN,
               b->as.BIN.operator = TDOT;
               G(s, STRING,
                 s->as.STRING.value = v);
               G_IDENT(bytes, "Bytes"))))));
    return;
  }

  FOREACH_SUB_CONST(ch, node) {
    if (ch->which != DEFCHOICE) {
      continue;
    }

    gen_enum_show_choices(mod, deft, par, ch);
  }
}

static void gen_enum_show(struct module *mod, struct node *deft,
                     struct node *m, struct node *body) {
  GSTART();
  G0(match, body, MATCH,
     G_IDENT(self, "self"));

  gen_enum_show_choices(mod, deft, match, deft);
}

static void add_auto_member(struct module *mod,
                            struct node *deft,
                            const struct typ *inferred_intf,
                            const struct typ *intf,
                            const struct tit *mi) {
  if (node_is_extern(deft) && !node_is_inline(deft)) {
    return;
  }

  struct node *existing = node_get_member(deft, tit_ident(mi));
  if (existing != NULL) {
    return;
  }

  struct node *m = define_builtin_start(mod, deft, intf, mi);

  if (deft->which == DEFINCOMPLETE) {
    assert(deft->as.DEFINCOMPLETE.is_isalist_literal);
    define_builtin_catchup(mod, m);
    return;
  }

  if (typ_is_trivial(inferred_intf)) {
    enum builtingen bg = BG__NOT;

    switch (tit_ident(mi)) {
    case ID_CTOR:
      bg = BG_TRIVIAL_CTOR_CTOR;
      break;
    case ID_DTOR:
      bg = BG_TRIVIAL_DTOR_DTOR;
      break;
    case ID_COPY_CTOR:
      bg = BG_TRIVIAL_COPY_COPY_CTOR;
      break;
    case ID_OPERATOR_COMPARE:
      bg = BG_TRIVIAL_COMPARE_OPERATOR_COMPARE;
      break;
    default:
      goto non_bg;
    }

    node_toplevel(m)->builtingen = bg;
    define_builtin_catchup(mod, m);
    return;
  }

non_bg:
  ;GSTART();
  G0(body, m, BLOCK);

  switch (tit_ident(mi)) {
  case ID_CTOR:
  case ID_DTOR:
  case ID_COPY_CTOR:
    gen_on_choices_and_fields(mod, deft, NULL, m, body);
    break;
  case ID_OPERATOR_COMPARE:
    gen_on_choices_and_fields_lexicographic(mod, deft, NULL, m, body);
    break;
  case ID_OPERATOR_EQ:
  case ID_OPERATOR_NE:
  case ID_OPERATOR_LE:
  case ID_OPERATOR_LT:
  case ID_OPERATOR_GT:
  case ID_OPERATOR_GE:
    gen_by_compare(mod, deft, m, body);
    break;
  case ID_FROM_TAG:
    node_subs_remove(m, body);
    node_toplevel(m)->builtingen = BG_ENUM_FROM_TAG;
    break;
  case ID_TAG:
    node_subs_remove(m, body);
    node_toplevel(m)->builtingen = BG_ENUM_TAG;
    break;
  case ID_SHOW:
    gen_enum_show(mod, deft, m, body);
    break;
  default:
    assert(false);
  }
  define_builtin_catchup(mod, m);
}

static ERROR add_auto_isa_eachisalist(struct module *mod,
                                      struct typ *t,
                                      struct typ *intf,
                                      bool *stop,
                                      void *user) {
  struct node *deft = user;
  struct tit *mi = typ_definition_members(intf, DEFMETHOD, DEFFUN, 0);
  while (tit_next(mi)) {
    add_auto_member(mod, deft, t, intf, mi);
  }

  return 0;
}

static void add_auto_isa(struct module *mod, struct node *deft,
                         const struct typ *i) {
  if (!typ_isa(deft->typ, i)) {
    struct node *isalist = subs_at(deft, IDX_ISALIST);
    assert(isalist->which == ISALIST);

    GSTART();
    G0(isa, isalist, ISA,
       isa->as.ISA.is_export = node_is_export(deft)
       && (node_is_inline(deft) || node_is_opaque(deft));
       G(what, DIRECTDEF);
       set_typ(&what->as.DIRECTDEF.typ, CONST_CAST(i)));

    error e = catchup(mod, NULL, isa, CATCHUP_BELOW_CURRENT);
    assert(!e);

    typ_create_update_quickisa(deft->typ);
  }

  if (!(node_is_extern(deft) && !typ_is_trivial(i))) {
    error never = add_auto_isa_eachisalist(mod, CONST_CAST(i),
                                           CONST_CAST(i),
                                           NULL, deft);
    assert(!never);
  }

  const uint32_t filter = (node_is_extern(deft) && !typ_is_trivial(i)) \
                          ? ISALIST_FILTEROUT_NONTRIVIAL_ISALIST : 0;
  error never = typ_isalist_foreach(mod, CONST_CAST(i), filter,
                                    add_auto_isa_eachisalist, deft);
  assert(!never);
}

static size_t count_defchoices(const struct node *node) {
  size_t count = 0;
  FOREACH_SUB_CONST(d, node) {
    if (d->which == DEFCHOICE) {
      if (d->as.DEFCHOICE.is_leaf) {
        count += 1;
      } else {
        count += count_defchoices(d);
      }
    }
  }
  return count;
}

static void add_enum_allbw(struct module *mod, struct node *node) {
  const size_t count = count_defchoices(node);
  if (count > 64) {
    // FIXME
    return;
  }

  const uint64_t bwall = (1 << count) - 1;

  char value[32] = { 0 };
  snprintf(value, ARRAY_SIZE(value), "0x%"PRIu64, bwall);

  GSTART();
  G0(let, node, LET,
     let->flags |= NODE_IS_GLOBAL_LET;
     G(defn, DEFNAME,
       defn->flags |= NODE_IS_GLOBAL_LET;
       G_IDENT(valn, "BWALL");
       G(valc, TYPECONSTRAINT,
         G(val, NUMBER,
           val->as.NUMBER.value = strdup(value));
         G(valt, DIRECTDEF,
           set_typ(&valt->as.DIRECTDEF.typ, TBI_U64)))));

  error e = catchup(mod, NULL, let, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

STEP_NM(step_autointf_enum_union,
        NM(DEFTYPE));
error step_autointf_enum_union(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    add_enum_allbw(mod, node);

    add_auto_isa(mod, node, TBI_ENUM);

    if (node->as.DEFTYPE.default_choice != NULL) {
      add_auto_isa(mod, node, TBI_TRIVIAL_CTOR);
    }
  } else if (node->as.DEFTYPE.kind == DEFTYPE_UNION) {
    add_auto_isa(mod, node, TBI_UNION);
  }

  return 0;
}

STEP_NM(step_autointf_detect_default_ctor_dtor,
        NM(DEFTYPE));
error step_autointf_detect_default_ctor_dtor(struct module *mod, struct node *node,
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
  struct node *ctor = node_get_member(proxy, ID_CTOR);
  if (ctor != NULL) {
    if (node_fun_max_args_count(ctor) == 0) {
      add_auto_isa(mod, node, TBI_DEFAULT_CTOR);
    }
  } else {
    // see step_autointf_infer_intfs
  }

  return 0;
}

struct inferred {
  bool default_ctor, copyable, return_by_copy, equality_by_compare, ordered_by_compare;
  bool trivial_ctor, trivial_dtor, trivial_copy_but_owned,
       trivial_copy, trivial_compare, trivial_equality, trivial_order;
};

static void infer(struct inferred *inferred, const struct node *node) {
  if (NM(node->which) & (NM(DEFTYPE) | NM(DEFCHOICE))) {
    FOREACH_SUB_CONST(f, node) {
      if (!(NM(f->which) & (NM(DEFFIELD) | NM(DEFCHOICE)))) {
        continue;
      }

      infer(inferred, f);
    }
    return;
  }

  if (inferred->default_ctor && !typ_isa(node->typ, TBI_DEFAULT_CTOR)) {
    inferred->trivial_ctor = inferred->default_ctor = false;
  }
  if (inferred->copyable && !typ_isa(node->typ, TBI_COPYABLE)) {
    inferred->trivial_copy = inferred->copyable = false;
  }
  if (inferred->equality_by_compare && !typ_isa(node->typ, TBI_EQUALITY_BY_COMPARE)) {
    inferred->equality_by_compare = false;
  }
  if (inferred->ordered_by_compare && !typ_isa(node->typ, TBI_ORDERED_BY_COMPARE)) {
    inferred->ordered_by_compare = false;
  }
  if (inferred->return_by_copy && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    inferred->return_by_copy = false;
  }

  if (inferred->trivial_ctor && !typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    inferred->trivial_ctor = false;
  }
  if (inferred->trivial_dtor && !typ_isa(node->typ, TBI_TRIVIAL_DTOR)) {
    inferred->trivial_dtor = false;
  }
  if (inferred->trivial_copy_but_owned && !typ_isa(node->typ, TBI_TRIVIAL_COPY_BUT_OWNED)) {
    inferred->trivial_copy_but_owned = false;
  }
  if (inferred->trivial_copy && !typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
    inferred->trivial_copy = false;
  }
  if (inferred->trivial_compare && !typ_isa(node->typ, TBI_TRIVIAL_COMPARE)) {
    inferred->trivial_order = inferred->trivial_equality = inferred->trivial_compare = false;
  }
  if (inferred->trivial_equality && !typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    inferred->trivial_equality = false;
  }
  if (inferred->trivial_order && !typ_isa(node->typ, TBI_TRIVIAL_ORDER)) {
    inferred->trivial_order = false;
  }
}

static bool immediate_isa(const struct node *node, const struct typ *i) {
  const struct node *isalist = subs_at_const(node, IDX_ISALIST);
  FOREACH_SUB_CONST(isa, isalist) {
    if (typ_equal(isa->typ, i)) {
      return true;
    }
  }
  return false;
}

STEP_NM(step_autointf_infer_intfs,
        NM(DEFTYPE));
error step_autointf_infer_intfs(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM
      || typ_equal(node->typ, TBI_VOID)) {
    return 0;
  }

  struct inferred inferred = {
    true, true, true, true, true,
    true, true, true, true, true, true, true,
  };

  if (node_is_extern(node)) {
    inferred = (struct inferred){ 0 };
    goto skip;
  }

  // Any isa at this point has been explicitly declared by the user.
  if (immediate_isa(node, TBI_NON_DEFAULT_CTOR)) {
    inferred.default_ctor = inferred.trivial_ctor = false;
  } else if (immediate_isa(node, TBI_DEFAULT_CTOR)) {
    inferred.trivial_ctor = false;
  }
  if (immediate_isa(node, TBI_DEFAULT_DTOR)) {
    inferred.trivial_dtor = false;
  }
  if (immediate_isa(node, TBI_NOT_COPYABLE)) {
    inferred.return_by_copy = inferred.trivial_copy
      = inferred.trivial_copy_but_owned = inferred.copyable = false;
  } else if (immediate_isa(node, TBI_COPYABLE)) {
    inferred.trivial_copy = false;
  }
  if (immediate_isa(node, TBI_NOT_HAS_EQUALITY)) {
    inferred.equality_by_compare = inferred.trivial_equality = false;
  } else if (immediate_isa(node, TBI_HAS_EQUALITY)
             || immediate_isa(node, TBI_EQUALITY_BY_COMPARE)) {
    inferred.trivial_equality = false;
  }
  if (immediate_isa(node, TBI_NOT_ORDERED)) {
    inferred.ordered_by_compare = inferred.trivial_order = false;
  } else if (immediate_isa(node, TBI_ORDERED)
             || immediate_isa(node, TBI_ORDERED_BY_COMPARE)) {
    inferred.trivial_order = false;
  }
  if (immediate_isa(node, TBI_NOT_RETURN_BY_COPY)) {
    inferred.return_by_copy = false;
  }

  infer(&inferred, node);

skip:
  if (inferred.trivial_ctor || typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    if (node->as.DEFTYPE.kind == DEFTYPE_UNION) {
      if (node->as.DEFTYPE.default_choice != NULL) {
        add_auto_isa(mod, node, TBI_UNION_TRIVIAL_CTOR);
      }
    } else {
      add_auto_isa(mod, node, TBI_TRIVIAL_CTOR);
    }
  }
  if (inferred.trivial_dtor || typ_isa(node->typ, TBI_TRIVIAL_DTOR)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_DTOR);
  }
  if (inferred.trivial_copy_but_owned || typ_isa(node->typ, TBI_TRIVIAL_COPY_BUT_OWNED)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_COPY_BUT_OWNED);
  } else if (inferred.trivial_copy || typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_COPY);
  } else {
    // Only infer `return_by_copy if `trivial_copy.
    inferred.return_by_copy = false;

    if (inferred.copyable) {
      add_auto_isa(mod, node, TBI_COPYABLE);
    }
  }
  if (inferred.trivial_compare || typ_isa(node->typ, TBI_TRIVIAL_COMPARE)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_COMPARE);
  }
  if (inferred.trivial_order || typ_isa(node->typ, TBI_TRIVIAL_ORDER)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_ORDER);
  } else if (inferred.ordered_by_compare || typ_isa(node->typ, TBI_ORDERED_BY_COMPARE)) {
    add_auto_isa(mod, node, TBI_ORDERED_BY_COMPARE);
  } else if (inferred.trivial_equality || typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_EQUALITY);
  } else if (inferred.equality_by_compare) {
    add_auto_isa(mod, node, TBI_EQUALITY_BY_COMPARE);
  }
  if (inferred.return_by_copy) {
    add_auto_isa(mod, node, TBI_RETURN_BY_COPY);
  }

  return 0;
}

STEP_NM(step_autointf_isalist_literal_protos,
        NM(DEFINCOMPLETE));
error step_autointf_isalist_literal_protos(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  if (!node->as.DEFINCOMPLETE.is_isalist_literal) {
    return 0;
  }

  const struct node *isalist = subs_at_const(node, IDX_ISALIST);
  FOREACH_SUB_CONST(isa, isalist) {
    add_auto_isa(mod, node, isa->typ);
  }

  return 0;
}

static void define_builtin(struct module *mod, struct node *deft,
                           const struct typ *intf,
                           ident name, enum builtingen bg) {
  struct tit *mi = typ_definition_one_member(intf, name);
  struct node *m = define_builtin_start(mod, deft, intf, mi);
  node_toplevel(m)->builtingen = bg;
  define_builtin_catchup(mod, m);
  tit_next(mi);
}

static ERROR add_environment_builtins_eachisalist(struct module *mod,
                                                  struct typ *t,
                                                  struct typ *intf,
                                                  bool *stop,
                                                  void *user) {
  struct node *deft = user;
  if (typ_generic_arity(intf) == 0
      || !typ_equal(typ_generic_functor_const(intf), TBI_ENVIRONMENT)) {
    return 0;
  }

  define_builtin(mod, deft, intf, ID_PARENT, BG_ENVIRONMENT_PARENT);
  define_builtin(mod, deft, intf, ID_INSTALL, BG_ENVIRONMENT_INSTALL);
  define_builtin(mod, deft, intf, ID_UNINSTALL, BG_ENVIRONMENT_UNINSTALL);
  return 0;
}

STEP_NM(step_autointf_add_environment_builtins,
        NM(DEFTYPE));
error step_autointf_add_environment_builtins(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);

  if (!typ_isa(node->typ, TBI_ANY_ENVIRONMENT)) {
    return 0;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTEROUT_TRIVIAL_ISALIST,
                                add_environment_builtins_eachisalist, node);
  EXCEPT(e);
  return 0;
}
