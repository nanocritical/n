#include "autointf.h"

#include "types.h"
#include "passes.h"
#include "parser.h"
#include <ctype.h>

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
  node_deepcopy_omit_tail_block(mod, dst, tit_node_ignore_any_overlay(src));

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
  toplevel->flags |= node_toplevel(deft)->flags & (TOP_IS_INLINE);

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

static const struct typ *corresponding_trivial(ident m) {
  switch (m) {
  case ID_CTOR:
    return TBI_TRIVIAL_CTOR;
  case ID_DTOR:
    return TBI_TRIVIAL_DTOR;
  case ID_COPY_CTOR:
    return TBI_TRIVIAL_COPY;
  default:
    assert(false);
    return NULL;
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

    if (typ_isa(f->typ, corresponding_trivial(node_ident(m)))) {
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
  default: assert(false);
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

static bool need_auto_member(struct typ *intf, struct tit *mi) {
  switch (tit_ident(mi)) {
  case ID_CTOR:
  case ID_DTOR:
  case ID_COPY_CTOR:
  case ID_OPERATOR_COMPARE:
  case ID_OPERATOR_EQ:
  case ID_OPERATOR_NE:
  case ID_OPERATOR_LE:
  case ID_OPERATOR_LT:
  case ID_OPERATOR_GT:
  case ID_OPERATOR_GE:
  case ID_FROM_TAG:
  case ID_TAG:
  case ID_SHOW:
    return true;
  default:
    return false;
  }
}

STEP_NM(step_autointf_newtype,
        NM(DEFTYPE));
error step_autointf_newtype(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  struct node *nt = node->as.DEFTYPE.newtype_expr;
  if (nt == NULL) {
    return 0;
  }

  struct typ *t = nt->typ;
  if (typ_definition_which(t) != DEFTYPE) {
    error e = mk_except_type(mod, node, "newtype argument must be a"
                             " struct, enum, or union, not '%s'", pptyp(mod, t));
    THROW(e);
  }

  struct tit *ms = typ_definition_members(t, DEFFUN, DEFMETHOD, LET, 0);
  while (tit_next(ms)) {
    ident name = tit_ident(ms);;
    if (tit_which(ms) == LET) {
      struct tit *def = tit_let_def(ms);
      name = tit_ident(def);
      tit_next(def);
    }

    if (name == ID_THIS || name == ID_FINAL) {
      continue;
    }

    struct node *existing = node_get_member(node, name);
    if (existing != NULL) {
      continue;
    }

    struct node *m = define_builtin_start(mod, node, node->typ, ms);
    if (m->which == DEFFUN) {
      m->as.DEFFUN.is_newtype_pretend_wrapper = true;
    } else if (m->which == DEFMETHOD) {
      m->as.DEFMETHOD.is_newtype_pretend_wrapper = true;
    }
    define_builtin_catchup(mod, m);
  }

  const ident namet = typ_definition_ident(t);
  const char *namet_s = idents_value(mod->gctx, namet);

  char *n = calloc(sizeof("From_") + strlen(namet_s), 1);
  sprintf(n, "%crom_%s", "fF"[!!isupper(namet_s[0])], namet_s);
  n[sizeof("From_")-1] = tolower(n[sizeof("From_")-1]);
  const ident from_name = idents_add_string(mod->gctx, n, strlen(n));
  free(n);
  const ident to_name = namet;

  GSTART();
  if (node_get_member(node, from_name) == NULL) {
    G0(from, node, DEFFUN,
       from->as.DEFFUN.is_newtype_converter = true;
       from->as.DEFFUN.is_newtype_pretend_wrapper = true;
       G(id, IDENT,
         id->as.IDENT.name = from_name);
       G(ga, GENARGS);
       G(args, FUNARGS,
         G(arg, DEFARG,
           G_IDENT(narg, "x");
           G(targ, DIRECTDEF,
             set_typ(&targ->as.DIRECTDEF.typ, t)));
         G(ret, DEFARG,
           G(nret, IDENT,
             nret->as.IDENT.name = ID_NRETVAL);
           G(tret, IDENT,
             tret->as.IDENT.name = ID_THIS);
           ret->as.DEFARG.is_retval = true));
       G(w, WITHIN));
    deffun_count_args(from);
    define_builtin_catchup(mod, from);
  }

  if (node_get_member(node, to_name) == NULL) {
    G0(to, node, DEFMETHOD,
       to->as.DEFMETHOD.is_newtype_converter = true;
       to->as.DEFMETHOD.is_newtype_pretend_wrapper = true;
       G(id, IDENT,
         id->as.IDENT.name = to_name);
       G(ga, GENARGS);
       G(args, FUNARGS,
         G(self, DEFARG,
           G(nself, IDENT,
             nself->as.IDENT.name = ID_SELF);
           G(rself, UN,
             rself->as.UN.operator = TREFDOT;
             G(tself, IDENT,
               tself->as.IDENT.name = ID_THIS)));
         G(ret, DEFARG,
           G(nret, IDENT,
             nret->as.IDENT.name = ID_NRETVAL);
           G(tret, DIRECTDEF,
             set_typ(&tret->as.DIRECTDEF.typ, t));
           ret->as.DEFARG.is_retval = true));
       G(w, WITHIN));
    deffun_count_args(to);
    define_builtin_catchup(mod, to);
  }

  return 0;
}

static ERROR add_auto_isa_eachisalist(struct module *mod,
                                      struct typ *inferred_intf,
                                      struct typ *intf,
                                      bool *stop,
                                      void *user) {
  struct node *deft = user;
  struct tit *mi = typ_definition_members(intf, DEFMETHOD, DEFFUN, 0);
  while (tit_next(mi)) {
    if (need_auto_member(intf, mi)) {
      add_auto_member(mod, deft, inferred_intf, intf, mi);
    }
  }

  return 0;
}

static void add_auto_isa_isalist(struct module *mod, struct node *deft,
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
}

static void add_auto_isa(struct module *mod, struct node *deft,
                         const struct typ *i) {
  add_auto_isa_isalist(mod, deft, i);

  if (deft->which == DEFINTF) {
    return;
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

static void add_enum_constants(struct module *mod, struct node *node) {
  const size_t count = count_defchoices(node);

  {
    char value[32] = { 0 };
    snprintf(value, ARRAY_SIZE(value), "0x%"PRIx64, count);

    GSTART();
    G0(let, node, LET,
       let->flags |= NODE_IS_GLOBAL_LET;
       G(defn, DEFNAME,
         G_IDENT(valn, "COUNT");
         G(valc, TYPECONSTRAINT,
           G(val, NUMBER,
             val->as.NUMBER.value = strdup(value));
           G(valt, DIRECTDEF,
             set_typ(&valt->as.DIRECTDEF.typ, TBI_UINT)))));

    error e = catchup(mod, NULL, let, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }

  if (count > 64) {
    // FIXME
    return;
  }

  {
    const uint64_t bwall = (1 << count) - 1;

    char value[32] = { 0 };
    snprintf(value, ARRAY_SIZE(value), "0x%"PRIx64, bwall);

    GSTART();
    G0(let, node, LET,
       let->flags |= NODE_IS_GLOBAL_LET;
       G(defn, DEFNAME,
         G_IDENT(valn, "BWALL");
         G(valc, TYPECONSTRAINT,
           G(val, NUMBER,
             val->as.NUMBER.value = strdup(value));
           G(valt, DIRECTDEF,
             set_typ(&valt->as.DIRECTDEF.typ, TBI_U64)))));

    error e = catchup(mod, NULL, let, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

STEP_NM(step_autointf_enum_union_isalist,
        NM(DEFTYPE));
error step_autointf_enum_union_isalist(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    add_enum_constants(mod, node);
    add_auto_isa_isalist(mod, node, TBI_ENUM);
  } else if (node->as.DEFTYPE.kind == DEFTYPE_UNION) {
    add_auto_isa_isalist(mod, node, TBI_UNION);
  }

  return 0;
}

STEP_NM(step_autointf_enum_union,
        NM(DEFTYPE));
error step_autointf_enum_union(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
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
  bool default_ctor, default_dtor, copyable, return_by_copy, equality_by_compare, ordered_by_compare;
  bool trivial_ctor, trivial_dtor, trivial_copy_but_owned,
       trivial_copy, trivial_compare, trivial_equality, trivial_order;
};

static void infer(struct inferred *inferred, const struct node *node) {
  if (node->which == DEFCHOICE) {
    FOREACH_SUB_CONST(f, node) {
      if (!(NM(f->which) & (NM(DEFFIELD) | NM(DEFCHOICE)))) {
        continue;
      }

      infer(inferred, f);
    }
    return;
  }

  if (inferred->default_ctor) {
    inferred->trivial_ctor &= inferred->default_ctor &= typ_isa(node->typ, TBI_DEFAULT_CTOR);
  }
  if (inferred->default_dtor) {
    inferred->trivial_dtor &= inferred->default_dtor &= typ_isa(node->typ, TBI_DEFAULT_DTOR);
  }
  if (inferred->copyable) {
    inferred->trivial_copy &= inferred->copyable &= typ_isa(node->typ, TBI_COPYABLE);
  }
  if (inferred->equality_by_compare) {
    inferred->equality_by_compare &= typ_isa(node->typ, TBI_EQUALITY_BY_COMPARE);
  }
  if (inferred->ordered_by_compare) {
    inferred->ordered_by_compare &= typ_isa(node->typ, TBI_ORDERED_BY_COMPARE);
  }
  if (inferred->return_by_copy) {
    inferred->return_by_copy &= typ_isa(node->typ, TBI_RETURN_BY_COPY);
  }

  if (inferred->trivial_ctor) {
    inferred->trivial_ctor &= typ_isa(node->typ, TBI_TRIVIAL_CTOR);
  }
  if (inferred->trivial_dtor) {
    inferred->trivial_dtor &= typ_isa(node->typ, TBI_TRIVIAL_DTOR);
  }
  if (inferred->trivial_copy_but_owned) {
    inferred->trivial_copy_but_owned &= typ_isa(node->typ, TBI_TRIVIAL_COPY_BUT_OWNED);
  }
  if (inferred->trivial_copy) {
    inferred->trivial_copy &= typ_isa(node->typ, TBI_TRIVIAL_COPY);
  }
  if (inferred->trivial_compare) {
    inferred->trivial_order &= inferred->trivial_equality &= inferred->trivial_compare &= typ_isa(node->typ, TBI_TRIVIAL_COMPARE);
  }
  if (inferred->trivial_equality) {
    inferred->trivial_equality &= typ_isa(node->typ, TBI_TRIVIAL_EQUALITY);
  }
  if (inferred->trivial_order) {
    inferred->trivial_order &= typ_isa(node->typ, TBI_TRIVIAL_ORDER);
  }
}

static void infer_top(struct inferred *inferred, const struct node *node) {
  inferred->trivial_ctor =
    NULL == node_get_member_const(node, ID_CTOR);
  inferred->trivial_copy =
    NULL == node_get_member_const(node, ID_COPY_CTOR);
  inferred->trivial_dtor =
    NULL == node_get_member_const(node, ID_DTOR);
  inferred->trivial_order = inferred->trivial_equality = inferred->trivial_compare =
    NULL == node_get_member_const(node, ID_OPERATOR_LE)
    && NULL == node_get_member_const(node, ID_OPERATOR_LT)
    && NULL == node_get_member_const(node, ID_OPERATOR_GT)
    && NULL == node_get_member_const(node, ID_OPERATOR_GE)
    && NULL == node_get_member_const(node, ID_OPERATOR_COMPARE);
  inferred->trivial_equality =
    NULL == node_get_member_const(node, ID_OPERATOR_EQ)
    && NULL == node_get_member_const(node, ID_OPERATOR_NE);

  FOREACH_SUB_CONST(f, node) {
    if (!(NM(f->which) & (NM(DEFFIELD) | NM(DEFCHOICE)))) {
      continue;
    }

    infer(inferred, f);
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
  } else if (immediate_isa(node, TBI_ERROR_DTOR)) {
    inferred.trivial_dtor = inferred.default_dtor = false;
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

  infer_top(&inferred, node);

skip:
  if (inferred.trivial_ctor || typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    if (node->as.DEFTYPE.kind == DEFTYPE_UNION) {
      if (node->as.DEFTYPE.default_choice != NULL) {
        add_auto_isa(mod, node, TBI_UNION_TRIVIAL_CTOR);
      }
    } else {
      add_auto_isa(mod, node, TBI_TRIVIAL_CTOR);
    }
  } else if (inferred.default_ctor || typ_isa(node->typ, TBI_DEFAULT_CTOR)) {
    add_auto_isa(mod, node, TBI_DEFAULT_CTOR);
  }
  if (inferred.trivial_dtor || typ_isa(node->typ, TBI_TRIVIAL_DTOR)) {
    add_auto_isa(mod, node, TBI_TRIVIAL_DTOR);
  } else if (inferred.default_dtor || typ_isa(node->typ, TBI_DEFAULT_DTOR)) {
    add_auto_isa(mod, node, TBI_DEFAULT_DTOR);
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

STEP_NM(step_autointf_inherit,
        NM(DEFTYPE) | NM(DEFINTF));
error step_autointf_inherit(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *isalist = subs_at(node, IDX_ISALIST);
  FOREACH_SUB(isa, isalist) {
    struct typ *i0 = typ_generic_functor(isa->typ);
    if (i0 == NULL || !typ_equal(i0, TBI_INHERIT)) {
      continue;
    }

    struct typ *what = typ_generic_arg(isa->typ, 0);
    struct typ *t = typ_generic_arg(isa->typ, 1);
    if (typ_isa(t, what)) {
      add_auto_isa(mod, node, what);
    }
  }
  return 0;
}
