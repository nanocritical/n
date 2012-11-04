#include "firstpass.h"

error pass(struct module *mod, struct node *root, step *down_steps, step *up_steps) {
  error e;
  if (root == NULL) {
    root = &mod->root;
  }

  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    e = down_steps[s](mod, root);
    EXCEPT(e);
  }

  for (size_t n = 0; n < root->subs_count; ++n) {
    struct node *node = root->subs[n];
    e = pass(mod, node, down_steps, up_steps);
    EXCEPT(e);
  }

  for (size_t s = 0; up_steps[s] != NULL; ++s) {
    e = up_steps[s](mod, root);
    EXCEPT(e);
  }

  return 0;
}

static void insert_last_at(struct node *node, size_t pos) {
  struct node *tmp = node->subs[pos];
  node->subs[pos] = node->subs[node->subs_count - 1];
  for (size_t n = pos + 1; n < node->subs_count - 1; ++n) {
    struct node *tmptmp = node->subs[n];
    node->subs[n] = tmp;
    tmp = tmptmp;
  }
  node->subs[node->subs_count - 1] = tmp;
}

static struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind) {
  struct node *n = node_new_subnode(mod, parent);
  n->which = kind;
  return n;
}

error step_add_builtin_members(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  struct node *let = mk_node(mod, node, LET);
  struct node *defn = mk_node(mod, let, DEFNAME);
  struct node *name = mk_node(mod, defn, IDENT);
  name->as.IDENT.name = ID_THIS;
  struct node *expr = mk_node(mod, defn, IDENT);
  expr->as.IDENT.name = node_ident(node);

  insert_last_at(node, node->subs[1]->which == ISALIST ? 2 : 1);

  return 0;
}

error step_add_builtin_functions(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFTYPE:
    break;
  default:
    return 0;
  }

  return 0;
}

error step_add_builtin_methods(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFTYPE:
    break;
  default:
    return 0;
  }

  return 0;
}

error step_add_builtin_self(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  if (node->as.DEFMETHOD.toplevel.is_prototype) {
    return 0;
  }

  struct node *block = node->subs[node->subs_count - 1];

  struct node *let = mk_node(mod, block, LET);
  struct node *defn = mk_node(mod, let, DEFNAME);
  struct node *name = mk_node(mod, defn, IDENT);
  name->as.IDENT.name = ID_SELF;
  mk_node(mod, defn, NUL);

// FIXME
//  struct node *ref = mk_node(mod, constraint, UN);
//  ref->as.UN.operator = node->as.DEFMETHOD.access;
//  struct node *typename = mk_node(mod, ref, IDENT);
//  typename->as.IDENT.name = ID_THIS;

  insert_last_at(block, 0);

  return 0;
}

error step_add_codegen_variables(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case FOR:
  case MATCH:
  case TRY:
  case DEFTYPE:
  case DEFINTF:
  case LET:
    break;
  default:
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  default:
    break;
  }

  return 0;
}

error step_add_scopes(struct module *mod, struct node *node) {
  // Builtin types already have a scope.
  if (node->scope == NULL) {
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

static size_t node_fun_args_count(struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  if (def->subs[def->subs_count-1]->which == BLOCK) {
    return def->subs_count-3;
  } else {
    return def->subs_count-2;
  }
}

error step_lexical_scoping(struct module *mod, struct node *node) {
  struct node *id = NULL;
  struct scope *sc = NULL;
  error e;

  struct node *container = NULL;

  switch (node->which) {
  case FOR:
    id = node->subs[0];
    sc = node->scope;
    break;
  case DEFFUN:
    id = node->subs[0];
    if (node->as.DEFFUN.toplevel.scope_name == 0) {
      sc = node->scope->parent;
    } else {
      e = scope_lookup_ident(&container, mod, node->scope->parent,
                             node->as.DEFFUN.toplevel.scope_name);
      sc = container->scope;
    }
    EXCEPT(e);
    break;
  case DEFMETHOD:
    id = node->subs[0];
    if (node->as.DEFMETHOD.toplevel.scope_name == 0) {
      sc = node->scope->parent;
    } else {
      e = scope_lookup_ident(&container, mod, node->scope->parent,
                             node->as.DEFMETHOD.toplevel.scope_name);
      sc = container->scope;
    }
    EXCEPT(e);
    break;
  case DEFTYPE:
  case DEFINTF:
  case DEFFIELD:
    id = node->subs[0];
    sc = node->scope->parent;
    break;
  case DEFNAME:
    id = node->subs[0];
    sc = node->scope->parent->parent;
    break;
  case TRY:
    id = node->subs[1];
    sc = node->scope;
    break;
  default:
    return 0;
  }

  e = scope_define(mod, sc, id, node);
  EXCEPT(e);

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    {
      for (size_t n = 0; n < node_fun_args_count(node); ++n) {
        assert(node->subs[n+1]->which == TYPECONSTRAINT);
        e = scope_define(mod, node->scope, node->subs[n+1]->subs[0],
                         node->subs[n+1]->subs[1]);
        EXCEPT(e);
      }
    }
    break;
  default:
    break;
  }

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  if (node_is_prototype(node)) {
    return 0;
  }

  const size_t first = node->subs[1]->which == ISALIST ? 2 : 1;
  for (size_t s = first; s < node->subs_count; ++s) {
    struct node *member = node->subs[s];
    struct node *name = member->subs[0];

    if (member->which == DEFFIELD) {
      e = scope_define(mod, node->scope, name, member);
      EXCEPT(e);
    }
  }

  return 0;
}

error step_type_destruct_mark(struct module *mod, struct node *node) {
  if (node->which == MODULE) {
    return 0;
  }

  struct typ *pending = typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT);
  size_t begin = 0, end = node->subs_count, incr = 1;

  switch (node->which) {
  case TYPECONSTRAINT:
    end = 1;
    goto mark_subs;
  case TRY:
    begin = 1;
    end = 2;
    goto mark_subs;
  case MATCH:
    begin = 1;
    incr = 2;
    goto mark_subs;
  case BIN:
    switch (OP_KIND(node->as.BIN.operator)) {
    case OP_BIN_RHS_TYPE:
      end = 1;
      goto mark_subs;
    case OP_BIN_ACC:
    case OP_BIN_SYM:
    case OP_BIN_SYM_BOOL:
    case OP_BIN_SYM_NUM:
    case OP_BIN_RHS_U16:
      goto inherit;
    default:
      assert(FALSE);
      break;
    }
  case INIT:
    begin = 1;
    goto mark_subs;
  case CALL:
    begin = 1;
    goto mark_subs;
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
  case DEFFIELD:
  case DEFNAME:
    node->subs[0]->typ = typ_lookup_builtin(mod, TBI__NOT_TYPEABLE);
    return 0;
  default:
    goto inherit;
  }

inherit:
  if (node->typ == pending) {
    goto mark_subs;
  }
  return 0;

mark_subs:
  for (size_t n = begin; n < end; n += incr){
    node->subs[n]->typ = pending;
  }
  return 0;
}

error step_type_definitions(struct module *mod, struct node *node) {
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  // Builtins are already typed.
  if (node->typ == NULL) {
    node->typ = typ_new(mod, node, TYPE_DEF, 0, 0);
    node->is_type = TRUE;
  }

  return 0;
}

static struct node *node_fun_retval(struct node *def) {
  if (def->subs[def->subs_count-1]->which == BLOCK) {
    return def->subs[def->subs_count-2];
  } else {
    return def->subs[def->subs_count-1];
  }
}

error step_type_gather_returns(struct module *mod, struct node *node) {
  if (node->which == MODULE) {
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_return_set(mod, node_fun_retval(node));
    break;
  default:
    break;
  }
  return 0;
}

error step_type_gather_excepts(struct module *mod, struct node *node) {
  if (node->which == MODULE) {
    return 0;
  }

  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod);
    return 0;
  case EXCEP:
    module_excepts_push(mod, node);
    break;
  default:
    break;
  }
  return 0;
}

static error type_destruct(struct module *mod, struct node *node, struct typ *constraint);

static error type_inference_unary_call(struct module *mod, struct node *node, struct node *def) {
  switch (def->which) {
  case DEFFUN:
  case DEFMETHOD:
    node->typ = node_fun_retval(def)->typ;
    break;
  default:
    assert(FALSE);
  }
  return 0;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;

  switch (OP_KIND(node->as.UN.operator)) {
  case OP_UN:
    node->typ = node->subs[0]->typ;
    node->is_type = node->subs[0]->is_type;
    break;
  case OP_UN_BOOL:
    if (typ_is_concrete(mod, node->subs[0]->typ)) {
      e = typ_check(mod, node, node->subs[0]->typ, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
      node->typ = node->subs[0]->typ;
    } else {
      e = type_destruct(mod, node, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
    }
    break;
  case OP_UN_NUM:
    e = typ_check_numeric(mod, node, node->subs[0]->typ);
    EXCEPT(e);
    node->typ = node->subs[0]->typ;
    break;
  case OP_UN_REF:
    e = typ_check_reference(mod, node, node->subs[0]->typ);
    EXCEPT(e);
    node->typ = node->subs[0]->typ;
    node->is_type = node->subs[0]->is_type;
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  error e;
  if (typ_is_concrete(mod, node->subs[0]->typ)
      && typ_is_concrete(mod, node->subs[1]->typ)) {
    e = typ_check(mod, node, node->subs[0]->typ, node->subs[1]->typ);
    EXCEPT(e);
  } else if (typ_is_concrete(mod, node->subs[0]->typ)) {
    e = type_destruct(mod, node->subs[1], node->subs[0]->typ);
    EXCEPT(e);
  } else if (typ_is_concrete(mod, node->subs[1]->typ)) {
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
    EXCEPT(e);
  }

  e = typ_unify(&node->typ, mod, node,
                node->subs[0]->typ, node->subs[1]->typ);
  EXCEPT(e);

  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM_BOOL:
    e = typ_check(mod, node, node->typ, typ_lookup_builtin(mod, TBI_BOOL));
    EXCEPT(e);
    break;
  case OP_BIN_SYM_NUM:
    e = typ_check_numeric(mod, node, node->typ);
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;
  struct node *def = NULL;
  e = scope_lookup(&def, mod, node->scope, node->subs[0]);
  EXCEPT(e);
  struct node *field = NULL;
  e = scope_lookup(&field, mod, def->scope, node->subs[1]);
  EXCEPT(e);

  switch (field->which) {
  case DEFFUN:
  case DEFMETHOD:
    return type_inference_unary_call(mod, node, field);
  default:
    node->typ = field->typ;
    return 0;
  }
}

static error type_inference_bin_rhs_u16(struct module *mod, struct node *node) {
  error e;
  e = typ_check_numeric(mod, node->subs[0], node->subs[0]->typ);
  EXCEPT(e);
  // FIXME handle the generic number case.
  e = type_destruct(mod, node->subs[1], typ_lookup_builtin(mod, TBI_U16));
  EXCEPT(e);
  node->typ = node->subs[0]->typ;
  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  if (!node->subs[1]->is_type) {
    e = mk_except_type(mod, node->subs[1], "right-hand side not a type");
    EXCEPT(e);
  }
  e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
  EXCEPT(e);
  node->typ = node->subs[1]->typ;
  return 0;
}

static error type_inference_bin(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
    return type_inference_bin_sym(mod, node);
  case OP_BIN_RHS_U16:
    return type_inference_bin_rhs_u16(mod, node);
  case OP_BIN_ACC:
    return type_inference_bin_accessor(mod, node);
  case OP_BIN_RHS_TYPE:
    return type_inference_bin_rhs_type(mod, node);
  default:
    assert(FALSE);
    return 0;
  }
}

static error type_inference_tuple(struct module *mod, struct node *node) {
  node->typ = typ_new(mod, typ_lookup_builtin(mod, TBI_PSEUDO_TUPLE)->definition,
                      TYPE_TUPLE, node->subs_count, 0);
  for (size_t n = 0; n < node->typ->gen_arity; ++n) {
    node->typ->gen_args[n] = node->subs[n]->typ;

    if (n > 0 && node->is_type != node->subs[n]->is_type) {
      error e = mk_except_type(mod, node->subs[n], "tuple combines values and types");
      EXCEPT(e);
    }
    node->is_type |= node->subs[n]->is_type;
  }
  module_needs_instance(mod, node->typ);
  return 0;
}

static error type_inference_init(struct module *mod, struct node *node) {
  struct node *def = node->subs[0]->typ->definition;

  for (size_t n = 1; n < node->subs_count; n += 2) {
    struct node *field_name = node->subs[n];
    struct node *field = NULL;
    error e = scope_lookup(&field, mod, def->scope, field_name);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[n+1], field->typ);
    EXCEPT(e);
  }

  node->typ = node->subs[0]->typ;
  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];
  error e;

  if (node->subs_count == 1) {
    return type_inference_unary_call(mod, node, fun->typ->definition);
  }

  for (size_t n = 1; n < node->subs_count; ++n) {
    if (n > 0 && node->is_type != node->subs[n]->is_type) {
      e = mk_except_type(mod, node->subs[n], "call combines value and type arguments");
      EXCEPT(e);
    }
    node->is_type |= node->subs[n]->is_type;
  }

  if (node->is_type) {
    assert(FALSE);
    //switch (fun->typ->definition->typ->which) {
    //case TYPE_FUNCTION:
    //  instance = instantiate_function(mod, fun, node);
    //  node->typ = instance->typ;
    //  break;
    //case TYPE_DEF:
    //  instance = instantiate_type(mod, fun, node);
    //  node->typ = instance->typ;
    //  break;
    //default:
    //  assert(FALSE);
    //}
    //
    //module_needs_instance(mod, node->typ);
  } else {
    if (node->subs_count - 1 != fun->typ->fun_arity) {
      e = mk_except_type(mod, node, "");
      EXCEPT(e);
    }
    for (size_t n = 1; n < node->subs_count; ++n) {
      e = type_destruct(mod, node->subs[n], fun->typ->fun_args[n-1]);
      EXCEPT(e);
    }
    node->typ = node_fun_retval(fun->typ->definition)->typ;
  }

  return 0;
}

static error type_inference_try(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct try_excepts *t = &mod->trys[mod->trys_count - 1];

  if (t->count == 0) {
    e = mk_except(mod, node, "try block has no except statement, catch is unreachable");
    EXCEPT(e);
  }

  struct typ *u = t->excepts[0]->typ;
  for (size_t n = 1; n < t->count; ++n) {
    struct node *exc = t->excepts[n];
    e = typ_unify(&u, mod, exc, u, exc->typ);
    EXCEPT(e);
  }

  e = type_destruct(mod, node->subs[1], u);
  EXCEPT(e);

  module_excepts_close_try(mod);
  return 0;
}

static error type_destruct(struct module *mod, struct node *node, struct typ *constraint) {
  error e;
  struct node *def = NULL;

  assert(node->typ != typ_lookup_builtin(mod, TBI__NOT_TYPEABLE));

  if (node->typ != NULL
      && node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      && typ_is_concrete(mod, node->typ)) {
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    return 0;
  }

  switch (node->which) {
  case NUL:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_LITERAL_NULL), constraint);
    EXCEPT(e);
    break;
  case NUMBER:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_LITERAL_NUMBER), constraint);
    EXCEPT(e);
    break;
  case STRING:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_STRING), constraint);
    EXCEPT(e);
    break;
  case IDENT:
    // FIXME make sure ident not used before definition.
    // FIXME let x, (y, z) = i32, (i32, i32)
    // In this case, y (for instance), will not have is_type set properly.
    // is_type needs to be set recursively when descending via
    // type_destruct.
    e = scope_lookup(&def, mod, node->scope, node);
    EXCEPT(e);

    //FIXME: remove, DEFNAME above does that now.
    assert(def != node->scope->parent->node);

    if (def->which == DEFNAME) {
      e = type_destruct(mod, def, constraint);
      EXCEPT(e);
    }
    node->typ = def->typ;
    break;
  case DEFNAME:
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[1], node->typ);
    EXCEPT(e);
    break;
  case BIN:
    switch (OP_KIND(node->as.BIN.operator)) {
    case OP_BIN_ACC:
      e = type_inference_bin_accessor(mod, node);
      EXCEPT(e);
      e = typ_unify(&node->typ, mod, node, node->typ, constraint);
      EXCEPT(e);
      break;
    case OP_BIN_SYM:
    case OP_BIN_SYM_BOOL:
    case OP_BIN_SYM_NUM:
      e = type_destruct(mod, node->subs[0], constraint);
      EXCEPT(e);
      e = type_destruct(mod, node->subs[1], constraint);
      EXCEPT(e);
      e = typ_unify(&node->typ, mod, node, node->subs[0]->typ, node->subs[1]->typ);
      EXCEPT(e);

      switch (OP_KIND(node->as.BIN.operator)) {
      case OP_BIN_SYM_BOOL:
        e = typ_check(mod, node, node->typ, typ_lookup_builtin(mod, TBI_BOOL));
        EXCEPT(e);
        break;
      case OP_BIN_SYM_NUM:
        e = typ_check_numeric(mod, node, node->typ);
        EXCEPT(e);
        break;
      default:
        break;
      }
      break;
    case OP_BIN_RHS_U16:
      e = type_destruct(mod, node->subs[0], constraint);
      EXCEPT(e);
      e = type_destruct(mod, node->subs[1], typ_lookup_builtin(mod, TBI_U16));
      EXCEPT(e);
      break;
    case OP_BIN_RHS_TYPE:
      if (!node->subs[1]->is_type) {
        e = mk_except_type(mod, node->subs[1], "right-hand side of type constraint is not a type");
        EXCEPT(e);
      }
      e = typ_unify(&node->typ, mod, node->subs[1], node->subs[1]->typ, constraint);
      EXCEPT(e);
      e = type_destruct(mod, node->subs[0], node->typ);
      EXCEPT(e);
      break;
    default:
      assert(FALSE);
    }
    node->typ = constraint;
    break;
  case TYPECONSTRAINT:
    e = type_destruct(mod, node->subs[1], constraint);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
    EXCEPT(e);
    node->typ = constraint;
    break;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    for (size_t n = 0; n < node->subs_count; ++n) {
      e = type_destruct(mod, node->subs[n], node->typ->gen_args[n]);
      EXCEPT(e);
    }
    break;
  case INIT:
    e = type_inference_init(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

error step_type_inference(struct module *mod, struct node *node) {
  error e;
  struct node *def = NULL;

  if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      || node->typ == typ_lookup_builtin(mod, TBI__NOT_TYPEABLE)) {
    return 0;
  }

  switch (node->which) {
  case NUL:
    node->typ = typ_lookup_builtin(mod, TBI_LITERAL_NULL);
    goto ok;
  case IDENT:
    e = scope_lookup(&def, mod, node->scope, node);
    EXCEPT(e);
    if (def->which == DEFFUN || def->which == DEFMETHOD) {
      e = type_inference_unary_call(mod, node, def);
      EXCEPT(e);
    } else {
      node->typ = def->typ;
      node->is_type = def->which == DEFTYPE;
    }
    assert(def->typ->which != TYPE__MARKER);
    assert(node->typ->which != TYPE__MARKER);
    goto ok;
  case NUMBER:
    node->typ = typ_lookup_builtin(mod, TBI_LITERAL_NUMBER);
    goto ok;
  case STRING:
    node->typ = typ_lookup_builtin(mod, TBI_STRING);
    goto ok;
  case BIN:
    e = type_inference_bin(mod, node);
    EXCEPT(e);
    goto ok;
  case UN:
    e = type_inference_un(mod, node);
    EXCEPT(e);
    goto ok;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    goto ok;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    goto ok;
  case INIT:
    e = type_inference_init(mod, node);
    EXCEPT(e);
    goto ok;
  case RETURN:
    if (node->subs_count > 0) {
      e = type_destruct(mod, node->subs[0], module_return_get(mod)->typ);
      EXCEPT(e);
      node->typ = node->subs[0]->typ;
    } else {
      node->typ = typ_lookup_builtin(mod, TBI_VOID);
    }
    goto ok;
  case EXCEP:
  case BLOCK:
  case BREAK:
  case CONTINUE:
  case PASS:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case FOR:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
    EXCEPT(e);
    goto ok;
  case WHILE:
  case IF:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    for (size_t n = 0; n < node->subs_count; n += 2) {
      e = typ_check(mod, node->subs[n], node->subs[n]->typ, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
    }
    goto ok;
  case MATCH:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    for (size_t n = 1; n < node->subs_count; n += 2) {
      e = type_destruct(mod, node->subs[n], node->subs[0]->typ);
      EXCEPT(e);
    }
    goto ok;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    goto ok;
  case TYPECONSTRAINT:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    goto ok;
  case DEFFUN:
  case DEFMETHOD:
    node->typ = typ_new(mod, node, TYPE_FUNCTION, 0, node_fun_args_count(node));
    for (size_t n = 0; n < node->typ->fun_arity; ++n) {
      node->typ->fun_args[n] = node->subs[n+1]->typ;
    }
    node->is_type = TRUE;
    module_return_set(mod, NULL);
    goto ok;
  case DEFINTF:
  case DEFTYPE:
    goto ok;
  case DEFNAME:
    // FIXME handle case where there is a constraint on the DEFNAME;
    node->typ = node->subs[1]->typ;
    node->is_type = node->subs[1]->is_type;
    goto ok;
  case LET:
  case DEFFIELD:
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
  case EXAMPLE:
  case ISALIST:
  case MODULE:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  default:
    goto ok;
  }

ok:
  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

error step_operator_call_inference(struct module *mod, struct node *node) {
  return 0;
}

error step_unary_call_inference(struct module *mod, struct node *node) {
  return 0;
}

error step_ctor_call_inference(struct module *mod, struct node *node) {
  return 0;
}

error step_call_arguments_prepare(struct module *mod, struct node *node) {
  return 0;
}

error step_temporary_inference(struct module *mod, struct node *node) {
  return 0;
}
