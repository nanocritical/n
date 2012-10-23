#include "firstpass.h"

error pass(struct module *mod, struct node *root, step *down_steps, step *up_steps) {
  error e;
  if (root == NULL) {
    root = &mod->node;
  }

  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    e = down_steps[s](mod, root);
    EXCEPT(e);
  }

  for (size_t n = 0; n < root->subs_count; ++n) {
    struct node *node = &root->subs[n];
    pass(mod, node, down_steps, up_steps);
  }

  for (size_t s = 0; up_steps[s] != NULL; ++s) {
    e = up_steps[s](mod, root);
    EXCEPT(e);
  }

  return 0;
}

error step_add_scopes(struct module *mod, struct node *node) {
  switch (node->which) {
  case BLOCK:
  case FOR:
  case WHILE:
  case MATCH:
  case TRY:
  case DEFFUN:
  case DEFTYPE:
  case DEFMETHOD:
  case DEFINTF:
  case DEFLET:
  case MODULE:
    break;
  default:
    return 0;
  }

  node->scope = scope_new(node);

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = &node->subs[n];
    if (s->scope != NULL) {
      assert(s->scope->parent == NULL);
      s->scope->parent = node->scope;
    }
  }

  return 0;
}

error step_lexical_scoping(struct module *mod, struct node *node) {
  struct node *id = NULL;
  struct scope *sc = NULL;

  switch (node->which) {
  case FOR:
    id = &node->subs[0];
    sc = node->scope;
    break;
  case DEFFUN:
  case DEFTYPE:
  case DEFMETHOD:
  case DEFINTF:
  case DEFLET:
    id = &node->subs[0];
    sc = node->scope->parent;
    break;
  case TRY:
    id = &node->subs[1];
    sc = node->scope;
    break;
  default:
    return 0;
  }

  scope_add(mod, sc, id, node);

  return 0;
}

error step_add_builtins(struct module *mod, struct node *node) {
  return 0;
}

error step_add_implicit_variables(struct module *mod, struct node *node) {
  switch (node->which) {
  case BLOCK:
  case FOR:
  case WHILE:
  case MATCH:
  case TRY:
  case DEFFUN:
  case DEFTYPE:
  case DEFMETHOD:
  case DEFINTF:
  case DEFLET:
  case DELEGATE:
    break;
  default:
    return 0;
  }

  return 0;
}

error step_type_destructuring(struct module *mod, struct node *node) {
  return 0;
}

error step_type_inference(struct module *mod, struct node *node) {
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

error step_validation(struct module *mod, struct node *node) {
  return 0;
}
