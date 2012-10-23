#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node);

error step_add_scopes(struct module *mod, struct node *node);

error step_lexical_scoping(struct module *mod, struct node *node);
error step_add_builtins(struct module *mod, struct node *node);
error step_add_implicit_variables(struct module *mod, struct node *node);
error step_type_destructuring(struct module *mod, struct node *node);
error step_type_inference(struct module *mod, struct node *node);
error step_operator_call_inference(struct module *mod, struct node *node);
error step_unary_call_inference(struct module *mod, struct node *node);
error step_ctor_call_inference(struct module *mod, struct node *node);
error step_call_arguments_prepare(struct module *mod, struct node *node);
error step_temporary_inference(struct module *mod, struct node *node);
error step_validation(struct module *mod, struct node *node);

error pass(struct module *mod, struct node *root, step *down_steps, step *up_steps);

#endif
