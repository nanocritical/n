#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error step_add_scopes(struct module *mod, struct node *node, void *, bool *);

error step_stop_submodules(struct module *mod, struct node *node, void *, bool *);
error step_lexical_scoping(struct module *mod, struct node *node, void *, bool *);
error step_detect_deftype_kind(struct module *mod, struct node *node, void *, bool *);
error step_assign_deftype_which_values(struct module *mod, struct node *node, void *, bool *);
error step_add_builtin_members(struct module *mod, struct node *node, void *, bool *);
error step_add_builtin_functions(struct module *mod, struct node *node, void *, bool *);
error step_add_builtin_methods(struct module *mod, struct node *node, void *, bool *);
error step_add_codegen_variables(struct module *mod, struct node *node, void *, bool *);
error step_type_destruct_mark(struct module *mod, struct node *node, void *, bool *);
error step_type_definitions(struct module *mod, struct node *node, void *, bool *);
error step_type_gather_returns(struct module *mod, struct node *node, void *, bool *);
error step_type_gather_excepts(struct module *mod, struct node *node, void *, bool *);
error step_type_inference(struct module *mod, struct node *node, void *, bool *);
error step_type_inference_isalist(struct module *mod, struct node *node, void *, bool *);
error step_operator_call_inference(struct module *mod, struct node *node, void *, bool *);
error step_unary_call_inference(struct module *mod, struct node *node, void *, bool *);
error step_ctor_call_inference(struct module *mod, struct node *node, void *, bool *);
error step_gather_generics(struct module *mod, struct node *node, void *, bool *);
error step_call_arguments_prepare(struct module *mod, struct node *node, void *, bool *);
error step_temporary_inference(struct module *mod, struct node *node, void *, bool *);

error pass(struct module *mod, struct node *root, const step *steps, const step *up_steps,
           void *user);

#endif
