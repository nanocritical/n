#include "passsem.h"

#include "constraints.h"

#include "passes.h"
#include "phi.h"
#include "passzero.h"
#include "passbody.h"
#include "inference.h"

STEP_NM(step_stop_already_early_constraining,
        NM(LET) | NM(ISA) | NM(GENARGS) | NM(FUNARGS) |
        NM(DEFGENARG) | NM(SETGENARG) | NM(DEFFIELD) | NM(DEFCHOICE) |
        NM(WITHIN));
error step_stop_already_early_constraining(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);

  *stop = node->constraint != NULL;

  return 0;
}

static error pass_early_constraining(struct module *mod, struct node *root,
                                     void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_block);
    DOWN_STEP(step_stop_already_early_constraining);
    ,
    UP_STEP(step_constraint_inference);
    UP_STEP(step_remove_typeconstraints);
    ,
    );
  return 0;
}

static error early_constraining(struct module *mod, struct node *node) {
  PUSH_STATE(mod->state->step_state);
  bool tentatively_saved = mod->state->tentatively;
  if (mod->state->prev != NULL) {
    mod->state->tentatively |= mod->state->prev->tentatively;
  }

  error e = pass_early_constraining(mod, node, NULL, -1);
  EXCEPT(e);

  mod->state->tentatively = tentatively_saved;
  POP_STATE(mod->state->step_state);

  return 0;
}

static STEP_NM(step_constrain_definitions,
               STEP_NM_DEFS_NO_FUNS);
static error step_constrain_definitions(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  error e = step_constraint_inference(mod, node, user, stop);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_constrain_genargs,
               STEP_NM_DEFS);
static error step_constrain_genargs(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *genargs = subs_at(node, IDX_GENARGS);
  error e = early_constraining(mod, genargs);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_constrain_aliases,
               NM(LET));
static error step_constrain_aliases(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *par = parent(node);
  if ((node_is_at_top(node) || node_is_at_top(par))
      && subs_first(node)->which == DEFALIAS) {
    error e = early_constraining(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_constrain_isalist,
               NM(ISA));
static error step_constrain_isalist(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  error e = early_constraining(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_constrain_lets,
               NM(LET));
static error step_constrain_lets(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *par = parent(node);
  if ((node_is_at_top(node) || node_is_at_top(par))
      && subs_first(node)->which == DEFNAME) {
    error e = early_constraining(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_constrain_deffields,
               NM(DEFCHOICE) | NM(DEFFIELD));
static error step_constrain_deffields(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  error e = early_constraining(mod, node);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_constrain_deffuns,
               NM(DEFMETHOD) | NM(DEFFUN));
static error step_constrain_deffuns(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  error e = early_constraining(mod, node);
  EXCEPT(e);

  return 0;
}

static error passsemfwd0(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_definitions);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd1(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_aliases);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd2(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    DOWN_STEP(step_constrain_genargs);
    ,
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd3(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_isalist);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd4(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_lets);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd5(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_deffields);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

static error passsemfwd6(struct module *mod, struct node *root,
                         void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_funblock);
    ,
    UP_STEP(step_constrain_deffuns);
    ,
    FINALLY_STEP(step_pop_state);
    );
  return 0;
}

a_pass passsemfwd[] = {
  passsemfwd0,
  passsemfwd1,
  passsemfwd2,
  passsemfwd3,
  passsemfwd4,
  passsemfwd5,
  passsemfwd6,
};

static error passsembody0(struct module *mod, struct node *root,
                          void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_already_early_constraining);
    DOWN_STEP(step_stop_generic_functor);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_branching_down);
    DOWN_STEP(step_branching_block_down);
    ,
    UP_STEP(step_constraint_inference);
    UP_STEP(step_check_exhaustive_match);
    UP_STEP(step_branching_up);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

a_pass passsembody[] = {
  passsembody0,
};
