#include "ssa.h"

#include "passbody.h"

static void ssa_sub(struct module *mod, struct node *statement,
                    struct node *node, struct node *sub) {
  if (sub->which == IDENT
      || sub->which == CALLNAMEDARG
      || (sub->flags & NODE_IS_TYPE)
      || (sub->which == UN && OP_KIND(sub->as.UN.operator) == OP_UN_REFOF)
      || (sub->which == BIN && OP_KIND(sub->as.BIN.operator) == OP_BIN_ACC)) {
    return;
  }

  const ident g = gensym(mod);
  struct node *replacement = mk_node(mod, node, IDENT);
  replacement->as.IDENT.name = g;
  node_subs_remove(node, replacement);
  node_subs_replace(node, sub, replacement);

  struct node *statement_parent = node_parent(statement);
  struct node *let = mk_node(mod, statement_parent, LET);
  node_subs_remove(statement_parent, let);
  node_subs_insert_before(statement_parent, statement, let);
  struct scope *scope = &statement_parent->scope;

  struct node *defp = mk_node(mod, let, DEFPATTERN);
  defp->as.DEFPATTERN.is_ssa_var = true;

  node_subs_append(defp, sub);
  struct node *name = mk_node(mod, defp, IDENT);
  name->as.IDENT.name = g;

  const struct node *except[] = { sub, NULL };
  error e = catchup(mod, except, let, scope, CATCHUP_BEFORE_CURRENT);
  assert(!e);

  e = catchup(mod, NULL, replacement, &node->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void ssa_all_subs(struct module *mod, struct node *statement,
                         struct node *node, struct node *first) {
  struct node *nxt, *sub = first != NULL ? first : subs_first(node);
  if (sub == NULL) {
    return;
  }

  do {
    // 'sub' is moved by ssa_sub(), so we must store the next one now.
    nxt = next(sub);
    ssa_sub(mod, statement, node, sub);
    sub = nxt;
  } while (sub != NULL);
}

STEP_FILTER(step_ssa_convert,
            ~(SF(MODULE) | SF(MODULE_BODY) | STEP_FILTER_DEFS));
error step_ssa_convert(struct module *mod, struct node *node,
                       void *user, bool *stop) {
  DSTEP(mod, node);
  if (mod->state->fun_state == NULL
      || mod->state->fun_state->block_state == NULL) {
    return 0;
  }

  struct node *statement = mod->state->fun_state->block_state->current_statement;
  switch (node->which) {
  case NUL:
  case IDENT:
  case NUMBER:
  case BOOL:
  case STRING:
    // noop
    break;

  case BIN:
    {
      struct node *left = subs_first(node);
      if (left->which == UN && OP_KIND(left->as.UN.operator) == OP_UN_DEREF) {
        ssa_all_subs(mod, statement, node, subs_last(node));
        break;
      }
      // fallthrough
    }
  case SIZEOF:
  case ALIGNOF:
  case UN:
  case TUPLE:
  case TUPLEEXTRACT:
  case TUPLENTH:
  case CALLNAMEDARG:
  case INIT:
  case RETURN:
  case PRE:
  case POST:
  case INVARIANT:
  case THROW:
    ssa_all_subs(mod, statement, node, NULL);
    break;

  case CALL:
    if (subs_count_atleast(node, 2)) {
      ssa_all_subs(mod, statement, node, subs_at(node, 1));
    }
    break;

  case WHILE:
  case MATCH:
    ssa_sub(mod, statement, node, subs_first(node));
    break;
  case IF:
    FOREACH_SUB_EVERY(cond, node, 0, 2) {
      if (next(cond) == NULL) {
        // else
        break;
      }

      // FIXME: places all the temporaries before the IF, which is really
      // wishfully thinking that the optimizer will see that they're only
      // needed in their respective branch. We should rewrite IF .. ELIF ..
      // ELSE into IF .. ELSE IF .. ELSE .. to solve this.
      ssa_sub(mod, statement, node, cond);
    }
    break;

  case EXCEP:
  case CATCH:
  case TRY:
  case FOR:
  case LAMBDA:
  case FUTURE:
  case BLOCK:
  case BREAK:
  case CONTINUE:
  case NOOP:
  case JUMP:
  case LANDING:
  case PHI:
  case TYPECONSTRAINT:
  case DYN:
  case DEFFUN:
  case DEFTYPE:
  case DEFINCOMPLETE:
  case DEFMETHOD:
  case DEFINTF:
  case DEFNAME:
  case DEFPATTERN:
  case FUNARGS:
  case DEFARG:
  case GENARGS:
  case DEFGENARG:
  case SETGENARG:
  case LET:
  case DEFFIELD:
  case DEFCHOICE:
  case DELEGATE:
  case EXAMPLE:
  case WITHIN:
  case ISALIST:
  case ISA:
  case IMPORT:
  case MODULE:
  case MODULE_BODY:
  case ROOT_OF_ALL:
  case DIRECTDEF:
    // noop
    break;

  default:
    assert(false);
    break;
  }

  return 0;
}

