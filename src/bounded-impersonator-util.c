#include "escheme.h"
#include "schpriv.h"

static Scheme_Object *remove_impersonator(int argc, Scheme_Object **argv);
static Scheme_Object *chaperone_opaque_struct(int argc, Scheme_Object **argv);
static Scheme_Object *impersonator_of;

Scheme_Object *scheme_initialize(Scheme_Env *namespace)
{
  Scheme_Env *mod_env;

  impersonator_of = scheme_builtin_value("impersonator-of?");
  
  mod_env = scheme_primitive_module(scheme_intern_symbol("bounded-impersonator-util"),
				    namespace);
  Scheme_Object* remove_impersonator_prim =
    scheme_make_prim_w_arity(remove_impersonator, "remove-impersonator", 3, 3);
  scheme_add_global("remove-impersonator",
		    remove_impersonator_prim,
		    mod_env);

  Scheme_Object* chaperone_opaque_struct_prim =
    scheme_make_prim_w_arity(chaperone_opaque_struct,
			     "chaperone-opaque-struct",
			     1, -1);
  scheme_add_global("chaperone-opaque-struct",
		    chaperone_opaque_struct_prim,
		    mod_env);
  
  scheme_finish_primitive_module(mod_env);
  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *namespace)
{
  return scheme_initialize(namespace);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("bounded-impersonator-util");
}


static Scheme_Object *remove_impersonator(int argc, Scheme_Object **argv)
{
  Scheme_Object *value = argv[0];
  Scheme_Object *impersonator = argv[1];
  Scheme_Object *orig = argv[2];

  if (!SCHEME_CHAPERONEP(value)) {
    scheme_wrong_contract("remove-impersonator", "impersonator?", 0, argc, argv);
    return NULL;
  }

  if (!SCHEME_CHAPERONEP(impersonator)) {
    scheme_wrong_contract("remove-impersonator", "impersonator?", 1, argc, argv);
    return NULL;
  }

  Scheme_Chaperone *imp = (Scheme_Chaperone *)impersonator;
  if (scheme_apply(impersonator_of, 2, (Scheme_Object *[]) { value, impersonator }) == scheme_false) {
    scheme_contract_error("remove-impersonator",
			  "impersonator must impersonate original value",
			  "impersonator", 1, impersonator,
			  "original", 1, orig,
			  NULL);
    return NULL;
  }
  if (impersonator == orig) {
    scheme_contract_error("remove-impersonator",
			  "cannot remove impersonator from itself",
			  "impersonator", 1, impersonator,
			  "original", 1, orig,
			  NULL);
    return NULL;
  }

  if (scheme_apply(impersonator_of, 2, (Scheme_Object *[]) { value, impersonator }) == scheme_false) {
    scheme_contract_error("remove-impersonator",
			  "value must impersonate impersonator",
			  "value", 1, value,
			  "impersonator", 1, impersonator,
			  NULL);
    return NULL;
  }

  Scheme_Object *outer = NULL;
  Scheme_Chaperone *parent = NULL;
  Scheme_Chaperone *child = (Scheme_Chaperone *)value;

  while (child != imp) {
    Scheme_Chaperone *newChaperone = (Scheme_Chaperone *)scheme_malloc_tagged(sizeof (Scheme_Chaperone));
    newChaperone->iso.so.type = SCHEME_P_CHAPERONEP(child) ? scheme_proc_chaperone_type : scheme_chaperone_type;
    newChaperone->val = child->val;
    newChaperone->prev = NULL;
    newChaperone->props = child->props;
    newChaperone->redirects = child->redirects;

    if (parent != NULL) {
      parent->prev = (Scheme_Object *)newChaperone;
    } else {
      outer = (Scheme_Object *)newChaperone;
    }

    parent = newChaperone;
    child = (Scheme_Chaperone *)child->prev;
  }

  if (parent != NULL) {
    parent->prev = orig;
  } else {
    outer = orig;
  }

  return outer;
}

#define PRE_REDIRECTS 2 /* magic number for struct chaperones */
static Scheme_Object *chaperone_opaque_struct(int argc, Scheme_Object **argv)
{
  Scheme_Object *val = argv[0];
  Scheme_Struct_Type *stype;
  Scheme_Object *redirects;
  Scheme_Hash_Tree *props = NULL;
  Scheme_Chaperone *px;

  if (SCHEME_CHAPERONEP(val)) {
    val = SCHEME_CHAPERONE_VAL(val);
  }
  
  if (!SCHEME_STRUCTP(val)) {
    scheme_wrong_contract("chaperone-opaque-struct", "struct?", 0, argc, argv);
    return NULL;
  }

  stype = ((Scheme_Structure *)val)->stype;
  redirects = scheme_make_vector(PRE_REDIRECTS + 2 * stype->num_slots, scheme_false);
  props = scheme_parse_chaperone_props("chaperone-opaque-struct", 1, argc, argv);

  px = (Scheme_Chaperone *)scheme_malloc_tagged(sizeof (Scheme_Chaperone));
  if (SCHEME_PROCP(val))
    px->iso.so.type = scheme_proc_chaperone_type;
  else
    px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  return (Scheme_Object *)px;
}
