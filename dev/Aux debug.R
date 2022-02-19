# De https://adv-r.hadley.nz/environments.html#env-recursion

source("~/Trabajo/SmartModel/Funciones auxiliares v11.4.R", verbose = T)

source("~/Trabajo/SmartModel/Utils Pres and Reports v2.0.R", verbose = T)

SmartModelApp:::load_range()

parent.env()

rlang::env_names(rlang::current_env())
rlang::env_names(environment())

rlang::env_print(rlang::current_env())

rlang::env_names(rlang::global_env())

rlang::env_parent(environment())
rlang::env_parents(environment())

rlang::env_parents(rlang::current_env(), last = rlang::empty_env())

where <- function(name, env = rlang::caller_env()) {
  if (identical(env, rlang::empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (rlang::env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, rlang::env_parent(env))
  }
}

where("var_func_aux")
where("var_lectura")
where("var_sesion")
where("load_range")

base::search()
rlang::search_envs()

rlang::fn_env(load_range)

lobstr::cst()

ls()

var_sesion
# Porqué no la encuentra?
# porque el parent_env de mod_lectura es el global_env
var_instancia
r 

rlang::env_names(rlang::global_env())
rlang::env_names(sension_env)

rlang::env_names(topenv())

rlang::env_names(rlang::ns_env("SmartModelApp"))
