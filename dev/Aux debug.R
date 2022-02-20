# De https://adv-r.hadley.nz/environments.html#env-recursion


library(magrittr)
source("~/Trabajo/SmartModel/Dev/TestSmartModelApp/Funciones auxiliares v11.5.R", verbose = TRUE, echo = TRUE)

parse("~/Trabajo/SmartModel/Dev/TestSmartModelApp/Funciones auxiliares v11.5.R", keep.source = TRUE) -> res

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

bt_rs |> 
  bt_rs_2_tab_bt_sum(score_var = score, score_niv_var = score_niv, 
                     group_vars = corte, tab_ref_alin = tab_niv) -> res_debug

res_debug |> tab_niv_bt_gt(title=corte) |> as_raw_html()

var_deb <- NA_character_
SmartModelApp:::string_has_no_data(var_deb)

if (string_has_no_data(par_ids)) print("no tiene datos") else print("si tiene")

if (string_has_no_data(par_ids)) { 
  df_work <- df_work |> mutate(id=row_number(), .before = 1) 
  par_ids <- 'id'
}

dir_dev <- tempdir()
path_dev <- fs::path(dir_dev, "Cache/Valid_STA/")
fs::dir_ls(dir_dev)
fs::dir_ls(fs::path(dir_dev, "Cache/"))
fs::dir_ls(path_dev)
fs::dir_exists(path_dev)
fs::dir_delete(path_dev)

path_knit <- fs::path(dir_dev, "Cache/Valid_STA/")
if (fs::dir_exists(path_knit)) fs::dir_delete(path_knit)
