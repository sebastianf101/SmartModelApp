
# TODO: Incorporar versión de bin factor que imita a Modeler como opcional. Quedó comentada
# Al aplicar la discretización de un continua ver 


# Gral Utils --------------------------------------------------------------

# sec 19.5
commas <- function(...) stringr::str_c(..., collapse = ", ")
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}

# Devuelve el tamaño del objeto en memoria RAM
. %>% (lobstr::obj_size) %>% 
  as.numeric %>% 
  (scales::label_bytes(units = "auto_si", accuracy = 1)) -> cuanta_ram

# list(NULL, NA, character(0), character(1)) %>% purrr::map_lgl(string_has_no_data)
# [1] TRUE TRUE TRUE TRUE
# Ojo que no anda con las versiones de base R
string_has_no_data <- function(c) {
  (rlang::is_missing(c) | rlang::is_null(c) | rlang::is_na(c) | (length(c)==0) | identical(c, character(1))) 
}

# Ej:
# df_Param %>% 
#   get_param_value('cols_no_predictoras') %>% 
#   csv_string_2_vec -> cols_no_predictoras_new
csv_string_2_vec <- function(s) {
  s  |>  
  stringr::str_remove_all("\\'|") |> 
  stringr::str_remove_all('\\"') |> 
  stringr::str_split(",") |> 
  purrr::map(stringr::str_trim) |> 
  purrr::flatten_chr()
}

last2first <- function(v) {vctrs::vec_c(dplyr::last(v), dplyr::head(v,-1))}
# 1:10 %>% last2first
# [1] 10  1  2  3  4  5  6  7  8  9
first2last <- function(v) {vctrs::vec_c(dplyr::tail(v,-1), dplyr::first(v))}
# 1:10 %>% first2last
#  [1]  2  3  4  5  6  7  8  9 10  1

with_project_path <- function(x) {fs::path(project_path,x)}

max_na <- function(x, subs=NA) {
  x_sinna <- na.omit(x)
  ifelse(length(x_sinna)==0, subs, max(x_sinna)) 
}

min_na <- function(x, subs=NA) {
  x_sinna <- na.omit(x)
  ifelse(length(x_sinna)==0, subs, min(x_sinna)) 
}

# Parameter management ----------------------------------------------------

# df_Param |> 
#   get_param_value('project_path') 
get_param_value <- function(df, parameter_name) df |> 
  dplyr::filter(parameter == parameter_name) |> 
  dplyr::pull(value)

# df_Param |> 
#   select(parameter, value, type) |> 
#   pwalk(par_init)
par_init <- function(parameter, value, type, envir) {
  switch (type,
          'list' = value |> csv_string_2_vec() |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          'numeric' = value |> as.numeric() |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          'string' =  value |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          stop('Los tipos admitidos son list, numeric o string.'))
}

# Establece la conexión usando el keyring opr defecto. 
# En Windows es la Credential Store
# Ejemplo de uso df_param |> odbc_connect_w_keyring() -> con
# query_res <- dbGetQuery(con, 'SELECT * FROM [BCRA].[dbo].[PERIODO_ACTUAL]')
# dbDisconnect(con)
odbc_connect_w_keyring <- function(df_param) {
  par_dsn <- df_param |> get_param_value('data_source_odbc_dsn')
  par_svc <- df_param |> get_param_value('keyring_svc_odbc')
  par_user <- keyring::key_list(par_svc)[1,2]
  par_pwd = keyring::key_get(par_svc, par_user)
  list(par_dsn, par_svc, par_user, par_pwd) |> purrr::map(~assert_that::assert_that(is.string(.), noNA(.)))
  assert_that::assert_that(odbcListDataSources() |> filter(name == par_dsn) |> not_empty())
  con <- odbc::dbConnect(odbc::odbc(), dsn = par_dsn, uid = par_user, pwd = par_pwd, timeout = 30, 
                   bigint = 'integer')
  assert_that::assert_that(odbc::dbIsValid(con))
  return(con)
}

# Las tres funciones siguientes se usan para convertir a nulos los valoes que aparecen en 
# Ej: cols_nulos_adic <- c("Contacto_Antiguedad = 0", "Contacto_Antiguedad = NA", "Ant_Laboral = 0")
# Datos_Modelo |> aplic_nulos_adic(cols_nulos_adic) -> Datos_Muestra
# 
anulador <- function(data, var, val) {
  data |>  
    mutate("{ var }" := na_if(.data[[var]], val))
}

aplic_nulos_adic <- function(df, nulos_adic) {
  eq2list <- function(cad) {
    cad  |> stringr::str_split("=",simplify = T) |> stringr::str_trim() -> l
    tibble(var = l[1], val = l[2])
  }
  
  nulos_adic  |>  
    reduce(.init = tibble(var = character(), val = character()), 
           .f = ~ .x |> bind_rows(.y |> eq2list())) %>% 
    {reduce2(.init = df, .x = .$var, .y = .$val, .f = anulador)}
}

# bin_monot y auxiliares ------------------------------------------------
cut_v2 <- function(num_vec, breaks, labels = NULL,
                   include.lowest = FALSE, right = FALSE, dig.lab = 3,
                   ordered_result = TRUE, ...) {
  max_v <- max(num_vec, na.rm = T)
  v <- cut(x=num_vec, breaks=breaks, labels=labels, include.lowest=include.lowest, right=right, 
           dig.lab=dig.lab, ordered_result=ordered_result, ...)
  ext_sup <- paste0("[", max_v, "]")
  ve <- forcats::fct_expand(v, ext_sup)
  res <- if_else(num_vec==max_v & is.na(v), factor(ext_sup, levels = levels(ve), ordered = ordered_result), 
                 ve)
  return(res)
}

safe_cor <- purrr::quietly(~ cor(.x, .y, method = "spearman", use = "complete.obs"))

bin.sf <- function(x, nbins, minpts = floor(length(x)/nbins), nvals.min = 5, verbose = F) {
  xtbl <- table(x)
  xval <- as.numeric(names(xtbl))
  nvals <- length(xval)
  if (nvals < nvals.min) {
    msg <- paste0("Valores unicos insuficientes!: ", nvals, "<",nvals.min)
    return(list(error=T, msg=msg))
  }
  if (is.null(nbins) | floor(minpts)!=minpts) {
    msg <- paste0("bins.sf: parametros inconsistentes ", 
                  "\nNumero de bines nulo? ", is.null(nbins), 
                  "\nCasos minimos x bin es entero? ", minpts)
    return(list(error=T, msg=msg))
  }
  binsz <- max(minpts, floor(length(x)/nbins))
  binlo <- vector(nbins, mode = "integer")
  binhi <- vector(nbins, mode = "integer")
  binct <- vector(nbins, mode = "integer")
  startbin <- TRUE
  k <- 1
  s <- 0
  for (i in 1:nvals) {
    s <- s + xtbl[i]
    if (startbin) {
      binlo[k] <- xval[i]
      startbin <- FALSE
    }
    if (s >= binsz) {
      binhi[k] <- xval[i]
      binct[k] <- s
      k <- k + 1
      s <- 0
      startbin <- TRUE
    }
  }
  if (!startbin) {
    # El último bucket no llegó al mínimo, entonces reseteo el actual y lo sumo al anterior
    binlo[k] <- 0
    k <- k - 1
    binhi[k] <- xval[nvals]
    binct[k] <- binct[k] + s
  }
  binlo <- binlo[1:k]
  binhi <- binhi[1:k]
  binct <- binct[1:k]
  names(binct) <- paste("[", binlo, ", ", binhi, "]", sep = "")
  if (verbose) 
    print(binct)
  return(list(binlo = binlo, binhi = binhi, binct = binct, 
              xtbl = xtbl, xval = xval, error=F))
}

woe_nulos <- function(df, min_pts=100) {
  df <- df |> rename(x=x_v, Good=y_v) 
  totals <- df |>
    summarise(TotGood=sum(Good), TotBad=n()-TotGood) |> 
    mutate(LnODDS=if_else(TotBad>0,log(TotGood/TotBad), NULL))
  res <- df |> filter(is.na(x)) |> 
    summarise(CntGood=sum(Good), CntBad=n()-CntGood) |> 
    mutate(LnOdds=if_else(CntBad>0,log(CntGood/CntBad), NULL)) |> 
    tidyr::crossing(totals) |> 
    mutate(WoE=if_else(CntBad>min_pts & CntGood>min_pts, LnOdds - LnODDS,0)) |> pull(WoE) 
  return(res)
}

eqbin_Lin <- function(x, nbins=50, minpts=floor(length(x)/nbins), nvals.min = 5) {
  # Para evitar vectores muy largos y debido a la falta de precision de table restringo la precision de x a 10 decimales
  x <- round(x,10)
  b <- bin.sf(x, nbins = nbins, minpts = minpts, nvals.min)
  if (b$error) return(list(error=T, msg=b$msg))
  cortes.lo <- b$binlo[!is.na(b$binct) & b$binct>0]
  cortes.hi <- b$binhi[!is.na(b$binct) & b$binct>0]  
  if (rlang::is_empty(cortes.lo) | rlang::is_empty(cortes.hi)) 
    return(list(error=T, msg="No se pudo partir en bines!", b=b))
  xi <- findInterval(x, cortes.lo, left.open = F)
  # Ojo que x_eq usa los cortes.lo! 
  df <- tibble(x=x, x_eq=cortes.lo[xi])
  tab <- df |> group_by(x_eq) |> 
    summarise(lo=min(x), median=median(x), mean=mean(x), hi=max(x)) |> arrange(lo)
  return(list(x_eq=df$x_eq, cortes.lo=tab$lo, cortes.median=tab$median, 
              cortes.mean=tab$mean, cortes.hi=tab$hi, error=F))
}

# Asume que df tiene las variables x, x_transf, x_agrup e y.  
# También que x y x_transf son numéricas (asume que existe mean(x) y min(x_transf)) 
agrup_2_ivtab_cont <- function(df) {
  df_nulos <- df |> 
    filter(is.na(x)) |> 
    filter(y==0 || y==1) |> 
    mutate(col_agrup=NA_character_)
  df_nonulos <- df |> 
    filter(!is.na(x) & !is.na(x_transf) & !is.na(x_agrup)) |> 
    select(x=x, x_transf, x_agrup, y=y) |> filter(y==0 || y==1) 
  df <- df_nonulos |> bind_rows(df_nulos)
  totals <- df |> summarise(TotRec=n(), TotGood=sum(y), TotBad=TotRec - TotGood,
                            LnODDS=log(TotGood/TotBad)) 
  tab <- df |> 
    group_by(x_agrup) |> 
    summarise(cut_lo=min(x), cut_median=median(x), cut_mean=mean(x), cut_hi=max(x), 
              CntRec=n(), CntGood=sum(y), CntBad=CntRec - CntGood) |> 
    tidyr::crossing(totals) |> arrange(cut_lo) |> 
    mutate(cut_lo_prox = lead(cut_lo, 1, default = NA)) |> 
    mutate(col_agrup=if_else(!is.na(cut_lo_prox), 
                             paste0("[", formatC(cut_lo, digits = 3, format = "f", drop0trailing = T), ",", 
                                    formatC(cut_lo_prox, digits = 3, format = "f", drop0trailing = T),")"), 
                             paste0("[", formatC(cut_lo, digits = 3, format = "f", drop0trailing = T), ",", 
                                    formatC(cut_hi, digits = 3, format = "f", drop0trailing = T),"]"))) |> 
    mutate(PctRec=CntRec/TotRec, PctGood=CntGood/TotGood, PctBad=CntBad/TotBad, 
           GoodRate=CntGood/CntRec, BadRate=CntBad/CntRec, 
           Odds=if_else(CntBad>0,CntGood/CntBad,NULL), LnOdds=log(Odds), 
           WoE=LnOdds - LnODDS, IV=WoE*(PctGood - PctBad),
           CntCumRec=cumsum(CntRec), CntCumGood=cumsum(CntGood), CntCumBad=cumsum(CntBad),
           Cutpoint=paste("<=",cut_hi)) |> 
    mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
    mutate(IV=coalesce(IV,0), IV=if_else(is.nan(IV) | is.infinite(IV),0,IV)) |> # Trato casos bad cero.
    select(col_agrup, cut_lo, cut_median, cut_hi, CntRec, CntGood, CntBad, PctRec, PctGood, PctBad, BadRate, WoE, IV)
  totals <- tab |> 
    summarise(col_agrup='Total', 
              cut_lo=ifelse(sum(!is.na(cut_lo))==0,NA_real_, min(cut_lo, na.rm = T)), #if_else arruina la prevención del warning!
              cut_median=NA_integer_, 
              cut_hi=ifelse(sum(!is.na(cut_hi))==0,NA_real_, max(cut_hi, na.rm = T)),
              CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  tab <- tab |> arrange(coalesce(cut_median,cut_lo)) |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(tab)
}

group_2_ivtab_cont <- function(df, col,col_agrup, woes_tab, Good="Good") {
  # Evito el colapso en un sólo tramo de los primeros dos puntos de manera poco elegante.
  woes_tab <- c(sort(woes_tab), max(woes_tab)+0.01)
  df_aux <- df <- df |> select(all_of(c(col, col_agrup, Good))) |> 
    mutate(col=!!rlang::parse_expr(col), col_agrup=!!rlang::parse_expr(col_agrup), Good=!!rlang::parse_expr(Good))
  df_aux |> 
    filter(!is.na(col)) |> 
    mutate(col_agrup=cut(col_agrup, woes_tab, include.lowest = T, ordered_result = T,
                         right=F, labels = NULL)) -> df_nonulos
  df_aux |> 
    filter(is.na(col)) |> 
    summarise(mean=mean(col_agrup)) |> pull(mean) -> woe_nulos
  df_aux |> 
    filter(is.na(col)) |> 
    mutate(col_agrup = paste('[', round(woe_nulos,3), ']')) -> df_nulos
  df_nonulos |> bind_rows(df_nulos) -> df
  totals <- df |> summarise(TotRec=n(), TotGood=sum(Good), TotBad=TotRec - TotGood,
                             LnODDS=if_else(TotBad>0,log(TotGood/TotBad),NULL)) 
  tab <- df |> group_by(col_agrup) |>
    summarise(cut_lo=min_na(col), cut_median=median(col, na.rm = T), cut_mean=mean(col, na.rm = T), 
              cut_hi=max_na(col), # algunos grupos o incluso todo df puede tener cero casos.  
              CntRec=n(), CntGood=sum(Good)) |> 
    mutate(CntBad=CntRec - CntGood, 
           GoodRate=CntGood/CntRec, BadRate=CntBad/CntRec, 
           Odds=if_else(CntBad>0,CntGood/CntBad,NULL), 
           LnOdds=if_else(CntBad>0,log(Odds), NULL)) |> 
    arrange(BadRate) |> 
    tidyr::crossing(totals) |> 
    mutate(PctRec=CntRec/TotRec, 
           PctGood=if_else(TotGood>0, CntGood/TotGood, 0),
           PctBad=if_else(TotBad>0,CntBad/TotBad,0), 
           WoE=LnOdds - LnODDS, IV=WoE*(PctGood - PctBad),
           CntCumRec=cumsum(CntRec), CntCumGood=cumsum(CntGood), CntCumBad=cumsum(CntBad),
           Cutpoint=paste("<=",cut_hi)) |> 
    mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
    mutate(IV=coalesce(IV,0), IV=if_else(is.nan(IV) | is.infinite(IV),0,IV)) |> # Trato casos bad cero.
    mutate(col_agrup=as.character(col_agrup)) |> 
    select(col_agrup, cut_lo, cut_median, cut_hi, CntRec, CntGood, CntBad, PctRec, PctGood, PctBad, BadRate, WoE, IV)
  totals <- tab |> 
    summarise(col_agrup='Total', 
              cut_lo=ifelse(sum(!is.na(cut_lo))==0,NA_real_, min(cut_lo, na.rm = T)), #if_else arruina la prevención del warning!
              cut_median=NA_integer_, 
              cut_hi=ifelse(sum(!is.na(cut_hi))==0,NA_real_, max(cut_hi, na.rm = T)),
              CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  tab <- tab |> arrange(coalesce(cut_median,cut_lo)) |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(tab)
}

isobin <- function(x,y) {
  reg <- stats::isoreg(x, y)
  k <- stats::knots(stats::as.stepfun(reg))
  xi <- findInterval(x, k, left.open = T)
  return(k[xi+1])
}
# Asume que df tiene las variables numéricas no nulas cut_median, cut_hi y WoE
woes_band <- function(df) {
  df <- df |> filter(!is.na(cut_median) & !is.na(cut_hi) &!is.na(WoE))
  medians <- df |> pull(cut_median)
  his <- df |> pull(cut_hi)
  woes <- df |> pull(WoE)
  woes_tab <- rep(NA_real_, length(woes))
  for (i in 1:(length(medians)-1)) {
    x1 <- medians[i]
    x2 <- medians[i+1]
    y1 <- woes[i]
    y2 <- woes[i+1]
    x <- his[i]
    woes_tab[i] <- (y2-y1)*(x-x1)/(x2-x1)+y1 
  }
  woes_tab[length(medians)] <- woes[length(woes)] 
  return(unique(c(woes[1],woes_tab)))
}

tab_iv <- function(df, par_col=col, par_col_agrup=col_agrup) {
  df |> rename(x=all_of(par_col), x_transf=all_of(par_col_agrup), y=all_of('Good')) |> select(x,y,x_transf) -> df
  df |> filter(!is.na(x)) |> 
    mutate(x_agrup = x_transf |> 
             eqbin_Lin(par_nbins2, par_minpts2, nvals.min = 2) |> purrr::pluck('x_eq')) |>   
    bind_rows(df |> filter(is.na(x)) |> mutate(x_agrup = x_transf)) |> 
    agrup_2_ivtab_cont() -> tab
  return(tab)
}


# Si se quiere sólo una pasada igualar los segundos parámetros con los primeros
# Ej.: monotbin <- function(df, y, x, nbins1=20, minpts1=100, nbins2=nbins1, minpts2=minpts1)
# No se admiten NA, missings o similares. Isobin no los admite. Realizar imputación de missings antes. 
# nvals.min es un parámetro implícito que se bajó a 2 para tratar los flags pero hay que revisar esto. 
# Notar que si hay menos de par_iv_cuantiles_gb_min buenos o malos nulos se asigna cero. 
# No sé si tiene sentido tratar los flags como continuas. 
# Discretizo por rampas por defecto, usando 0 se discretiza por escalera
bin_monot <- function(df, y, x, nbins1=par_nbins1, minpts1=par_minpts1, nbins2=par_nbins2, minpts2=par_minpts2, rampa=par_discret) {
  # Ojo que eqbin corta la precision del argumento a 10 decimales
  # y smbinning.custom la corta a 4 decimales!
  # y, x son nombres de variables, no vectores!
  j=which(names(df) == x)
  df_nulos <- df |> select(x_v=all_of(x), y_v=all_of(y)) 
  woe_nulos <- woe_nulos(df_nulos, min_pts = par_iv_cuantiles_gb_min)  
  df_nulos <- df_nulos |> filter(is.na(x_v))
  df_nulos <- df_nulos |> mutate(x_agrup=woe_nulos)
  df_nonulos <- df |> select(x_v=all_of(x), y_v=all_of(y)) |> filter(!is.na(x_v))
  # df pasa a ser df con no nulos. Luego de monot añado los nulos
  x_v=df_nonulos |> pull(x_v)
  y_v=df_nonulos |> pull(y_v)
  # La primera vez controlo que sean 5 valores únicos por lo menos
  # Lo bajé a 2 para que trate los flags pero no me convence mucho. 
  res <- eqbin_Lin(x_v, nbins1, minpts1, nvals.min = 2) 
  if (res$error) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste(res$msg, " Cortes: ", res$b$binlo)))
  x_eq <- res |> purrr::pluck("x_eq") 
  c <- safe_cor(x_eq, y_v)
  if (!is.null(c$error) | is.na(c$result)) 
    return(list(x = x, y = y, col_id = j, error=T, 
      error_detalle=paste("Correlacion de Spearman entre variable agrupada y objetivo nula o cero. ",
                          c$error, c$warnings)))
  if (abs(c$result)>=0.95) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste("Correlacion de Spearman entre variable agrupada y objetivo mayor a 0.95! ",
                                    c$result)))  
  x_iso <- isobin(x_eq, c$result/abs(c$result) * y_v) 
  # pero la segunda vez no
  res <- eqbin_Lin(x_iso,nbins2, minpts2, nvals.min = 2)
  if (res$error) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste(res$msg, " Cortes: ", res$b$binlo)))
  if (length(res$cortes.median)<2) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste("La variable",x, "no alcanzó a partirse en dos grupos con valores no nulos con al menos",
                                    round(100/nbins2,1), "% de las observaciones y al menos", minpts2, "observaciones")))
  x_eq2 <- res |> purrr::pluck("x_eq") 
  tab <- tibble(x=x_v, x_transf=x_iso, x_agrup=x_eq2, y=y_v) |> 
    bind_rows(df_nulos |> rename(x=x_v, y=y_v) |> mutate(x_transf=x_agrup)) |> 
    agrup_2_ivtab_cont() 
  woes_tab <- tab |> filter(!is.na(col_agrup) & col_agrup!='Total') |> woes_band()
  # bands y cuts sólo quedan por compatibilidad 
  # Para lograr una comparación justa IV antes lo calculo con nbins2 y minpts2
  res_antes <- eqbin_Lin(x_v, nbins2, minpts2, nvals.min = 2) 
  if (res_antes$error) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste(res_antes$msg, " Cortes: ", res_antes$b$binlo)))
  if (length(res_antes$cortes.median)<2) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste("La variable",x, "no alcanzó a partirse en dos grupos con valores no nulos con al menos",
                round(100/nbins2,1), "% de las observaciones y al menos", minpts2, "observaciones")))
  x_eq <- res_antes |> purrr::pluck("x_eq") 
  iv_antes <- tibble(x=x_v, x_transf=x_eq, x_agrup=x_eq, y=y_v) |> bind_rows(
    df_nulos |> rename(x=x_v, y=y_v) |> mutate(x_transf=x_agrup)) |> 
    agrup_2_ivtab_cont() |> 
    filter(is.na(col_agrup) | col_agrup !='Total') |> pull("IV") |> sum()
  res <- list(x = x, y = y, col_id = j, type = "Continua", error = F, 
              nbins1 = nbins1, minpts1 = minpts1, nbins2 = nbins2, minpts2 = minpts2,
              groups = NULL, 
              bands = c(tab |> filter(!is.na(cut_lo)) |> pull(cut_lo) |> min(), 
                        tab |> filter(!is.na(cut_hi)) |> pull(cut_hi)), 
              cuts = tab$cut_hi[-length(tab$cut_hi)], 
              woes_tab = woes_tab,
              woe_nulos = woe_nulos, ivtable = tab)
  # Agregado para calcular diferentes versiones de IVs posteriores.
  # los IVs discretos y continuos se calculan dividiendo en 20 bines el rango de WoE
  col_agrup <- paste0(x,"_g")
  woes_eq <- seq(from=c(res$woes_nulos, res$woes_tab) |> min(), 
                 to=c(res$woes_nulos, res$woes_tab) |> max(), length.out = 20) 
  df_stp <- df |> apply_grouping_monot(res, col_agrup, rampa=0, woe_nulos = res$woe_nulos) |> purrr::pluck("df") 
  tab_iv_stp <- df_stp |> tab_iv(par_col=x, col_agrup)
  tab_iv_stp_eq20 <- df_stp |> group_2_ivtab_cont(x,col_agrup, woes_eq, y)
  df_pwl <- df |> apply_grouping_monot(res, col_agrup, rampa=1, woe_nulos = res$woe_nulos) |> purrr::pluck("df")
  tab_iv_pwl <- df_pwl |> tab_iv(par_col=x, col_agrup)
  tab_iv_pwl_eq20 <- df_pwl |> group_2_ivtab_cont(x,col_agrup, woes_eq, y)
  iv_monot_discret <- tab_iv_stp |> 
    filter(is.na(col_agrup) | col_agrup !='Total') |> summarise(IV=sum(IV)) |> pull()
  iv_monot_cont <- tab_iv_pwl |> 
    filter(is.na(col_agrup) | col_agrup !='Total') |> summarise(IV=sum(IV)) |> pull()
  # Dejo iv_despues por compatibilidad.  Ojo que rampas es un parámetro global!
  if (rampa==1) iv_despues <- iv_monot_cont else iv_despues <- iv_monot_discret
  res <- append(res, 
           list(woes_eq=woes_eq, tab_iv_stp=tab_iv_stp, tab_iv_pwl=tab_iv_pwl, 
             iv_antes_monot=iv_antes, 
             iv_monot_discret=iv_monot_discret, 
             iv_monot_cont=iv_monot_cont, 
             iv = iv_despues,  # La dejo por compatibilidad y agrego las otras pérd.
             iv_perd = if_else(iv_antes==0,0,100*(iv_antes-iv_despues)/iv_antes),
             iv_perd_discret = if_else(iv_antes==0,0,100*(iv_antes-iv_monot_discret)/iv_antes),
             iv_perd_cont = if_else(iv_antes==0,0,100*(iv_antes-iv_monot_cont)/iv_antes)
             ))
  return(res)
}

# TODO: Informar la distribución de col_agrup en df para comparar con la de res?
# Por defecto signa WoE cero a los nulos, lo que equivale a imputarles la media 
# de la población pero esto se cambia con el parámetro woe_nulos.
apply_grouping_monot <- function (df, res_bc, col_agrup, rampa=rampa, woe_nulos=0) {
  df = data.frame(df, tmpname = NA)
  ncol = ncol(df)
  col_id <- which(colnames(df)==res_bc$x) # Es más robusto que el original ivout$col_id
  if (length(col_id)==1) {
    df[, ncol][is.na(df[, col_id])] = woe_nulos
    # Dentro de esta condición sólo usa ivtable de res_bc
    ivtable <- res_bc$ivtable |> filter(!is.na(cut_lo) & !is.na(cut_median) & !is.na(cut_hi))
    b <- ivtable$cut_median
    if (length(b)<2) {
      alerta=T   
      msg <- paste("La variable continua  ", res_bc$x, " no tiene cortes definidos!")
    }
    else {
      if (rampa) { # Discretiza usando lineal a trozos
        df[, ncol][df[, col_id] <= b[1]] <- ivtable$WoE[1]
        for (i in 2:length(b)) {
          x1 <- b[i-1]
          x2 <- b[i]
          y1 <- ivtable$WoE[i-1]
          y2 <- ivtable$WoE[i]
          x <- df[, col_id]
          idx <- which(x > x1 & x <= x2)
          df[, ncol][idx] <- (y2-y1)*(x[idx]-x1)/(x2-x1)+y1 
        }
        idx <- which(x >= b[length(b)])
        df[, ncol][idx] <- ivtable$WoE[length(b)]
      }
      else { # Discretiza usando escalera
        b <- c(ivtable |> pull(cut_lo) |> min(), 
               ivtable |> pull(cut_hi))
        if (ivtable$CntRec[1]>0) woe <- ivtable$WoE[1] else woe <- 0 
        df[, ncol][df[, col_id] <= b[2]] = woe
        if (length(b) > 3) {
          for (i in 2:(length(b) - 2)) {
            if (ivtable$CntRec[i]>0) woe <- ivtable$WoE[i] else woe <- 0 
            df[, ncol][df[, col_id] > b[i] & df[, col_id] <= b[i + 1]] = woe
          }
        }
        if (ivtable$CntRec[length(b) - 1]>0) woe <- ivtable$WoE[length(b) - 1] 
        else woe <- 0 
        df[, ncol][df[, col_id] > b[length(b) - 1]] = woe
      }
      names(df)[names(df) == "tmpname"] = col_agrup
      alerta = F
      msg <- NA
    }
  }
  else {
    alerta=T   
    msg <- paste("Variable ",res_bc$x ," en discretizacion no hallada en df!")
  }
  return(list(df=df, alerta=alerta, msg=msg))
}

# Sucessive Medians Binning -----------------------------------------------
# Binning por medianas sucesivas 

partir_x_med <- function(df, col, lim_izq, lim_der, woe_izq, woe_der, ext_izq=FALSE, 
                         nbins2=par_nbins2, minpts2=par_minpts2, Good='Good') {
  list(lim_izq, lim_der, woe_izq, woe_der) |> purrr::map(~assert_that::assert_that(is.number(.)))
  list(nbins2, minpts2) |> purrr::map(~assert_that::assert_that(is.count(.)))
  assert_that::assert_that(lim_izq < lim_der, woe_izq != woe_der)
  df |> ungroup() |> select(all_of(col), all_of(Good)) |> 
    rename(x=all_of(col), good=all_of(Good)) |> mutate(bad=1-good) |> 
    select(x,bad) -> tab 
  assert_that::assert_that(all(tab$bad %in% c(0,1)), is.numeric(tab$x))
  minpts <- min(minpts2, floor(length(tab$x)/nbins2))
  assert_that::assert_that(is.count(minpts))
  
  tab |> summarise(n=n(), goods=sum(1-bad), bads=sum(bad), bad_rate=bads/n, 
                   odds=if_else(bads>0, goods/bads, NULL)) -> tot 
  if (ext_izq) tab |> filter(lim_izq <= x & x <= lim_der) -> tab
  else tab |> filter(lim_izq < x & x <= lim_der) -> tab
  tab |>
    summarise(min=min(x), max=max(x), 
              n=n(), bads=sum(bad), goods=sum(1-bad)) |> 
    mutate(PctGood=goods/tot$goods, PctBad=bads/tot$bads, 
           bad_rate=bads/n, good_rate=goods/n, 
           odds=if_else(bads>0, goods/bads, NULL), 
           WoE=log(odds)-log(tot$odds), IV=WoE*(PctGood - PctBad)) -> tab_nopart
  list(raiz=tibble(lim_izq=lim_izq, lim_der=lim_der, woe_izq=woe_izq, woe_der=woe_der, 
                   n=tab_nopart$n, min=tab_nopart$min, max=tab_nopart$max, 
                   WoE=tab_nopart$WoE, bad_rate=tab_nopart$bad_rate)) -> res_no_part
  tab |>
    mutate(lte_med=(x <= median(x))) |> 
    group_by(lte_med) |> 
    summarise(min=min(x), max=max(x), 
              n=n(), bads=sum(bad), goods=sum(1-bad)) |> 
    mutate(PctGood=goods/tot$goods, PctBad=bads/tot$bads, 
           bad_rate=bads/n, good_rate=goods/n, 
           odds=if_else(bads>0, goods/bads, NULL), 
           WoE=log(odds)-log(tot$odds), IV=WoE*(PctGood - PctBad)) -> tab_part
  tab_part |> filter(lte_med) -> fila_izq 
  tab_part |> filter(!lte_med) -> fila_der  
  if (not_empty(fila_izq) & not_empty(fila_der)) {
    if (fila_izq$n >= minpts & fila_der$n >= minpts) {
      if (woe_izq <= fila_izq$WoE & fila_izq$WoE < fila_der$WoE & fila_der$WoE <= woe_der) 
        return(list(izq=partir_x_med(df, col, lim_izq, fila_izq$max, woe_izq, fila_der$WoE, 
                                     ext_izq, nbins2, minpts2), 
                    der=partir_x_med(df, col, fila_izq$max, lim_der, fila_izq$WoE, woe_der, 
                                     ext_izq=F, nbins2, minpts2)))
      else if (woe_izq >= fila_izq$WoE & fila_izq$WoE > fila_der$WoE & fila_der$WoE >= woe_der)  
        return(list(izq=partir_x_med(df, col, lim_izq, fila_izq$max, woe_izq, fila_der$WoE, 
                                     ext_izq, nbins2, minpts2), 
                    der=partir_x_med(df, col, fila_izq$max, lim_der, fila_izq$WoE, woe_der, 
                                     ext_izq=F, nbins2, minpts2))) 
      else return(res_no_part) 
      } else return(res_no_part) 
    } else return(res_no_part) 
  }

red_tree_2_cuts <- function(node) {
  if (names(node)[[1]]=="raiz") 
    if (node$raiz$lim_der == Inf) return(c(node$raiz$lim_izq, Inf)) 
  else return(node$raiz$lim_izq)
  else return(c(red_tree_2_cuts(node$izq), red_tree_2_cuts(node$der))) 
}

bin_x_med <- function(df, col, col_agrup,  
                      lim_izq=-Inf, lim_der=Inf, 
                      woe_izq=-Inf, woe_der=Inf, ext_izq=T) {
  df |> filter(is.na(.data[[col]])) -> df_nulos
  df_nulos |> rename(x_v=all_of(col), y_v=Good) |> 
    woe_nulos(min_pts = par_iv_cuantiles_gb_min) -> woe_nulos 
  # Por ahora sólo interesa las tablas de IV no el modelado
  # df_nulos |> mutate("{col_agrup}" := woe_nulos) -> df_nulos
  df_nulos |> mutate("{col_agrup}" := "Missing") -> df_nulos
  df |> filter(!is.na(.data[[col]])) -> df_nonulos
  df_nonulos[[col]] -> x
  df_nonulos |> partir_x_med(col, lim_izq, lim_der, woe_izq, woe_der, ext_izq) -> res_part
  res_part |> red_tree_2_cuts() -> cortes
  df_nonulos |> mutate("{col_agrup}" := cut(.data[[col]], cortes)) -> df_nonulos
  df_nonulos |> bind_rows(df_nulos) -> df
  return(list(df=df, cortes=cortes))
}

# bin_factor() y auxiliares -----------------------------------------------

group_2_ivtab_factor <- function(df, col, col_agrup, Good="Good") {
  df <- df |> select(all_of(c(col, col_agrup, Good))) |> 
    mutate(col=!!rlang::parse_expr(col), col_agrup=!!rlang::parse_expr(col_agrup), Good=!!rlang::parse_expr(Good)) 
  totals <- df |> summarise(TotRec=n(), TotGood=sum(Good), TotBad=TotRec - TotGood,
                             LnODDS=log(TotGood/TotBad)) 
  tab <- df |> group_by(col_agrup) |>
    summarise(Cutpoint=toString(unique(col_agrup)), 
              groups=paste("'", paste(sort(unique(col), na.last = T), collapse="','"), "'", sep=""), 
              CntRec=n(), CntGood=sum(Good), CntBad=CntRec - CntGood, 
              cut_lo=min(col), cut_hi=max(col)) |> 
    select(-col_agrup) |> tidyr::crossing(totals) |> 
    mutate(PctRec=CntRec/TotRec, PctGood=CntGood/TotGood, PctBad=CntBad/TotBad, 
           GoodRate=CntGood/CntRec, BadRate=CntBad/CntRec, 
           Odds=if_else(CntBad>0,CntGood/CntBad,NULL), LnOdds=log(Odds), 
           WoE=LnOdds - LnODDS, IV=WoE*(PctGood - PctBad)) |> 
    mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
    mutate(IV=coalesce(IV,0), IV=if_else(is.nan(IV) | is.infinite(IV),0,IV)) |> # Trato casos bad cero.
    arrange(BadRate) |> mutate(CntCumRec=cumsum(CntRec), 
                                CntCumGood=cumsum(CntGood), CntCumBad=cumsum(CntBad))
  totals <- tab |> 
    summarise(Cutpoint='Total', groups=NA_character_, CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  tab <- tab |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(tab)
}

# Ej de uso: bins <- bin_factor(train, col, minpts = 30)
bin_factor <- function(df, col, minpts=30, Good="Good", maxlevels=50) {
  j=which(names(df) == col)
  iv_antes <- df |> group_2_ivtab_factor(col = col, col_agrup = col, Good=Good) |> 
    filter(is.finite(IV) & Cutpoint!="Total") |> summarise(IV=sum(IV)) |> pull()
  frec_tab <- df |> rename(col=all_of(col)) |> group_by(col) |> summarise(n=n()) |> 
    mutate(col_agrup=if_else(is.na(col), 'Missing_', if_else(n>=minpts, col, 'Otros_')))
  if (frec_tab |> nrow() > maxlevels) {
    error <- T
    msg <- paste("La cantidad de grupos", frec_tab |> nrow(), "supera maxlevels", maxlevels)
  } else {
    error <- F
    msg <- NULL
  }
  df <- df |> rename(col=all_of(col)) |> select(col, Good) |> 
    inner_join(frec_tab, by="col") |> select(-n)
  tab <- df |> 
    group_2_ivtab_factor(col="col", col_agrup="col_agrup", Good=Good) |> 
    mutate(WoE = if_else(CntRec < minpts,0,WoE), IV = if_else(CntRec < minpts,0,IV))
  woes_tab <- frec_tab |> inner_join(tab |> filter(is.finite(IV) & Cutpoint!="Total"), 
                                      by=c("col_agrup" = "Cutpoint")) |> 
    select(col, WoE) |> rename(!!sym(col) := col, !!sym(paste0(col,"_g")) := WoE)
  iv_despues = tab |> filter(is.finite(IV) & Cutpoint!="Total") |> summarise(IV=sum(IV)) |> pull()
  # bands y cuts sólo quedan por compatibilidad 
  res <- list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
              error = error, msg = msg, 
              ivtable = tab, iv = iv_despues, iv_antes_discret=iv_antes, 
              iv_perd = if_else(iv_antes==0,0,(iv_antes-iv_despues)/iv_antes),
              iv_despues = iv_despues,
              groups = tab |> select(Cutpoint, groups) |> deframe(), 
              bands = NULL, cuts = NULL, woes_tab=woes_tab)
}

apply_grouping_factor <- function(df, res_bf, col_agrup) {
  col <- res_bf$x
  wt <- res_bf |> purrr::pluck("woes_tab")
  WoE_Otros <- res_bf |> purrr::pluck("ivtable") |> filter(Cutpoint=="Otros_") |> pull(WoE)
  WoE_Missing <- res_bf |> purrr::pluck("ivtable") |> filter(Cutpoint=="Missing_") |> pull(WoE)
  df <- df |> left_join(wt, by=col) 
  no_categ <- df |> select(x=all_of(col), x_g=all_of(col_agrup)) |> 
    filter(is.na(x_g)) |> group_by(x) |> summarise(n=n())
  if (no_categ |> nrow()>0) {
    val_no_categ <- no_categ |> pull(x) |> paste(collapse = ', ')
    if (is_empty(WoE_Otros) || is.na(WoE_Otros)) {
      if (is_empty(WoE_Missing) || is.na(WoE_Missing)) {
        cat_no_categ <- '0'
        woe_no_categ <- 0
      } else {
        cat_no_categ <- 'Missing_'
        woe_no_categ <- coalesce(WoE_Missing, 0)
      } 
    } else {
      cat_no_categ <- 'Otros_'
      woe_no_categ <- coalesce(WoE_Otros, 0)
    }
    df[, col_agrup] <- woe_no_categ
    alerta=T   
    msg <- paste("En el dataset aparecieron valores de",col, "que no tenian WoEs asociados:'",
                 val_no_categ,"'. Se transformaron con valores de WoE de", cat_no_categ)} 
  else {
    alerta=F   
    msg <- NULL
  }
  return(list(df=df, no_categ=no_categ, alerta=alerta, msg=msg))
}

wilson_width <- function(bad_rate, n) {
  p_hat <- bad_rate
  z <- 2
  2*z/(1+z^2/n)*sqrt(z^2/(4*n^2)+p_hat*(1-p_hat)/n)
}

wilson_n <- function(bad_rate, w) {
  p_hat <- bad_rate
  z <- 2
  z^2/w^2*(2*p_hat*(1-p_hat)-w^2+sqrt((w^2-2*p_hat*(1-p_hat))^2+w^2*(1-w^2)))
}

graph_min_sizes <- function(bin_factor_res) {
  ivtable <- bin_factor_res |> purrr::pluck("ivtable")
  minpts <- bin_factor_res |> purrr::pluck("minpts")
  tab_prec_now <- ivtable |> select(Grupo=Cutpoint, CntRec, BadRate) |> 
    mutate(width=wilson_width(BadRate, CntRec), 
           prec_Real=width/(2*BadRate), Prec_Real=100*prec_Real)
  prec_range <- tab_prec_now |> pull(Prec_Real) 
  Prec <- seq(from= prec_range |> min() %>% `*`(0.9), to=prec_range |> max() %>% `*`(1.1), 
              by=1/10) 
  tab_prec_range <- ivtable |> select(Grupo=Cutpoint, CntRec, BadRate) |> 
    tidyr::crossing(Prec) |> mutate(prec=Prec/100, width=2*BadRate*prec,
                              Casos_req=ceiling(wilson_n(BadRate, width))) 
  eje_y <- seq(from=0, to=tab_prec_range |> pull(Casos_req) |> max(), length.out=100)
  pretty_mod <- pretty(eje_y)[abs(pretty(eje_y) - minpts) > 0.25]
  g1 <- tab_prec_now |> 
    ggplot2::ggplot(ggplot2::aes(x=Prec_Real, y=CntRec, color=Grupo)) +
    ggplot2::geom_point() + 
    ggplot2::scale_y_continuous(breaks = c(pretty_mod, minpts), labels = c(pretty_mod, 'minpts')) + 
    ggplot2::labs(x='Imprecisión x (Tasa de Mora real +/- x% con 95% de confianza)', y='Mínimo de casos requeridos') +
    ggplot2::geom_text(ggplot2::aes(label=Grupo), size=3, nudge_y = 50) + 
    ggplot2::geom_label(ggplot2::aes(label=round(100*BadRate,1), fill=Grupo), color="white", size=3, nudge_y = 15) +     
    ggplot2::theme(legend.position = "bottom", axis.title=ggplot2::element_text(size=8)) + 
    ggplot2::geom_hline(yintercept = minpts, linetype=3)
  g2 <- g1 + ggplot2::geom_line(data=tab_prec_range, ggplot2::aes(x=Prec, y=Casos_req, color=Grupo)) 
  return(g2)
}






# Logistic Forward AK -----------------------------------------------------
logit.step <- function(train, target) { ##function(train, target="Bad6m") {
  max.vars <- length(colnames(train)) - 1  
  if (max.vars>0) {
    train2 <- train
    fmla.all <- as.formula(paste(target, " ~ ", paste(paste(colnames(train), collapse = "+")), " - ",target, sep = ""))
    fmla <- as.formula(paste(target, " ~ NULL", sep = ""))
    mod.step <- stats::glm(fmla, train2, family = "binomial") 
    coef.step <- mod.step |> broom::tidy() |> select(term, estimate) |>
      rename(Variable=term, Beta=estimate)
    idx <- 0
    names(coef.step) <- c("Variable", paste("Beta.",idx, sep = ""))
    coef.steps <- coef.step
    ult.coef <- 1
    vars.fwd.res <- c()
    var.sel <- "pp"
    while (idx <= max.vars && ult.coef > 0 && length(var.sel)>0 && !is.na(var.sel)) {
      mod.curr <- mod.step
      if (max.vars > mod.curr$df.null - mod.curr$df.residual) {
        vars.fwd <- mod.curr |> add1(scope = fmla.all, test = "Chisq") |> 
          as_tibble(rownames="Variable") |> 
          slice_max(order_by = LRT, n = 10) # Grabo las 10 mejores variables por si hay demasiadas
        var.sel <- vars.fwd  |> 
          filter(`Pr(>Chi)` < 0.1 & Variable != "<none>") |> arrange(desc(LRT)) |> 
          top_n(1,LRT) |> select(Variable) |> pull()
        if (length(var.sel)>0 && !is.na(var.sel)) {
          mod.step <- update(mod.curr, as.formula(paste(".~.+", var.sel)))
          idx <- idx + 1
          # A futuro no renombrar y dejar la salida del tidy que tiene  term estimate std.error statistic  p.value
          coef.step <- mod.step |> broom::tidy() |> select(term, estimate) |>
            rename(Variable=term, Beta=estimate)
          if (coef.step |> filter(Variable!="(Intercept)") |> 
              select(Beta) |> sign() |> min() > 0) { 
              ult.coef <- 1
              # no sé como hacer esto en dplyr
              names(coef.step) <- c("Variable", paste("Beta.",idx, sep = ""))
              coef.steps <- coef.steps |> full_join(coef.step, by = "Variable")  
              vars.fwd.res <- bind_rows(vars.fwd.res, vars.fwd |> mutate(Paso=idx))
          } else ult.coef <- -1
        }
        else  {
          var.sel <- NA
          ult.coef <- 0
        }
      }
      else {
        var.sel <- NA
        ult.coef <- 0
      }
    }
    return(list(ult.coef=ult.coef, idx=idx, var.sel=var.sel, coef.step=coef.step, 
                coef.steps=coef.steps, mod.curr=mod.curr, ult.list.vars=vars.fwd |>  
                  filter(Variable != "<none>"), vars.fwd.res=vars.fwd.res))
  }
}

logit.fwd <- function(train, target, verbose=F) { ##function(train, target="Bad6m") {
  train2 <- train
  vars.excl <- c()
  res.step <- logit.step(train2, target)
  idx <- 1
  while (res.step$ult.coef < 0 && length(res.step$var.sel) > 0 && !is.na(res.step$var.sel)) {
    vars.excl <- c(vars.excl, res.step$var.sel)
    train2 <- train2 |> select(-one_of(res.step$var.sel))
    res.step <- logit.step(train2, target) ##logit.step(train2)
    idx <- idx + 1
    if (verbose) message("Paso ", idx, " Se incorporó variable ", res.step$var.sel)
  }
  return(list(vars.excl=vars.excl, det=res.step))
}



# Generación de SQL ----------------------------------------------------------

lin_interp_sql <- function(x_var,x1,x2,y1,y2) {
  if (x1==x2) return(NULL)
  else return(paste("when",x_var, "<",x2,"then",
                    "(",y2,"-",y1,")*(",x_var,"-",x1,")/(",x2,"-",x1,")+",y1))
}

step_2_pwl <- function(xs, ys) {
  # le quito los valores para missing y total
  stopifnot(!is.unsorted(xs) || !is.unsorted(rev(xs)))
  ys.n <- length(ys)
  # Si hay menos de 4 valores únicos entonces no hay dos puntos interiores. 
  # En este caso la interpolación es la simple
  if (n_distinct(xs) < 4) {
    # Asumo que el último elemento de $bands es el máximo sino hay un problema.
    stopifnot(length(xs)>=3 || nth(xs,-2) < nth(xs,-1))
    if (length(xs)>=3 && nth(xs,-3) < nth(xs,-2)) 
      return(tibble(x=c(nth(xs,-3),nth(xs,-2),nth(xs,-1)), 
                    y=c(nth(ys,-3),nth(ys,-2),nth(ys,-1))))
    else 
      return(tibble(x=c(nth(xs,-2),nth(xs,-1)), y=c(nth(ys,-2),nth(ys,-1))))
  } 
  else {
    # Si 2 o más puntos interiores se usan para construir la interpolación media.
    ys.m <- (ys[c(1,1:ys.n)]+ys[c(1:ys.n, ys.n)])/2
    stopifnot(length(ys.m)==length(xs)) 
    # Verificar que sea TRUE!
    # rbind(xs, ys.m)
    # Agrego el punto intermedio entre xs[l-1] y xs[l] que continua la recta determinada 
    # por xs[l-2] y xs[l-1] y que tiene ordenada ys.m[l]
    ys.l <- length(ys.m)
    x <- lin_interp(ys.m[ys.l],ys.m[ys.l-2],ys.m[ys.l-1],xs[ys.l-2],xs[ys.l-1])
    # Me aseguro que es un punto intermedio entre xs[l-1] y xs[l] 
    x <- min(x,xs[ys.l])
    # xs <- append(xs, x, after = ys.l-1)
    # ys.m <- append(ys.m, ys.m[ys.l] , after = ys.l - 1)
    # Reemplazo directamente la última posición
    xs <- replace(xs,ys.l,x)
    ys.m <- replace(ys.m, ys.l, ys.m[ys.l])
    # rbind(xs, ys.m)
    # Ahora agrego el punto intermedio entre xs[1] y xs[2] que tiene ordenada ys.m[1]
    # notar que no importa que haya cambiado la longitud de xs y ys si se hace en este orden. 
    x <- lin_interp(ys.m[1],ys.m[2],ys.m[3],xs[2],xs[3])
    # de la misma manera me aseguro que el punto intermedio sea mayor que xs[1]
    x <- max(x, xs[1])
    # xs <- append(xs, x, after = 1)
    xs <- replace(xs,1,x)
    # ys.m <- append(ys.m, ys.m[1] , after = 1)
    ys.m <- replace(ys.m, ys.l, ys.m[ys.l])
    return(tibble(x=xs, y=ys.m))
  }
}

pwl_2_sql <- function(x_var, x_var_gen, pwl, woe_nulos) {
  if (pwl |> filter(is.na(x)) |> nrow() == 1) {
    res <- paste("case when",x_var, "is null then", woe_nulos)
    pwl <- pwl |> filter(!is.na(x)) 
    res <- paste(res, "when",x_var, "<=",pwl$x[1],"then",pwl$y[1])
  } else res <- paste("case when",x_var, "<=",pwl$x[1],"then",pwl$y[1])
  l <- nrow(pwl)
  for (i in 1:(l-1))
    res <- paste(res, lin_interp_sql(x_var,pwl$x[i],pwl$x[i+1],pwl$y[i],pwl$y[i+1]))
  res <- paste(res,"when",x_var, ">=",pwl$x[l],"then",pwl$y[l]) 
  # No debería ocurrir casos nulos o no contemplados.  Cero WoE es el WoE de toda la poblacion y de la mediana.
  res <- paste(res,"when",x_var, "is null then 0 else 0 end as", x_var_gen)
  res
}

ivout_2_sql <- function (ivout) {
  col <- ivout$x
  col_agrup <- ivout$var_gen
  if (is.null(ivout$groups)) {
    # New code that builds PieceWise Linear interpolation and translate it to sql case sentence
    # Assumes that when $groups is null $bands is not null
    # When the variable is special or factor, the opposite is true, i.e. $bands is null and $groups is not null
    return(pwl_2_sql(ivout$x, ivout$var_gen, 
              ivout$ivtable |> filter(!col_agrup == "Total") |> select(x=cut_median, y=WoE), ivout$woe_nulos))
  }
  else {
    when_clause <- tab.bins |> bin.table(ivout$x) |> 
      purrr::pluck('ivtable') |> 
      filter(!Cutpoint == "Total") |> 
      select(Cutpoint, groups, WoE) |> 
      mutate(when_clause=if_else(Cutpoint=='Missing_', 
                                 paste("when", all_of(col), "is null then", WoE),
                                 paste("when", all_of(col), "in (", groups, ") then", WoE))) |> 
      summarise(when_clause=paste(when_clause, collapse = " ")) |> pull(when_clause)
    when_clause <- paste("case", when_clause, "else 0 end as", col_agrup, "", sep = " ")
    return(when_clause)
  }
}

### Función para construir sentencia que traduce un modelo a sql
# Necesita transformaciones de variables especiales, discretizaciones, 
# el beta de calibración y coeficientes
# además de la tabla de sql con las variables primitivas y 
# Tiene efectos porque abre la conexión con
# Ejemplo de invocación
# data_source_query |>
#   mod_2_sql_source(tab.co, tab.bins) -> scores_sql_source
# scores_sql_source |> write_file("scores.sql")
# expr_sql_eval <- "
#   select cuil, entidad, periodo, score_nue as score, objetivo as Target 
#   from Q"
# ```{r Verif_Implement}
# con <- dbConnect(odbc::odbc(), "BCRA", UID = "BCRA", PWD = "Monta2180",timeout = 10)
# scores_sql <- dbGetQuery(con, paste(scores_sql_source, expr_sql_eval, " where 1=1 ", par_dev_period))
# dbDisconnect(con)
# df_comp_sql <- df.scores |> rename(score_r=score) |> inner_join(scores_sql |> rename(score_sql=score))
# tabOrig_sql <- scores_sql |> select(Target, score) |> genTab_ntil(20)
# tab_all_sql <- scores_sql |> select(Target, score) |> genTab_f(genAsocCuantil(tabOrig_sql))
# ```
mod_2_sql_source <- function(sql_source=data_source_query, con, tab.coef, tab.bins, vars.transf) {
  
  transf.cad <- function(cad, vars.transf) {
    for (i in 1:nrow(vars.transf)) 
      cad <- cad |> stringr::str_replace(vars.transf[[i,"var_orig"]], vars.transf[[i,"var_tran"]])
    return(cad) 
  }
  
  val_2_chr <- function(val) {
    (suppressWarnings(val |> as.numeric() |> is.na())) -> p 
    if_else(p, paste0('"',val,'"'), as.character(val))
  }
  
  eq2list <- function(cad) {
    cad |> stringr::str_split("=",simplify = T) |> stringr::str_trim() -> l
    tibble(var = l[1], val = l[2])
  }
  
  dbGetQuery(conn = con, statement = sql_source, n = 5) -> first_5r
  
  cols_nulos_adic |> 
    reduce(.init = tibble(var = character(), val = character()), 
           .f = ~ .x |> bind_rows(.y |> eq2list() )) |> 
    group_by(var) |> 
    summarise(nulos = val |> val_2_chr() |> paste(collapse = ",")) |> 
    mutate(sent=paste('case when ',var,'in (', nulos, ') then null else ', var, 'end as', var)) -> tab_nulos_adic 
  
  first_5r |> colnames() |> setdiff(tab_nulos_adic |> pull(var)) |> 
    paste(collapse = ", ") |> 
    paste(tab_nulos_adic |> pull(sent) |> paste(collapse = ", "), sep = ", ") -> cols_clause 
  
  with_clause <- paste("with U as (", sql_source, ") ") 
  with_clause <- paste(with_clause, ",\r\n T as ( SELECT", cols_clause, "FROM U)")

  tab <- tab.coef |> select(Variable, Coef=Beta) |> filter(!is.na(Coef)) |> arrange(Variable)
  var.list <- tab$Variable[-1]
  vars2cases <- c()
  var.list.orig <- list()
  for (v in var.list ) {
    idx <- which(sapply(tab.bins, function(l) (l$var_gen==v)))[1]
    var.list.orig <- paste(var.list.orig, tab.bins[[idx]]$var, sep=",\r\n\t")
    vars2cases <- paste(vars2cases, ivout_2_sql(tab.bins[[idx]]), sep=",\r\n\t")
  }
  vars2cases <- paste(vars2cases, " 1 as Intercept", sep = ", \r\n")
  # Variables adicionales construídas en R
  # with_clause <- paste(with_clause, ", T as (select *, case when peor_situ_ult_13_24>=3 then 1 else 0 end as ult_situ_ge3_13_24 from Q)")
  # Transformaciones 
  with_clause <- paste(with_clause, ",\r\n R as (select *",vars2cases, " from T) ", sep = "")
  # Coeficientes
  tab$Variable[[1]] <- "Intercept"
  with_clause <- paste(with_clause, ",\r\n S as (select *, ",
                       paste(tab$Variable, tab$Coef, sep = " * ", collapse = " + "),
                       " as eta from R) ")
  modDesa <- with_clause
  # Trans Final
  select_clause <- paste(with_clause, "\r\n select *, cast(1000/(1+exp(-eta)) as int) as score from S ", sep = "")
  scores_sql_source <- paste(with_clause, ", \r\n Q as (select *, cast(1000/(1+exp(-eta)) as int) as score_nue from S)", sep = "")
  
  return(scores_sql_source)
}


# Range to new vars -------------------------------------------------------

# Ej Uso
# load_range(control_file, 'Valid', par_rango_niveles, c("Nombre Nivel", "Regla", "Tasa de malos máxima"), 
#            c("level_name", "rule", "max_allow_bad_rate")) -> tab_niv
load_range <- function(file, sheet, range, col_names, new_col_names) {
  file |> readxl::read_excel(sheet = sheet, range = range, col_names = T) -> df
  df |> colnames() |> assertthat::are_equal(col_names) |> 
    assertthat::assert_that(msg = "Los nombres de columna de la tabla de niveles de Score no son los esperados!")
  if (!missing(new_col_names)) colnames(df) <- new_col_names
  df |> mutate(level_order = row_number())
}

# Ej Uso
# tab_niv |> pull(max_allow_bad_rate) |> check_sorted_score_levels()
check_sorted_score_levels <- function(br) {
  assertthat::assert_that(!anyNA(br), msg = "Hay tasas de malo vacías!")
  assertthat::assert_that(!is.unsorted(br, strictly = TRUE), msg = "Las tasas de malos deben ser crecientes!")
  assertthat::assert_that(last(br) <= 1, msg = "Las peor tasa debe ser <= 1. En formato % es 100%.")
  assertthat::assert_that(first(br) > 0, msg = "Las mejor tasa debe ser > 0.")
}

# Ej Uso
# c(1:10, 10:1) |> vars_rep() 
# [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
vars_rep <- function(vec_char) {stringr::str_split(vec_char, pattern = " *, *") |> purrr::flatten_chr() |> unique()}

# Ej Uso
# df_work |> check_all_reps_vars_in_df(tab_rep)
check_all_reps_vars_in_df <- function(df, tab_rep) {
  assertthat::assert_that("report_name" %in% colnames(tab_rep), msg = "la tabla tab_rep debe tener la columna report_name")
  tab_rep |> pull(report_name) |> vars_rep() -> vars_rep
  all((vars_rep |> setdiff(c("Segmento", "score_niv"))) %in% colnames(df)) |> assertthat::assert_that(
    msg = "No se encuentran todas la variables de reporte listadas en los datos!")
  vars_rep
}


# Ej Uso
# df.new |> rename(score = score.new) |> 
#   range_2_newvar(tab_niv, "score_niv") |> 
#   range_2_newvar(tab_seg, "Segmento") |> 
#   select(1:5,ClienteNuevo, ClienteNuevoViejo, Segmento, score, score_niv) -> df_desa
# 
# df_desa |> group_by(score_niv) |> 
#   summarise(score_mn = min(score), score_mx = max(score), br = mean(bad)) |> 
#   arrange(br)
# 
# df_desa |> count(ClienteNuevo, ClienteNuevoViejo, Segmento) 
range_2_newvar <- function(df, tab_range, newvar) {
  all(c("level_name", "rule") %in% colnames(tab_range)) |> 
    assertthat::assert_that(msg = "To build {newvar} with case_when() the tab_range table need to have level_name and rule columns" |> 
                            stringr::str_glue())
  tab_range |> bind_rows(tibble(level_name = "Error", rule ="TRUE")) |> # For defense coding I add this last condition to the case_when() statement.
    purrr::pmap(function(rule, level_name, ...) expr(!!rlang::parse_expr(rule) ~ !!level_name)) -> conds
  df |> mutate({{newvar}} := case_when(!!!conds))
}

corte_2_exprs <- function(corte) {
  corte |> stringr::str_split(pattern = " *, *") |> purrr::pluck(1) |> rlang::parse_exprs()
}

tab_rep_count_row <- function(l, df, var) {
  l |> corte_2_exprs() -> l_exprs
  df |> summarise({{var}} := l, q_reps = n_distinct(!!!l_exprs))
}

# Ej Uso
# df_work |> tab_rep_count(tab_rep, report_name)
tab_rep_count <- function(df, tab_rep, var) {
  tab_rep |> pull({{var}}) |> purrr::map_dfr(tab_rep_count_row, df, var)  
}

# Shiny App ---------------------------------------------------------------

binning_pack <- function(df) {
  tab = list()
  generada <- 1
  idx <- 1
  for (col in pred.cont) {
    res <- df |> bin_monot(y="Good",x=col, nbins1=par_nbins1, minpts1=par_minpts1, nbins2=par_nbins2, minpts2=par_minpts2, 
                           rampa=par_discret) 
    res_g <- df |> apply_grouping_monot(res, paste0(col,"_g"), rampa=par_discret, woe_nulos = res$woe_nulos)
    df <- res_g$df
    tab[[idx]] <- c(var=col, var_gen=paste0(col,"_g"), generada=generada, rampa=par_discret, res)
    idx <- idx + 1
  }
  
  for (col in (pred.fac)) {
    res <- df |> bin_factor(col, minpts=par_minpts_cat, Good="Good", maxlevels=100)
    res_g <- df |> apply_grouping_factor(res, paste0(col,"_g"))
    df <- res_g$df
    tab[[idx]] <- c(var=col, var_gen=paste0(col,"_g"), generada=generada, res)
    idx <- idx + 1
  }
  return(list(df=df, tab.bins=tab))
}

bin_tab_gt <- function(df, nomvar) {
  df |> 
    mutate("{paste0(nomvar, '_g')}":=round(df[[paste0(nomvar, "_g")]], 3)) |> 
    group_2_ivtab_factor(col = nomvar, col_agrup = paste0(nomvar, "_g")) |> 
    arrange(cut_lo) |>
    tab_fmt_fct(cut_lo, cut_hi) |> 
    select(-groups) |> 
    relocate(cut_lo, cut_hi, .after=Cutpoint) |> 
    gt::gt() |> 
    gt::tab_header(title = paste('Binning de ', nomvar)) |> 
    gt::fmt_number(c('WoE'), n_sigfig = 3, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_number(c('IV'), n_sigfig = 2, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_percent(c('PctRec', 'PctGood', 'PctBad', 'BadRate'), decimals = 1, scale = F, locale = 'es_AR') |> 
    gt::cols_label(Cutpoint = "Bin", CntRec = '# Obs', CntBad = '# Malos', 
               PctRec = '% Obs', PctGood = '% Buenos', PctBad = '% Malos') |> 
    gt::fmt_missing(columns = gt::everything())  
}

pesos_nue <- function(df, var = Tipo_Cliente, nivel = 'NUEVO', p_nue = prop_nue) {
  df |> summarise(n=n()) |> pull(n) -> tot
  df |> filter({{var}} == nivel) |> summarise(n=n()) |> pull(n) -> tot_nue
  w_vie <- 1
  if (p_nue==0 | tot_nue==0) w_nue <- 0 
  else if (p_nue==1) {w_nue <- 1; w_vie <- 0} 
  else w_nue <- (tot - tot_nue) / tot_nue * p_nue/(1-p_nue)
  df |> mutate(w = if_else({{var}} == nivel, w_nue, w_vie)) -> df
  return(df)
}

df_2_gt <- function(df) {
  df |> 
    group_by(Tipo_Cliente) |> 
    summarise(n=n()) |> 
    mutate(n_p=n/sum(n)) |> 
    rename(n_q=n) |> 
    select(Tipo_Cliente, n_q, n_p) |> 
    gt::gt(rowname_col = "Tipo_Cliente") |> 
    gt::tab_stubhead("Tipo_Cliente") |> 
    gt::cols_label(n_q="#", n_p="%") |> 
    gt::fmt_number(columns = c(n_q), decimals = 0) |> 
    gt::fmt_percent(columns = c(n_p), decimals = 0) |> 
    gt::grand_summary_rows(columns = c(n_q), fns = list(Total = ~sum(.)), 
                       formatter = gt::fmt_number, decimals = 0) |> 
    gt::grand_summary_rows(columns = c(n_p), fns = list(Total = ~sum(.)), 
                       formatter = gt::fmt_percent, decimals = 0) -> res
  return(res)
}

# Otras -------------------------------------------------------------------

Binning.plot <- function(result) {
  n <- nrow(result$ivtable)-2
  g <- tibble(x=result$bands[-1], y=result$ivtable[1:n, "BadRate"])
  g <- g |> ggplot2::ggplot(ggplot2::aes(x=x, y=y)) + 
    ggplot2::labs(x=result$x, y="Bad Rate")
  g <- g + ggplot2::geom_line(ggplot2::aes(colour="Binning")) + 
    ggplot2::geom_smooth(method = "loess", ggplot2::aes(colour="Suav"), se=F) + 
    ggplot2::geom_smooth(method = "lm", ggplot2::aes(colour="Lineal"), se=F) + 
    ggplot2::coord_cartesian(ylim = c(0, 1))
  return(g)  
}

na.sf.replace <- function (frame) {
  vars <- names(frame)
  for (j in vars) {
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      frame[[j]] <- x
    }
  }
  frame    
}

vif_func <- function(in_frame,thresh=10,trace=T,...){
# SF: Originalmente de https://www.kaggle.com/robertoruiz/dealing-with-multicollinearity
# pero uso el vif del paquete car xq es innecesario sumar otro paquete como el fmsb 
# Además cambié el método de impresión por kable.
# de todas formas es una porquería como fabrica vif_vals y el control de flujo con break...  
  
  # require(car) #  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, vif(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      # prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      # Si hay mas de 4 variables es buena la impresión. Tendría que graficar.
      if (ncol(vif_init)<=4) print(knitr::kable(cbind(vif_init[,1], 
                               apply(vif_init[,-1], 2, function(x) round(as.numeric(x),2)))))
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      # Si llegamos a dos variables paramos
      if(length(var_names)<=2) break
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-vif(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        # prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        if (ncol(vif_vals)<=4) print(knitr::kable(cbind(vif_vals[,1], 
                                 apply(vif_vals[,-1], 2, function(x) round(as.numeric(x),2)))))
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

bin.table <- function(tab.bins, nomvar) {
  res <- sapply(tab.bins, function(l) (l$var==nomvar))
  tab <- tab.bins[[which(res)[1]]]
  return(tab)
}

woe.table <- function(tab.bins, nomvar) {
  bin.table(tab.bins, nomvar)$ivtable |> 
  knitr::kable(caption = paste("Binning de ", nomvar, "con binning monótono"), digits = 2)
}

# Deprecated
char_estab <- function(var.sel="edad", 
                           var.sel.agrup="edad_g", source.dev, source.sample) {
  select_clause <- paste("select", var.sel.agrup, ",min(", var.sel, ") as var_mn, 
                         max(", var.sel, ") as var_mx, count(1) as q  from ",
                         source.dev, "group by", var.sel.agrup)
  sampleDataQuery <- select_clause
  #inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  tab.iv.1 <- rxImport(inDataSource) |> rename(q.dev=q)
  select_clause <- paste("select", var.sel.agrup, ",count(1) as q  from ",
                         source.sample, "group by", var.sel.agrup)
  sampleDataQuery <- select_clause
  #inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  tab.iv.2 <- rxImport(inDataSource) |> rename(q.sam=q)
  tab.char.an <- tab.iv.1 |> full_join(tab.iv.2) |> arrange(var_mx) |> 
    replace_na(list(q.dev=0.1, q.sam=0.1))
  tab.char.an <- tab.char.an |> 
    mutate(t.dev=sum(q.dev), t.sam=sum(q.sam), f.dev=q.dev/t.dev, f.sam=q.sam/t.sam,
           dif.p=f.sam-f.dev, woe=log(f.sam/f.dev), iv=dif.p*woe) 
  tab <- tab.char.an |> select(var_mn, var_mx, f.dev, f.sam, dif.p, woe, iv)
  return(tab)
}

### Función para que devuelve un reporte de IV con totales.  
### Es bastante general, solo exije una variable de agrupamiento y Good.
### Está basada en agrup_factor.  Se aplica a continuas y factores porque sólo evalúa el IV
### luego de agrupar.  
### Tampoco calcula los integrantes o rangos de la variable sin agrupar de cada grupo.
repIV <- function(df, col, Good) {
  iv <- df |> group_2_ivtab_factor(col=col, 
                                    col_agrup=col, Good=Good) 
  iv <- iv |> select(-Cutpoint, -TotRec, -TotGood, -TotBad, -LnOdds, -LnODDS, -CntCumRec, 
                      -CntCumGood, -CntCumBad)
  return(iv)
}


# Ejemplo de llamada
# ivs_x_per <- function(col, col_agrup, woes_tab) {
#   iv_x_per( df |> mutate(Good=as.integer(as.character(Good))), col, col_agrup, woes_tab)
# }
# res <- tab_woes |> slice(1:10) |> mutate(iv=pmap(list(var, var_gen, woes_tab), ivs_x_per))
# res
# res$iv |> reduce(bind_rows)
# Asume que train contiene Good, periodo y las variables listadas en cols. 
# TODO: Agregar manejo de errores y advertencias cuando bads o recs son cero dentro de algún corte. 
# También asume la variable Good en df. Lo resuelvo por ahora con IV=sum(IV, na.rm = T)
# Agregar el detalle de ivs a res.
iv_x_per <- function(df, tipo, col, col_agrup, woes_tab) {
  ivs <- tibble()
  pers <- df |> pull(par_periodo) |> unique()
  for (p in pers) {
    if (tipo=='Continua') {
      iv <- df |> filter(periodo==p) |> mutate(Good=as.integer(as.character(Good))) |> 
        group_2_ivtab_cont(col, col_agrup, woes_tab = woes_tab) |> 
        slice(-n()) |> 
        summarise(periodo=ymd(paste0(p, "01")), grupos=n(), monot_fl=!is.unsorted(BadRate), 
                  IV=sum(IV, na.rm = T), 
                  cut_lo=min(cut_lo), cut_hi=max(cut_hi), TotRec=sum(CntRec), 
                  TotBad=sum(CntBad), BadRate=100*TotBad/TotRec)
    } else if (tipo=='Factor') {
      # A diferencias de la versión Continua siempre agrupamos por col_agrup. Ese es el mapeo obtenido en train.
      iv <- df |> filter(periodo==p) |> mutate(Good=as.integer(as.character(Good))) |> 
        group_2_ivtab_factor(col, col_agrup) |> 
        tab_fmt_fct() |> slice(-n()) |>  
        summarise(periodo=ymd(paste0(p, "01")), grupos=n(), monot_fl=!is.unsorted(BadRate), 
                  IV=sum(IV, na.rm = T), 
                  TotRec=sum(CntRec), 
                  TotBad=sum(CntBad), BadRate=100*TotBad/TotRec)
      
    } else abort("Tipo inesperado. Se esperaba Continua o Factor y fue:", "error_func_aux", tipo)
    ivs <- ivs |> bind_rows(iv)
  }
  res <- ivs |> summarise(variable=col, tipo=tipo, pers=length(pers), 
                           iv_sd=sd(IV), iv_min=min(IV), iv_mean=mean(IV), iv_median=median(IV),  
                           iv_max=max(IV))
  return(res)
}

# Llama a la función iv_x_per usando el df del entorno local.  No es lo más elegante.  Fue necesario pq 
# el primer argumento debe ser la lista de los otros.
ivs_x_per <- function(tipo, col, col_agrup, woes_tab) {
  iv_x_per( df |> mutate(Good=as.integer(as.character(Good))), tipo, col, col_agrup, woes_tab)
}

# Dado un vector, devuelve el porcentaje de posiciones que coinciden con el mismo vector ordenado.
porc_sorted <- function(vec, asc=T) {
  vec_sorted <- sort(vec, decreasing = !asc)
  q_sorted <- sum(vec==vec_sorted)
  return(q_sorted/length(vec))
}

# Calcula los porcentajes de cuantiles ordenados solo considerando aquellos con por lo menos q_gb_min buenos y malos. 
iv_tab_porc_sorted <- function(iv_tab, q_gb_min=50, tipo_var = 'Continua') {
  assertthat::assert_that(tipo_var %in% c('Continua', 'Factor'), msg = 'iv_tab_porc_sorted(): Los tipos de variables admitidos son Continua o Factor!')
  if (tipo_var=='Continua') {
    iv_tab |> # Asume que ya viene ordenada por los WoEs de Train.  Es decir, por BadRate desc según Train.
      filter(col_agrup != 'Total', !is.na(cut_hi), CntGood >= q_gb_min, CntBad >= q_gb_min) |> 
      summarise(monot_fl=!is.unsorted(BadRate), p_sorted=porc_sorted(BadRate, asc = F), q_measured=length(BadRate))    
  } else {
    iv_tab |> 
      filter(Cutpoint != 'Total', !is.na(Cutpoint), CntGood >= q_gb_min, CntBad >= q_gb_min) |>   
      arrange(Cutpoint) |> # Aquí el orden implícito lo dá el Cutpoint que representa los WoEs de Train. 
      summarise(monot_fl=!is.unsorted(BadRate), p_sorted=porc_sorted(BadRate, asc = F), q_measured=length(BadRate))    
  }
}

iv_x_grupos <- function(df, tipo, col, col_agrup, woes_tab, var_grupo, grupos) {
  assertthat::assert_that(exists("par_iv_cuantiles_gb_min"),  exists("par_iv_tot_min"), 
                          exists("par_iv_tot_gb_min"))
  
  assertthat::assert_that(0 < par_iv_cuantiles_gb_min, par_iv_cuantiles_gb_min < par_iv_tot_gb_min, par_iv_tot_gb_min < par_iv_tot_min)
  ivs <- tibble()
  for (v in grupos) { 
    if (tipo=='Continua') {
      #print(paste("Tipo Continua Grupo", v, "col", col, "col_agrup", col_agrup, "woes_tab", woes_tab))
      iv <- df |> filter(!!rlang::parse_expr(var_grupo)==v) |> mutate(Good=as.integer(as.character(Good))) |> 
        group_2_ivtab_cont(col, col_agrup, woes_tab = woes_tab) 
      iv <- iv |> slice(n()) |>                  # La ultima fila es la que tiene los totales
        rename(TotRec=CntRec, TotGood=CntGood, TotBad=CntBad) |> 
        mutate(variable=col, tipo=tipo, var_grupo=var_grupo, valor_grupo=v, !!var_grupo:=v) |> 
        bind_cols(iv |> summarise(bines=n()-1)) |> 
        bind_cols(iv |> iv_tab_porc_sorted(tipo_var = tipo, q_gb_min = par_iv_cuantiles_gb_min)) |>
        select(-col_agrup, -PctRec, -PctGood, -PctBad, -WoE)
    } 
    else if (tipo=='Factor') {
      # A diferencias de la versión Continua siempre agrupamos por col_agrup. Ese es el mapeo obtenido en train.
      iv <- df |> filter(!!rlang::parse_expr(var_grupo)==v) |> mutate(Good=as.integer(as.character(Good))) |> 
        group_2_ivtab_factor(col, col_agrup) |> 
        tab_fmt_fct() 
      iv <- iv |> slice(-n()) |>  
        summarise(variable=col, tipo=tipo, var_grupo=var_grupo, valor_grupo=v, !!var_grupo:=v, 
                  IV=sum(IV, na.rm = T), 
                  TotRec=sum(CntRec), TotGood=sum(CntGood), TotBad=sum(CntBad), 
                  BadRate=100*TotBad/TotRec) |> 
        bind_cols(iv |> summarise(bines=n()-1)) |> 
        bind_cols(iv |> iv_tab_porc_sorted(tipo_var = tipo, q_gb_min = par_iv_cuantiles_gb_min))
    } 
    else abort("Tipo inesperado. Se esperaba Continua o Factor y fue:", "error_func_aux", tipo)
    ivs <- ivs |> bind_rows(iv)
  }
  
  # ivs_sum <- ivs |> filter(TotRec >= par_iv_tot_min & 
  #                             TotGood >= par_iv_tot_gb_min & 
  #                             TotBad >= par_iv_tot_gb_min) 
  # if (nrow(ivs_sum) >= 2) {
  #   sin_datos_sufic <- FALSE
  #   ivs_sum <- ivs_sum |> 
  #     summarise(iv_sd=sd(IV), iv_min=min(IV), iv_mean=mean(IV), iv_median=median(IV),  
  #               iv_max=max(IV), grupos_ordenados=sum(monot_fl), grupos_con_datos_sufic=n())
  # } else {
  #   sin_datos_sufic <- TRUE
  #   ivs_sum <- tibble(iv_sd=NA_real_, iv_min=NA_real_, iv_mean=NA_real_, iv_median=NA_real_,   
  #                     iv_max=NA_real_, grupos_ordenados=0)
  # }
  
  return(ivs)
}

estab_x_grupo <- function(df, var_grupo, tab_woes) {
  df <- df |> mutate("{var_grupo}":=coalesce(.data[[var_grupo]], "Missing"), 
                     Good=as.integer(as.character(Good)))
  grupos <- df |> pull(var_grupo) |> unique()
  
  xvars <- tab_woes |> purrr::pmap_dfr(~ iv_x_grupos(df=df, tipo=..1, col=..2, 
    col_agrup=..3, woes_tab=..4, var_grupo=var_grupo, grupos=grupos))

  xgrupos <- xvars |> 
    group_by(var_grupo, valor_grupo) |> 
    summarise(across(c(TotRec, TotGood, TotBad, BadRate), first)) |>  
    arrange(desc(TotRec))
  
  return(list(var_grupo=var_grupo, xvars=xvars, xgrupos=xgrupos, tab_woes=tab_woes))
}

estab_tab_2_gt <- function(res_estab, var_list=NULL) {
  
  xgrupo_tab <- res_estab |> purrr::pluck("xgrupos") |> gt::gt() |> #view("x Prov")
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Grupo")) |> 
    gt::fmt_number(c('TotRec', 'TotGood', 'TotBad'), decimals = 0, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_percent(c('BadRate'), decimals = 1, scale = F, locale = 'es_AR') |> 
    gt::cols_label(valor_grupo = "Grupo", TotRec = gt::html('#<br>Obs'), 
               TotGood = gt::html('#<br>Buenos'), TotBad =gt::html('#<br>Malos'), 
               BadRate = gt::html('Tasa<br>de Malos'))|> 
    gt::fmt_missing(columns = everything()) |> 
    gt::opt_row_striping() 
  
  tab_top_10 <- res_estab |> purrr::pluck("xgrupos") |> slice_max(order_by = TotRec, n = 10) |> 
    mutate(sufic_datos = TotRec >= par_iv_tot_min & TotGood >= par_iv_tot_gb_min & TotBad >= par_iv_tot_gb_min)

  xvar_tab <- res_estab |> purrr::pluck("xvars") |> 
    # Se usa este argumento para restringir la tabla a variables del modelo por ejemplo
    filter(is.null(var_list) | variable %in% var_list) |> 
    filter(valor_grupo %in% (tab_top_10 |> pull(valor_grupo))) |> 
    mutate(p_measured=q_measured/bines) |> 
    arrange(variable, desc(TotRec)) |> 
    select(variable, valor_grupo, IV, p_measured, p_sorted) |> 
    rename(`% Bines c/ datos`=p_measured, `% Ordenados`=p_sorted) |> 
    tidyr::pivot_wider(names_from = valor_grupo, values_from = c(IV, `% Bines c/ datos`, `% Ordenados`), names_sep = ".") |> 
    gt::gt() |> 
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Variable")) |> 
    gt::tab_spanner_delim(delim = ".") |> 
    gt::cols_label(variable="Variable") |> 
    gt::fmt_number(columns = starts_with("IV"), n_sigfig = 2, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_percent(columns = starts_with("% Bines c/ datos") | starts_with("% Ordenados"), decimals = 0, scale = T, locale = 'es_AR') |> 
    gt::fmt_missing(columns = gt::everything()) |> 
    gt::tab_style(locations = gt::cells_column_labels(columns = gt::ends_with(tab_top_10 |> filter(!sufic_datos) |> pull(valor_grupo))), 
              style = list(gt::cell_text(color = "orange"))) |> 
    gt::opt_row_striping() |> 
    gt::tab_footnote(locations = gt::cells_column_spanners(spanners = '% Bines c/ datos'), 
                 footnote = glue::glue("% de bines con por lo menos ", par_iv_cuantiles_gb_min, " buenos y malos")) |> 
    gt::tab_footnote(locations = gt::cells_column_spanners(spanners = '% Ordenados'), 
                 footnote = "% de bines que coinciden con los cuantiles ordenados por tasa de malos") |> 
    gt::tab_source_note(source_note = gt::html(glue::glue("Los grupos en ", "<span style='color:orange'>naranja</span>" ,
                                                  " no cumplen que haya por lo menos ", par_iv_tot_min, 
                                                  " casos o que por lo menos hayan ", par_iv_tot_gb_min, " buenos y malos en total.")))
  return(list(xgrupo_tab=xgrupo_tab, xvar_tab=xvar_tab))
}

# Auxiliares para publicar las tablas de estabilidad en gt
details_tag <- function(html_content, html_head) {
  glue::glue("<details><summary>{html_head}</summary><p>{html_content}<p></details>") |> 
    gt::html()
}

# 11 representa el lugar de la variable de segmento
ivs_det_2_gt <- function(df) {
  df |> 
    select(11, TotRec, TotGood, TotBad, IV, BadRate, q_measured, p_sorted) |> 
    arrange(desc(q_measured)) |> 
    slice_head(n = 10) |> gt::gt() |> 
    gt::cols_label(TotRec = '#', TotGood = gt::html('Buenos'), 
               TotBad = gt::html('Malos'), BadRate = gt::html('Tasa<br>Malos'), 
               q_measured = gt::html('Cuantiles<br>medidos'), 
               p_sorted = gt::html('Cuantiles<br>ordenados')) |> 
    gt::fmt_number(columns = c('IV'), decimals = 1, locale = 'es_AR') |> 
    gt::fmt_percent(columns = c('BadRate'), decimals = 1, scale = F, locale = 'es_AR') |> 
    gt::fmt_percent(p_sorted, decimals = 1, locale = "es_AR") |> 
    gt::fmt_missing(p_sorted)
  
}

# Auxiliares para la generación de Excels. 
tab_fmt_cont <- function(tab) {
  tab <- tab |> 
    select(cut_lo, cut_median, cut_mean, cut_hi, CntRec, CntGood, CntBad, PctRec, PctGood, PctBad, BadRate, WoE, IV)
  totals <- tab |> 
    summarise(cut_lo=min(cut_lo), cut_median=NA_integer_, cut_mean=NA_integer_, 
              cut_hi=max(cut_hi), CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  tab <- tab |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(tab)
}

tab_fmt_fct <- function(tab, ...) {
  select_vars <- enquos(...)
  tab <- tab |> 
    select(Cutpoint, CntRec, CntGood, CntBad, 
           PctRec, PctGood, PctBad, BadRate, WoE, IV, !!!select_vars, groups)
  return(tab)
}

# list24lists <- function(long_list, name='Variable') {
#   l <- length(long_list) %/% 4 + 1
#   res <- list(tibble(!!name:=long_list) |> slice(1:l), 
#               matrix(numeric(), nrow=0, ncol=1), 
#               tibble(!!name:=long_list) |> slice((l+1):(2*l)), 
#               matrix(numeric(), nrow=0, ncol=1),                    
#               tibble(!!name:=long_list) |> slice((2*l+1):(3*l)), 
#               matrix(numeric(), nrow=0, ncol=1),
#               tibble(!!name:=long_list) |> slice((3*l+1):length(long_list)))
#   return(res)
# }

list24lists <- function(long_list, name='Var', long_min=12) {
  if (length(long_list) < long_min) return(tibble(!!name:=long_list))
  if (length(long_list) %% 4 > 0) # Relleno con NA_char
    long_list <- c(long_list, rep(NA_character_,4 - (length(long_list) %% 4)))
  l <- length(long_list) %/% 4
  res <- tibble(tibble(!!paste(name,1,"a",l):=long_list) |> slice(1:l), 
                   tibble(!!paste(name,l+1,"a",2*l):=long_list) |> slice((l+1):(2*l)), 
                   tibble(!!paste(name,2*l+1,"a",3*l):=long_list) |> slice((2*l+1):(3*l)), 
                   tibble(!!paste(name,3*l+1,"a",4*l):=long_list) |> slice((3*l+1):length(long_list)),
                .name_repair="minimal")
  return(res)
}

df24df <- function(long_df, long_min=12) {
  if (nrow(long_df) < long_min) return(long_df)
  if (nrow(long_df) %% 4 > 0) # Relleno con NA
    long_df <- bind_rows(long_df, long_df |> slice(1:(4 - (nrow(long_df) %% 4))) |> mutate_all(~NA))
  l <- nrow(long_df) %/% 4
  res <- tibble(long_df |> slice(1:l), 
                   long_df |> slice((l+1):(2*l)), 
                   long_df |> slice((2*l+1):(3*l)), 
                   long_df |> slice((3*l+1):nrow(long_df)), .name_repair="universal") |> 
    suppressMessages()
  return(res)
}

df23df <- function(long_df, long_min=12) {
  if (nrow(long_df) < long_min) return(long_df)
  if (nrow(long_df) %% 3 > 0) # Relleno con NA
    long_df <- bind_rows(long_df, long_df |> slice(1:(3 - (nrow(long_df) %% 3))) |> mutate_all(~NA))
  l <- nrow(long_df) %/% 3
  res <- tibble(long_df |> slice(1:l), 
                long_df |> slice((l+1):(2*l)), 
                long_df |> slice((2*l+1):nrow(long_df)), .name_repair="universal") |> 
    suppressMessages()
  return(res)
}

df23gt <- function(long_df, label_col1='Variable', label_col2='Valor', long_min=12) {
  long_df |> 
  rename('col1'=1, 'col2'=2) |> 
  df23df(long_min = long_min) -> tab 
  if (ncol(tab)==2) {
    tab |> 
      gt::gt() |> 
      gt::fmt_missing(gt::everything()) |> 
      gt::cols_label(col1 = label_col1, col2 = label_col2) -> res
  } else {
    tab |> 
      gt::gt() |> 
      gt::fmt_missing(gt::everything()) |> 
      gt::cols_label(col1...1 = label_col1, col2...2 = label_col2, 
                 col1...3 = label_col1, col2...4 = label_col2, 
                 col1...5 = label_col1, col2...6 = label_col2) -> res
  }
  return(res)
}

df24gt <- function(long_df, label_col1='Variable', label_col2='Valor', long_min=12) {
  long_df |> 
    rename('col1'=1, 'col2'=2) |> 
    df24df(long_min = long_min) -> tab 
  if (ncol(tab)==2) {
    tab |> 
      gt::gt() |> 
      gt::fmt_missing(gt::everything()) |> 
      gt::cols_label(col1 = label_col1, col2 = label_col2) -> res
  } else {
    tab |> 
      gt::gt() |> 
      gt::fmt_missing(gt::everything()) |> 
      gt::cols_label(col1...1 = label_col1, col2...2 = label_col2, 
                 col1...3 = label_col1, col2...4 = label_col2, 
                 col1...5 = label_col1, col2...6 = label_col2, 
                 col1...7 = label_col1, col2...8 = label_col2) -> res
  }
  return(res)
}


# En desarrollo -----------------------------------------------------------

# Versión que imita a Modeler 
# bin.sf <- function(x, nbins, minpts = floor(length(x)/nbins), nvals.min = 5,
#                    verbose = F) {
#   xtbl <- table(x)
#   xval <- as.numeric(names(xtbl))
#   nvals <- length(xval)
#   if (nvals < nvals.min || is.null(nbins) || floor(minpts)!=minpts) 
#     stop("bins.sf: parametros inconsistentes ", 
#          "\nValores unicos: ", nvals, "<",nvals.min,
#          "\nNumero de bines nulo? ", is.null(nbins), 
#          "\nCasos minimos x bin es entero? ", minpts)
#   binsz <- max(minpts, floor(length(x)/nbins))
#   binacc <- binsz
#   binlo <- vector(nbins, mode = "integer")
#   binhi <- vector(nbins, mode = "integer")
#   binct <- vector(nbins, mode = "integer")
#   startbin <- TRUE
#   k <- 1
#   s <- 0
#   for (i in 1:nvals) {
#     s <- s + xtbl[i]
#     if (startbin) {
#       binlo[k] <- xval[i]
#       startbin <- FALSE
#     }
#     if (s >= binacc) {
#       binhi[k] <- xval[i]
#       binct[k] <- s
#       # Descuenta del tamaño mínimo del bin el sobrante anterior.
#       # Tiene el efecto de que el tamaño acumulado se aproxime al tamaño acumulado por binsz
#       binacc <- binsz - (s- binacc) 
#       k <- k + 1
#       s <- 0
#       startbin <- TRUE
#     }
#   }
#   if (!startbin) {
#     # El último bucket no llegó al mínimo, entonces reseteo el actual y lo sumo al anterior
#     binlo[k] <- 0
#     k <- k - 1
#     binhi[k] <- xval[nvals]
#     binct[k] <- binct[k] + s
#   }
#   binlo <- binlo[1:k]
#   binhi <- binhi[1:k]
#   binct <- binct[1:k]
#   names(binct) <- paste("[", binlo, ", ", binhi, "]", sep = "")
#   if (verbose) 
#     print(binct)
#   return(list(binlo = binlo, binhi = binhi, binct = binct, 
#               xtbl = xtbl, xval = xval))
# }


