# Funciones auxiliares para la Presentación


###  Reporte de medidas desde DF con las columnas apropiadas.  
# Devuelve Per_pred con la convención 0=train, 0.5=valid, Z-{0}=OOT
# Ejemplo:
# Data <- BSF_RF_v3_Res_old %>% select(Modelo, Periodo, Per_Pred, truth=Real, prob.Bad=Prob.Bad, prob.Good=Prob.Good, 
#                                      response=Predicho) 
# 
# modelos <- list(list(name="RF_201407", train_per=201407), 
#                 list(name="RF_201408", train_per=201408))
# periodos <- list(201407, 201408, 201504)
# res <- medidasxPersyMods(Data, modelos, periodos)
medidasxPeryMod <- function(data, modelo, train_per, periodo, valid=NA, msrs=measures) {
  if (is.na(valid)) {
    data <- data %>% filter(Modelo == modelo, Periodo == periodo) %>%
      filter(!is.na(response)) %>% select(truth, prob.Bad, prob.Good, response) %>%
      as.data.frame()
    Per_pred <- diff_months(train_per, periodo)
  } 
  else if (valid==0) {
    data <- data %>% filter(Modelo == modelo, Per_Pred == 0) %>%
      filter(!is.na(response)) %>% select(truth, prob.Bad, prob.Good, response) %>%
      as.data.frame()
    Per_pred <- 0
  }
  else {
    # asumo valid = 1 
    data <- data %>% filter(Modelo == modelo, Per_Pred == 0.5) %>%
      filter(!is.na(response)) %>% select(truth, prob.Bad, prob.Good, response) %>%
      as.data.frame()
    Per_pred <- 0.5
  }
  if (nrow(data)==0) stop("No hay datos para medir! ", "Modelo: ", modelo, "Periodo: ", periodo,
                          "Per_pred ", Per_pred, "valid: ", valid)
  # Construyo un prediction result a mano.  Ojo que hay mucho hardcodeo!
  td <- list(id="BSF", type="classif", target="target", size=368516, 
             n.feat=list("numerics"=53, "factors"=1, "ordered"=0), 
             has.missings=FALSE, has.weights=FALSE, has.blocking=FALSE, 
             class.levels=c("Bad","Good"), positive="Bad", negative="Good")
  class(td) <- c("ClassifTaskDesc", "SupervisedTaskDesc", "TaskDesc")
  pred <- list(predict.type = "prob", 
               data = data, 
               threshold = c(Bad=0.5, Good=0.5), 
               task.desc = td, time = 1234567890, 
               error = as.character(NA), dump = NULL)
  class(pred) <- list("PredictionClassif", "Prediction")
  res <- performance(pred, measures = msrs)
  res <- enframe(res, name = "Medida", value = "Valor")
  res <- data.frame(Modelo=modelo, Periodo=periodo, Per_train=train_per, 
                    Per_pred=Per_pred, res)
  return(as_data_frame(res))
}

medidasxPersyMods <- function(Data, modelos, periodos) {
  res <- tribble(~Modelo, ~Periodo, ~Per_train, ~Per_pred, ~Medida, ~Valor)
  for (mod in modelos) {
    res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, 0, valid=0))
    res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, 0, valid=1))
    for (per in periodos) 
      res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, per, valid=NA))
  } 
  return(res)  
}

# SF, Calculate months difference between two dates
# requires lubridate package
# Ex: diff_months(201407, 201504)
# Note the format "%Y%m" is not used. So, I didn't change valid formats.
diff_months <- function(init.date, end.date, frmt = "%Y%m") {
  if (frmt=="%Y%m") {
    init.date <- as.Date(paste0(init.date, '01'), format='%Y%m%d')
    end.date <- as.Date(paste0(end.date, '01'), format='%Y%m%d')
  } else {
    init.date <- as.Date(init.date, format=frmt)
    end.date <- as.Date(end.date, format=frmt)
  }
  lubridate::interval(init.date, end.date) %/% months(1) 
}


# Tablas por cuantiles y KS -----------------------------------------------
# generación de reportes de performance a cuantiles y con intervalos fijos.
# 20180111: Dejé todos los campos para construir medidas personalizadas
# Asume que df tiene los campos score y target.  Malos = sum(target)
# Ojo! Siguiendo los problemas con coalesce (ver https://github.com/tidyverse/dplyr/issues/2254)
# fuerzo la conversión a integer de Target
genTab_ntil <- function(df,n) {
  res <- df %>% 
    arrange(desc(score)) %>% 
    mutate(Cuantil=ntile(x=score, n=n), Target=dplyr::coalesce(as.integer(as.character(target)),0L)) %>% 
    group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score), 
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos) %>% 
    arrange(Cuantil) %>%     
    mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos), 
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,           
           P.asc.Total=100*asc.total/sum(Total),
           P.asc.Buenos=100*asc.buenos/sum(Buenos), 
           P.asc.Malos=100*asc.malos/sum(Malos),
           Tasa_Malos_desc=100*desc.malos/desc.total,
           asc.Odds=asc.buenos/asc.malos, 
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))), 
           KS=abs(P.asc.Buenos - P.asc.Malos)) %>% 
    arrange(desc(Cuantil)) %>% 
    select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total, 
           Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, 
           KS, Odds) 
  return(res)
}

genTab_f <- function(df,Asoc, rev=F, vars_adic=NULL) {
  res <- df %>% 
    mutate(Target=dplyr::coalesce(as.integer(as.character(target)),0L), score=round(score)) %>% 
    group_by(score) %>% arrange(desc(score)) %>% 
    summarise(buenos=sum(1-Target), malos=sum(Target)) %>% 
    mutate(Cuantil=Asoc(score)) %>% group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score), 
              Buenos=sum(buenos), Malos=sum(malos),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos)
  if (rev) res <- res %>% arrange(desc(Cuantil))
  else  res <- res %>% arrange(Cuantil)
    res <- res %>% 
      mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
             asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos), 
             desc.total=sum(Total) - asc.total + Total,
             desc.malos=sum(Malos) - asc.malos + Malos,           
             P.asc.Total=100*asc.total/sum(Total),
             P.asc.Buenos=100*asc.buenos/sum(Buenos), 
             P.asc.Malos=100*asc.malos/sum(Malos),
             Tasa_Malos_desc=100*desc.malos/desc.total,
             asc.Odds=asc.buenos/asc.malos, 
             Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))), 
             KS=abs(P.asc.Buenos - P.asc.Malos))
  if (rev) res <- res %>% arrange(Cuantil)
  else  res <- res %>% arrange(desc(Cuantil))
  if (missing(vars_adic)) 
    res <- res %>% select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total, 
           Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, 
           KS, Odds) 
  else res <- res %>% 
      select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total, 
             Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, 
             KS, Odds, all_of(vars_adic))
  return(res)
}

### Versión con mas campos
# quité el round de mean(score):     summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score), 
genTab_n_plus <- function(df,n) {
  res <- df %>% 
    arrange(desc(score)) %>% 
    mutate(Cuantil=ntile(x=score, n=n), Target=dplyr::coalesce(as.integer(as.character(target)),0L)) %>% 
    group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score), 
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos) %>% 
    arrange(Cuantil) %>%     
    mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos), 
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,           
           P.asc.Total=100*asc.total/sum(Total),
           P.asc.Buenos=100*asc.buenos/sum(Buenos), 
           P.asc.Malos=100*asc.malos/sum(Malos),
           Tasa_Malos_desc=100*desc.malos/desc.total,
           asc.Odds=asc.buenos/asc.malos, 
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))), 
           KS=abs(P.asc.Buenos - P.asc.Malos)) %>%
    arrange(desc(Cuantil))
    # %>% 
    # select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total, 
    #        Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, 
    #        KS, Odds) 
  return(res)
}


genAsocCuantil <- function(tab, rev=F) {
  res <- tab  %>%  select(Cuantil, Score_Max)
  res[1,2] <- 999  
  f <- function(score) {
    if (rev)     cbind(res, score) %>% filter(score <= Score_Max) %>%
      summarise(Cuantil=max(Cuantil))  %>% pull()
    else 
    cbind(res, score) %>% filter(score <= Score_Max) %>%
      summarise(Cuantil=min(Cuantil))  %>% pull()
  }
  return(Vectorize(f))
}

# Reports with GT package -------------------------------------------------

# Resume una tabla por la última variable de agrupación, calcula porcentajes y lo publica en gt
# Asume por lo menos dos variables de agrupación! 
# Ej de uso Datos_muestra %>% group_by(Bad) %>% sum_table_1_gt
sum_table_1_gt <- function(tab) {
  ult_group_col <- tab %>% group_vars() %>% tail(1)
  tab_gt <- tab %>% summarise(Q=n()) %>% mutate(Perc=Q/sum(Q)) %>% 
    gt(rowname_col = all_of(ult_group_col)) %>% 
    tab_header(title=str_c('Distribucion de ',ult_group_col)) %>% 
    tab_stubhead(label = all_of(ult_group_col)) %>% 
    fmt_missing(columns = everything()) %>% 
    fmt_number(columns = 'Q', locale = 'es_AR', decimals = 0) %>% 
    fmt_percent(columns = 'Perc', locale = 'es_AR', decimals = 1) %>% 
    cols_align(align = 'right', columns = c(Q, Perc)) %>% 
    tab_style(style = cell_text(align = 'right'), locations = cells_stub()) %>% 
    cols_align(align = 'center', columns = all_of(ult_group_col)) %>%   
    cols_label(Perc=gt::html("%")) %>% 
    grand_summary_rows(columns = Q, fns = list(`Total`=~sum(., na.rm = T)), 
                       formatter = fmt_number, decimals = 0, sep_mark = '', locale = 'es_AR') %>% 
    opt_row_striping() 
  return(tab_gt)
}

# Resume una tabla por la última variable de agrupación, calcula totales por las otras variables 
# de agrupación y lo publica en gt
# Asume por lo menos dos variables de agrupación! 
# Ej de uso Datos_muestra %>% group_by(Part, Bad) %>% sum_table_2_gt
sum_table_2_gt <- function(tab) {
  ult_group_col <- tab %>% group_vars() %>% tail(1)
  other_group_cols <- tab %>% group_vars() %>% head(-1)
  tab_gt <- tab %>% summarise(Q=n()) %>% mutate(Perc=Q/sum(Q)) %>% 
    gt(rowname_col = all_of(ult_group_col), groupname_col = other_group_cols) %>% 
    tab_header(title=str_c('Distribucion de ',ult_group_col), 
               subtitle = str_c('x ', str_c(other_group_cols, collapse = ' y '))) %>% 
    tab_stubhead(label = all_of(ult_group_col)) %>% 
    fmt_missing(columns = everything()) %>% 
    fmt_number(columns = 'Q', locale = 'es_AR', decimals = 0) %>% 
    fmt_percent(columns = 'Perc', locale = 'es_AR', decimals = 1) %>% 
    cols_align(align = 'right', columns = c(Q, Perc)) %>% 
    tab_style(style = cell_text(align = 'right'), locations = cells_stub()) %>% 
    cols_align(align = 'center', columns = all_of(ult_group_col)) %>%   
    cols_label(Perc=gt::html("%")) %>% 
    summary_rows(groups = T, columns = c(Q), fns = list(Total=~sum(., na.rm = T)), 
                 formatter = fmt_number, decimals = 0, sep_mark = '', locale = 'es_AR') %>% 
    summary_rows(groups = T, columns = c(Perc), fns = list(Total=~sum(., na.rm = T)), 
                 formatter = fmt_percent, decimals = 1, locale = 'es_AR') %>% 
    grand_summary_rows(columns = Q, fns = list(`Gran Total`=~sum(., na.rm = T)), 
                       formatter = fmt_number, decimals = 0, sep_mark = '', locale = 'es_AR') %>% 
    opt_row_striping() 
  return(tab_gt)
}

# Asume que df continen las columnas Target y score_niv y construye un reportes de performance con ellas. 
# Asume que tab_ref_alin contiene las columnas level_name, TM_min y TM_max
# En ... poner las columnas que se quieren seleccionar en la tabla de salida entre 
# score_niv, Score_Min, Score_Prom, Score_Max, Buenos, Malos, Tasa_Malos, Odds, 
# Total, P.Total, asc.total, asc.buenos, asc.malos, desc.total, desc.malos, 
# P.asc.Total, P.asc.Buenos, P.asc.Malos, Tasa_Malos_desc, asc.Odds, 
# Tasa_Malos_Rel, KS
# La hice como función auxiliar de tab_niv_2_gt
# Ej. uso: df.scores %>% gentTab_niv %>% gt
# Ej. uso: df.scores %>% tab_niv_2_gt(title='Todos') 
gentTab_niv <- function(df, tab_ref_alin = tab_niv, ...) {
  sel_vars <- rlang::enquos(...)
  group_vars <- df %>% group_vars()
  df <- df %>% mutate(Target=dplyr::coalesce(as.integer(as.character(Target)),0L)) 
  tab <- df %>% 
    group_by(score_niv, .add=T) %>% 
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score), 
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>% 
    arrange(desc(Score_Prom)) %>%     
    mutate(Total=Buenos+Malos, P.Total=Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos), 
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,           
           P.asc.Total=asc.total/sum(Total),
           P.asc.Buenos=asc.buenos/sum(Buenos), 
           P.asc.Malos=asc.malos/sum(Malos),
           Tasa_Malos_desc=desc.malos/desc.total,
           Tasa_Malos_asc=asc.malos/asc.total,
           asc.Odds=asc.buenos/asc.malos, 
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))), 
           KS=abs(P.asc.Buenos - P.asc.Malos), 
           S_Min=(1-Score_Min/1000), S_Prom=(1-Score_Prom/1000), S_Max=(1-Score_Max/1000))
  tab |> left_join(tab_ref_alin, by = c("score_niv" = "level_name")) |> 
    mutate(Alineado = TM_min <= Tasa_Malos & Tasa_Malos <= TM_max) -> tab
  KS <- tab %>% summarise(KS=max(KS), Alineado=all(Alineado), 
                          TM_min=min(TM_min), TM_max=max(TM_max))
  ult_fila <- df %>% 
    summarise(score_niv='Total', Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score), 
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>% 
    mutate(Total=Buenos+Malos, P.Total=1,
           asc.total=Total, asc.buenos=Buenos, asc.malos=Malos, 
           desc.total=0,
           desc.malos=0,           
           P.asc.Total=1,
           P.asc.Buenos=1, 
           P.asc.Malos=1,
           Tasa_Malos_desc=Tasa_Malos, 
           Tasa_Malos_asc=Tasa_Malos,
           asc.Odds=Odds, 
           Tasa_Malos_Rel=1, 
           S_Min=(1-Score_Min/1000), 
           S_Prom=(1-Score_Prom/1000), 
           S_Max=(1-Score_Max/1000)) %>% 
    inner_join(KS, by = group_vars)
  tab <- tab %>% bind_rows(ult_fila)
  if (!missing(...)) return(tab %>% select(all_of(group_vars),!!!sel_vars)) else return(tab)
}

gentTab_B <- function(df, par_times = 2000, par_quantiles = c(0.025, 0.5, 0.975), ...) {
  other_args <- rlang::enquos(...)
  group_vars <- df %>% group_vars()
  df %>% bootstraps(times = par_times) -> bt_rs
  df %>% 
    gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min, 
                Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) -> tab
  bt_rs %>% 
    transmute(boot_id=id, tab_niv=map(splits, ~ .x %>% (rsample::analysis) %>% 
      gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min, 
                  Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc))) %>% 
    unnest(cols = c(tab_niv)) -> tab_bt
  tab_bt %>% group_by(across(all_of(group_vars)), score_niv) %>% 
    summarise(B_Cuantil=par_quantiles, Tasa_Malos_B=quantile(Tasa_Malos, probs=par_quantiles), 
              KS_B=quantile(KS, probs=par_quantiles), 
              Alineado_B=mean(Alineado)) -> tab_bt_sum
  tab_bt_sum %>% 
    pivot_wider(id_cols = c(Segmento, score_niv, Alineado_B), 
                names_from = B_Cuantil, values_from = c(Tasa_Malos_B, KS_B)) -> tab_bt_sum
  tab %>% inner_join(tab_bt_sum, by=c(group_vars, "score_niv")) -> res
  return(res)
}  

tab_niv_2_gt <- function(df, title='ALL') {
  res <- df %>% 
    gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min, 
                Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) %>% 
    gt %>%
    tab_header(title = title) %>% 
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      Score_Min = gt::html("Score<br> m&iacute;nimo"),
      Score_Prom = gt::html("Score<br> medio"),
      Score_Max = gt::html("Score<br> m&aacute;ximo"),      
      S_Min = gt::html("Riesgo S+<br> m&iacute;nimo"),
      S_Prom = gt::html("Riesgo S+<br> medio"),
      Tasa_Malos = gt::html("% M"),      
      S_Max = gt::html("Riesgo S+<br> m&aacute;ximo"),      
      Alineado = gt::html("<b>Score<br> alineado?<b>"),      
      Total = gt::html("#"),
      Malos = gt::html("Malos"), 
      P.Total = gt::html("# %"),
      P.asc.Total = gt::html("# %+"),
      Tasa_Malos_asc = gt::html("% M+"),
      KS='KS') %>% 
    fmt_number(columns = c(Score_Min, Score_Prom, Score_Max), decimals = 0) %>% 
    fmt_percent(columns = c(S_Min, S_Prom, S_Max), decimals = 1) %>% 
    fmt_percent(columns = c(P.Total, P.asc.Total, KS), decimals = 0) %>% 
    fmt_percent(columns = c(Tasa_Malos, Tasa_Malos_asc), decimals = 1) %>% 
    cols_align(
      align = "center",
      columns = c(score_niv, Score_Prom)
    ) %>% 
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>% 
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=c(S_Min, Tasa_Malos, S_Max))
    ) %>% 
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Alineado), rows = (Alineado==F))
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Tasa_Malos), rows = (Alineado==F))
    ) %>%
    tab_options(
      table.font.size = px(10L)
    ) %>% 
    opt_table_lines(extent = 'all') %>% 
    opt_row_striping() 
  return(res)
}

# Asume que la tabla fue construida por gentTab_B()
tab_niv_bt_gt <- function(tab_bt, title='ALL') {
  res <- tab_bt %>% 
    select(all_of(group_vars(tab_bt)), 
           score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, 
           TM_min, Tasa_Malos_B_0.025, Tasa_Malos, Tasa_Malos_B_0.975, TM_max, 
           S_Min, Alineado, Alineado_B, KS_B_0.025, KS, KS_B_0.975, 
           Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) %>% 
    gt %>%
    tab_header(title = title) %>% 
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      Score_Min = gt::html("Score<br> m&iacute;nimo"),
      Score_Prom = gt::html("Score<br> medio"),
      Score_Max = gt::html("Score<br> m&aacute;ximo"),      
      S_Min = gt::html("Riesgo S+<br> m&iacute;nimo"),
      S_Prom = gt::html("Riesgo S+<br> medio"),
      TM_min = gt::html("Tasa de malos<br> m&iacute;nima<br> permitida"), 
      Tasa_Malos_B_0.025 = gt::html("2.5% quant<br><b>% M<b>"),      
      Tasa_Malos = gt::html("<b>% M<b>"),      
      Tasa_Malos_B_0.975 = gt::html("97.5% quant<br><b>% M<b>"),            
      TM_max = gt::html("Tasa de malos<br> m&aacute;xima<br> permitida"), 
      S_Max = gt::html("Riesgo S+<br> m&aacute;ximo"),      
      Alineado = gt::html("Score<br> alineado?"),      
      Alineado_B = gt::html("<b>% veces Alineado<b>"),      
      Total = gt::html("#"),
      Malos = gt::html("Malos"), 
      P.Total = gt::html("# %"),
      P.asc.Total = gt::html("# %+"),
      Tasa_Malos_asc = gt::html("% M+"),
      KS_B_0.025 = gt::html("2.5% quant<br><b>% KS<b>"),            
      KS=gt::html('<b>KS<b>'), 
      KS_B_0.975 = gt::html("97.5% quant<br><b>% KS<b>")) %>%                 
    fmt_number(columns = c(Score_Min, Score_Prom, Score_Max), decimals = 0) %>% 
    fmt_percent(columns = c(S_Min, S_Prom, S_Max, Alineado_B), decimals = 1) %>% 
    fmt_percent(columns = c(P.Total, P.asc.Total, KS, KS_B_0.025, KS_B_0.975), decimals = 0) %>% 
    fmt_percent(columns = c(Tasa_Malos, Tasa_Malos_asc, Tasa_Malos_B_0.025, 
                               Tasa_Malos_B_0.975, TM_min, TM_max), decimals = 1) %>% 
    cols_align(
      align = "center",
      columns = c(score_niv, Score_Prom)
    ) %>% 
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>% 
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=c(S_Min, Tasa_Malos, S_Max))
    ) %>% 
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=KS, rows = score_niv == "Total")
    ) %>% 
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Alineado), rows = (Alineado==F))
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Tasa_Malos), rows = (Alineado==F))
    ) %>%
    tab_options(
      table.font.size = px(10L)
    ) %>% 
    opt_table_lines(extent = 'all') %>% 
    opt_row_striping() 
  return(res)
}

# La sigiente es una función que dada una tabla con más de n_row_min filas 
# la formatea con gt() en n_main_cols tablas y las pone una al lado de la otra. 
# Sirve para que tablas largas no ocupen muchas líneas en un reporte. 
# Ejemplo de uso
# tab |> 
# pivot_longer(everything(), names_to = c('Variable', '.value'), names_pattern = "(.*)_(Q|Pct)") |> 
#   long_tab_2_wide_gt() |> 
#   tab_header(title = "Variables con tratamiento de nulos y % casos afectados") 
long_tab_2_wide_gt <- function(long_tab, n_main_cols = 3, 
                               col_labs = list("Variable"="Variable", "Q"="Q", "Pct"="%"), 
                               n_row_min=10) {
  group_gt <- function(.x, .y, cols_labs) {
    gt(.x) |> 
      cols_label(.list=col_labs) |> 
      # Ojo que por ahora formatea así todas las columnas numéricas!
      fmt_number(columns = where(is.numeric), decimals = 2, locale = 'es_AR') |> 
      as_raw_html() -> gt_tab 
    as.character(.y[[2]]) -> gt_nom
    names(gt_tab) <- gt_nom
    gt_tab
  }
  
  if (nrow(long_tab) < n_row_min) { 
    gt(long_tab) |> 
      cols_label(.list=col_labs) |> 
      # Ojo que por ahora formatea así todas las columnas excepto la primera!
      fmt_number(columns = where(is.numeric), decimals = 2, locale = 'es_AR')
  } 
  else long_tab |> 
    mutate(row_n = row_number(), 
           wide_col = row_n %/% (nrow(long_tab) %/% n_main_cols + 1) + 1) |> 
    group_by(wide_col) |> 
    mutate(row_min = cur_data() |> pull(row_n) |> min(), 
           row_max = cur_data() |> pull(row_n) |> max(), 
           main_col = paste0(row_min, " - ", row_max)) |> 
    group_by(wide_col, main_col) |> 
    select(-row_n, -row_min, -row_max) |> 
    group_map(~ group_gt(.x, .y, col_labs)) %>%  
    { set_names(., nm = . |> map_chr(names))} |> 
    map_dfc(identity) |> gt() |> 
    tab_style( style = "vertical-align:top", locations = cells_body(columns = everything())) |> 
    fmt_markdown(columns = everything()) 
}

resumen_fwd_gt <- function(logit.fwd.res=res.trad, tab_bins=tab.bins) {
  
  logit.fwd.res$det$mod.curr |> tidy() |> 
    select(term, estimate) |> 
    rename(Variable=term, Beta=estimate) |> 
    filter(Variable!="(Intercept)") |> 
    pull(Variable) -> vars_mod
  
  
  logit.fwd.res |> pluck("det", "vars.fwd.res") |> group_by(Paso) |> 
    slice_max(order_by = LRT, n = 1) -> tab_elegidas
  
  logit.fwd.res |> pluck("det", "vars.fwd.res") |> group_by(Paso) |> 
    arrange(desc(LRT)) |> 
    mutate(puesto=row_number()) |> 
    filter(puesto |> between(1,3)) |> 
    select(Paso, puesto, Variable) |> 
    pivot_wider(names_from = puesto, values_from = Variable, 
                names_prefix = "Variable") -> tab_compet
  
  tab_bins |> 
    map(~tibble(Variable=.$var_gen, IV=.$iv)) |> 
    reduce(bind_rows) |> 
    filter(Variable %in% (vars_mod)) -> tab_ivs
  
  tab_elegidas |> 
    inner_join(tab_ivs, by = "Variable") |> 
    inner_join(tab_compet, by = c("Paso", "Variable"="Variable1")) -> tab_fwd_res
  
  tab_fwd_res |> 
    select(Paso, Variable, LRT, `Pr(>Chi)`, IV, Variable2, Variable3) |> 
    ungroup() |> gt() |> 
    fmt_number(columns = c(LRT, IV), decimals = 2, locale = 'es_AR') |> 
    fmt_number(columns = `Pr(>Chi)`, decimals = 5, locale = 'es_AR') |> 
    tab_header(title = "Resumen Pasos") |> 
    cols_label(Variable2=gt::html("Variable<br>alternativa 1"), Variable3=gt::html("Variable<br>alternativa 2"))
  
}


# Excel Reports -----------------------------------------------------------

res_part_2_excel <- function(res_part) {
  options('openxlsx.numFmt' = '#,#0.00')
  Boldstyle <- createStyle(textDecoration = "bold")
  wb <- write.xlsx(x = res_part |> map(~ as_tibble(.x) |> rename(variable=skim_variable)),  
                   file = file_name, asTable = T, tableStyle = "TableStyleLight9", 
                   overwrite = T, creator = 'BeSmart', 
                   startRow = 5, startCol = 1, gridLines = F)
  setColWidths(wb = wb, sheet = 'character', cols = 1:20, widths = 'auto')
  setColWidths(wb = wb, sheet = 'numeric', cols = 1:20, widths = 'auto')
  addWorksheet(wb, sheetName = 'Gral', gridLines = F)
  activeSheet(wb) <- "Gral"
  addStyle(wb = wb, sheet = 'Gral',  style = Boldstyle, rows = 1:4, cols = 1, gridExpand = T)
  setColWidths(wb = wb, sheet = 'Gral', cols = 1:2, widths = 20)
  wb |> writeData(sheet = 'Gral', startRow = 1, startCol = 1, 
                  x = 'Descripción general de la tabla de observaciones')  
  wb |> writeData(sheet = 'Gral', startRow = 3, startCol = 1, 
                  x = 'En las hojas siguientes de detallan las distribuciones de las variables predictoras')
  wb |> writeData(x = paste('Variables no predictoras:              ', paste(cols_no_predictoras, collapse = ", ")), sheet = 'Gral', startRow = 5)  
  wb |> writeData(x = paste('Variables forzadas a predictoras:              ', paste(cols_forzadas_a_predictoras, collapse = ", ")), sheet = 'Gral', startRow = 6)  
  wb |> writeData(x = paste('Variables forzadas a categóricas: ', paste(cols_forzadas_a_cat, collapse = ", ")), sheet = 'Gral', startRow = 7)
  wb |> writeData(x = paste('Variables con nulos adicionales:  ', paste(cols_nulos_adic, collapse = ", ")), sheet = 'Gral', startRow = 8)  
  wb |> writeData(x = res |> summary(), sheet = 'Gral', startRow = 10)  
  worksheetOrder(wb) <- worksheetOrder(wb) |> last2first()
  return(wb)
}

# Chequeo alineación ------------------------------------------------------

# Esta función usa la salida de  bootstraps(apparent = T) en el argumento bt_rs
# Uso la opción apparent para obtener el dataset completo pero no sé si es rebuscado. 
bt_rs_2_tab_bt_sum <- function(bt_rs, par_quantiles = c(0.025, 0.5, 0.975), 
                               score_var = score, score_niv_var = score_niv, 
                               group_vars = "Part", tab_ref_alin, ...) {
  other_args <- rlang::enquos(...)
  corte_2_exprs(group_vars) -> group_vars_exprs
  bt_rs %>% 
    transmute(boot_id=id, tab_niv=
                map(splits, ~ .x %>% (rsample::analysis) %>% 
                      mutate(score={{score_var}}, score_niv={{score_niv_var}}) %>% 
                      group_by(!!!group_vars_exprs) %>% 
                      gentTab_niv(tab_ref_alin, score_niv, Score_Min, Score_Prom, Score_Max, 
                                  TM_min, S_Max, S_Prom, Tasa_Malos, S_Min, TM_max, 
                                  Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc))) %>% 
    unnest(cols = c(tab_niv)) -> tab_bt
  tab_bt %>% filter(boot_id=='Apparent') %>% group_by(!!!group_vars_exprs) -> tab
  tab_bt %>% filter(boot_id!='Apparent') %>% group_by(!!!group_vars_exprs, score_niv) %>% 
    summarise(B_Cuantil=par_quantiles, Tasa_Malos_B=quantile(Tasa_Malos, probs=par_quantiles), 
              KS_B=quantile(KS, probs=par_quantiles), 
              Alineado_B=mean(Alineado)) -> tab_bt_sum
  tab_bt_sum %>% 
    pivot_wider(id_cols = c(!!!group_vars_exprs, score_niv, Alineado_B), 
                names_from = B_Cuantil, values_from = c(Tasa_Malos_B, KS_B)) -> tab_bt_sum
  tab %>% inner_join(tab_bt_sum, by=dplyr::group_vars(tab_bt_sum)) -> res
  return(res)
}   

tab_ref_niv_alin_gt_print <- function(tab_ref, title = 'Modelo Obtenido', 
                                      subtitle = 'Límites para Tasas de malos') {
  tab_ref %>% 
    gt %>% 
    tab_header(title = title, subtitle = subtitle) %>% 
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      TM_min = gt::html("<b>% M<b><br> m&iacute;nimo"),      
      TM_max = gt::html("<b>% M<b><br> m&aacute;ximo")
    ) %>% 
    fmt_percent(columns = vars(TM_min, TM_max), decimals = 1) %>% 
    cols_align(
      align = "center",
      columns = vars(score_niv)
    ) %>% 
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>% 
    opt_table_lines(extent = 'all') %>% 
    opt_row_striping()     
}

# Función para obtener una tabla de referencia para comprobar la alienación de cada nivel de riesgo.
df_2_tab_ref_niv_alin <- function(df, score_niv_var=score_niv, score_var = score) {
  df %>% 
    mutate(Target=dplyr::coalesce(as.integer(as.character(Target)),0L)) %>% 
    mutate(score={{score_var}}, score_niv={{score_niv_var}}) %>% 
    group_by(score_niv, .add=T) %>% 
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score), 
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>% 
    arrange(desc(Score_Prom)) %>% 
    mutate(TM_min=lag(Tasa_Malos, default = 0), 
           TM_max=lead(Tasa_Malos, default = 1)) %>% 
    select(score_niv, TM_min, TM_max) 
}

# Función auxiliar para calcular alineación con respecto a una tabla de referencia
# Por ahora se asume una tabla basada en score_niv y donde se compara la tasa de Malos.
# tab_ref
nivel_alineado_Train <- function(tab, tab_ref) {
  tab %>% inner_join(tab_ref, by = 'score_niv') %>% 
    mutate(Alin_niv_Train=(TM_min <= Tasa_Malos & Tasa_Malos <= TM_max)) 
}

comp_2_scores_niv_gt <- function(bt_rs, group_vars) {
  bt_rs %>% bt_rs_2_tab_bt_sum(score_var = score, score_niv_var = score_niv, 
                               group_vars = !!enquo(group_vars), tab_ref_alin = tab_ref_niv_alin_obtenido) %>% 
    tab_niv_bt_gt(title='Modelo Obtenido') %>% print()
  bt_rs %>% bt_rs_2_tab_bt_sum(score_var = score_inic, score_niv_var = score_inic_niv, 
                               group_vars = !!enquo(group_vars), tab_ref_alin = tab_ref_niv_alin_inic) %>% 
    tab_niv_bt_gt(title='Modelo Inicial') %>% print()
}


# Performance -------------------------------------------------------------

# Resumen en una sola fila de una tabla de performance.
# Calcula KS, Gini, bad rate cortando los peores 10,20,30%.  
# Es aproximado porque usa tablas de peformance pero anda bien a 20 cuantiles. 
# Ej Uso
# tab.train <- df.scores |> filter(Part=="1_Train") |> select(-Part) |> 
#   genTab_f(genAsocCuantil(tabOrig), vars_adic = "P.asc.Buenos")
# tab.train |> tab_perf_resumen() |> pivot_longer(everything())
tab_perf_resumen <- function(tab_perf) {
  c("Cuantil", "P.asc.Total", "P.asc.Malos", "P.asc.Buenos", "Score_Min", "Score_Max", 
    "Total", "Malos", "P.Total", "Tasa_Malos_desc") %in% names(tab_perf) -> cond
  assertthat::assert_that(all(cond)) -> res
  tab_perf |> 
    arrange(desc(Cuantil)) |> 
    summarise(
      Cuantiles=n(), 
      br_corte_peor_10 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=10), 
      br_corte_peor_20 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=20), 
      br_corte_peor_30 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=30),
      malos_prim_cuant=first(Malos), malos_ult_cuant=last(Malos), peor_mejor_cuantil=malos_ult_cuant/malos_prim_cuant,
      KS=max(KS), auc = tab_perf_2_auc_cols(Cuantil, P.asc.Malos, P.asc.Buenos), 
      # Ojo! Para evitar conflictos por reusar nombres, lo hago aquí. Después de los cálculos que NO usan la información resumida. 
      Score_Min=min(Score_Min), Score_Max=max(Score_Max),  
      Total=sum(Total), `P.Total`=sum(`P.Total`), `P.asc.Total`=first(`P.asc.Total`), 
      Malos=sum(Malos), `P.asc.Malos`=first(`P.asc.Malos`), Tasa_Malos_desc=last(Tasa_Malos_desc), 
      # Aqui ya uso los resúmenes.
      Buenos=Total - Malos, Odds=(Total-Malos)/Malos, Tasa_Malos_Rel=100, Tasa_Malos=100*Malos/Total, 
      Gini = 2*auc-1, Gini = 100*Gini, auc = 100*auc) |> 
    relocate(c("Total", "Malos", "Tasa_Malos", "Odds", "br_corte_peor_10", "br_corte_peor_20", "br_corte_peor_30", 
               "peor_mejor_cuantil", "KS", "auc", "Gini"))
}

# Resumen x grupos. 
# Ej uso df.scores |> perf_resumen_x_grupos(tabOrig, Part) -> res
perf_resumen_x_grupos <- function(scores, tab_cortes, var_grupo, sel_cols) {
  if (missing(sel_cols)) {
    c("Cuantiles", "Total", "Malos", "Tasa_Malos", "Odds", 
      "br_corte_peor_10", "br_corte_peor_20", "br_corte_peor_30", "peor_mejor_cuantil", 
      "KS", "auc", "Gini") -> sel_cols
  }
  scores |> group_by({{var_grupo}}) |> 
    group_map(~ bind_cols(.y, genTab_f(.x, genAsocCuantil(tab_cortes), vars_adic = c("P.asc.Buenos")) |> 
                            tab_perf_resumen() |> 
                            select(all_of(sel_cols)))) |> 
    bind_rows() |> 
    pivot_longer(cols = !{{var_grupo}}) |> 
    pivot_wider(names_from = {{var_grupo}}, values_from = value) |> 
    rename(estad_fmt=name)
}

# Publica con gt la tabla de resúmenes de performance por grupo
# Ej uso:
# df.scores |> perf_resumen_x_grupos(tabOrig, Part) |> res_x_grupos_2_gt("Resumen de Performance x Partición") 
res_x_grupos_2_gt <- function(res_x_grupos, title) {
  res_x_grupos |> gt() |> 
    text_transform(locations = cells_body(columns = c(estad_fmt)), 
                   fn = function(x) {case_when(
                     x == "br_corte_peor_10" ~ "Tasa de Malos<br>cortando el 10%",
                     x == "br_corte_peor_20" ~ "Tasa de Malos<br>cortando el 20%",
                     x == "br_corte_peor_30" ~ "Tasa de Malos<br>cortando el 30%",      
                     x == "Tasa_Malos" ~ "Tasa de Malos", 
                     x == "peor_mejor_cuantil" ~ "<small># Malos mejor cuantil /<br> # Malos peor</small>", 
                     x == "auc" ~ "AUC", 
                     x == "Gini" ~ "&Iacute;ndice Gini",
                     T ~ x) |> map(~ gt::html(.x))}) |> 
    fmt_number(columns = !estad_fmt, decimals = 1,
               drop_trailing_zeros = T, drop_trailing_dec_mark = T, locale = "es_AR") |>
    cols_label(estad_fmt = "Estad&iacute;stico") |> 
    opt_row_striping() |> 
    tab_header(title = title)  
}

# % de Malos al dejar Xporc afuera.  Es aproximado xq usa tablas de performance
# Ej uso. 
# tab.train |> 
#   summarise(br_corte_peor_10 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=10), 
#             br_corte_peor_20 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=20), 
#             br_corte_peor_30 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=30))
br_corte_peor_Xp <- function(P.asc.Total, Tasa_Malos_desc, Xporc=30) {
  tibble(P.asc.Total, Tasa_Malos_desc) -> tab
  tab |> arrange(desc(P.asc.Total)) |> 
    mutate(pat_sig = lead(P.asc.Total, default = 0)) |> 
    filter((pat_sig |> round()) >= Xporc) |> 
    summarise(malos_corte_peor_Xp = last(Tasa_Malos_desc)) -> res
  res |> pull(malos_corte_peor_Xp)
}


# de https://www.r-bloggers.com/2016/11/calculating-auc-the-area-under-a-roc-curve/
# Ej uso: tab.train |> tab_perf_2_auc()
# Con una tabla de 20 cuantiles es bastante preciso.
tab_perf_2_auc <- function(tab_perf) {
  assertthat::assert_that(tab_perf |> is_tibble(), msg = "Para calcular el auc se necesita una tabla del tipo tibble!")
  assertthat::assert_that(c("Cuantil", "P.asc.Malos", "P.asc.Buenos") %in% (tab_perf |> names()) |> all(), 
                          msg = "La tabla de performance para calcular el auc necesita las columnas Cuantil, P.asc.Malos, P.asc.Buenos")
  tab_perf |> arrange(Cuantil) |> 
    select(P.asc.Malos, P.asc.Buenos) |> 
    rename(TPR=P.asc.Malos, FPR=P.asc.Buenos) -> tab
  bind_rows(tibble(TPR=0, FPR=0), tab) |> 
    mutate(TPR=TPR/100, FPR=FPR/100) |> 
    mutate(dFPR=lead(FPR, default = 1) - FPR, dTPR=lead(TPR, default = 1) - TPR) |> 
    summarise(auc = sum(TPR * dFPR) + sum(dTPR * dFPR)/2) -> res
  res |> pull(auc)
}

# de https://www.r-bloggers.com/2016/11/calculating-auc-the-area-under-a-roc-curve/
# Ej Uso: tab.train |> summarise(auc = tab_perf_2_auc_cols(Cuantil, P.asc.Malos, P.asc.Buenos))
# Con una tabla de 20 cuantiles es bastante preciso.
tab_perf_2_auc_cols <- function(Cuantil, TPR, FPR) {
  assertthat::assert_that(length(Cuantil) == length(TPR), length(Cuantil) == length(FPR)) 
  tibble(Cuantil, TPR, FPR) -> tab
  tab |> arrange(Cuantil) |> select(TPR, FPR) -> tab
  bind_rows(tibble(TPR=0, FPR=0), tab) |> 
    mutate(TPR=TPR/100, FPR=FPR/100) |> 
    mutate(dFPR=lead(FPR, default = 1) - FPR, dTPR=lead(TPR, default = 1) - TPR) |> 
    summarise(auc = sum(TPR * dFPR) + sum(dTPR * dFPR)/2) -> res
  res |> pull(auc)
}


plotROC_Pred1vs2y3 <- function(Pred1, Pred2, Pred3, per, main="ROC") {
  # TO DO:  fix hardcoded legend.names
  legend.colors <- array(data = NA, 4)
  legend.lty <- array(data = NA, dim = 4)
  legend.names <- array(data = NA, dim = 4)
  plot(c(0, 1), c(0, 1), type = "l", lty = 4, col = "black", 
       xlab = "FPR (1-F0)", ylab = "TPR (1-F1)", main = main)
  legend.lty[1] <- 4
  legend.colors[1] <- "black"
  legend.names[1] <- "trivial"
  # Pred1 results
  pred <- Pred1 %>% filter(Periodo == per)
  results <- HMeasure(pred$truth,pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l", 
        lty = 1, col = "blue")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3, 
        col = "blue")
  legend.lty[2] <- 1
  legend.colors[2] <- "blue"
  legend.names[2] <- "LR Reaj"
  # Pred2 results
  pred <- Pred2 %>% filter(Periodo == per)
  results <- HMeasure(pred$truth,pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l", 
        lty = 1, col = "red")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3, 
        col = "red")
  legend.lty[3] <- 1
  legend.colors[3] <- "red"
  legend.names[3] <- "RF Reaj"
  
  # Hardcoded! LR Orig per=201408
  pred <- Pred3 %>% filter(Periodo == per)
  results <- HMeasure(pred$truth,pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l", 
        lty = 1, col = "green")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3, 
        col = "green")
  legend.lty[4] <- 1
  legend.colors[4] <- "green"
  legend.names[4] <- "LR Orig"
  
  legend("bottomright", legend = legend.names, lty = legend.lty, 
         col = legend.colors)
}


tab_KS <- function(df, bins=20, var_target="Bad") {
  df <- df |> mutate(target=df[[var_target]])
  tabOrig <- df  |> genTab_ntil(bins)
  tab <- df |> genTab_f(genAsocCuantil(tabOrig))
  return(tab)
}

repKS <- function(tab, caption, totales=F) {
  if (totales) 
    tab <- tab |> mutate(Cuantil=as.character(Cuantil)) |>  
      bind_rows(tab |> summarise(Cuantil="Total", Score_Min=min(Score_Min), Score_Max=max(Score_Max),  
                                 Total=sum(Total), `P.Total`=sum(`P.Total`), `P.asc.Total`=max(`P.asc.Total`), 
                                 Malos=sum(Malos), `P.asc.Malos`=max(`P.asc.Malos`), Tasa_Malos=100*Malos/Total, 
                                 Tasa_Malos_Rel=100, Tasa_Malos_desc=Tasa_Malos, KS=max(KS), Odds=(Total-Malos)/Malos))
  tab |> gt() |> 
    tab_style(locations = cells_body(rows = Cuantil == "Total"), 
              style = cell_borders(sides = "top", weight = px(2)) ) |> 
    tab_header(title = caption) |> 
    opt_row_striping() |> 
    tab_style(locations = cells_body(columns = KS, rows = KS == max(KS)), 
              style = cell_text(weight = "bold")) |> 
    tab_style(locations = cells_body(columns = Tasa_Malos_desc, 
                                     rows = Tasa_Malos_desc==max(Tasa_Malos_desc)), 
              style = cell_text(weight = "bold")) |> 
    tab_style(locations = cells_body(columns = Tasa_Malos, 
                                     rows = Tasa_Malos==min(Tasa_Malos) | Tasa_Malos==max(Tasa_Malos)), 
              style = cell_text(weight = "bold")) |> 
    fmt_number(columns = c(P.Total, P.asc.Total, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, KS, Odds), 
               decimals = 1, locale = "es_AR") |> 
    fmt_number(decimals = 0, columns = c(Score_Min, Score_Max, Total, Malos), locale = "es_AR") |> 
    cols_label(Cuantil='Cuantil', Score_Min='Score Min', Score_Max='Score Max', Total='Total', 
               P.Total='% Total', P.asc.Total='% Asc Total', Malos='Malos', P.asc.Malos='% Asc Malos', 
               Tasa_Malos='Tasa Malos', Tasa_Malos_Rel='Tasa Malos Rel', Tasa_Malos_desc='Tasa Malos desc',
               KS='KS', Odds='Odds')
}  



# Reportes de resúmenes por skim -------------------

top_percents <- function(vec, n = 10) {
  sorted_count(vec)/length(vec) -> vec 
  min(n, length(vec)) -> n
  vec[1:n] %>% {100*round(., digits = 4)} -> vec 
  paste(names(vec), ': ', vec, '%', sep = '', collapse = ' | ')
}

my_skim <- skimr::skim_with(
  base = skimr::sfl(
    q = length,
    q_nulos = skimr::n_missing,
    porc_compl = skimr::complete_rate
  ),
  character = skimr::sfl(
    q_vacios = ~ skimr::n_empty(.x),
    q_blancos = ~ skimr::n_whitespace(.x),
    q_unicos = ~ skimr::n_unique(.x),
    long_min = ~ skimr::min_char(.x), 
    long_max = ~ skimr::max_char(.x), 
    mostFreq20 = ~ skimr::top_percents(.x, n = 20)
  ),
  numeric = skimr::sfl(
    avg = ~ mean(.x, na.rm = T), 
    sd = ~ sd(.x, na.rm = T),
    min = ~ min(.x, na.rm = T),
    p01 = ~ quantile(.x, probs = .01, na.rm = T),
    p05 = ~ quantile(.x, probs = .05, na.rm = T),    
    p25 = ~ quantile(.x, probs = .25, na.rm = T),    
    mediana = ~ quantile(.x, probs = .50, na.rm = T),        
    p75 = ~ quantile(.x, probs = .75, na.rm = T),        
    p95 = ~ quantile(.x, probs = .95, na.rm = T),        
    p99 = ~ quantile(., probs = .99, na.rm = T),
    max = ~ max(.x, na.rm = T),
    hist = ~ skimr::inline_hist(.x, n_bins = 8) # Parametrizar?
  ), 
  append = F
) 

