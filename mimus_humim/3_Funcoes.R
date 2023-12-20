

# Uso geral ---------------------------------------------------------------

estabelecer_cenario_base <- function(df){
  df <- subset(df, df$cenario == "A")
}


# Modelo AIS - Poluicao do ar ---------------------------------------------

# funcao para adicionar a taxa de exposicao (exposure rate) ao PM2.5 por modo ou atividade
adicionar_taxa_expo <- function(df){
  df$taxa_expo_ape <- 1.6
  df$taxa_expo_bici <- 2.0
  df$taxa_expo_TI <- 2.5
  df$taxa_expo_TP <- 1.9
  df$taxa_expo_ape_TP <- 1.6
  df$taxa_expo_sono <- 1.0
  df$taxa_expo_outros <- 1.0
  return(df)
}

calcular_PM2.5_inalado <- function(df) {
  var_list <- c("ape", "bici", "TI", "TP", "ape_TP", "sono", "outros") # modos de transporte/atividades
  
  # calculo de risco relativo para o cenario
  for (var in var_list) {
    new_var <- paste0("PM2.5_inalado_", var)
    df <- df %>%
      mutate(!!sym(new_var) := (!!sym(paste0("ar_inalado_", var)) * df$PM2.5_conc_geral * (!!sym(paste0("taxa_expo_", var))))  )
  }
  return(df)
}

# calcular a variacao na concentracao do poluente do cenario (df) versus uma referencia/cenario base (df1)
calcular_variacao_conc <- function(df,df1){
  indices_df6 <- match(df$faixa_etaria,df1$faixa_etaria)
  
  df <- df %>%
    mutate(PM2.5_conc_cen_vari = df$PM2.5_conc_cen-df1$PM2.5_conc_cen[indices_df6])
  
}

# parametrizar a funcao exposicao resposta
funcao_exposicaoresposta_pm2.5 <- function(df){
  df$RR_ref_med <- 1.08 # risco relativo (centro)
  df$RR_ref_inf <- 1.06 # risco relativo (limite inferior)
  df$RR_ref_sup <- 1.09 # risco relativo (limite superior)
  df$expo_ref <- 10 # exposicao referencia
  return(df)
}

# calcular o risco relativo
riscorelativo_pm2.5 <- function(df){
  df$RR_cen_med <- df$RR_ref_med**(df$PM2.5_conc_cen_vari/df$expo_ref)
  df$RR_cen_inf <- df$RR_ref_inf**(df$PM2.5_conc_cen_vari/df$expo_ref)
  df$RR_cen_sup <- df$RR_ref_sup**(df$PM2.5_conc_cen_vari/df$expo_ref)
  return(df)
}

# calcular a fracao atribuivel populacional
fracaoatribuivel_pm2.5 <- function(df){
  df$FAP_cen_med <- (df$RR_cen_med-1)/df$RR_cen_med
  df$FAP_cen_inf <- (df$RR_cen_inf-1)/df$RR_cen_inf
  df$FAP_cen_sup <- (df$RR_cen_sup-1)/df$RR_cen_sup
  return(df)
}

# calcular os obitos atribuiveis ao poluente
obitos_pm2.5 <- function(df){
  indices_df7 <- match(df$faixa_etaria,df_saude$faixa_etaria)
  
  df <- df %>%
    mutate(obitos_med = df$FAP_cen_med * df_saude$obitos_2019_causanat[indices_df7])
  
  df <- df %>%
    mutate(obitos_inf = df$FAP_cen_inf * df_saude$obitos_2019_causanat[indices_df7])
  
  df <- df %>%
    mutate(obitos_sup = df$FAP_cen_sup * df_saude$obitos_2019_causanat[indices_df7])
  
  return(df)
}



# Modelo AIS - Atividade fisica --------------------------

# funcao para calcular o risco relativo a partir de todas as indicacoes de Woodcock et al (2011)
calcular_cen_RR_af <- function(df) {
  var_list <- c("insuf_ativo", "ativo", "muito_ativo") # estabelece os niveis de atividade fisica
  
  RR_med <- c(0.86, 0.84, 0.83, 0.81, 0.80, 0.78, 0.77, 0.77, 0.76, 0.75, 0.74, 0.72) # funcao: risco relativo
  RR_inf <- c(0.83, 0.81, 0.79, 0.76, 0.76, 0.74, 0.73, 0.72, 0.71, 0.7, 0.68, 0.66) # funcao: risco relativo (limite inferior)
  RR_sup <- c(0.89, 0.88, 0.87, 0.85, 0.84, 0.83, 0.82, 0.82, 0.81, 0.8, 0.79, 0.78) # funcao: risco relativo (limite superior)
  METh_cen <- c(2.5, 4.5, 6.25, 11.25, 12.5, 17.5, 22.5, 25, 31.5, 35, 45, 63) # funcao: quantidade de exposicao
  expoente <- 0.25
  
  # calculo de risco relativo para o cenario
  for (var in var_list) {
    new_var <- paste0("cen_RR_med_", var)
    df <- df %>%
      mutate(!!sym(new_var) := case_when(
        !!sym(paste0("METh_total_", var)) <= METh_cen[1]      ~RR_med[1]**((!!sym(paste0("METh_total_", var))/METh_cen[1])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[1] && !!sym(paste0("METh_total_", var)) <= METh_cen[2]      ~RR_med[2]**((!!sym(paste0("METh_total_", var))/METh_cen[2])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[2] && !!sym(paste0("METh_total_", var)) <= METh_cen[3]      ~RR_med[3]**((!!sym(paste0("METh_total_", var))/METh_cen[3])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[3] && !!sym(paste0("METh_total_", var)) <= METh_cen[4]      ~RR_med[4]**((!!sym(paste0("METh_total_", var))/METh_cen[4])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[4] && !!sym(paste0("METh_total_", var)) <= METh_cen[5]      ~RR_med[5]**((!!sym(paste0("METh_total_", var))/METh_cen[5])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[5] && !!sym(paste0("METh_total_", var)) <= METh_cen[6]      ~RR_med[6]**((!!sym(paste0("METh_total_", var))/METh_cen[6])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[6] && !!sym(paste0("METh_total_", var)) <= METh_cen[7]      ~RR_med[7]**((!!sym(paste0("METh_total_", var))/METh_cen[7])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[7] && !!sym(paste0("METh_total_", var)) <= METh_cen[8]      ~RR_med[8]**((!!sym(paste0("METh_total_", var))/METh_cen[8] )**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[8]  && !!sym(paste0("METh_total_", var)) <= METh_cen[9]       ~RR_med[9]**((!!sym(paste0("METh_total_", var))/METh_cen[9])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[9] && !!sym(paste0("METh_total_", var)) <= METh_cen[10]      ~RR_med[10]**((!!sym(paste0("METh_total_", var))/METh_cen[10])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[10] && !!sym(paste0("METh_total_", var)) <= METh_cen[11]      ~RR_med[11]**((!!sym(paste0("METh_total_", var))/METh_cen[11])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[11]      ~RR_med[12]**((!!sym(paste0("METh_total_", var))/METh_cen[12])**expoente)
      ))
  }
  
  # calculo de risco relativo (limite inferior) para o cenario
  for (var in var_list) {
    new_var1 <- paste0("cen_RR_inf_", var)
    df <- df %>%
      mutate(!!sym(new_var1) := case_when(
        !!sym(paste0("METh_total_", var)) <= METh_cen[1]      ~RR_inf[1]**((!!sym(paste0("METh_total_", var))/METh_cen[1])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[1] && !!sym(paste0("METh_total_", var)) <= METh_cen[2]      ~RR_inf[2]**((!!sym(paste0("METh_total_", var))/METh_cen[2])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[2] && !!sym(paste0("METh_total_", var)) <= METh_cen[3]      ~RR_inf[3]**((!!sym(paste0("METh_total_", var))/METh_cen[3])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[3] && !!sym(paste0("METh_total_", var)) <= METh_cen[4]      ~RR_inf[4]**((!!sym(paste0("METh_total_", var))/METh_cen[4])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[4] && !!sym(paste0("METh_total_", var)) <= METh_cen[5]      ~RR_inf[5]**((!!sym(paste0("METh_total_", var))/METh_cen[5])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[5] && !!sym(paste0("METh_total_", var)) <= METh_cen[6]      ~RR_inf[6]**((!!sym(paste0("METh_total_", var))/METh_cen[6])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[6] && !!sym(paste0("METh_total_", var)) <= METh_cen[7]      ~RR_inf[7]**((!!sym(paste0("METh_total_", var))/METh_cen[7])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[7] && !!sym(paste0("METh_total_", var)) <= METh_cen[8]      ~RR_inf[8]**((!!sym(paste0("METh_total_", var))/METh_cen[8] )**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[8]  && !!sym(paste0("METh_total_", var)) <= METh_cen[9]       ~RR_inf[9]**((!!sym(paste0("METh_total_", var))/METh_cen[9])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[9] && !!sym(paste0("METh_total_", var)) <= METh_cen[10]      ~RR_inf[10]**((!!sym(paste0("METh_total_", var))/METh_cen[10])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[10] && !!sym(paste0("METh_total_", var)) <= METh_cen[11]      ~RR_inf[11]**((!!sym(paste0("METh_total_", var))/METh_cen[11])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[11]      ~RR_inf[12]**((!!sym(paste0("METh_total_", var))/METh_cen[12])**expoente)
      ))
  }
  
  # calculo de risco relativo (limite superior) para o cenario
  for (var in var_list) {
    new_var2 <- paste0("cen_RR_sup_", var)
    df <- df %>%
      mutate(!!sym(new_var2) := case_when(
        !!sym(paste0("METh_total_", var)) <= METh_cen[1]      ~RR_sup[1]**((!!sym(paste0("METh_total_", var))/METh_cen[1])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[1] && !!sym(paste0("METh_total_", var)) <= METh_cen[2]      ~RR_sup[2]**((!!sym(paste0("METh_total_", var))/METh_cen[2])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[2] && !!sym(paste0("METh_total_", var)) <= METh_cen[3]      ~RR_sup[3]**((!!sym(paste0("METh_total_", var))/METh_cen[3])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[3] && !!sym(paste0("METh_total_", var)) <= METh_cen[4]      ~RR_sup[4]**((!!sym(paste0("METh_total_", var))/METh_cen[4])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[4] && !!sym(paste0("METh_total_", var)) <= METh_cen[5]      ~RR_sup[5]**((!!sym(paste0("METh_total_", var))/METh_cen[5])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[5] && !!sym(paste0("METh_total_", var)) <= METh_cen[6]      ~RR_sup[6]**((!!sym(paste0("METh_total_", var))/METh_cen[6])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[6] && !!sym(paste0("METh_total_", var)) <= METh_cen[7]      ~RR_sup[7]**((!!sym(paste0("METh_total_", var))/METh_cen[7])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[7] && !!sym(paste0("METh_total_", var)) <= METh_cen[8]      ~RR_sup[8]**((!!sym(paste0("METh_total_", var))/METh_cen[8] )**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[8]  && !!sym(paste0("METh_total_", var)) <= METh_cen[9]       ~RR_sup[9]**((!!sym(paste0("METh_total_", var))/METh_cen[9])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[9] && !!sym(paste0("METh_total_", var)) <= METh_cen[10]      ~RR_sup[10]**((!!sym(paste0("METh_total_", var))/METh_cen[10])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[10] && !!sym(paste0("METh_total_", var)) <= METh_cen[11]      ~RR_sup[11]**((!!sym(paste0("METh_total_", var))/METh_cen[11])**expoente),
        !!sym(paste0("METh_total_", var)) > METh_cen[11]      ~RR_sup[12]**((!!sym(paste0("METh_total_", var))/METh_cen[12])**expoente)
      ))
  }
  
  return(df)
}


# funcao para calcular a fracao atribuivel populacional por cenario
calcular_cen_FAP_af <- function(df) {
  var_list <- c("insuf_ativo", "ativo", "muito_ativo") # estabelece os niveis de atividade fisica
  
  # calculo de risco relativo para o cenario
  for (var in var_list) {
    new_var <- paste0("FAP_med_", var)
    df <- df %>%
      mutate(!!sym(new_var) := (!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_med_", var)) - 1)) / ((!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_med_", var)) - 1)) + 1)))) 
  }
  
  
  for (var in var_list) {
    new_var <- paste0("FAP_inf_", var)
    df <- df %>%
      mutate(!!sym(new_var) := (!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_inf_", var)) - 1)) / ((!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_inf_", var)) - 1)) + 1)))) 
  }
  
  
  for (var in var_list) {
    new_var <- paste0("FAP_sup_", var)
    df <- df %>%
      mutate(!!sym(new_var) := (!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_sup_", var)) - 1)) / ((!!sym(paste0("prev_", var)) * ((!!sym(paste0("cen_RR_sup_", var)) - 1)) + 1)))) 
  }  
  
  return(df)
  
}

# ajustar os valores do intervalo de confianca para que o menor valor seja o limite inferior e o maior valor o limite superior
ajuste_IC95 <- function(df){
  df <- df %>%
    mutate(FAP_inf_vari_1 = case_when(
      df$FAP_inf_vari <= df$FAP_sup_vari      ~df$FAP_inf_vari,
      df$FAP_inf_vari > df$FAP_sup_vari      ~df$FAP_sup_vari,
    ))
  
  df <- df %>%
    mutate(FAP_sup_vari_1 = case_when(
      df$FAP_inf_vari <= df$FAP_sup_vari      ~df$FAP_sup_vari,
      df$FAP_inf_vari > df$FAP_sup_vari      ~df$FAP_inf_vari,
    ))
  
  df$FAP_inf_vari<-df$FAP_inf_vari_1 # atribui o valor da coluna criada
  df$FAP_sup_vari<-df$FAP_sup_vari_1 # atribui o valor da coluna criada
  
  df$FAP_inf_vari_1 <-NULL # exclui o valor da coluna criada
  df$FAP_sup_vari_1 <-NULL # exclui o valor da coluna criada
  return(df)
}



# Resultados --------------------------------------------------------------

obitos_pm2.5_resultados <- function(df,df1){
  df1$obitos_PM2.5_med <- df$obitos_med
  df1$obitos_PM2.5_inf <- df$obitos_inf
  df1$obitos_PM2.5_sup <- df$obitos_sup
  return(df1)
  
}

ajuste_IC95_pm2.5 <- function(df){
  df <- df %>%
    mutate(obitos_PM2.5_inf_1 = case_when(
      df$obitos_PM2.5_inf <= df$obitos_PM2.5_sup      ~df$obitos_PM2.5_inf,
      df$obitos_PM2.5_inf > df$obitos_PM2.5_sup      ~df$obitos_PM2.5_sup,
    ))
  
  df <- df %>%
    mutate(obitos_PM2.5_sup_1 = case_when(
      df$obitos_PM2.5_inf <= df$obitos_PM2.5_sup      ~df$obitos_PM2.5_sup,
      df$obitos_PM2.5_inf > df$obitos_PM2.5_sup      ~df$obitos_PM2.5_inf,
    ))
  
  df$obitos_PM2.5_inf<-df$obitos_PM2.5_inf_1 # atribui o valor da coluna criada
  df$obitos_PM2.5_sup<-df$obitos_PM2.5_sup_1 # atribui o valor da coluna criada
  
  df$obitos_PM2.5_inf_1 <-NULL # exclui o valor da coluna criada
  df$obitos_PM2.5_sup_1 <-NULL # exclui o valor da coluna criada
  return(df)
}


soma_obitos <- function(df){
  df$obitos_total_med <- df$obitos_af_med + df$obitos_PM2.5_med + df$obitos_sinistros
  df$obitos_total_inf <- df$obitos_af_inf + df$obitos_PM2.5_inf + df$obitos_sinistros
  df$obitos_total_sup <- df$obitos_af_sup + df$obitos_PM2.5_sup + df$obitos_sinistros
  return(df)
}


obitos_cenario <- function(df,df1){
  df <- df1 %>%
    group_by(cenario) %>%
    summarise(
      obitos_sinistros = sum(obitos_sinistros),
      obitos_af_med = sum(obitos_af_med),
      obitos_af_inf = sum(obitos_af_inf),
      obitos_af_sup = sum(obitos_af_sup),
      obitos_PM2.5_med = sum(obitos_PM2.5_med),
      obitos_PM2.5_inf = sum(obitos_PM2.5_inf),
      obitos_PM2.5_sup = sum(obitos_PM2.5_sup),
      obitos_total_med = sum(obitos_total_med),
      obitos_total_inf = sum(obitos_total_inf),
      obitos_total_sup = sum(obitos_total_sup)
    )
  
  df$faixa_etaria <- "Todas"
  df <- df %>%
    select(faixa_etaria, everything())
  return(df)
}


impacto_obitostotais <- function(df){
  # Vincular o data frame novo com o data frame de dados de saude
  indices_df15 <- match(df$faixa_etaria, df_saude$faixa_etaria)
  
  df <- df %>%
    mutate(obitos_sinistros_pct = round(df$obitos_sinistros/df_saude$obitos_2019_total[indices_df15],4))
  
  
  df <- df %>%
    mutate(obitos_af_med_pct = round(df$obitos_af_med/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_af_inf_pct = round(df$obitos_af_inf/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_af_sup_pct = round(df$obitos_af_sup/df_saude$obitos_2019_total[indices_df15],4))
  
  
  df <- df %>%
    mutate(obitos_PM2.5_med_pct = round(df$obitos_PM2.5_med/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_PM2.5_inf_pct = round(df$obitos_PM2.5_inf/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_PM2.5_sup_pct = round(df$obitos_PM2.5_sup/df_saude$obitos_2019_total[indices_df15],4))
  
  
  df <- df %>%
    mutate(obitos_total_med_pct = round(df$obitos_total_med/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_total_inf_pct = round(df$obitos_total_inf/df_saude$obitos_2019_total[indices_df15],4))
  df <- df %>%
    mutate(obitos_total_sup_pct = round(df$obitos_total_sup/df_saude$obitos_2019_total[indices_df15],4))
  
}




