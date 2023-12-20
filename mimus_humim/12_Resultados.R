library(dplyr)

# Carregar o csv basico (faixa etaria e cenarios)
caminho_completo <- here("csv", "data_frame_basico_faixaetariaxcenario.csv")
df_resultados <- read.csv2(caminho_completo)


# OBITOS ------------------------------------------------------------------


# obitos absolutos (unidade: obitos em 2019)

# sinistros de transito
df_resultados$obitos_sinistros <- df_sinistrostransito$obitos_sinistros_2019_vari

# atividade fisica
df_resultados$obitos_af_med <- df_atividadefisica$obitos_med
df_resultados$obitos_af_inf <- df_atividadefisica$obitos_inf
df_resultados$obitos_af_sup <- df_atividadefisica$obitos_sup

#poluicao do ar

df_resultados_N0 <- df_resultados
df_resultados_N1 <- df_resultados
df_resultados_N2 <- df_resultados
df_resultados_N3 <- df_resultados
df_resultados_N4 <- df_resultados


df_resultados_N0 <- obitos_pm2.5_resultados(df_PM2.5_N0,df_resultados_N0)
df_resultados_N1 <- obitos_pm2.5_resultados(df_PM2.5_N1,df_resultados_N1)
df_resultados_N2 <- obitos_pm2.5_resultados(df_PM2.5_N2,df_resultados_N2)
df_resultados_N3 <- obitos_pm2.5_resultados(df_PM2.5_N3,df_resultados_N3)
df_resultados_N4 <- obitos_pm2.5_resultados(df_PM2.5_N4,df_resultados_N4)

df_resultados_N0_1 <- obitos_pm2.5_resultados(df_PM2.5_N0_1,df_resultados_N0)
df_resultados_N1_1 <- obitos_pm2.5_resultados(df_PM2.5_N1_1,df_resultados_N1)
df_resultados_N2_1 <- obitos_pm2.5_resultados(df_PM2.5_N2_1,df_resultados_N2)
df_resultados_N3_1 <- obitos_pm2.5_resultados(df_PM2.5_N3_1,df_resultados_N3)
df_resultados_N4_1 <- obitos_pm2.5_resultados(df_PM2.5_N4_1,df_resultados_N4)


df_resultados_N0 <- ajuste_IC95_pm2.5(df_resultados_N0)
df_resultados_N1 <- ajuste_IC95_pm2.5(df_resultados_N1)
df_resultados_N2 <- ajuste_IC95_pm2.5(df_resultados_N2)
df_resultados_N3 <- ajuste_IC95_pm2.5(df_resultados_N3)
df_resultados_N4 <- ajuste_IC95_pm2.5(df_resultados_N4)

df_resultados_N0_1 <- ajuste_IC95_pm2.5(df_resultados_N0_1)
df_resultados_N1_1 <- ajuste_IC95_pm2.5(df_resultados_N1_1)
df_resultados_N2_1 <- ajuste_IC95_pm2.5(df_resultados_N2_1)
df_resultados_N3_1 <- ajuste_IC95_pm2.5(df_resultados_N3_1)
df_resultados_N4_1 <- ajuste_IC95_pm2.5(df_resultados_N4_1)




df_resultados_N0 <- soma_obitos(df_resultados_N0)
df_resultados_N1 <- soma_obitos(df_resultados_N1)
df_resultados_N2 <- soma_obitos(df_resultados_N2)
df_resultados_N3 <- soma_obitos(df_resultados_N3)
df_resultados_N4 <- soma_obitos(df_resultados_N4)

df_resultados_N0_1 <- soma_obitos(df_resultados_N0_1)
df_resultados_N1_1 <- soma_obitos(df_resultados_N1_1)
df_resultados_N2_1 <- soma_obitos(df_resultados_N2_1)
df_resultados_N3_1 <- soma_obitos(df_resultados_N3_1)
df_resultados_N4_1 <- soma_obitos(df_resultados_N4_1)



# impacto nos obitos totais

# fazer o data frame dos cenarios (aqui "faixa_etaria" esta preenchida como "Todas" porque calcularemos o impacto do cenario nos obitos totais)
caminho_completo <- here("csv", "data_frame_basico_cenario_todasidades.csv")
df_resultados_cenarios <- read.csv2(caminho_completo)


# comparacao versus cenario "A" de cada nivel de eletrificacao
df_resultados_cenarios_N0 <- obitos_cenario(df_resultados_cenarios,df_resultados_N0)
df_resultados_cenarios_N1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N1)
df_resultados_cenarios_N2 <- obitos_cenario(df_resultados_cenarios,df_resultados_N2)
df_resultados_cenarios_N3 <- obitos_cenario(df_resultados_cenarios,df_resultados_N3)
df_resultados_cenarios_N4 <- obitos_cenario(df_resultados_cenarios,df_resultados_N4)

# comparacao versus cenario base "A0"
df_resultados_cenarios_N0_1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N0_1)
df_resultados_cenarios_N1_1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N1_1)
df_resultados_cenarios_N2_1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N2_1)
df_resultados_cenarios_N3_1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N3_1)
df_resultados_cenarios_N4_1 <- obitos_cenario(df_resultados_cenarios,df_resultados_N4_1)



df_resultados_cenarios_N0 <- impacto_obitostotais(df_resultados_cenarios_N0)
df_resultados_cenarios_N1 <- impacto_obitostotais(df_resultados_cenarios_N1)
df_resultados_cenarios_N2 <- impacto_obitostotais(df_resultados_cenarios_N2)
df_resultados_cenarios_N3 <- impacto_obitostotais(df_resultados_cenarios_N3)
df_resultados_cenarios_N4 <- impacto_obitostotais(df_resultados_cenarios_N4)

df_resultados_cenarios_N0_1 <- impacto_obitostotais(df_resultados_cenarios_N0_1)
df_resultados_cenarios_N1_1 <- impacto_obitostotais(df_resultados_cenarios_N1_1)
df_resultados_cenarios_N2_1 <- impacto_obitostotais(df_resultados_cenarios_N2_1)
df_resultados_cenarios_N3_1 <- impacto_obitostotais(df_resultados_cenarios_N3_1)
df_resultados_cenarios_N4_1 <- impacto_obitostotais(df_resultados_cenarios_N4_1)




# DATA FRAMES SINTESE DOS RESULTADOS --------------------------------

# Carregar o caminho do csv basico (cenarios)
caminho_completo <- here("csv", "data_frame_basico_cenario.csv")

# Obitos anuais (valor absoluto)

# versus cenario "A" de cada nivel de eletrificacao
df_resultados_obitos_vsA <- read.csv2(caminho_completo)

df_resultados_obitos_vsA$obitos_sinistros <- df_resultados_cenarios_N0$obitos_sinistros
df_resultados_obitos_vsA$obitos_af_med <- df_resultados_cenarios_N0$obitos_af_med
df_resultados_obitos_vsA$obitos_af_inf <- df_resultados_cenarios_N0$obitos_af_inf
df_resultados_obitos_vsA$obitos_af_sup <- df_resultados_cenarios_N0$obitos_af_sup
df_resultados_obitos_vsA$obitos_PM2.5_med_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_med
df_resultados_obitos_vsA$obitos_PM2.5_inf_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_inf
df_resultados_obitos_vsA$obitos_PM2.5_sup_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_sup
df_resultados_obitos_vsA$obitos_PM2.5_med_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_med
df_resultados_obitos_vsA$obitos_PM2.5_inf_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_inf
df_resultados_obitos_vsA$obitos_PM2.5_sup_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_sup
df_resultados_obitos_vsA$obitos_PM2.5_med_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_med
df_resultados_obitos_vsA$obitos_PM2.5_inf_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_inf
df_resultados_obitos_vsA$obitos_PM2.5_sup_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_sup
df_resultados_obitos_vsA$obitos_PM2.5_med_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_med
df_resultados_obitos_vsA$obitos_PM2.5_inf_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_inf
df_resultados_obitos_vsA$obitos_PM2.5_sup_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_sup
df_resultados_obitos_vsA$obitos_PM2.5_med_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_med
df_resultados_obitos_vsA$obitos_PM2.5_inf_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_inf
df_resultados_obitos_vsA$obitos_PM2.5_sup_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_sup
df_resultados_obitos_vsA$obitos_total_med_N0 <- df_resultados_cenarios_N0$obitos_total_med
df_resultados_obitos_vsA$obitos_total_inf_N0 <- df_resultados_cenarios_N0$obitos_total_inf
df_resultados_obitos_vsA$obitos_total_sup_N0 <- df_resultados_cenarios_N0$obitos_total_sup
df_resultados_obitos_vsA$obitos_total_med_N1 <- df_resultados_cenarios_N1$obitos_total_med
df_resultados_obitos_vsA$obitos_total_inf_N1 <- df_resultados_cenarios_N1$obitos_total_inf
df_resultados_obitos_vsA$obitos_total_sup_N1 <- df_resultados_cenarios_N1$obitos_total_sup
df_resultados_obitos_vsA$obitos_total_med_N2 <- df_resultados_cenarios_N2$obitos_total_med
df_resultados_obitos_vsA$obitos_total_inf_N2 <- df_resultados_cenarios_N2$obitos_total_inf
df_resultados_obitos_vsA$obitos_total_sup_N2 <- df_resultados_cenarios_N2$obitos_total_sup
df_resultados_obitos_vsA$obitos_total_med_N3 <- df_resultados_cenarios_N3$obitos_total_med
df_resultados_obitos_vsA$obitos_total_inf_N3 <- df_resultados_cenarios_N3$obitos_total_inf
df_resultados_obitos_vsA$obitos_total_sup_N3 <- df_resultados_cenarios_N3$obitos_total_sup
df_resultados_obitos_vsA$obitos_total_med_N4 <- df_resultados_cenarios_N4$obitos_total_med
df_resultados_obitos_vsA$obitos_total_inf_N4 <- df_resultados_cenarios_N4$obitos_total_inf
df_resultados_obitos_vsA$obitos_total_sup_N4 <- df_resultados_cenarios_N4$obitos_total_sup

# versus cenario base "A0" (divisao modal e eletrificacao da frota em 2019)
df_resultados_obitos_vsA0 <- read.csv2(caminho_completo)

df_resultados_obitos_vsA0$obitos_sinistros <- df_resultados_cenarios_N0_1$obitos_sinistros
df_resultados_obitos_vsA0$obitos_af_med <- df_resultados_cenarios_N0_1$obitos_af_med
df_resultados_obitos_vsA0$obitos_af_inf <- df_resultados_cenarios_N0_1$obitos_af_inf
df_resultados_obitos_vsA0$obitos_af_sup <- df_resultados_cenarios_N0_1$obitos_af_sup
df_resultados_obitos_vsA0$obitos_PM2.5_med_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_med
df_resultados_obitos_vsA0$obitos_PM2.5_inf_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_inf
df_resultados_obitos_vsA0$obitos_PM2.5_sup_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_sup
df_resultados_obitos_vsA0$obitos_PM2.5_med_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_med
df_resultados_obitos_vsA0$obitos_PM2.5_inf_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_inf
df_resultados_obitos_vsA0$obitos_PM2.5_sup_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_sup
df_resultados_obitos_vsA0$obitos_PM2.5_med_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_med
df_resultados_obitos_vsA0$obitos_PM2.5_inf_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_inf
df_resultados_obitos_vsA0$obitos_PM2.5_sup_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_sup
df_resultados_obitos_vsA0$obitos_PM2.5_med_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_med
df_resultados_obitos_vsA0$obitos_PM2.5_inf_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_inf
df_resultados_obitos_vsA0$obitos_PM2.5_sup_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_sup
df_resultados_obitos_vsA0$obitos_PM2.5_med_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_med
df_resultados_obitos_vsA0$obitos_PM2.5_inf_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_inf
df_resultados_obitos_vsA0$obitos_PM2.5_sup_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_sup
df_resultados_obitos_vsA0$obitos_total_med_N0 <- df_resultados_cenarios_N0_1$obitos_total_med
df_resultados_obitos_vsA0$obitos_total_inf_N0 <- df_resultados_cenarios_N0_1$obitos_total_inf
df_resultados_obitos_vsA0$obitos_total_sup_N0 <- df_resultados_cenarios_N0_1$obitos_total_sup
df_resultados_obitos_vsA0$obitos_total_med_N1 <- df_resultados_cenarios_N1_1$obitos_total_med
df_resultados_obitos_vsA0$obitos_total_inf_N1 <- df_resultados_cenarios_N1_1$obitos_total_inf
df_resultados_obitos_vsA0$obitos_total_sup_N1 <- df_resultados_cenarios_N1_1$obitos_total_sup
df_resultados_obitos_vsA0$obitos_total_med_N2 <- df_resultados_cenarios_N2_1$obitos_total_med
df_resultados_obitos_vsA0$obitos_total_inf_N2 <- df_resultados_cenarios_N2_1$obitos_total_inf
df_resultados_obitos_vsA0$obitos_total_sup_N2 <- df_resultados_cenarios_N2_1$obitos_total_sup
df_resultados_obitos_vsA0$obitos_total_med_N3 <- df_resultados_cenarios_N3_1$obitos_total_med
df_resultados_obitos_vsA0$obitos_total_inf_N3 <- df_resultados_cenarios_N3_1$obitos_total_inf
df_resultados_obitos_vsA0$obitos_total_sup_N3 <- df_resultados_cenarios_N3_1$obitos_total_sup
df_resultados_obitos_vsA0$obitos_total_med_N4 <- df_resultados_cenarios_N4_1$obitos_total_med
df_resultados_obitos_vsA0$obitos_total_inf_N4 <- df_resultados_cenarios_N4_1$obitos_total_inf
df_resultados_obitos_vsA0$obitos_total_sup_N4 <- df_resultados_cenarios_N4_1$obitos_total_sup



# Impacto no total de obitos (valor relativo)

# versus cenario "A" de cada nivel de eletrificacao
df_resultados_obitos_pct_vsA <- read.csv2(caminho_completo)

df_resultados_obitos_pct_vsA$obitos_sinistros_pct <- df_resultados_cenarios_N0$obitos_sinistros_pct
df_resultados_obitos_pct_vsA$obitos_af_med_pct <- df_resultados_cenarios_N0$obitos_af_med_pct
df_resultados_obitos_pct_vsA$obitos_af_inf_pct <- df_resultados_cenarios_N0$obitos_af_inf_pct
df_resultados_obitos_pct_vsA$obitos_af_sup_pct <- df_resultados_cenarios_N0$obitos_af_sup_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N0 <- df_resultados_cenarios_N0$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N1 <- df_resultados_cenarios_N1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N2 <- df_resultados_cenarios_N2$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N3 <- df_resultados_cenarios_N3$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N4 <- df_resultados_cenarios_N4$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA$obitos_total_med_pct_N0 <- df_resultados_cenarios_N0$obitos_total_med_pct
df_resultados_obitos_pct_vsA$obitos_total_inf_pct_N0 <- df_resultados_cenarios_N0$obitos_total_inf_pct
df_resultados_obitos_pct_vsA$obitos_total_sup_pct_N0 <- df_resultados_cenarios_N0$obitos_total_sup_pct
df_resultados_obitos_pct_vsA$obitos_total_med_pct_N1 <- df_resultados_cenarios_N1$obitos_total_med_pct
df_resultados_obitos_pct_vsA$obitos_total_inf_pct_N1 <- df_resultados_cenarios_N1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA$obitos_total_sup_pct_N1 <- df_resultados_cenarios_N1$obitos_total_sup_pct
df_resultados_obitos_pct_vsA$obitos_total_med_pct_N2 <- df_resultados_cenarios_N2$obitos_total_med_pct
df_resultados_obitos_pct_vsA$obitos_total_inf_pct_N2 <- df_resultados_cenarios_N2$obitos_total_inf_pct
df_resultados_obitos_pct_vsA$obitos_total_sup_pct_N2 <- df_resultados_cenarios_N2$obitos_total_sup_pct
df_resultados_obitos_pct_vsA$obitos_total_med_pct_N3 <- df_resultados_cenarios_N3$obitos_total_med_pct
df_resultados_obitos_pct_vsA$obitos_total_inf_pct_N3 <- df_resultados_cenarios_N3$obitos_total_inf_pct
df_resultados_obitos_pct_vsA$obitos_total_sup_pct_N3 <- df_resultados_cenarios_N3$obitos_total_sup_pct
df_resultados_obitos_pct_vsA$obitos_total_med_pct_N4 <- df_resultados_cenarios_N4$obitos_total_med_pct
df_resultados_obitos_pct_vsA$obitos_total_inf_pct_N4 <- df_resultados_cenarios_N4$obitos_total_inf_pct
df_resultados_obitos_pct_vsA$obitos_total_sup_pct_N4 <- df_resultados_cenarios_N4$obitos_total_sup_pct


# versus cenario base "A0" (divisao modal e eletrificacao da frota em 2019)
df_resultados_obitos_pct_vsA0 <- read.csv2(caminho_completo)

df_resultados_obitos_pct_vsA0$obitos_sinistros_pct <- df_resultados_cenarios_N0_1$obitos_sinistros_pct
df_resultados_obitos_pct_vsA0$obitos_af_med_pct <- df_resultados_cenarios_N0_1$obitos_af_med_pct
df_resultados_obitos_pct_vsA0$obitos_af_inf_pct <- df_resultados_cenarios_N0_1$obitos_af_inf_pct
df_resultados_obitos_pct_vsA0$obitos_af_sup_pct <- df_resultados_cenarios_N0_1$obitos_af_sup_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_med_pct_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_inf_pct_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_sup_pct_N0 <- df_resultados_cenarios_N0_1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_med_pct_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_inf_pct_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_sup_pct_N1 <- df_resultados_cenarios_N1_1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_med_pct_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_inf_pct_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_sup_pct_N2 <- df_resultados_cenarios_N2_1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_med_pct_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_inf_pct_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_sup_pct_N3 <- df_resultados_cenarios_N3_1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_med_pct_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_med_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_inf_pct_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_inf_pct
df_resultados_obitos_pct_vsA0$obitos_PM2.5_sup_pct_N4 <- df_resultados_cenarios_N4_1$obitos_PM2.5_sup_pct
df_resultados_obitos_pct_vsA0$obitos_total_med_pct_N0 <- df_resultados_cenarios_N0_1$obitos_total_med_pct
df_resultados_obitos_pct_vsA0$obitos_total_inf_pct_N0 <- df_resultados_cenarios_N0_1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA0$obitos_total_sup_pct_N0 <- df_resultados_cenarios_N0_1$obitos_total_sup_pct
df_resultados_obitos_pct_vsA0$obitos_total_med_pct_N1 <- df_resultados_cenarios_N1_1$obitos_total_med_pct
df_resultados_obitos_pct_vsA0$obitos_total_inf_pct_N1 <- df_resultados_cenarios_N1_1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA0$obitos_total_sup_pct_N1 <- df_resultados_cenarios_N1_1$obitos_total_sup_pct
df_resultados_obitos_pct_vsA0$obitos_total_med_pct_N2 <- df_resultados_cenarios_N2_1$obitos_total_med_pct
df_resultados_obitos_pct_vsA0$obitos_total_inf_pct_N2 <- df_resultados_cenarios_N2_1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA0$obitos_total_sup_pct_N2 <- df_resultados_cenarios_N2_1$obitos_total_sup_pct
df_resultados_obitos_pct_vsA0$obitos_total_med_pct_N3 <- df_resultados_cenarios_N3_1$obitos_total_med_pct
df_resultados_obitos_pct_vsA0$obitos_total_inf_pct_N3 <- df_resultados_cenarios_N3_1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA0$obitos_total_sup_pct_N3 <- df_resultados_cenarios_N3_1$obitos_total_sup_pct
df_resultados_obitos_pct_vsA0$obitos_total_med_pct_N4 <- df_resultados_cenarios_N4_1$obitos_total_med_pct
df_resultados_obitos_pct_vsA0$obitos_total_inf_pct_N4 <- df_resultados_cenarios_N4_1$obitos_total_inf_pct
df_resultados_obitos_pct_vsA0$obitos_total_sup_pct_N4 <- df_resultados_cenarios_N4_1$obitos_total_sup_pct






# GRÁFICOS ----------------------------------------------------------------

# Impacto no total de obitos - versus cenario "A" (%)

obitos_pct <- data.frame(
  cenarios = c("B", "C", "D", "E"),
  sinistros = df_resultados_obitos_pct_vsA$obitos_sinistros_pct[2:5] * 100,
  atividade_fisica = df_resultados_obitos_pct_vsA$obitos_af_med_pct[2:5] * 100,
  atividade_fisica_inferior = df_resultados_obitos_pct_vsA$obitos_af_inf_pct[2:5] * 100,
  atividade_fisica_superior = df_resultados_obitos_pct_vsA$obitos_af_sup_pct[2:5] * 100, 
  PM2.5_N0 = df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N0[2:5] * 100,
  PM2.5_N0_inferior = df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N0[2:5] * 100,
  PM2.5_N0_superior = df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N0[2:5] * 100,
  PM2.5_N1 = df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N1[2:5] * 100,
  PM2.5_N1_inferior = df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N1[2:5] * 100,
  PM2.5_N1_superior = df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N1[2:5] * 100,
  PM2.5_N2 = df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N2[2:5] * 100,
  PM2.5_N2_inferior = df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N2[2:5] * 100,
  PM2.5_N2_superior = df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N2[2:5] * 100,
  PM2.5_N3 = df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N3[2:5] * 100,
  PM2.5_N3_inferior = df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N3[2:5] * 100,
  PM2.5_N3_superior = df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N3[2:5] * 100,
  PM2.5_N4 = df_resultados_obitos_pct_vsA$obitos_PM2.5_med_pct_N4[2:5] * 100,
  PM2.5_N4_inferior = df_resultados_obitos_pct_vsA$obitos_PM2.5_inf_pct_N4[2:5] * 100,
  PM2.5_N4_superior = df_resultados_obitos_pct_vsA$obitos_PM2.5_sup_pct_N4[2:5] * 100
)

# Reorganizar os dados em formato longo
obitos_pct_long <- obitos_pct %>%
  pivot_longer(
    cols = c(sinistros, atividade_fisica, PM2.5_N0, PM2.5_N1, PM2.5_N2, PM2.5_N3, PM2.5_N4),
    names_to = "exposicao",
    values_to = "obitos"
  )

# Adicionar os limites inferior e superior para cada exposição
obitos_pct_long <- obitos_pct_long %>%
  mutate(
    exposicao_inferior = ifelse(exposicao == "atividade_fisica", atividade_fisica_inferior,
                                ifelse(exposicao == "PM2.5_N0", PM2.5_N0_inferior,
                                       ifelse(exposicao == "PM2.5_N1", PM2.5_N1_inferior,
                                              ifelse(exposicao == "PM2.5_N2", PM2.5_N2_inferior,
                                                     ifelse(exposicao == "PM2.5_N3", PM2.5_N3_inferior,
                                                            ifelse(exposicao == "PM2.5_N4", PM2.5_N4_inferior,NA)))))),
    
    exposicao_superior = ifelse(exposicao == "atividade_fisica", atividade_fisica_superior,
                                ifelse(exposicao == "PM2.5_N0", PM2.5_N0_superior,
                                       ifelse(exposicao == "PM2.5_N1", PM2.5_N1_superior,
                                              ifelse(exposicao == "PM2.5_N2", PM2.5_N2_superior,
                                                     ifelse(exposicao == "PM2.5_N3", PM2.5_N3_superior,
                                                            ifelse(exposicao == "PM2.5_N4", PM2.5_N4_superior,NA))))))
  )


# Plotar o gráfico de barras com intervalos de confiança
ggplot(obitos_pct_long, aes(x = cenarios, y = obitos, fill = exposicao)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = exposicao_inferior, ymax = exposicao_superior),
                position = position_dodge(width = 0.9),
                width = 0.2,
                linewidth = .1) +
  labs(x = 'Cenarios (versus "A")', y = "Impacto no total de obitos (%)", fill = "Exposicao") +
  scale_fill_manual(values = c("sinistros" = "#575656", 
                               "atividade_fisica" = "#3AAA35", 
                               "PM2.5_N0" = "red", 
                               "PM2.5_N1" = "#E62C2A", 
                               "PM2.5_N2" = "#E53D46", 
                               "PM2.5_N3" = "#EB5E59", 
                               "PM2.5_N4" = "#EF7E85" 
  ),
  labels = c("Atividade Fisica", "Poluicao do ar (Nivel 0)", "Poluicao do ar (Nivel 1)", "Poluicao do ar (Nivel 2)", "Poluicao do ar (Nivel 3)", "Poluicao do ar (Nivel 4)", "Sinistros de transito", "Todas as exposicoes")) +
  theme_grey() + 
  theme(plot.title = element_text(size=16, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size=12, face="bold"), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=8, face="bold"),
        legend.text = element_text(size = 6))+
  scale_y_continuous(limits = c(-3, 4))

ggsave("resultados_grafico/obitos_pct_eletr_vsA.jpg", width=1920, height=1080, units = "px", dpi=300)


# MONTAR O CSV FINAL ------------------------------------------------------


# Impacto no total de obitos

# variacao em relacao ao cenario A
write.csv(df_resultados_obitos_pct_vsA, "resultados_csv/versus_cenario_A/obitos_pct_vsA.csv")

# variacao em relacao ao cenario base "A0"
write.csv(df_resultados_obitos_pct_vsA0, "resultados_csv/versus_cenario_A0/obitos_pct_vsA0.csv")

# Quantidade de obitos

# variacao em relacao ao cenario A
write.csv(df_resultados_obitos_vsA, "resultados_csv/versus_cenario_A/obitos_vsA.csv")

# variacao em relacao ao cenario base "A0"
write.csv(df_resultados_obitos_vsA0, "resultados_csv/versus_cenario_A0/obitos_vsA0.csv")



# Apagar data frames e objetos intermediarios -----------------------------

rm(df_resultados)
rm(df_resultados_cenarios)
rm(df_resultados_N0)
rm(df_resultados_N0_1)
rm(df_resultados_N1)
rm(df_resultados_N1_1)
rm(df_resultados_N2)
rm(df_resultados_N2_1)
rm(df_resultados_N3)
rm(df_resultados_N3_1)
rm(df_resultados_N4)
rm(df_resultados_N4_1)
rm(df_resultados_cenarios_N0)
rm(df_resultados_cenarios_N0_1)
rm(df_resultados_cenarios_N1)
rm(df_resultados_cenarios_N1_1)
rm(df_resultados_cenarios_N2)
rm(df_resultados_cenarios_N2_1)
rm(df_resultados_cenarios_N3)
rm(df_resultados_cenarios_N3_1)
rm(df_resultados_cenarios_N4)
rm(df_resultados_cenarios_N4_1)
rm(obitos_pct)
rm(obitos_pct_long)

rm(caminho_completo)
rm(indices_df1)
rm(indices_df10)
rm(indices_df11)
rm(indices_df12)
rm(indices_df13)
rm(indices_df14)
rm(indices_df2)
rm(indices_df3)
rm(indices_df4)
rm(indices_df5)
rm(indices_df8)
rm(indices_df9)