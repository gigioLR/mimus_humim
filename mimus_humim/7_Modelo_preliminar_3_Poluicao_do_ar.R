# Modelo para estimar a concentracao media anual "de fundo" (background) de PM2.5 em cada cenario
# Usa dados medidos in loco por sensores (concentracao media anual no cenario base "A0") e dados modelados de emissao no transporte

#### EMISSAO PM2.5 ####


# Carregar o csv basico (cenarios)
caminho_completo <- here("csv", "data_frame_basico_cenario.csv")
df_emissaoPM2.5_geral<-read.csv2(caminho_completo)

# emissao total = distancia * frota * fator de emissao
# link fator de emissao: https://www.sciencedirect.com/science/article/pii/S004896972204058X?via%3Dihub
# diesel: 28.9 | gasolina: 28.7 | eletrico (freio regenerativo 90%): 13.9 (unidade: mg/V*km)

## niveis eletrificacao:
# TP: N0 = 0%, N1 = 60%,  N2 = 75%, N3 = 90%,  N4 = 100%
# TI: N0 = 0,04%, N1 = 30%,  N2 = 60%, N3 = 75%,  N4 = 100%


# Emissao PM2.5 -------------------------------------

# Vincular o data frame novo com os de total de viagens e divisao modal
indices_df5 <- match(df_distancia_ano_cenario$ano, df_frota_cenarios$ano)

# transporte publico - TP

PM2.5_geral_TP_N0 <- df_distancia_ano_cenario$dist_total_TP * df_frota_cenarios$frota_TP[indices_df5] * 28.9 
PM2.5_geral_TP_N1 <- (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.40) * 28.9) + (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.60) * 13.9)
PM2.5_geral_TP_N2 <- (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.25) * 28.9) + (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.75) * 13.9)
PM2.5_geral_TP_N3 <- (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.10) * 28.9) + (df_distancia_ano_cenario$dist_total_TP * (df_frota_cenarios$frota_TP[indices_df5]*0.90) * 13.9)
PM2.5_geral_TP_N4 <- df_distancia_ano_cenario$dist_total_TP * df_frota_cenarios$frota_TP[indices_df5] * 13.9

# transporte individual motorizado - TI

PM2.5_geral_TI_N0 <- (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.9996) * 28.7) + (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.0004) * 13.9)
PM2.5_geral_TI_N1 <- (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.7) * 28.7) + (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.3) * 13.9)
PM2.5_geral_TI_N2 <- (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.4) * 28.7) + (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.60) * 13.9)
PM2.5_geral_TI_N3 <- (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.25) * 28.7) + (df_distancia_ano_cenario$dist_total_TI * (df_frota_cenarios$frota_TI[indices_df5]*0.75) * 13.9)
PM2.5_geral_TI_N4 <- df_distancia_ano_cenario$dist_total_TI * df_frota_cenarios$frota_TI[indices_df5] * 13.9

# Transporte motorizado (TI+TP)

PM2.5_geral_transporte_N0 <- PM2.5_geral_TP_N0 + PM2.5_geral_TI_N0
PM2.5_geral_transporte_N1 <- PM2.5_geral_TP_N1 + PM2.5_geral_TI_N1
PM2.5_geral_transporte_N2 <- PM2.5_geral_TP_N2 + PM2.5_geral_TI_N2
PM2.5_geral_transporte_N3 <- PM2.5_geral_TP_N3 + PM2.5_geral_TI_N3
PM2.5_geral_transporte_N4 <- PM2.5_geral_TP_N4 + PM2.5_geral_TI_N4


# Concentracao anual PM2.5 no cenario -------------------------------------------

# participação do transporte nas emissões em Porto Alegre
emissao_transporte_pct <- 39 # referencia que usei: https://doi.org/10.1186/s12940-016-0172-6

# concentracao media anual PM2.5 (sensores)
PM2.5_concentracao_medida <- 11.55

# esse dado é crucial
# se deixar o mesmo valor para todos os cenários, acaba dizendo que mata mais gente por poluição no cenário com mais mobilidade ativa (dentro do modelo, isso faz sentido, já que é preciso inalar mais ar para realizar mobilidade ativa)
# há um pressuposto aqui de que a redução do uso de modais motorizados implica em menores concentrações de PM2.5 (o impacto ambiental da motorizacao) --> neste caso, a concentração seria proporcional ao VKT

# CONCENTRACAO PM2.5 ANUAL = CONCENTRACAO PM2.5 TRANSPORTE + CONCENTRACAO PM2.5 NAO-TRANSPORTE
# Concentracao anual PM2.5 cenario = concentracao medida PM2.5 * participacao transporte emissao * (PM2.5 emitido no cenario/PM2.5 emitido no cenario base)

df_emissaoPM2.5_geral$PM2.5_conc_N0 <- (PM2.5_concentracao_medida*(emissao_transporte_pct/100)*(PM2.5_geral_transporte_N0/PM2.5_geral_transporte_N0[1])) + PM2.5_concentracao_medida*(1-(emissao_transporte_pct/100))
df_emissaoPM2.5_geral$PM2.5_conc_N1 <- (PM2.5_concentracao_medida*(emissao_transporte_pct/100)*(PM2.5_geral_transporte_N1/PM2.5_geral_transporte_N0[1])) + PM2.5_concentracao_medida*(1-(emissao_transporte_pct/100))
df_emissaoPM2.5_geral$PM2.5_conc_N2 <- (PM2.5_concentracao_medida*(emissao_transporte_pct/100)*(PM2.5_geral_transporte_N2/PM2.5_geral_transporte_N0[1])) + PM2.5_concentracao_medida*(1-(emissao_transporte_pct/100))
df_emissaoPM2.5_geral$PM2.5_conc_N3 <- (PM2.5_concentracao_medida*(emissao_transporte_pct/100)*(PM2.5_geral_transporte_N3/PM2.5_geral_transporte_N0[1])) + PM2.5_concentracao_medida*(1-(emissao_transporte_pct/100))
df_emissaoPM2.5_geral$PM2.5_conc_N4 <- (PM2.5_concentracao_medida*(emissao_transporte_pct/100)*(PM2.5_geral_transporte_N4/PM2.5_geral_transporte_N0[1])) + PM2.5_concentracao_medida*(1-(emissao_transporte_pct/100))


# apagar data frames e objetos intermediarios
rm(PM2.5_geral_TP_N0)
rm(PM2.5_geral_TP_N1)
rm(PM2.5_geral_TP_N2)
rm(PM2.5_geral_TP_N3)
rm(PM2.5_geral_TP_N4)

rm(PM2.5_geral_TI_N0)
rm(PM2.5_geral_TI_N1)
rm(PM2.5_geral_TI_N2)
rm(PM2.5_geral_TI_N3)
rm(PM2.5_geral_TI_N4)

rm(PM2.5_geral_transporte_N0)
rm(PM2.5_geral_transporte_N1)
rm(PM2.5_geral_transporte_N2)
rm(PM2.5_geral_transporte_N3)
rm(PM2.5_geral_transporte_N4)

rm(emissao_transporte_pct)
rm(PM2.5_concentracao_medida)
