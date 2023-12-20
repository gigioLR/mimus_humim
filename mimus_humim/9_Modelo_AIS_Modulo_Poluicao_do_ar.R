# Modulo para simular a inalacao diaria de poluente PM2.5 em cada cenario a partir do padrao de mobilidade urbana e calcular o impacto na saude

#Criar data frame com dados da tabela do padrao de mobilidade
df_PM2.5 <- df_tempoviagem

t_transporte <- df_PM2.5$t_dia_ape+df_PM2.5$t_dia_bici+df_PM2.5$t_dia_TI+df_PM2.5$t_dia_TP+df_PM2.5$t_dia_ape_TP

df_PM2.5$t_dia_sono <- 8

df_PM2.5$t_dia_outros <- 24-t_transporte-df_PM2.5$t_dia_sono

#taxa de ventilacao (Woodcock) ou ventilacao minuto (Rojas-Rueda)

vent_ape <- 1.14
vent_bici <- 2.22
vent_TI <- 0.61
vent_TP <- 0.61
vent_ape_TP <- 1.14
vent_sono <- 0.27
vent_outros <- 0.61

#ar inalado por dia
#duracao (h) * taxa de ventilacao (m3/h)

df_PM2.5$ar_inalado_ape <- df_PM2.5$t_dia_ape * vent_ape
df_PM2.5$ar_inalado_bici <- df_PM2.5$t_dia_bici * vent_bici
df_PM2.5$ar_inalado_TI <- df_PM2.5$t_dia_TI * vent_TI
df_PM2.5$ar_inalado_TP <- df_PM2.5$t_dia_TP * vent_TP
df_PM2.5$ar_inalado_ape_TP <- df_PM2.5$t_dia_ape_TP * vent_ape_TP
df_PM2.5$ar_inalado_sono <- df_PM2.5$t_dia_sono * vent_sono
df_PM2.5$ar_inalado_outros <- df_PM2.5$t_dia_outros * vent_outros


df_PM2.5$ar_inalado <- df_PM2.5$ar_inalado_ape+df_PM2.5$ar_inalado_bici+df_PM2.5$ar_inalado_TI+df_PM2.5$ar_inalado_TP+df_PM2.5$ar_inalado_ape_TP+df_PM2.5$ar_inalado_sono+df_PM2.5$ar_inalado_outros

df_PM2.5_N0 <- df_PM2.5
df_PM2.5_N1 <- df_PM2.5
df_PM2.5_N2 <- df_PM2.5
df_PM2.5_N3 <- df_PM2.5
df_PM2.5_N4 <- df_PM2.5

df_PM2.5_N0$PM2.5_conc_geral <- df_emissaoPM2.5_geral$PM2.5_conc_N0 # aqui vem os dados de entrada de poluição (modelar para cada cenario)
df_PM2.5_N1$PM2.5_conc_geral <- df_emissaoPM2.5_geral$PM2.5_conc_N1
df_PM2.5_N2$PM2.5_conc_geral <- df_emissaoPM2.5_geral$PM2.5_conc_N2
df_PM2.5_N3$PM2.5_conc_geral <- df_emissaoPM2.5_geral$PM2.5_conc_N3
df_PM2.5_N4$PM2.5_conc_geral <- df_emissaoPM2.5_geral$PM2.5_conc_N4


# taxa de exposicao (exposure rate) ao PM2.5 por modo ou atividade 

# funcao "adicionar_taxa_expo" esta em "Funcoes.R"

df_PM2.5_N0 <- adicionar_taxa_expo(df_PM2.5_N0)
df_PM2.5_N1 <- adicionar_taxa_expo(df_PM2.5_N1)
df_PM2.5_N2 <- adicionar_taxa_expo(df_PM2.5_N2)
df_PM2.5_N3 <- adicionar_taxa_expo(df_PM2.5_N3)
df_PM2.5_N4 <- adicionar_taxa_expo(df_PM2.5_N4)


## Calculo PM2.5 inalado por atividade
# taxa ventilacao * duracao * concentracao de fundo PM2.5 * taxa de exposicao = ar inalado * concentracao de fundo PM2.5 * taxa de exposicao

# funcao "calcular_PM2.5_inalado" esta em "Funcoes.R"

# atribuir os valores ao data frame
df_PM2.5_N0 <- calcular_PM2.5_inalado(df_PM2.5_N0)
df_PM2.5_N1 <- calcular_PM2.5_inalado(df_PM2.5_N1)
df_PM2.5_N2 <- calcular_PM2.5_inalado(df_PM2.5_N2)
df_PM2.5_N3 <- calcular_PM2.5_inalado(df_PM2.5_N3)
df_PM2.5_N4 <- calcular_PM2.5_inalado(df_PM2.5_N4)

df_PM2.5_N0$PM2.5_inalado <- df_PM2.5_N0$PM2.5_inalado_ape+df_PM2.5_N0$PM2.5_inalado_bici+df_PM2.5_N0$PM2.5_inalado_TI+df_PM2.5_N0$PM2.5_inalado_TP+df_PM2.5_N0$PM2.5_inalado_ape_TP+df_PM2.5_N0$PM2.5_inalado_sono+df_PM2.5_N0$PM2.5_inalado_outros
df_PM2.5_N1$PM2.5_inalado <- df_PM2.5_N1$PM2.5_inalado_ape+df_PM2.5_N1$PM2.5_inalado_bici+df_PM2.5_N1$PM2.5_inalado_TI+df_PM2.5_N1$PM2.5_inalado_TP+df_PM2.5_N1$PM2.5_inalado_ape_TP+df_PM2.5_N1$PM2.5_inalado_sono+df_PM2.5_N1$PM2.5_inalado_outros
df_PM2.5_N2$PM2.5_inalado <- df_PM2.5_N2$PM2.5_inalado_ape+df_PM2.5_N2$PM2.5_inalado_bici+df_PM2.5_N2$PM2.5_inalado_TI+df_PM2.5_N2$PM2.5_inalado_TP+df_PM2.5_N2$PM2.5_inalado_ape_TP+df_PM2.5_N2$PM2.5_inalado_sono+df_PM2.5_N2$PM2.5_inalado_outros
df_PM2.5_N3$PM2.5_inalado <- df_PM2.5_N3$PM2.5_inalado_ape+df_PM2.5_N3$PM2.5_inalado_bici+df_PM2.5_N3$PM2.5_inalado_TI+df_PM2.5_N3$PM2.5_inalado_TP+df_PM2.5_N3$PM2.5_inalado_ape_TP+df_PM2.5_N3$PM2.5_inalado_sono+df_PM2.5_N3$PM2.5_inalado_outros
df_PM2.5_N4$PM2.5_inalado <- df_PM2.5_N4$PM2.5_inalado_ape+df_PM2.5_N4$PM2.5_inalado_bici+df_PM2.5_N4$PM2.5_inalado_TI+df_PM2.5_N4$PM2.5_inalado_TP+df_PM2.5_N4$PM2.5_inalado_ape_TP+df_PM2.5_N4$PM2.5_inalado_sono+df_PM2.5_N4$PM2.5_inalado_outros


# concentracao equivalente para cada cenario
df_PM2.5_N0$PM2.5_conc_cen<- df_PM2.5_N0$PM2.5_inalado/df_PM2.5_N0$ar_inalado
df_PM2.5_N1$PM2.5_conc_cen<- df_PM2.5_N1$PM2.5_inalado/df_PM2.5_N1$ar_inalado
df_PM2.5_N2$PM2.5_conc_cen<- df_PM2.5_N2$PM2.5_inalado/df_PM2.5_N2$ar_inalado
df_PM2.5_N3$PM2.5_conc_cen<- df_PM2.5_N3$PM2.5_inalado/df_PM2.5_N3$ar_inalado
df_PM2.5_N4$PM2.5_conc_cen<- df_PM2.5_N4$PM2.5_inalado/df_PM2.5_N4$ar_inalado



# variacao de concentracao em relacao ao cenario base

# estabelecer cenario base
df_PM2.5_base_N0 <- estabelecer_cenario_base(df_PM2.5_N0)
df_PM2.5_base_N1 <- estabelecer_cenario_base(df_PM2.5_N1)
df_PM2.5_base_N2 <- estabelecer_cenario_base(df_PM2.5_N2)
df_PM2.5_base_N3 <- estabelecer_cenario_base(df_PM2.5_N3)
df_PM2.5_base_N4 <- estabelecer_cenario_base(df_PM2.5_N4)

# funcao "calcular_variacao_conc" esta em "Funcoes.R"

# diferenca em relacao ao cenario A de cada nivel de eletrificacao
df_PM2.5_N0 <- calcular_variacao_conc(df_PM2.5_N0,df_PM2.5_base_N0)
df_PM2.5_N1 <- calcular_variacao_conc(df_PM2.5_N1,df_PM2.5_base_N1)
df_PM2.5_N2 <- calcular_variacao_conc(df_PM2.5_N2,df_PM2.5_base_N2)
df_PM2.5_N3 <- calcular_variacao_conc(df_PM2.5_N3,df_PM2.5_base_N3)
df_PM2.5_N4 <- calcular_variacao_conc(df_PM2.5_N4,df_PM2.5_base_N4)

# diferenca em relacao ao cenario A do nivel "0" de eletrificacao
df_PM2.5_N0_1 <- calcular_variacao_conc(df_PM2.5_N0,df_PM2.5_base_N0)
df_PM2.5_N1_1 <- calcular_variacao_conc(df_PM2.5_N1,df_PM2.5_base_N0)
df_PM2.5_N2_1 <- calcular_variacao_conc(df_PM2.5_N2,df_PM2.5_base_N0)
df_PM2.5_N3_1 <- calcular_variacao_conc(df_PM2.5_N3,df_PM2.5_base_N0)
df_PM2.5_N4_1 <- calcular_variacao_conc(df_PM2.5_N4,df_PM2.5_base_N0)






# RISCO RELATIVO PARA O CENARIO -------------------------------------------

# atribuir a Funcao exposicao-resposta utilizada
# funcao "funcao_exposicaoresposta_pm2.5" esta em "Funcoes.R"

df_PM2.5_N0 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N0)
df_PM2.5_N1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N1)
df_PM2.5_N2 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N2)
df_PM2.5_N3 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N3)
df_PM2.5_N4 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N4)

df_PM2.5_N0_1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N0_1)
df_PM2.5_N1_1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N1_1)
df_PM2.5_N2_1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N2_1)
df_PM2.5_N3_1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N3_1)
df_PM2.5_N4_1 <- funcao_exposicaoresposta_pm2.5(df_PM2.5_N4_1)


#calculo risco relativo: RR referencia^(variacao PM2.5 cenario/variacao PM2.5 referencia) 
# funcao "riscorelativo_pm2.5" esta em "Funcoes.R"

df_PM2.5_N0 <- riscorelativo_pm2.5(df_PM2.5_N0)
df_PM2.5_N1 <- riscorelativo_pm2.5(df_PM2.5_N1)
df_PM2.5_N2 <- riscorelativo_pm2.5(df_PM2.5_N2)
df_PM2.5_N3 <- riscorelativo_pm2.5(df_PM2.5_N3)
df_PM2.5_N4 <- riscorelativo_pm2.5(df_PM2.5_N4)

df_PM2.5_N0_1 <- riscorelativo_pm2.5(df_PM2.5_N0_1)
df_PM2.5_N1_1 <- riscorelativo_pm2.5(df_PM2.5_N1_1)
df_PM2.5_N2_1 <- riscorelativo_pm2.5(df_PM2.5_N2_1)
df_PM2.5_N3_1 <- riscorelativo_pm2.5(df_PM2.5_N3_1)
df_PM2.5_N4_1 <- riscorelativo_pm2.5(df_PM2.5_N4_1)


# FRAÇÃO ATRIBUIVEL POPULACIONAL ------------------------------------------
# Fracao atribuivel = (risco relativo - 1)/risco relativo
# funcao "fracaoatribuivel_pm2.5" esta em "Funcoes.R"

df_PM2.5_N0 <- fracaoatribuivel_pm2.5(df_PM2.5_N0)
df_PM2.5_N1 <- fracaoatribuivel_pm2.5(df_PM2.5_N1)
df_PM2.5_N2 <- fracaoatribuivel_pm2.5(df_PM2.5_N2)
df_PM2.5_N3 <- fracaoatribuivel_pm2.5(df_PM2.5_N3)
df_PM2.5_N4 <- fracaoatribuivel_pm2.5(df_PM2.5_N4)

df_PM2.5_N0_1 <- fracaoatribuivel_pm2.5(df_PM2.5_N0_1)
df_PM2.5_N1_1 <- fracaoatribuivel_pm2.5(df_PM2.5_N1_1)
df_PM2.5_N2_1 <- fracaoatribuivel_pm2.5(df_PM2.5_N2_1)
df_PM2.5_N3_1 <- fracaoatribuivel_pm2.5(df_PM2.5_N3_1)
df_PM2.5_N4_1 <- fracaoatribuivel_pm2.5(df_PM2.5_N4_1)

# neste caso, a fracao atribuivel ja esta como uma variacao

# CARGA DA DOENCA ATRIBUIVEL (CENARIOS) ---------------------------------------

# obitos
# funcao "obitos_pm2.5" esta em "Funcoes.R"



df_PM2.5_N0 <- obitos_pm2.5(df_PM2.5_N0)
df_PM2.5_N1 <- obitos_pm2.5(df_PM2.5_N1)
df_PM2.5_N2 <- obitos_pm2.5(df_PM2.5_N2)
df_PM2.5_N3 <- obitos_pm2.5(df_PM2.5_N3)
df_PM2.5_N4 <- obitos_pm2.5(df_PM2.5_N4)

df_PM2.5_N0_1 <- obitos_pm2.5(df_PM2.5_N0_1)
df_PM2.5_N1_1 <- obitos_pm2.5(df_PM2.5_N1_1)
df_PM2.5_N2_1 <- obitos_pm2.5(df_PM2.5_N2_1)
df_PM2.5_N3_1 <- obitos_pm2.5(df_PM2.5_N3_1)
df_PM2.5_N4_1 <- obitos_pm2.5(df_PM2.5_N4_1)


# Apagar data frames e objetos intermediarios -----------------------------

rm(df_PM2.5)
rm(df_PM2.5_base_N0)
rm(df_PM2.5_base_N1)
rm(df_PM2.5_base_N2)
rm(df_PM2.5_base_N3)
rm(df_PM2.5_base_N4)
rm(t_transporte)
rm(vent_ape)
rm(vent_ape_TP)
rm(vent_bici)
rm(vent_outros)
rm(vent_sono)
rm(vent_TI)
rm(vent_TP)