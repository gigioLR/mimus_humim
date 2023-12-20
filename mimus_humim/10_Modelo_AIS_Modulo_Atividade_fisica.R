# Modulo para simular o gasto energetico semanal medio em cada cenario (mobilidade urbana e outras atividades) e calcular o impacto na saude


# Calculo de atividade fisica

# Carregar o csv basico (faixa etaria e cenarios)
caminho_completo <- here("csv", "data_frame_basico_faixaetariaxcenario.csv")
df_atividadefisica<-read.csv2(caminho_completo)

# df_perfilpop_af_PNS = informacoes de prevalencia e gasto energetico semanal fora do transporte por nivel de atividade fisica (insuficientemente ativo, ativo, muito ativo) 


#### GASTO ENERGETICO SEMANAL (MET-HORA) - ATIVIDADE FISICA NO TRANSPORTE ####

df_atividadefisica$METh_semana_ape <- df_tempoviagem$t_dia_ape*6*4 # somente viagens a pe
df_atividadefisica$METh_semana_ape_TP <- df_tempoviagem$t_dia_ape_TP*6*4 # viagens a pe para usar o transporte publico (10 minutos de caminhada por viagem)
df_atividadefisica$METh_semana_bici <- df_tempoviagem$t_dia_bici*6*6.8 # somente viagens de bicicleta


# PONDERACAO DO GASTO METABOLICO (MET) NO TRANSPORTE EM FUNCAO DA IDADE (65 anos ou mais)
# para o calculo dos impactos da atividade fisica, Mueller et al (2017) consideram que o gasto energetico em MET para idosos iguala-se a 75% do gasto energetico
# Mueller et al (2017): atividade moderada = 4 MET para adultos e 3 MET para idosos; atividade intensa = 8 MET para adultos e 6 MET para idosos 
# racional: envelhecimento reduz a capacidade aerobica, etc
# isso ja foi feito para o gasto metabolico no lazer e no trabalho no script da PNS, o codigo a seguir serve somente para a parte da mobilidade ativa

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_semana_ape = case_when(
    faixa_etaria == "65mais"      ~METh_semana_ape*0.75,
    faixa_etaria != "65mais"      ~METh_semana_ape
  ))

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_semana_ape_TP = case_when(
    faixa_etaria == "65mais"      ~METh_semana_ape_TP*0.75,
    faixa_etaria != "65mais"      ~METh_semana_ape_TP
  ))

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_semana_bici = case_when(
    faixa_etaria == "65mais"      ~METh_semana_bici*0.75,
    faixa_etaria != "65mais"      ~METh_semana_bici
  ))

df_atividadefisica$METh_semana_transporte <- df_atividadefisica$METh_semana_ape+df_atividadefisica$METh_semana_ape_TP+df_atividadefisica$METh_semana_bici


#### GASTO ENERGETICO SEMANAL (MET-HORA) - ATIVIDADE FISICA FORA DO TRANSPORTE ####

# vincular a coluna "faixa_etaria" do data frame do modelo preliminar com a do modulo 
indices_df8 <- match(df_atividadefisica$faixa_etaria,df_perfilpop_af_PNS$faixa_etaria)


# INSUFICIENTEMENTE ATIVO

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_lt_insuf_ativo = df_perfilpop_af_PNS$METhsem_lt_insu_ativo[indices_df8])


## ATIVO

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_lt_ativo = df_perfilpop_af_PNS$METhsem_lt_ativo[indices_df8])



## MUITO ATIVO

df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_lt_muito_ativo = df_perfilpop_af_PNS$METhsem_lt_muito_ativo[indices_df8])




# GASTO METABOLICO SEMANAL TOTAL (POR NIVEL DE ATIVIDADE FISICA)


df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_total_insuf_ativo = df_atividadefisica$METh_lt_insuf_ativo + df_atividadefisica$METh_semana_transporte)


df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_total_ativo = df_atividadefisica$METh_lt_ativo + df_atividadefisica$METh_semana_transporte)


df_atividadefisica <- df_atividadefisica %>%
  mutate(METh_total_muito_ativo = df_atividadefisica$METh_lt_muito_ativo + df_atividadefisica$METh_semana_transporte)



#### PREVALENCIA DE CADA NIVEL DE ATIVIDADE FISICA ####


# INSUFICIENTEMENTE ATIVO

df_atividadefisica <- df_atividadefisica %>%
  mutate(prev_insuf_ativo = df_perfilpop_af_PNS$prev_insu_ativo[indices_df8])

## ATIVO

# prevalencia
df_atividadefisica <- df_atividadefisica %>%
  mutate(prev_ativo = df_perfilpop_af_PNS$prev_ativo[indices_df8])

## MUITO ATIVO

# prevalencia
df_atividadefisica <- df_atividadefisica %>%
  mutate(prev_muito_ativo = df_perfilpop_af_PNS$prev_muito_ativo[indices_df8])




# CALCULO RISCO RELATIVO (CENARIOS) --------------------------------------------------

# atribuir os valores ao data frame
df_atividadefisica <- calcular_cen_RR_af(df_atividadefisica) # Funcao "calcular_cen_RR_af" esta no script "Funcoes.R"




# FRACAO ATRIBUIVEL POPULACIONAL ------------------------------------------

# atribuir os valores ao data frame
df_atividadefisica <- calcular_cen_FAP_af(df_atividadefisica) # Funcao "calcular_cen_FAP_af" esta no script "Funcoes.R"


# somar as fracoes atribuiveis dos 3 niveis de atividade fisica
df_atividadefisica$FAP_med <- df_atividadefisica$FAP_med_insuf_ativo+df_atividadefisica$FAP_med_ativo+df_atividadefisica$FAP_med_muito_ativo
df_atividadefisica$FAP_inf <- df_atividadefisica$FAP_inf_insuf_ativo+df_atividadefisica$FAP_inf_ativo+df_atividadefisica$FAP_inf_muito_ativo
df_atividadefisica$FAP_sup <- df_atividadefisica$FAP_sup_insuf_ativo+df_atividadefisica$FAP_sup_ativo+df_atividadefisica$FAP_sup_muito_ativo


## variacao em relacao ao cenario base

df_atividadefisica_base <- estabelecer_cenario_base(df_atividadefisica) # funcao "estabelecer_cenario_base" esta em "Funcoes.R"

# vincular a coluna "faixa_etaria" do data frame do cenario base com a de todos os cenarios 
indices_df9 <- match(df_atividadefisica$faixa_etaria,df_atividadefisica_base$faixa_etaria)

# calcular a variacao
df_atividadefisica <- df_atividadefisica %>%
  mutate(FAP_med_vari = df_atividadefisica$FAP_med - df_atividadefisica_base$FAP_med[indices_df9])

df_atividadefisica <- df_atividadefisica %>%
  mutate(FAP_inf_vari = df_atividadefisica$FAP_inf - df_atividadefisica_base$FAP_inf[indices_df9])

df_atividadefisica <- df_atividadefisica %>%
  mutate(FAP_sup_vari = df_atividadefisica$FAP_sup - df_atividadefisica_base$FAP_sup[indices_df9])


df_atividadefisica <- ajuste_IC95(df_atividadefisica) # Funcao "ajuste_IC95" esta no script "Funcoes.R"




# OBITOS ATRIBUIVEIS ---------------------------------

# vincular a coluna "faixa_etaria" do data frame dos dados de saude com a da atividade fisica nos cenarios 
indices_df10 <- match(df_atividadefisica$faixa_etaria,df_saude$faixa_etaria)

# obitos

df_atividadefisica <- df_atividadefisica %>%
  mutate(obitos_med = df_atividadefisica$FAP_med_vari * df_saude$obitos_2019_causanat[indices_df10])

df_atividadefisica <- df_atividadefisica %>%
  mutate(obitos_inf = df_atividadefisica$FAP_inf_vari * df_saude$obitos_2019_causanat[indices_df10])

df_atividadefisica <- df_atividadefisica %>%
  mutate(obitos_sup = df_atividadefisica$FAP_sup_vari * df_saude$obitos_2019_causanat[indices_df10])


# Apagar data frames e objetos intermediarios -----------------------------

rm(df_atividadefisica_base)
