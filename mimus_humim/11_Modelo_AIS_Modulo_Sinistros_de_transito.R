
# CONCEITO ------------------------------------------------------------

# Este módulo é uma adaptação do ITHIM Califórnia (Dr. Neil Maizlish)

# Um sinistro tem duas partes: a vitima (victim) e o colidente (colliding). 
# Para calcular os impactos nos sinistros de trânsito, utilizamos uma matriz de sinistros de trânsito considerando combinações entre os modais analisados no modelo.
# Vítima é o modal da pessoa que faleceu (geralmente é o mais vulnerável) enquanto o colidente é o outro modal envolvido (geralmente é o menos vulnerável).

# Em ordem decrescente de vulnerabilidade, os modos analisados são:
# A pé
# Bicicleta
# TI: automóvel (condutor + passageiro) e motocicleta (condutor + passageiro)
# TP: ônibus (urbano, metropolitano, intermunicipal), lotação (micro-ônibus), trem

# A partir da distribuicao dos obitos por par vitima-colidente e da distancia "harmonizadas" para o par, calculamos os obitos


# ESTABELECIMENTO DA TAXA DE OBITOS POR PAR NO CENARIO BASE -------------------------------

# construcao do data frame
caminho_completo <- here("csv", "data_frame_basico_faixaetaria.csv")
df_sinistrostransito_base<-read.csv2(caminho_completo)

# obitos para cada par em 2019
df_sinistrostransito_base$obitos2019_apebici <- c(0,1) #obitos em 2019 para o par a pe/bicicleta
df_sinistrostransito_base$obitos2019_apeTI <- c(11,13) #obitos em 2019 para o par a pe/TI
df_sinistrostransito_base$obitos2019_apeTP <- c(2,2) #obitos em 2019 para o par a pe/TP
df_sinistrostransito_base$obitos2019_apeNA <- c(4,0) #obitos em 2019 para o par a pe/nao especificado

df_sinistrostransito_base$obitos2019_biciTI <- c(1,0) #obitos em 2019 para o par bicicleta/TI
df_sinistrostransito_base$obitos2019_biciTP <- c(1,0) #obitos em 2019 para o par bicicleta/TP

df_sinistrostransito_base$obitos2019_TIbici <- c(1,0) #obitos em 2019 para o par TI/bicicleta
df_sinistrostransito_base$obitos2019_TITI <- c(16,1) #obitos em 2019 para o par TI/TI
df_sinistrostransito_base$obitos2019_TITP <- c(8,0) #obitos em 2019 para o par TI/TP
df_sinistrostransito_base$obitos2019_TINA <- c(15,2) #obitos em 2019 para o par TI/nao especificado

df_sinistrostransito_base$obitos2019_TPTP <- c(0,1) #obitos em 2019 para o par TP/TP
df_sinistrostransito_base$obitos2019_TPNA <- c(1,1) #obitos em 2019 para o par TP/nao especificado



# taxa de obitos por km para cada par
df_sinistrostransito_base$taxa_obitos_apebici <- df_sinistrostransito_base$obitos2019_apebici/((df_distancia_ano_cenario$dist_total_ape[1]*df_distancia_ano_cenario$dist_total_bici[1])**0.5)
df_sinistrostransito_base$taxa_obitos_apeTI <- df_sinistrostransito_base$obitos2019_apeTI/((df_distancia_ano_cenario$dist_total_ape[1]*df_distancia_ano_cenario$dist_total_TI[1])**0.5)
df_sinistrostransito_base$taxa_obitos_apeTP <- df_sinistrostransito_base$obitos2019_apeTP/((df_distancia_ano_cenario$dist_total_ape[1]*df_distancia_ano_cenario$dist_total_TP[1])**0.5)
df_sinistrostransito_base$taxa_obitos_apeNA <- df_sinistrostransito_base$obitos2019_apeNA/df_distancia_ano_cenario$dist_total_ape[1]

df_sinistrostransito_base$taxa_obitos_biciTI <- df_sinistrostransito_base$obitos2019_biciTI/((df_distancia_ano_cenario$dist_total_bici[1]*df_distancia_ano_cenario$dist_total_TI[1])**0.5)
df_sinistrostransito_base$taxa_obitos_biciTP <- df_sinistrostransito_base$obitos2019_biciTP/((df_distancia_ano_cenario$dist_total_bici[1]*df_distancia_ano_cenario$dist_total_TP[1])**0.5)

df_sinistrostransito_base$taxa_obitos_TIbici <- df_sinistrostransito_base$obitos2019_TIbici/(((df_distancia_ano_cenario$dist_total_TI[1]*1.4)*df_distancia_ano_cenario$dist_total_bici[1])**0.5)
df_sinistrostransito_base$taxa_obitos_TITI <- df_sinistrostransito_base$obitos2019_TITI/(((df_distancia_ano_cenario$dist_total_TI[1]*1.4)*df_distancia_ano_cenario$dist_total_TI[1])**0.5)
df_sinistrostransito_base$taxa_obitos_TITP <- df_sinistrostransito_base$obitos2019_TITP/(((df_distancia_ano_cenario$dist_total_TI[1]*1.4)*df_distancia_ano_cenario$dist_total_TP[1])**0.5)
df_sinistrostransito_base$taxa_obitos_TINA <- df_sinistrostransito_base$obitos2019_TINA/(df_distancia_ano_cenario$dist_total_TI[1]*1.4)

df_sinistrostransito_base$taxa_obitos_TPTP <- df_sinistrostransito_base$obitos2019_TPTP/(((df_distancia_ano_cenario$dist_total_TP[1]*32.3)*df_distancia_ano_cenario$dist_total_TP[1])**0.5)
df_sinistrostransito_base$taxa_obitos_TPNA <- df_sinistrostransito_base$obitos2019_TPNA/(df_distancia_ano_cenario$dist_total_TP[1]*32.3)


# CALCULO POR CENARIO -----------------------------------------------------


# Carregar o csv basico (faixa etaria e cenarios)
caminho_completo <- here("csv", "data_frame_basico_faixaetariaxcenario.csv")
df_sinistrostransito<-read.csv2(caminho_completo)


# OBITOS POR PAR EM CADA CENARIO

# Vincular o data frame novo com os de taxa de obitos por distancia e distancia total
indices_df11 <- match(df_sinistrostransito$faixa_etaria, df_sinistrostransito_base$faixa_etaria)
indices_df12 <- match(df_sinistrostransito$cenario, df_distancia_ano_cenario$cenario)


# obitos por par

# a pe/bicicleta
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_apebici_cenario= df_sinistrostransito_base$taxa_obitos_apebici[indices_df11] * ((df_distancia_ano_cenario$dist_total_ape[indices_df12]*df_distancia_ano_cenario$dist_total_bici[indices_df12])**0.5))

# a pe/TI
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_apeTI_cenario= df_sinistrostransito_base$taxa_obitos_apeTI[indices_df11] * ((df_distancia_ano_cenario$dist_total_ape[indices_df12]*df_distancia_ano_cenario$dist_total_TI[indices_df12])**0.5))

# a pe/TP
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_apeTP_cenario= df_sinistrostransito_base$taxa_obitos_apeTP[indices_df11] * ((df_distancia_ano_cenario$dist_total_ape[indices_df12]*df_distancia_ano_cenario$dist_total_TP[indices_df12])**0.5))

# a pe/nao especificado
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_apeNA_cenario= df_sinistrostransito_base$taxa_obitos_apeNA[indices_df11] * df_distancia_ano_cenario$dist_total_ape[indices_df12])

# bicicleta/TI
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_biciTI_cenario= df_sinistrostransito_base$taxa_obitos_biciTI[indices_df11] * ((df_distancia_ano_cenario$dist_total_bici[indices_df12]*df_distancia_ano_cenario$dist_total_TI[indices_df12])**0.5))

# bicicleta/TP
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_biciTP_cenario= df_sinistrostransito_base$taxa_obitos_biciTP[indices_df11] * ((df_distancia_ano_cenario$dist_total_bici[indices_df12]*df_distancia_ano_cenario$dist_total_TP[indices_df12])**0.5))

# TI/bicicleta
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TIbici_cenario= df_sinistrostransito_base$taxa_obitos_TIbici[indices_df11] * (((df_distancia_ano_cenario$dist_total_TI[indices_df12]*1.4)*df_distancia_ano_cenario$dist_total_bici[indices_df12])**0.5))

# TI/TI
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TITI_cenario= df_sinistrostransito_base$taxa_obitos_TITI[indices_df11] * (((df_distancia_ano_cenario$dist_total_TI[indices_df12]*1.4)*df_distancia_ano_cenario$dist_total_TI[indices_df12])**0.5))

# TI/TP
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TITP_cenario= df_sinistrostransito_base$taxa_obitos_TITP[indices_df11] * (((df_distancia_ano_cenario$dist_total_TI[indices_df12]*1.4)*df_distancia_ano_cenario$dist_total_TP[indices_df12])**0.5))

# TI/nao especificado
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TINA_cenario= df_sinistrostransito_base$taxa_obitos_TINA[indices_df11] * (df_distancia_ano_cenario$dist_total_TI[indices_df12]*1.4))

# TP/TP
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TPTP_cenario= df_sinistrostransito_base$taxa_obitos_TPTP[indices_df11] * (((df_distancia_ano_cenario$dist_total_TP[indices_df12]*32.3)*df_distancia_ano_cenario$dist_total_TP[indices_df12])**0.5))

# TP/nao especificado
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_TPNA_cenario= df_sinistrostransito_base$taxa_obitos_TPNA[indices_df11] * (df_distancia_ano_cenario$dist_total_TP[indices_df12]*32.3))


# calcular o total de obitos por sinistros no cenario
df_sinistrostransito$obitos_2019_cenario <- df_sinistrostransito$obitos_apebici_cenario + df_sinistrostransito$obitos_apeTI_cenario + df_sinistrostransito$obitos_apeTP_cenario + df_sinistrostransito$obitos_apeNA_cenario + df_sinistrostransito$obitos_biciTI_cenario + df_sinistrostransito$obitos_biciTP_cenario + df_sinistrostransito$obitos_TIbici_cenario + df_sinistrostransito$obitos_TITI_cenario + df_sinistrostransito$obitos_TITP_cenario + df_sinistrostransito$obitos_TINA_cenario + df_sinistrostransito$obitos_TPTP_cenario + df_sinistrostransito$obitos_TPNA_cenario


# separar os obitos no cenario base
#funcao "estabelecer_cenario_base" esta em "Funcoes.R"
df_sinistrostransito_cenariobase <- estabelecer_cenario_base(df_sinistrostransito)

# Vincular o data frame novo com os de taxa de obitos por distancia e distancia total
indices_df13 <- match(df_sinistrostransito$faixa_etaria, df_sinistrostransito_cenariobase$faixa_etaria)


# Fracao atribuivel populacional (FAP) ------------------------------------------
 
# O total de obitos nos pares nao representa o total de obitos por sinistros de transito (alguns registros de obitos nao identificam vitima), por isso calculamos a FAP
df_sinistrostransito <- df_sinistrostransito %>%
  mutate(FAP= (df_sinistrostransito$obitos_2019_cenario-df_sinistrostransito_cenariobase$obitos_2019_cenario[indices_df13])/df_sinistrostransito_cenariobase$obitos_2019_cenario[indices_df13])



# Variacao nos obitos -----------------------------------------------------

# Vincular o data frame novo com o data frame de dados de saude
indices_df14 <- match(df_sinistrostransito$faixa_etaria, df_sinistrostransito_cenariobase$faixa_etaria)


df_sinistrostransito <- df_sinistrostransito %>%
  mutate(obitos_sinistros_2019_vari = df_sinistrostransito$FAP * df_saude$obitos_2019_sinistros[indices_df14])


# Apagar data frames e objetos intermediarios -----------------------------

rm(df_sinistrostransito_base)
rm(df_sinistrostransito_cenariobase)