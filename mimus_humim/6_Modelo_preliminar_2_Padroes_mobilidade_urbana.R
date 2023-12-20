# Modelo para calcular os indicadores de mobilidade urbana utilizados como dados de entrada (tempo diario de viagem por modo e distancia total anual por modo)

# FROTA -------------------------------------------------------------------

#referência: https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/

# Carregar o csv com os dados de frota
caminho_completo <- here("csv", "frota_carroemoto.csv")
df_frota_existente <- read.csv2(caminho_completo)

plot(df_frota_existente$Frota~df_frota_existente$Ano, data=df_frota_existente) #visualizar os dados


# Modelo de crescimento logistico

coef(lm(logit(df_frota_existente$Frota/1000000)~df_frota_existente$Ano,data=df_frota_existente)) #coeficientes

POA_frota <- nls(df_frota_existente$Frota~phi1/(1+exp(-(phi2+phi3*df_frota_existente$Ano))),
               start=list(phi1=1000000,phi2=-144.13788626,phi3=0.07185832),data=df_frota_existente,trace=TRUE) # modelo logistico (a partir dos coeficientes usados acima)

summary(POA_frota) # aqui podemos verificar a significancia de cada parametro no modelo

#estabelecer parametros da equacao
phi1 <- coef(POA_frota)[1]
phi2 <- coef(POA_frota)[2]
phi3 <- coef(POA_frota)[3]


# equacao
x <- c(min(df_frota_existente$Ano):2032) # X: anos de abrangencia (colocamos ate 2032 por causa dos cenarios contrafactuais)
y <- phi1/(1+exp(-(phi2+phi3*x))) # Y: frota estimada

# construcao do data frame com a frota prevista
df_frota_predicao <- data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)

df_frota_predicao$y <- round(df_frota_predicao$y, digits = 0)
df_frota_predicao

# ver como ficou
ggplot(data=df_frota_existente,aes(x=Ano,y=Frota))+
  geom_point(color='blue',size=5)+theme_bw()+
  labs(x='Ano',y='Frota (Automóvel e Motocicleta)')+
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020,2025,2030,2035,2040,2050))+
  scale_y_continuous(breaks=c(0,400000,600000,800000))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12))+
  geom_line(data=df_frota_predicao,aes(x=x,y=y), linewidth=1)

# Sintese: dados de frota para os cenarios
df_frota_cenarios <- data.frame(ano = c(2003, 2019, 2032))
df_frota_cenarios$frota_TI <- c(df_frota_existente[3,2],
                                df_frota_existente[19,2],
                                df_frota_predicao[32,2])

df_frota_cenarios$frota_TP <- 1360

df_frota_cenarios

# apagar data frames e objetos intermediarios
rm(df_frota_existente)
rm(df_frota_predicao)
rm(phi1)
rm(phi2)
rm(phi3)
rm(x)
rm(y)
rm(POA_frota)

# PASSAGEIROS ANUAIS DE ONIBUS ---------------------------------------------------

# Carregar o csv com os dados de passageiros anuais
caminho_completo <- here("csv", "passageiros_onibus_ano_EPTC.csv")
df_passageiros_onibus_existente <- read.csv2(caminho_completo)
df_passageiros_onibus_existente

# Ajustar o modelo de regressão linear
POA_passageiros_onibus <- lm(total_geral ~ ano, data = df_passageiros_onibus_existente)

# Resumo do modelo
summary(POA_passageiros_onibus)

# Ano para o qual você deseja fazer a previsão
ano_predicao <- 2032

# Criar um novo dataframe com o ano desejado
novo_dados <- data.frame(ano = ano_predicao)

# Fazer a previsão
passageiros_onibus_previsao <- predict(POA_passageiros_onibus, newdata = novo_dados)

# Exibir a previsão
print(passageiros_onibus_previsao)


# Sintese: dados de passageiros de onibus para os cenarios

df_passageiros_onibus_cenarios <- data.frame(ano = c(2003, 2019, 2032))
df_passageiros_onibus_cenarios$passageiros_onibus <- c(df_passageiros_onibus_existente$total_geral[6],
                                                        df_passageiros_onibus_existente$total_geral[22],
                                                        passageiros_onibus_previsao)

# apagar data frames e objetos intermediarios
rm(df_passageiros_onibus_existente)
rm(passageiros_onibus_previsao)
rm(novo_dados)
rm(POA_passageiros_onibus)
rm(ano_predicao)

# TOTAL DE VIAGENS --------------------------------------------------------

# Carregar o csv com os dados de passageiros anuais
caminho_completo <- here("csv", "data_frame_basico_ano.csv")
df_viagenstotais <- read.csv2(caminho_completo)

df_viagenstotais$populacao <- c(1333246,
                                df_populacao$pop_estimada[1],
                                df_populacao$pop_estimada[2]) #populacao total de Porto Alegre (EDOM 2003 e estimada no modelo)

df_viagenstotais$viagens_totais <- 2920739*(df_viagenstotais$populacao/df_viagenstotais$populacao[1])


# INDICE DE MOBILIDADE ----------------------------------------------------


# Carregar o csv com os dados
caminho_completo <- here("csv", "indice_mobilidade.csv")
df_indice_mob<-read.csv2(caminho_completo)

# Calcular o valor considerando o total de viagens da matriz origem-destino (procedimento especifico para o caso analisado)
# indice mobilidade matriz od = (total viagens matriz od/total viagens entrevista domiciliar) * indice mobilidade entrevista domiciliar
df_indice_mob$indice_mob_matriz <- (2920739/2203168)*df_indice_mob$indice_mob_edom


# DIVISAO MODAL -----------------------------------------------------------

# Carregar o csv com os valores ja estabelecidos (cenarios B, D e E) - cenarios A e C estao vazios
caminho_completo <- here("csv", "divisao_modal_cenario.csv")
df_dm_cenario<-read.csv2(caminho_completo)

# Calcular a divisao modal nos cenarios A e C ("atualizacao" da EDOM 2003 por meio de proxies)

df_divisaomodal_AeC <- df_viagenstotais
df_divisaomodal_AeC$viagens_totais <- NULL

df_divisaomodal_AeC$frota <- df_frota_cenarios$frota_TI

df_divisaomodal_AeC$passageiros_onibus <- df_passageiros_onibus_cenarios$passageiros_onibus

df_divisaomodal_AeC$passageiros_hab <- df_divisaomodal_AeC$passageiros_onibus/df_divisaomodal_AeC$populacao
df_divisaomodal_AeC$veiculos_hab <- df_divisaomodal_AeC$frota/df_divisaomodal_AeC$populacao

df_divisaomodal_AeC$dm_TI <- (1051898/2920739)*(df_divisaomodal_AeC$veiculos_hab/df_divisaomodal_AeC$veiculos_hab[1])
df_divisaomodal_AeC$dm_TI <- round(df_divisaomodal_AeC$dm_TI, digits = 3)

df_divisaomodal_AeC$dm_TP <- (1155895/2920739)*(df_divisaomodal_AeC$passageiros_hab/df_divisaomodal_AeC$passageiros_hab[1])
df_divisaomodal_AeC$dm_TP <- round(df_divisaomodal_AeC$dm_TP, digits = 3)

df_divisaomodal_AeC$dm_ativos <- 1-(df_divisaomodal_AeC$dm_TI+df_divisaomodal_AeC$dm_TP)
df_divisaomodal_AeC$dm_ativos[1]<- (626064+15336)/2920739
df_divisaomodal_AeC$dm_ativos <- round(df_divisaomodal_AeC$dm_ativos, digits = 3)

df_divisaomodal_AeC$dm_ape <- (626064/2920739)*(df_divisaomodal_AeC$dm_ativos/df_divisaomodal_AeC$dm_ativos[1])
df_divisaomodal_AeC$dm_ape <- round(df_divisaomodal_AeC$dm_ape, digits = 3)

df_divisaomodal_AeC$dm_bici <- (15336/2920739)*(df_divisaomodal_AeC$dm_ativos/df_divisaomodal_AeC$dm_ativos[1])
df_divisaomodal_AeC$dm_bici <- round(df_divisaomodal_AeC$dm_bici, digits = 3)


# inserir os valores calculados (cenarios A e C)
df_dm_cenario[c(1,3),4:7]<-df_divisaomodal_AeC[c(2,3),c(10:11,7:8)]
df_dm_cenario

# apagar data frames e objetos intermediarios
rm(df_divisaomodal_AeC)


# TEMPO MEDIO DIARIO DE VIAGEM POR MODO (h) ---------------------

# Carregar o csv basico (faixa etaria e cenarios)
caminho_completo <- here("csv", "data_frame_basico_faixaetariaxcenario.csv")
df_tempoviagem<-read.csv2(caminho_completo)


# Vincular o data frame novo com os de indice de mobilidade e divisao modal
indices_df1 <- match(df_tempoviagem$faixa_etaria, df_indice_mob$faixa_etaria)
indices_df2 <- match(df_tempoviagem$cenario, df_dm_cenario$cenario)

# Calcula os tempos diretamente usando os índices
df_tempoviagem$t_dia_ape <- df_indice_mob$indice_mob_matriz[indices_df1] * df_dm_cenario$dm_ape[indices_df2] * (16.4/60)
df_tempoviagem$t_dia_bici <- df_indice_mob$indice_mob_matriz[indices_df1] * df_dm_cenario$dm_bici[indices_df2] * (16.4/60)
df_tempoviagem$t_dia_TI <- df_indice_mob$indice_mob_matriz[indices_df1] * df_dm_cenario$dm_TI[indices_df2] * (29.6/60)
df_tempoviagem$t_dia_TP <- df_indice_mob$indice_mob_matriz[indices_df1] * df_dm_cenario$dm_TP[indices_df2] * (49/60)
df_tempoviagem$t_dia_ape_TP <- df_indice_mob$indice_mob_matriz[indices_df1] * df_dm_cenario$dm_TP[indices_df2] * (10/60)

# Mostra o resultado final
print(df_tempoviagem)




# DISTANCIA TOTAL ANUAL POR MODO (km) -------------------------------------

# Carregar o csv basico (cenarios)
caminho_completo <- here("csv", "data_frame_basico_cenario.csv")
df_distancia_ano_cenario<-read.csv2(caminho_completo)

df_distancia_ano_cenario$ano <- c(2019,
                                  rep(2032,4))

# Vincular o data frame novo com os de total de viagens e divisao modal
indices_df3 <- match(df_distancia_ano_cenario$ano, df_viagenstotais$ano)
indices_df4 <- match(df_distancia_ano_cenario$cenario, df_dm_cenario$cenario)



# Calcular os valores por modo

# total de viagens diarias por modo * tempo medio de viagem por modo * velocidade em km/h * 365 dias
# total de viagens diarias por modo = total de viagens diarias * divisao modal

# a pe (assumimos como velocidade media 5 km/h, vide ITHIM)
df_distancia_ano_cenario$dist_total_ape <- df_viagenstotais$viagens_totais[indices_df3]*df_dm_cenario$dm_ape[indices_df4]*(16.4/60)*5*365

# bicicleta (assumimos como velocidade media 15 km/h, vide ITHIM)
df_distancia_ano_cenario$dist_total_bici <- df_viagenstotais$viagens_totais[indices_df3]*df_dm_cenario$dm_bici[indices_df4]*(16.4/60)*15*365

# transporte individual motorizado - TI (assumimos como velocidade media 25 km/h, vide ITHIM)
df_distancia_ano_cenario$dist_total_TI <- df_viagenstotais$viagens_totais[indices_df3]*df_dm_cenario$dm_TI[indices_df4]*(29.6/60)*25*365


# transporte publico (TP)

# existe a distancia total diaria percorrida pelo onibus no GTFS (GTFS2EMIS)
# calculei a distancia media por viagem em 2019 e multipliquei pelo total de cada cenario --> assumi que se mantem constante 


dist_viagem_TP <- 1181751.33/(df_viagenstotais$viagens_totais[2]*df_dm_cenario$dm_TP[1])

df_distancia_ano_cenario$dist_total_TP <- (df_viagenstotais$viagens_totais[indices_df3]*df_dm_cenario$dm_TP[indices_df4])*dist_viagem_TP*365

df_distancia_ano_cenario

# apagar data frames e objetos intermediarios
rm(dist_viagem_TP)
