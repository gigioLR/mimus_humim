# Modelo utilizado para extrair e calcular os indicadores de atividade fisica a partir dos dados da PNS 2019
# Indicadores: prevalencia dos niveis de atividade fisica (insuficientemente ativo, ativo e muito ativo) e gasto energetico semanal no lazer e no trabalho

# Importar microdados -----------------------------------------------------

#vamos baixar os dados brutos da PNS 2019 a partir do pacote PNSIBGE
#fonte: https://rpubs.com/gabriel-assuncao-ibge/pns
#https://github.com/Gabriel-Assuncao/PNSIBGE
#https://cran.r-project.org/web/packages/PNSIBGE/PNSIBGE.pdf

#se quero baixar todas as variaveis
dadosPNS <- get_pns(year=2019, #ano da pesquisa
                    selected = TRUE, #questionario do morador
                    anthropometry = FALSE, #questionario de antropometria
                    labels = FALSE, #categorias
                    design = FALSE) #nao importar como survey


#OBS: se quero baixar somente algumas variaveis (ex: atividade fisica no lazer/tempo livre)
#dadosPNS_teste_bruto <- get_pns(year=2019, vars=c("P034","P035", "P03701", "P03702", "P036"), design=FALSE)

#Selecionando registros validos e calculando peso amostral - summary de verificacao
dadosPNS.1<-dadosPNS %>% filter(V0025A==1) #Selecionado para questionario individual
dadosPNS.1<-dadosPNS.1 %>% mutate(peso_morador_selec=((V00291*(94114/168426190)))) #peso amostral de cada respondente
dadosPNS.1<-dadosPNS.1 %>% filter(!is.na(peso_morador_selec)) #filtra os NA
summary(dadosPNS.1$peso_morador_selec)


# Calcular tempo de atividade física semanal ---------------------------------

#tempo livre/lazer
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_lazer=((P035*(P03701+(P03702/60))))) #gerando o tempo de atividade semanal em horas
dadosPNS.1$tsem_lazer[which(is.na(dadosPNS.1$tsem_lazer))] <- 0 #transforma as respostas vazias de tempo em zero
dadosPNS.1$P036[which(is.na(dadosPNS.1$P036))] <- "00" #insere código de nenhuma atividade

summary(dadosPNS.1$tsem_lazer)

#trabalho
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_trab=((P03904*(P03905+(P03906/60)))))
dadosPNS.1$tsem_trab[which(is.na(dadosPNS.1$tsem_trab))] <- 0

#deslocamento
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_desloc=((P04001*(P04101+(P04102/60))))+(P042*(P04301+(P04302/60))))
dadosPNS.1$tsem_desloc[which(is.na(dadosPNS.1$tsem_desloc))] <- 0


# Calcular gasto energetico semanal (MET) ---------------------------------


#tempo livre/lazer

#Atribuicao do MET para cada atividade
#Referencia:https://stackoverflow.com/questions/51029322/add-new-columns-and-insert-values-in-columns-based-on-value-in-another-column

dadosPNS.1 <- dadosPNS.1 %>%
  mutate(MET_lazer = case_when(
    P036 == "01"      ~4,
    P036 == "02"      ~4,
    P036 == "14"     ~4,
    P036 == "17"     ~4,
    P036 == "03"       ~8,
    P036 == "04"       ~8,
    P036 == "13"      ~8,
    P036 == "05"       ~6,
    P036 == "09"       ~6,
    P036 == "06"       ~7.3,
    P036 == "15"      ~7.3,
    P036 == "07"       ~5.5,
    P036 == "08"       ~3,
    P036 == "10"      ~10.3,
    P036 == "11"      ~6.8,
    P036 == "12"      ~7.3,
    P036 == "16"      ~5.5,
    P036 == "00"       ~0,
    P036 == 99      ~0
  ))
dadosPNS.1<-dadosPNS.1 %>% mutate(METhsem_lazer=tsem_lazer*MET_lazer) # OBS: para calcular as MET-horas usei o tempo real, o ponderado serve para contar o tempo recomendado semanal


summary(dadosPNS.1$METhsem_lazer)


#trabalho
dadosPNS.1<-dadosPNS.1 %>% mutate(METhsem_trab=tsem_trab*4)

#deslocamento
dadosPNS.1<-dadosPNS.1 %>% mutate(METhsem_desloc=tsem_desloc*4)

#total (lazer, trabalho, deslocamento)
dadosPNS.1<-dadosPNS.1 %>% mutate(METhsem_total=METhsem_lazer+METhsem_trab+METhsem_desloc)

#lazer + trabalho (para os calculos)
dadosPNS.1<-dadosPNS.1 %>% mutate(METhsem_lazertrab = METhsem_lazer + METhsem_trab)


summary(dadosPNS.1$METhsem_lazertrab)


# Designar pessoas insuficientemente ativas ---------------------------------

# Ha uma diferenca entre os criterios da PNS e os adotados aqui
# a PNS considera pessoas "ativas" as que realizam 2,5 h (150 min) de atividade fisica semanal moderada ou 1,25 h (75 min) de atividade fisica semanal intensa
# No entanto, aqui utilizamos tres niveis de atividade fisica, conforme Bauman et al. (2009)
# Link: https://doi.org/10.1186/1479-5868-6-21

# insuficientemente ativo (low): gasto metabolico semanal menor que 10 MET-horas (600 MET-minutos)
# ativo (moderate): gasto metabolico semanal maior ou igual a 10 MET-horas e menor que 50 MET-horas (600 a 3000 MET-minutos)
# muito ativo (high): gasto metabolico semanal maior ou igual a 50 MET-horas (3000 MET-minutos)



# insuficientemente ativo
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(insuf_ativo = case_when(
    METhsem_total < 10        ~1,
    METhsem_total >= 10    ~0
  ))

# ativo
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(ativo = case_when(
    METhsem_total >= 10 & METhsem_total < 50     ~1,
    METhsem_total < 10 | METhsem_total >= 50     ~0
  ))


# muito ativo
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(muito_ativo = case_when(
    METhsem_total >= 50      ~1,
    METhsem_total < 50    ~0
    
  ))



summary(dadosPNS.1$insuf_ativo)
summary(dadosPNS.1$ativo)
summary(dadosPNS.1$muito_ativo)

mean(dadosPNS.1$ativo)+mean(dadosPNS.1$insuf_ativo)+mean(dadosPNS.1$muito_ativo) # afericao dos dados



#### Definicoes de abrangências ####

#Sexo
dadosPNS.1 <- dadosPNS.1 %>% rename(Sexo=C006)
dadosPNS.1$Sexo<-factor(dadosPNS.1$Sexo, levels=c(1,2), labels=c("Masculino", "Feminino"))
summary(dadosPNS.1$Sexo)

#Estados - UFs
dadosPNS.1 <- dadosPNS.1 %>% rename(Unidades_da_Federacao=V0001)
dadosPNS.1$Unidades_da_Federacao<-factor(dadosPNS.1$Unidades_da_Federacao, levels=c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                                         label=c("Rondonia","Acre","Amazonas","Roraima","Para","Amapa","Tocantins","Maranhao","Piaui","Ceara",
                                                 "Rio Grande do Norte","Paraiba","Pernambuco","Alagoas","Sergipe","Bahia",
                                                 "Minas Gerais","Espirito Santo","Rio de Janeiro","Sao Paulo",
                                                 "Parana","Santa Catarina","Rio Grande do Sul", 
                                                 "Mato Grosso do Sul","Mato Grosso","Goias","Distrito Federal"))
summary(dadosPNS.1$Unidades_da_Federacao)

#Faixas Etárias
dadosPNS.1 <-  dadosPNS.1 %>% mutate(faixa_idade=cut(C008,
                                                     breaks = c(20, 65, Inf),
                                                     labels = c("20 a 64 anos", "65 anos ou mais"), 
                                                     ordered_result = TRUE, right = FALSE))
summary(dadosPNS.1$faixa_idade)



####TRANSFORMANDO EM SURVEY####

dadosPNS.1_survey <-svydesign(id=~UPA_PNS, strat=~V0024, weight=~peso_morador_selec, nest=TRUE, data=dadosPNS.1)


dadosPNS.1_survey_POA <- subset(dadosPNS.1_survey, #objeto survey da PNS
                                Unidades_da_Federacao=="Rio Grande do Sul" & V0031==1 #filtrando respostas por UF (RS) e somente para capital
)

svymean(x=~muito_ativo, design=dadosPNS.1_survey_POA, na.rf=TRUE)
svymean(x=~ativo, design=dadosPNS.1_survey_POA, na.rf=TRUE)
svymean(x=~insuf_ativo, design=dadosPNS.1_survey_POA, na.rf=TRUE)


# separando por faixa etária: "20 a 64 anos", "65 anos ou mais"

dadosPNS.1_survey_POA_20a64 <- subset(dadosPNS.1_survey_POA, faixa_idade == "20 a 64 anos")
dadosPNS.1_survey_POA_65mais <- subset(dadosPNS.1_survey_POA, faixa_idade == "65 anos ou mais")

# PREVALENCIA DOS NIVEIS DE ATIVIDADE FISICA: proporcao de pessoas ativas, muito ativas e insuficientemente ativas ####

# populacao 20 a 64 anos
prev_20a64_insuf_ativo <- svymean(x=~insuf_ativo, design=dadosPNS.1_survey_POA_20a64, na.rf=TRUE)
prev_20a64_ativo <- svymean(x=~ativo, design=dadosPNS.1_survey_POA_20a64, na.rf=TRUE)
prev_20a64_muito_ativo <- svymean(x=~muito_ativo, design=dadosPNS.1_survey_POA_20a64, na.rf=TRUE)

# populacao 65 anos ou mais
prev_65mais_insuf_ativo <- svymean(x=~insuf_ativo, design=dadosPNS.1_survey_POA_65mais, na.rf=TRUE)
prev_65mais_ativo <- svymean(x=~ativo, design=dadosPNS.1_survey_POA_65mais, na.rf=TRUE)
prev_65mais_muito_ativo <- svymean(x=~muito_ativo, design=dadosPNS.1_survey_POA_65mais, na.rf=TRUE)


# SEPARANDO POR NIVEL DE ATIVIDADE FISICA #### 

# criando os subconjuntos

# populacao 20 a 64 anos
dadosPNS.1_survey_POA_20a64_insuf_ativo <- subset(dadosPNS.1_survey_POA_20a64, insuf_ativo == 1)
dadosPNS.1_survey_POA_20a64_ativo <- subset(dadosPNS.1_survey_POA_20a64, ativo == 1)
dadosPNS.1_survey_POA_20a64_muito_ativo <- subset(dadosPNS.1_survey_POA_20a64, muito_ativo == 1)

# populacao 65 anos ou mais
dadosPNS.1_survey_POA_65mais_insuf_ativo <- subset(dadosPNS.1_survey_POA_65mais, insuf_ativo == 1)
dadosPNS.1_survey_POA_65mais_ativo <- subset(dadosPNS.1_survey_POA_65mais, ativo == 1)
dadosPNS.1_survey_POA_65mais_muito_ativo <- subset(dadosPNS.1_survey_POA_65mais, muito_ativo == 1)


# CALCULO DO GASTO METABOLICO MEDIO/MEDIANO (ver o melhor de usar) ####

METhsem_lt_20a64_ia <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_20a64_insuf_ativo)
METhsem_lt_20a64_a <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_20a64_ativo)
METhsem_lt_20a64_ma <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_20a64_muito_ativo)

METhsem_lt_65mais_ia <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_65mais_insuf_ativo)
METhsem_lt_65mais_a <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_65mais_ativo)
METhsem_lt_65mais_ma <- svymean(x=~METhsem_lazertrab, design=dadosPNS.1_survey_POA_65mais_muito_ativo)

# ajuste do gasto energetico semanal para populacao de 65 anos ou mais

METhsem_lt_65mais_ia <- METhsem_lt_65mais_ia*0.75
METhsem_lt_65mais_a <- METhsem_lt_65mais_a*0.75
METhsem_lt_65mais_ma <- METhsem_lt_65mais_ma*0.75

# MONTAR DATA FRAME DAS POPULACOES ------------------------------

caminho_completo <- here("csv", "data_frame_basico_faixaetaria.csv")
df_perfilpop_af_PNS<-read.csv2(caminho_completo)

# para cada nivel de atividade fisica: prevalencia, MET-horas semanais no lazer e no trabalho

# insuficientemente ativo

df_perfilpop_af_PNS$prev_insu_ativo <- c(prev_20a64_insuf_ativo,
                                         prev_65mais_insuf_ativo)

df_perfilpop_af_PNS$METhsem_lt_insu_ativo <- c(METhsem_lt_20a64_ia,
                                               METhsem_lt_65mais_ia)

# ativo

df_perfilpop_af_PNS$prev_ativo <- c(prev_20a64_ativo,
                                    prev_65mais_ativo)

df_perfilpop_af_PNS$METhsem_lt_ativo <- c(METhsem_lt_20a64_a,
                                          METhsem_lt_65mais_a)

# muito ativo

df_perfilpop_af_PNS$prev_muito_ativo <- c(prev_20a64_muito_ativo,
                                          prev_65mais_muito_ativo)

df_perfilpop_af_PNS$METhsem_lt_muito_ativo <- c(METhsem_lt_20a64_ma,
                                                METhsem_lt_65mais_ma)


# Apagar data frames e objetos intermediarios -----------------------------

rm(dadosPNS)
rm(dadosPNS.1)
rm(dadosPNS.1_survey)
rm(dadosPNS.1_survey_POA)
rm(dadosPNS.1_survey_POA_20a64)
rm(dadosPNS.1_survey_POA_20a64_ativo)
rm(dadosPNS.1_survey_POA_20a64_insuf_ativo)
rm(dadosPNS.1_survey_POA_20a64_muito_ativo)
rm(dadosPNS.1_survey_POA_65mais)
rm(dadosPNS.1_survey_POA_65mais_ativo)
rm(dadosPNS.1_survey_POA_65mais_insuf_ativo)
rm(dadosPNS.1_survey_POA_65mais_muito_ativo)
rm(METhsem_lt_20a64_a)
rm(METhsem_lt_20a64_ia)
rm(METhsem_lt_20a64_ma)
rm(METhsem_lt_65mais_a)
rm(METhsem_lt_65mais_ia)
rm(METhsem_lt_65mais_ma)
rm(prev_20a64_ativo)
rm(prev_20a64_insuf_ativo)
rm(prev_20a64_muito_ativo)
rm(prev_65mais_ativo)
rm(prev_65mais_insuf_ativo)
rm(prev_65mais_muito_ativo)

