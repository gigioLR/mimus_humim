# Modelo para estimar a populacao total a partir dos dados do Censo

#referência: https://rstudio-pubs-static.s3.amazonaws.com/752_54c50d2916a34e87be430b97c6b5abbe.html


# Importar o csv e gerar o data frame dos dados dos Censos
caminho_completo <- here("csv", "populacao_censos_1970a2010_poa.csv")
df_censo <- read.csv2(caminho_completo)

#Here's the data

plot(populacao~ano, data=df_censo) #visualizar os dados

# Modelo de crescimento logistico da populacao ----------------------------

POA <- nls(populacao ~ SSlogis(ano, phi1, phi2, phi3), data = df_censo) #modelo

summary(POA) #parametros e sua significancia

alpha <- coef(POA)  #extraindo coeficientes
plot(populacao ~ ano, data = df_censo, main = "Modelo de crescimento logistico da população em Porto Alegre", 
     xlab = "Ano", ylab = "Populacao", xlim = c(1970, 2040), ylim = c(0, 2e+06))  #grafico com os dados do Censo
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")  #extrapolacao



# Estimar a populacao -----------------------------------------------------

df_populacao <- data.frame(ano = c(2019, 2032))

predict.POA <- predict(POA, df_populacao)
predict.POA

df_populacao$pop_estimada <- predict.POA
df_populacao

#Populacao em 2019 (estimada): 1429861 
#Populacao em 2032 (estimada): 1445799


# Limpar data frames e objetos intermediarios ---------------------------------------

rm(df_censo)
rm(predict.POA)
rm(alpha)
rm(POA)