library(here)

# Script para importar o csv dos dados de saude e gerar o data frame "df_saude", utilizado no modelo

# Importar o csv e gerar o data frame
caminho_completo <- here("csv", "obitos_2019_SIM_poa.csv")
df_saude<-read.csv2(caminho_completo)