# Este e o console principal do modelo, basta rodar os comandos deste script para ativar o modelo e obter os resultados

# Para um funcionamento mais fluido, chame um script por vez (chamar todos simultaneamente pode gerar erros)


# Gerenciamento de bibliotecas utilizadas ---------------------------------

# Instalacao bibliotecas
# Opcional - realizar na primeira vez que rodar o modelo na maquina (tirar a "#" da frente de "source")
#source("1_Instalar_bibliotecas_se_necessario.R")

# Carregar bibliotecas - rodar quando iniciar a sessao
source("2_Carregar_bibliotecas.R") #rodar quando iniciar a sessao


# Instalar as funcoes exclusivas do modelo --------------------------------
source("3_Funcoes.R")


# Instalar os dados de saude ----------------------------------------------
source("4_Dados_de_saude.R")



# Funcionamento dos modelos preliminares

source("5_Modelo_preliminar_1_Populacao.R")
source("6_Modelo_preliminar_2_Padroes_mobilidade_urbana.R")
source("7_Modelo_preliminar_3_Poluicao_do_ar.R")
source("8_Modelo_preliminar_4_Atividade_fisica.R")


# Funcionamento dos modulos do modelo AIS

source("9_Modelo_AIS_Modulo_Poluicao_do_ar.R")
source("10_Modelo_AIS_Modulo_Atividade_fisica.R")
source("11_Modelo_AIS_Modulo_Sinistros_de_transito.R")


# Resultados (csv e grafico) ----------------------------------------------

source("12_Resultados.R")
