library(data.table)
library(tidyverse)


Abates <- fread("Abates.csv")
# 3 tipos diferentes de abates: Normal, Sanitário, Emergência
# Abates$MMA_CLA_CAR_GR corresponde a camada gordura
# Abates$MMA_CLA_CAR_CF corresponde a classificacao da carcaça na escala EUROP
# Abates$MMA_CLA_CAR_CT corrresponde a classificação de acordo com idade e genero
# Regulamento 1308/2013 para verificar
# TL;DR: escalas ordinais 

# Variaveis continuas: Peso
# Variaveis nominais: Matadouro, Tipo de abate, Raça, Sexo
# Variaveis ordinais: Classificações 

# Analises sugeridas:
# Peso por raça
# 