
read.csv2("Listagem de Pequenos Ruminantes.csv")
dados <- read.csv2("Listagem de Pequenos Ruminantes.csv")
View(dados)

library(dplyr)
library(tidyr)
dim(dados)
pop1 <- nrow(dados)

#função amostra para uma população x
amostra <- function(prev, erro, pop){
  n <- (prev * (1-prev) * 1.96^2) / erro^2
  x <- n / (1 + ((n-1)/pop))
  return(x)
}

#isto é a nossa amostra#
amostra.1 <- amostra(0.5, 0.05, pop1) %>% print

#função que a Bia criou para dizer quantas explorações devemos avaliar por brigada#
exp_brigada <- mutate(summarize(group_by(dados, Brigada),count=n()),"prop" = (amostra.1/pop1)*count) %>%
  mutate("prop_round"=ceiling(prop))
View(exp_brigada)

#tabela que contém apenas uma coluna com os números da brigada e a amostra a 
#analisar por brigada
amostra_brigada <- select(exp_brigada, Brigada, prop_round) 
View(amostra_brigada)

#Tem por ordem o número da amostra para cada brigada#
prop_round <- amostra_brigada$prop_round %>% print

#Tabela só com a Birgada e o Número Interno de cada exloração#
tabela1 <- select(dados, Brigada,Número.Interno) %>% arrange(Brigada)
View(tabela1)

#vetor brigadas - tem o número das brigadas por ordem ascendente
brigadas <- as.character(unique(tabela1$Brigada))

#função que extrai em vetor os id das explorações da brigada x#
id_exp_brigada <- function(x){
  tabela1 %>% filter(Brigada == x) %>% 
    pull(Número.Interno)
}

#É uma lista com a função acima em loop
#É uma lista com 20 entradas, correspondentes às brigadas por ordem ascendente
#com os vetores correspondentes à frente
#cada vetor tem os id's das exploraçôes que correspondem à brigada
lista_exp_brigada <- lapply(brigadas, id_exp_brigada) %>% print
View(lista_exp_brigada)
#o problema é que cada entrada está numerada de 1 a 20 em vez de ter o número da brigada
#no entanto como ordenámos em cima significa que a primeira entrada é a brigada 3 e daí por diante


#esta função tira uma amostra de cada entrada da lista
#como na lista os vetores estão ordenados por brigada e no vetor prop_round também
#significa que a posição 1 destes dois corresponde à mesma brigada
#portanto ao aplica p.e. x=1 vai tirar uma amostra aleatória da primeira entrada da lista, em
#quantidade correspondente ao valor da primeira posição do vetor prop_round
amostra_aleatoria <- function(x){
  sample(lista_exp_brigada[[x]], prop_round[x])
}

#É um loop com a função acima
#coloquei um vetor de 1 a 20 porque tanto a lista como prop_round tem 20 elementos
#assim esta função vai aplicar de cada vez, cada número do vetor ao x da função acima
#o resultado é uma nova lista, que é aleatória e muda todas as vezes que damos print
Amostra_Final <- lapply(c(1:20), amostra_aleatoria) %>% cbind(brigadas) %>% print
View(Amostra_Final)

