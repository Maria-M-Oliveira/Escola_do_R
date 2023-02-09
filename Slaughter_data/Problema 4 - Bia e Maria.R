library(tidyverse)
library(data.table)
library(lubridate)
library(freqtables)
library(plotly)
library(rstatix)
library(ggplot2)

Abates <- fread("./Abates.csv") 

# 3 tipos diferentes de abates: Normal, Sanitário, Emergência
# Abates$MMA_CLA_CAR_GR corresponde a camada gordura
# Abates$MMA_CLA_CAR_CF corresponde a classificacao da carcaça na escala SEUROP
# Abates$MMA_CLA_CAR_CT corrresponde a classificação de acordo com idade e genero
# Regulamento 1308/2013 para verificar


# Temos de fazer se nao me engano a parte de Peso ao abate tendo em conta a raça, idade e sexo
##Descrição das variáveis
#Peso: variável quantitativa contínua
#Sexo: variável qualitativa nominal (binomial)
#Raça: variável qualitativa nominal
#Idade: variável quantitativa contínua

# Assim, vou selecionar as colunas que me interessam e garantir que sao da classe certa
# Calculei idade ao abate usando funcao do lubridate %--% e arredondei a 1 casa decimal
# Peso convertido a numerico, necessario substituir as , por .
# Queria agrupar as racas por carne e leite para ser mais facil de ter um grafico facil de ler mas tou meio presa
carne<- c("CARNE, IND.| CRUZADO CHAROL�S|CRUZADO DE CARNE|MERTOLENGA|CRUZADO LIMOUSINE|CHAROLESA|CRUZADO BBB | 
          CRUZADO DE BLONDE|LIMOUSINE|ALENTEJANA|CRUZADO SIMMENTAL-FLECKVIEH|BLONDE D AQUITAINE|RAMO GRANDE|BARROSA|
          BRAVA DE LIDE|SALERS|CRUZADO ABERDEEN-ANGUS|MIRANDESA|SIMMENTAL-FLECKVIEH|HEREFORD|MARINHOA|CACHENA|BLANC - BLUE BELGE|
          MINHOTA|CRUZADO ALENTEJANO|ABERDEEN-ANGUS|AROUQUESA|JARMELISTA|GARVONESA")
leite<- c("FRISIA|Tipo Fr�sia|LEITE, IND.|JERSEY|NORUEGUESA")
Abates_peso <- select(Abates, Data_abate, Data_nasc, Peso, Raca, Sexo) %>% 
  mutate(
    idade_ao_abate= round ((Data_nasc %--% Data_abate) / years(1),1),
    idade_ao_abate_dias= (Data_nasc %--% Data_abate) / days(1),
    Peso = as.numeric(str_replace(Peso, ",", ".")),
    Raca_agrupada = str_replace(Raca, leite, "Leite")
  ) %>% 
  mutate(Raca = (str_replace(Raca,c("<",">"),"")))

Abates_peso$Raca_agrupada <- ifelse(Abates_peso$Raca_agrupada == "Leite", "Leite", "Carne")

##ESTATÍSTICA DESCRITIVA
# Acho que repeti as tabelas de frequencia, mas if anything temos 2 maneiras possiveis de as fazer
# Fiz à brute force e com library freqtables

#VARIÁVEIS QUALITATIVAS 
# Raca
Abates_prop_raca <- Abates_peso %>% 
  group_by(Raca) %>% 
  summarise(Frequencia = n()) %>% 
  mutate(Proporcao= Frequencia / sum(Frequencia))%>% 
  mutate(Percentagem = (Frequencia / sum(Frequencia) * 100) %>% round(3))

Abates_prop_raca_agrupada <- Abates_peso %>% 
  group_by(Raca_agrupada) %>% 
  summarise(Frequencia = n()) %>% 
  mutate(Proporcao= Frequencia / sum(Frequencia))%>% 
  mutate(Percentagem = (Frequencia / sum(Frequencia) * 100) %>% round(3))

Freq_abates_raca <- Abates_peso %>% 
  freq_table(Raca)

# Sexo
Abates_prop_sexo <- Abates_peso %>% 
  group_by(Sexo) %>% 
  summarise(Frequencia = n()) %>% 
  mutate(Proporcao = Frequencia / sum(Frequencia))%>% 
  mutate(Percentagem = (Frequencia / sum(Frequencia) * 100) %>% round(3))

Freq_abates_sexo <- Abates_peso %>% 
  freq_table(Sexo)


#VARIÁVEIS QUANTITATIVAS CONTÍNUAS
# Idade
stats_idade <- Abates_peso %>% 
  get_summary_stats(idade_ao_abate)

Abates_peso$idade_range <- cut(Abates_peso$idade_ao_abate, breaks= c(0,1,2,3,4,5,10,20,30),
                               labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","+20"))

Abates_prop_idade <- Abates_peso %>% 
  group_by(idade_range) %>% 
  summarise(Frequencia = n()) %>% 
  mutate(Proporcao = Frequencia / sum(Frequencia))%>% 
  mutate(Percentagem = (Frequencia / sum(Frequencia) * 100) %>% round(3))

Freq_abates_idade <- Abates_peso %>% 
  freq_table(idade_range)


#Peso
stats_peso <- Abates_peso %>% 
  get_summary_stats(Peso)

Abates_peso$peso_range <- cut(Abates_peso$Peso, breaks= c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,800),
                              labels=c("0-50","50-100","100-150","150-200","200-250","250-300","300-350","350-400", "400-450", "450-500", "500-550", "550-600", "600-650", "650-700", "+700"))

Abates_prop_peso <- Abates_peso %>% 
  group_by(peso_range) %>% 
  summarise(Frequencia = n()) %>% 
  mutate(Proporcao = Frequencia / sum(Frequencia))%>% 
  mutate(Percentagem = (Frequencia / sum(Frequencia) * 100) %>% round(3))
#ou
Freq_abates_peso <- Abates_peso %>% 
  freq_table(peso_range) %>%
  mutate(Percent = (n / sum(n) * 100) %>% round(3)) #a diferença está nos arrendondamentos e no facto de que nao inclui NA's


#TABELAS CONTIGÊNCIA 2VARIÁVEIS
#peso-raça
tabela_PxR <- as.data.frame.matrix(table(Abates_peso$peso_range,Abates_peso$Raca))
tabela_PxRA <- as.data.frame.matrix(table(Abates_peso$peso_range,Abates_peso$Raca_agrupada))
tabela_PxR
tabela_PxRA
#peso-sexo
tabela_PxS <- as.data.frame.matrix(table(Abates_peso$peso_range,Abates_peso$Sexo))
tabela_PxS
#peso-idade
tabela_PxI <- as.data.frame.matrix(table(Abates_peso$peso_range,Abates_peso$idade_range))
tabela_PxI


# REPRESENTAÇÃO GRÁFICA DA ANÁLISE DESCRITIVA
 
graph_abates_sexo <- Abates_prop_sexo %>%
  plot_ly(x = ~Frequencia, y = ~Sexo, type = 'bar') %>%
  layout(title = "Abates por Sexo") %>%
  layout(xaxis = list(title = "Frequencia"), yaxis = list(title = "Sexo"))  
graph_abates_sexo

pie_abates_sexo <- Abates_prop_sexo %>%
  plot_ly(labels = ~Sexo, values = ~Percentagem, type = 'pie') %>%
  layout(title = "Abates por Sexo",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie_abates_sexo ##VAle a pena? 


graph_abates_raca <- Abates_prop_raca %>% 
  plot_ly(x = ~Frequencia, y = ~Raca, type = 'bar') %>% 
  layout(title = "Abates por Raca") %>%
  layout(xaxis = list(title = "Frequência"), yaxis = list(title = "Raça"))
graph_abates_raca_agrupada <- Abates_prop_raca_agrupada %>% 
  plot_ly(y = ~Frequencia, x = ~Raca_agrupada, type = 'bar') %>% 
  layout(title = "Abates por Tipo de Raça") %>%
  layout(yaxis = list(title = "Frequência"), xaxis = list(title = "Raça"))
graph_abates_raca
graph_abates_raca_agrupada

# Fiz um grafico de barras por range de idades (no plotly)
graph_abates_idade <- Abates_prop_idade %>% 
  plot_ly(x=~Frequencia, y=~idade_range, type="bar") %>%
  layout(title="Abates por Idade") %>%
  layout(xaxis = list(title = "Frequência"), yaxis = list(title = "Idade"))  
graph_abates_idade

#Histograma idades
histo_idade <- Abates_peso %>% 
  plot_ly(x=~idade_range, type="histogram") %>%
  layout(title="Abates por Idade") %>%
  layout(xaxis = list(title = "Idade"), yaxis = list(title = "Frequência"))
histo_idade

#grafico plotly para as ranges de peso
graph_abates_peso <- Abates_peso %>% 
  plot_ly(x=~peso_range, type="histogram") %>%
  layout(title="Abates por Peso") %>%
  layout(xaxis = list(title = "Peso"), yaxis = list(title = "Frequência"))
graph_abates_peso


##TENTATIVA DE ANÁLISE
#BOXPLOTS
#para comparar a distribuição dos pesos tendo em conta o sexo, raça e idade.
#tirei as legendas porque achei desnecessário e resolvia-me o problema  do boxplot da raça ficar estranho. 
#se achares melhor ficar, reverte-se
box_peso_sexo <- Abates_peso %>%
  plot_ly(y=~Peso, x=~Sexo, type="box", color = ~Sexo) %>%
  layout(title="Peso ao Abate por Sexo") %>%
  layout(showlegend = FALSE)
box_peso_sexo 
  
box_peso_raca <- Abates_peso %>%
  plot_ly(y=~Raca, x=~Peso, type="box", color = ~Raca)%>% 
  layout(title="Peso ao Abate por Raça") %>%
  layout(yaxis = list(title= "Raça")) %>%
  layout(showlegend = FALSE)
box_peso_raca  
box_peso_raca_agrupada <- Abates_peso %>%
  plot_ly(y=~Raca_agrupada, x=~Peso, type="box", color = ~Raca_agrupada)%>% 
  layout(title="Peso ao Abate por Raça") %>%
  layout(yaxis = list(title= "Raça")) %>%
  layout(showlegend = FALSE)
box_peso_raca_agrupada
  
box_peso_idade <- Abates_peso %>%
  plot_ly(y=~idade_range, x=~Peso, type="box", color=~idade_range)%>%
  layout(title="Peso ao Abate por Grupo de Idade")%>%
  layout(yaxis = list(title= "Idade"))%>%
  layout(showlegend = FALSE)   
box_peso_idade


###PESO-IDADE (quantitativa-quantitativa)

##testar permissas da ANOVA
#1. Homogeneidade - Levene Test testa se a variancia entre grupos é igual. H0=variancia igual; H1=variancia NAO igual
library(car)
leveneTest(Peso ~ idade_range, data=Abates_peso) 
#como Pr<0.05, rejeita-se H0, ou seja: variancia NAO é igual (suportada pelo boxplot) e nao se pode usar ANOVA?

#2. testar dist. normal - Kolmogorov-Smirnov Test para n>50. nao faço ideia se isto está bem
ks.test(Abates_peso$idade_ao_abate, Abates_peso$Peso)

# Acho que é mais fazeres cada variavel a comparar com a distribuicao normal
ks.test(Abates_peso$Peso, pnorm)
ks.test(Abates_peso$idade_ao_abate, pnorm)
# E deram as duas <0.05 = NAO tem dist normal

# Temos entao de entrar nos testes nao parametricos
#Alternativa teste não paramétrico do One way ANOVA test é o KRUSKAL-WALLIS TEST (e correlaçao de spearman em vez de pearson)

kruskal.test(Peso ~ idade_ao_abate, data=Abates_peso)
# Ora isto deu p<0.05, significando que rejeitas H0 e portanto temos diferencas estatisticamente significativas no peso consoante idade (previsivel)
cor.test(Abates_peso$Peso, Abates_peso$idade_ao_abate_dias, method = "spearman", exact = FALSE) #dava um erro, na net resolvia-se com o exact=FALSE; na prática, deu o mesmo resultado
# Este tambem deu <0.05 e portanto ha correlacao (again, expected)


### PESO-RACA (quantitativa- qualitativa)
# isto seria um chisquare acho
chisq.test(Abates_peso$peso_range, Abates_peso$Raca) ##na sebenta de biomat diz que a variavel tem que ser quantitaiva em intervalos de classe, por isso troquei
# Deu p<0.05 e portanto verificamos associacao estatisticamente significativa entre raca e peso
chisq.test(Abates_peso$peso_range, Abates_peso$Raca_agrupada)
# Deu p<0.05 e portanto verificamos associacao estatisticamente significativa entre raca agrupada e peso


### PESO-SEXO (quantitativa - qualitativa binaria)
chisq.test(Abates_peso$peso_range, Abates_peso$Sexo)
# p<0-05 tambem


plot(Peso ~ idade_ao_abate, data=Abates_peso[Abates_peso$idade_ao_abate<2,])


Abates_peso_2 <- subset(Abates_peso[Abates_peso$idade_ao_abate<2,])
# Subset com peso >30 pq peso minimo de vitelos ao nascimento
Abates_peso_2 <- subset(Abates_peso_2[Abates_peso_2$Peso>30,])


#Criei nova tabela com as contagens por raça, tirei o top 20 e depois voltei à nossa tabela nova (abates_peso_2) para filtar 
#mantendo só as raças contidas na outra tabela. os mutates nao sei bem para que servem, tirei da net o codigo
Abates_peso_3 <- Abates_peso_2 %>% 
  count(Raca) %>% 
  top_n(20) %>%
  arrange(n, Raca) %>%
  mutate(Raca = factor(Raca, levels = unique(Raca)))
Abates_peso_2 <- Abates_peso_2 %>% filter(Raca %in% Abates_peso_3$Raca) %>%
  mutate(Raca = factor(Raca, levels = levels(Abates_peso_3$Raca)))





# Como nao me lembrava do que era, fiz isto para idade e raca, mas com o subset de idade <2 e peso >30
# Nao me perguntes o que esta por aqui feito, eu estou tao confusa como tu confia
teste <- lm(Peso ~ idade_ao_abate_dias + Raca_agrupada, data=Abates_peso_2)
summary(teste)
# Call:
#   lm(formula = Peso ~ idade_ao_abate_dias + Raca_agrupada, data = Abates_peso_2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -352.22  -42.84   -1.64   43.78  358.91 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         121.872057   0.571226   213.4   <2e-16 ***
#   idade_ao_abate_dias   0.374490   0.001229   304.6   <2e-16 ***
#   Raca_agrupadaLeite  -63.054908   0.394671  -159.8   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 62.73 on 252378 degrees of freedom
# Multiple R-squared:  0.4537,	Adjusted R-squared:  0.4536 
# F-statistic: 1.048e+05 on 2 and 252378 DF,  p-value: < 2.2e-16

layout(matrix(c(1,2,3,4),2,2)) #Isto era para ter os plots de diagnostico separados (avaliar os pressupostos basicamente)
# cuidado que tem de se voltar a por o layout como deve ser
plot(teste) #plots de diagnostico de assumptions


layout(matrix(c(1,1))) #repor o layout


# agr para dar plot mesmo acho que e isto
# plot com top 20 racas, que faz + sentido do que com todas, em que ate tinhas vitelos de leite a pesar mais a nascenca que os de carne
ggplot(Abates_peso_2, aes(x = idade_ao_abate_dias, y = Peso, shape=Raca_agrupada)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Raca_agrupada))

# Vou tentar fazer uma coisa pq nao temos dados com dist normal
library(mblm)
# Fazer um subset dos dados random e ver se corre pq estava a demorar mais de 3h a correr com a df toda

subseto <- Abates_peso_2[sample(nrow(Abates_peso_2), 100), ]
teste_m <- mblm(Peso ~idade_ao_abate_dias, data=subseto)
teste_m
summary(teste_m)

# Residuals:
# Min      1Q  Median      3Q     Max 
# -200.18  -50.54  -16.18   38.61  120.06 
# 
# Coefficients:
#   Estimate      MAD V value Pr(>|V|)    
# (Intercept)          61.9250 110.8739    4252 2.92e-09 ***
#   idade_ao_abate_dias   0.4940   0.2125    4874 6.75e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 68.52 on 98 degrees of freedom

# Isto nao me parece diferir do resultado que temos do linear model parametrico, entao foi so uma experiencia gira, moving on
