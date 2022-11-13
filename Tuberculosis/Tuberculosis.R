

# Libraries
library(tidyverse)
library(data.table)
library(measurements)
library(lubridate)
library(sf)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Docs
Abates <- fread("Abates-pesquisa Mycobac_2010-21.csv") %>% unique
Codme <- fread("C?d_ME_DiCo.csv") %>% unique
IDTC <- fread("IDTC+_2010-21.csv") %>% unique

Correspondecia <- fread("Correspond?ncias freg 2013-14.csv") %>% unique

Total_Caract_Expl <- fread("FicheiroTotalCaracterizacaoExploracoes-2022-10-04.csv") %>% unique

Total_Ent_Expl <- fread("FicheiroTotalEntidadesExploracoes-2022-10-04.csv") %>% unique

Total_Tipo_Ent_Expl <- fread("FicheiroTotalTiposEntidadeExploracoes-2022-10-04.csv")%>% unique 

#Adicionamos o sufixo PT a marca de exploracao
Codme <- mutate(Codme, ME = paste("PT", Codme$ME, sep = '')) #Dont run twice, you'll add another PT

#Junção das tabelas
#Primeiro junção de tabela Abates com o Codigo, fazendo corresponder o ME_alt ao cod
#O mesmo foi feito para IDTC
abates <- inner_join(Abates, Codme, by= c("ME_alt" = "CÃ³d")) %>% unique
tuberculo <- inner_join(IDTC, Codme, by= c("ME_alt" = "CÃ³d")) %>% unique


#Agora temos de juntar as coordenadas geográficas à tabela, através dos ficheiros com todas as explorações do país
# CEX_MAR_EXP corresponde a código oficial de marca de exploração na folha Total_Caract_Expl
# Juntando as colunas CEX_COD_FRE, CEX_COD_CON e CEX_COD_DIS obtemos o código DiCoFre
# Juntando as colunas CEX_GRA_N, CEX_MIN_N, CEX_SEG_N obtemos as coordenadas geograficas N e W
# Latitude é N e longitude é W
# Mas não é tão simples como juntar as colunas de GRA_, MIN_, SEG_, estas têm de ser convertidas
# Juntar as coordenadas em "dd mm ss", para utilizar library(measurements), função conv_unit()
# Portanto, transformar dados de deg_min_sec para dec_deg: conv_unit( *coluna*, "deg_min_sec","dec_deg")

Total_Caract_Expl <- unite(Total_Caract_Expl, "LATITUDE", CEX_GRA_N, CEX_MIN_N, CEX_SEG_N, sep = ' ') %>% 
  unite("LONGITUDE", CEX_GRA_W, CEX_MIN_W, CEX_SEG_W, sep = ' ') 

Total_Caract_Expl <- mutate(Total_Caract_Expl, LATITUDE = conv_unit(Total_Caract_Expl$LATITUDE, from = "deg_min_sec", to = "dec_deg")) %>% 
  mutate(LONGITUDE = conv_unit(Total_Caract_Expl$LONGITUDE, from = "deg_min_sec", to = "dec_deg"))

Total_Caract_Expl <- mutate(Total_Caract_Expl, LATITUDE = round(as.numeric(Total_Caract_Expl$LATITUDE), digit = 5)) %>% 
  mutate(LONGITUDE = round(as.numeric(Total_Caract_Expl$LONGITUDE), digit = 5))

#Isto é só para arrumar a casa
tuberculo <- select(tuberculo, -DiCo.x)
abates <- select(abates, -DiCo.x)



#Converter coluna das datas no formato correto
Total_Caract_Expl$DAT_ALT <- as_datetime(Total_Caract_Expl$DAT_ALT, format="%Y-%m-%d %H:%M")

#Esta linha de codigo so da uma coluna com marcas de exploracao mais recentes, mantendo todas as outras coluns atraves do keep_all = TRUE
Total_Caract_Expl_ordenado <- Total_Caract_Expl %>% arrange(desc(DAT_ALT)) %>% distinct(CEX_MAR_EXP, .keep_all = TRUE)
Total_Caract_Expl_ordenado <- unite(Total_Caract_Expl_ordenado, "DiCoFre", CEX_COD_DIS, CEX_COD_CON, CEX_COD_FRE, sep = "")

# Vou tentar tirar aqui apenas os que me interessa
select_Total_Caract_Expl_ordenado <- select(Total_Caract_Expl_ordenado, CEX_MAR_EXP, DiCoFre, LATITUDE, LONGITUDE)
names(select_Total_Caract_Expl_ordenado)[names(select_Total_Caract_Expl_ordenado) == 'CEX_MAR_EXP'] <- 'ME'

#Com o left_join, juntar as tabelas e obtemos todos os dados já com coordenadas certinhas e ME oficiais
abates1 <- left_join(abates, select_Total_Caract_Expl_ordenado)

tuberculo1 <- left_join(tuberculo, select_Total_Caract_Expl_ordenado)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://www.dgterritorio.gov.pt/cartografia/cartografia-tematica/caop e escolhem a versão que quiserem (eu escolhi 2020)
# Nao me lembro se era preciso usar uma versao mais antiga da carta tho
# Atencao que vao descarregar um ficheiro zip e vao ter de manter todos os ficheiros que ele tem juntos na mesma pasta ou acho que depois o mapa nao carrega

#Carregar o mapa
#O ficheiro vai sem extensao porque isto nao estava a conseguir abrir a layer, justificacao:
# https://stackoverflow.com/questions/50949028/readogr-cannot-open-layer-error 
continente<- st_read("Cont_AAD_CAOP2020")

# Em comum do ficheiro do mapa e ficheiros abates e IDTC temos o DiCoFre, juntar por ai
#Como o leaflet nao estava a gostar da festa com os dados como tinhamos antes, tive de ir ao stack overflow investigar a situa?ao de converter os dados em Lat-Long
#https://stackoverflow.com/questions/45710087/plotting-sfc-polygon-in-leaflet
continente$geometry <- st_transform(continente$geometry, "+init=epsg:4326")

# Tabela com a soma de abates positivos por Dicofre com a geometria
pos_freguesia_ab <- abates1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(`PM +`, na.rm = TRUE),
            .groups = 'drop')

# Por os 0 como NA para nao aparecerem no mapa
pos_freguesia_ab[pos_freguesia_ab == 0] <- NA

# Merge dos dois ficheiros para dar o ficheiro do mapa
continente_ab <- sp::merge(continente,pos_freguesia_ab, by.x="Dicofre", by.y="DiCoFre")


# Palete de cores para abates
bins_ab <- c(0, 2, 4, 8, 16, 32, 64, 128, Inf)

pal_ab <- colorBin("Greens",continente_ab$Total_Ps, bins_ab, na.color = NA) 

# Texto para o pop up
mytext_ab <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_ab$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_ab$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)


#Mapa dos abates
portugal_abates <- leaflet(data = continente_ab) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, 
              fillColor = ~pal_ab(Total_Ps), 
              fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_ab, 
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_ab, 
            values = ~Total_Ps, 
            opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_abates

#Tabela com a soma de positivos ao IDTC para cada Dicofre com a geometry associada
pos_freguesia_IDTC <- tuberculo1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(Ps, na.rm = TRUE),
            .groups = 'drop')

# Merge para base de dados do mapa
continente_IDTC <- sp::merge(continente,pos_freguesia_IDTC, by.x="Dicofre", by.y="DiCoFre")

# Palete para IDTC
bins_IDTC <- c(0, 10, 20, 40, 80, 160, 320, 640, Inf)
pal_IDTC <- colorBin("Greens",continente_IDTC$Total_Ps, bins_IDTC) 

# Texto para o pop up
mytext_IDTC <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_IDTC$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_IDTC$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Mapa de casos IDTC
portugal_IDTC <- leaflet(data = continente_IDTC) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, 
              fillColor = ~pal_IDTC(Total_Ps), 
              fillOpacity = .7, 
              color = "black", 
              dashArray = "",
              label = mytext_IDTC, 
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_IDTC, values = ~Total_Ps, opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_IDTC

# Tabela com todos os que ja foram positivos
# Tabela com as unicas colunas que nos interessam
tuberculo_2 <- select(tuberculo1, ME, DiCoFre, LATITUDE, LONGITUDE, Ps, Ano) %>% unique
abates_2 <- select(abates1, ME, DiCoFre, LATITUDE, LONGITUDE, `PM +`, DtAbate) %>% unique %>% 
  mutate(DtAbate =  as_datetime(DtAbate, format="%d/%m/%Y")) %>% mutate(DtAbate = year(DtAbate))

# Passar os 0's para NA's
tuberculo_2[tuberculo_2 == 0] <- NA
abates_2[abates_2 == 0] <- NA

# Mudar os nomes para facilitar visualizacao
names(tuberculo_2)[names(tuberculo_2) == 'Ps'] <- 'Pos_IDTC'
names(abates_2)[names(abates_2) == 'PM +'] <- 'Pos_AB'

# Merge das duas tabelas
tabela_TB <- full_join(tuberculo_2, abates_2) %>%  unique

# Filtrar a tabela para manter apenas as rows que tem um dos testes positivos
tabela_TB_1 <- tabela_TB[(tabela_TB$Pos_IDTC >= 1 | tabela_TB$Pos_AB >= 1),] %>%  unique

# Antes de por o unique, a tabela continha varios duplicados em relacao a marca de exploracao, mas com valores de casos positivos diferentes
# Tentar somar esses casos positivos numa so linha, mas isso nao vai correr muito bem visto que tenho anos de detecao diferentes
# Somar os Pos e manter ano + antigo e ano + recente 

# Criar duas novas colunas, uma que diz se a exploracao foi positiva no abate ou no IDTC

tabela_TB_final <- tabela_TB_1 %>% 
  mutate(Positivo = case_when(Pos_IDTC | Pos_AB > 1 ~ TRUE)) %>% 
  mutate(Ano_IDTC = case_when(Pos_IDTC >= 1 ~ tabela_TB_1$Ano)) %>% 
  mutate(Ano_AB = case_when(Pos_AB >= 1 ~ tabela_TB_1$DtAbate)) %>%
  select(-Ano, -DtAbate) %>%
  mutate("Primeiro Ano" = pmin(Ano_IDTC, Ano_AB, na.rm = TRUE)) %>% 
  mutate("Ultimo Ano" = pmax(Ano_IDTC, Ano_AB, na.rm = TRUE)) %>%
  select(-Ano_IDTC, -Ano_AB) %>% 
  unique

tabela_TB_final <- tabela_TB_final[(tabela_TB_final$Positivo == TRUE)] %>% arrange(desc("Ultimo Ano")) %>% distinct(ME, .keep_all = TRUE)

# Exportar a tabela para um ficheiro CSV
write.csv(tabela_TB_final,"...\\csv_tb.csv", row.names = FALSE)

# Histograma para avaliar a distribuicao temporal dos casos
# Da para fazer mais bonito, mas por agora fica assim sem titulo e axis labels random

hist(tabela_TB_final$`Primeiro Ano`)
hist(tabela_TB_final$`Ultimo Ano`)


# Plot para avaliar correlação entre abates e IDTC (será scatterplot?)



# Identificar explorações e zonas geograficas onde aparecem primeiro positiovs no abate que IDTC  -- comparar 1o ano deteção abate vs IDTC e criar coluna com indicação de qual o metodo de deteção

# Percentagem de positivos em função do estatuto (estatistica descritiva simples, x% em T3, x% em T2, etc etc, e preciso numero total de casos e estatuto: criar tabela nova)
# Preciso fazer uma nova tabela com numero de casos e estatuto da exploraçao pre-rastreio/abate




# Distribuição espacial dos casos que aparecem primeiro em matadouro (identificação de areas onde ha falta de tuberculinização. Falamos de distritos? Andar a brincar com case whens e isso)

# Identificar/Investigar se há meses com maior prevalencia da doença (teste estatistico com casos por mes (somar todos os casos do mesmo mes dos varios anos e realizar teste estatistico - investigar qual))


