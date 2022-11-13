

#Bibliotecas necessárias
library(devtools)
library(RCurl)
library(Rempres)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(plotly)
library(sp)

start <- today() - years(10)
stop <- today()

doenca<-"Rabies"
dinicio<-gsub("-","", start)
dfim<-gsub("-","", stop)

focos<-Empres.data(doenca,,dinicio,dfim,dinicio,dfim)
focos$ano<-substr(focos$observationDateYYYYMMDD,1,4)


#Gráfico de barras com divisão de casos por continente
focos_reg_ano <- select(focos, idRegion, ano)
europe <- filter(focos_reg_ano, idRegion == "Europe")
africa <- filter(focos_reg_ano, idRegion == "Africa")
asia <- filter(focos_reg_ano, idRegion == "Asia")
americas <- filter(focos_reg_ano, idRegion == "Americas")
oceania <- filter(focos_reg_ano, idRegion == "Oceania")

hist_focos_ano_reg <- plot_ly(type = 'histogram', name = "Global",
                              x = as.numeric(focos_reg_ano$ano), bingroup = 1) %>%
  add_trace(
    type='histogram', name = "Europe",
    x = as.numeric(europe$ano),
    bingroup=1) %>%
  add_trace(
    type='histogram', name = "Asia",
    x = as.numeric(asia$ano),
    bingroup=1) %>%
  add_trace(
    type='histogram', name = "Africa",
    x = as.numeric(africa$ano),
    bingroup=1) %>%
  add_trace(
    type='histogram', name = "Americas",
    x = as.numeric(americas$ano),
    bingroup=1) %>%
  add_trace(
    type='histogram', name = "Oceania",
    x = as.numeric(oceania$ano),
    bingroup=1) %>%
  layout(
    title = "Events by Year", 
    xaxis = list(title = "Years"),
    yaxis = list(title = "Events"),
    bargap=0.1)

hist_focos_ano_reg

# Gr?fico de barras casos por esp?cie
focos_esp_ano <- select(focos, speciesDescription, ano)

focos_esp_ano <- separate(focos_esp_ano, speciesDescription, sep= ",", into = c("status", "species"))

hist_focos_ano_espe <- plot_ly( type= "histogram", name="Every species", 
                               y= focos_esp_ano$species) %>%
  layout(
    title = "Frequency of Species", 
    yaxis = list(title = "Species"),
    xaxis = list(title = "Frequency"),
    bargap=0.1)

hist_focos_ano_espe 


# World map with cases
paises<- geojsonio::geojson_read("custom.geo.json", what="sp")


data_Map <- paises[paises$sovereignt %in% focos$country, ]

casos_pais <- mutate(
  summarize(
    group_by(focos, country), 
    count=n()
  )
)

casos_pais <- arrange(casos_pais, count)


data_Map <- sp::merge(data_Map,casos_pais, by.x="sovereignt", by.y="country")

mytext <- paste(
  "Country: ", data_Map@data$name,"<br/>", 
  "Events: ", data_Map@data$count, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)


bins <- c(1, 2,5, 20, 100, 500,1000)

pal3 <- colorBin("YlOrRd",casos_pais$count, bins)

mapa1 <- leaflet(data=data_Map) %>%
  addTiles() %>% 
  addPolygons(
    weight=0.5,
    fillColor = ~pal3(count),
    fillOpacity = 0.5,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"))%>% 
  addLegend(pal = pal3, values = ~count, opacity = 0.7, title = NULL,
            position = "bottomright")

mapa1




