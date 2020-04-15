library(leaflet)
library(curl)
library(jsonlite)
library(magrittr)
library(tibble)
library(dplyr)
library(shiny)
library(ggplot2)
library(gganimate)
library(viridis)
library(leaflet.minicharts)
titletext <- paste0("Data Source: Provinz  Bozen <br />  Date : ", Sys.Date(), "<br /> Created with: R by J. Mayr")
pal <- colorNumeric("viridis", NULL)
temp = tempfile(fileext = ".xlsx")
tilesURL <- rgdal::readOGR("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_provinces.geojson")
dataURL<- 'https://github.com/MatteoHenryChinaski/Comuni-Italiani-2018-Sql-Json-excel/blob/master/italy_provincies.xlsx?raw=true'
download.file(dataURL, destfile=temp, mode='wb')
dfprov <- readxl::read_excel(temp, sheet =2)
dfprov <- filter(dfprov, dfprov$id_regione %in% 1:20)
#dfprov <- dfprov %>%  rename( reg_istat_code = cod_istat)
#dfprov <- dfprov %>%  rename( codice_regione = id_regione)
#dfprov$codice_regione <- sprintf("%02d",as.numeric(dfprov$codice_regione))
ev <- select(dfprov, c("sigla","provincia","residenti","id_regione"))
pcmdpcfr <- read.csv(curl("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"),na.strings=c("NaN", " "))
pcmdpcr <- filter(pcmdpcfr,pcmdpcfr$lat > 0 & pcmdpcfr$long >0)
pcmdpcr$data <- as.Date(as.factor(gsub("T.*","",pcmdpcr$data)))
ev <- ev %>%  rename(sigla_provincia = sigla)
# pcmdpcr<- pcmdpcr%>% select(reg_istat_code, totale_positivi,isolamento_domiciliare,variazione_totale_positivi,dimessi_guariti,deceduti)%>% group_by(reg_istat_code) %>% summarise_all(sum)
pcmdpcr <-full_join(ev, pcmdpcr,  by = "sigla_provincia")
# pcmdpcr <- pcmdpcr[order(pcmdpcr$reg_istat_code),]
leaflet(tilesURL) %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(baseGroups = c("Carto DB Positron", "World Imagery")) %>%
  #  addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png') %>%
  addPolygons(stroke = F, smoothFactor = 0.1, fillOpacity = .7, fillColor = ~pal(pcmdpcr$totale_casi/pcmdpcr$residenti*100),labelOptions = labelOptions(textsize = "15px",opacity = 0.7), label = lapply(paste0("<b>",pcmdpcr$provincia,"</b> <br />  in Positive in %: ", formatC(pcmdpcr$totale_casi/pcmdpcr$residenti*100, big.mark = ","),"<br />  Fälle: ", formatC(pcmdpcr$totale_casi, big.mark = ","))), htmltools::HTML) %>%
  addLegend(pal = pal,title = "Positive in % zur EZ",labFormat = labelFormat(),na.label = "Keine Angabe",position = "bottomright", values = ~pcmdpcr$totale_casi/pcmdpcr$residenti*100, opacity = .7)%>% addControl(titletext, position = "topright", className="map-title")