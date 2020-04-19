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
pal <- colorNumeric("viridis", NULL)
temp = tempfile(fileext = ".xlsx")
tilesURL <- rgdal::readOGR("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson")
test <- 
dataURL<- 'https://github.com/MatteoHenryChinaski/Comuni-Italiani-2018-Sql-Json-excel/raw/master/italy_regions.xlsx'
download.file(dataURL, destfile=temp, mode='wb')
dfregions <- readxl::read_excel(temp, sheet =2)
dfregions <- filter(dfregions, dfregions$id_regione %in% 1:20)
dfregions <- dfregions %>%  rename( reg_istat_code = cod_istat)
dfregions <- dfregions %>%  rename( codice_regione = id_regione)
dfregions$codice_regione <- sprintf("%02d",as.numeric(dfregions$codice_regione))
ev <- select(dfregions, c("codice_regione","reg_istat_code","num_residenti","regione"))
pcmdpcfr <- read.csv(curl("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv"))
pcmdpcr <- filter(pcmdpcfr,pcmdpcfr$lat > 0 & pcmdpcfr$long >0)
pcmdpcr$data <- as.Date(as.factor(gsub("T.*","",pcmdpcr$data)))
pcmdpcr$codice_regione <- sprintf("%02d",pcmdpcr$codice_regione)
pcmdpcr <- pcmdpcr %>%  rename( reg_istat_code = codice_regione)
pcmdpcr<- pcmdpcr%>% select(reg_istat_code, totale_positivi,isolamento_domiciliare,variazione_totale_positivi,dimessi_guariti,deceduti)%>% group_by(reg_istat_code) %>% summarise_all(sum)
pcmdpcr <-full_join(ev, pcmdpcr,  by = "reg_istat_code")
pcmdpcr <- pcmdpcr[order(pcmdpcr$reg_istat_code),]
#basemap <- leaflet(tilesURL) %>% addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png') 
#basemap%>%addPolygons(stroke = F, smoothFactor = 0.3,fillColor = ~pal(pcmdpcr$totale_positivi/pcmdpcr$num_residenti*100),labelOptions = labelOptions(textsize = "15px",opacity = 0.7))) 
leaflet(tilesURL) %>% 
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addLayersControl(baseGroups = c("Carto DB Positron", "World Imagery")) %>%
        #  addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png') %>%
        addPolygons(stroke = F, smoothFactor = 0.1, fillOpacity = .7, fillColor = ~pal(pcmdpcr$totale_positivi/pcmdpcr$num_residenti*100),labelOptions = labelOptions(textsize = "15px",opacity = 0.7), label = lapply(paste0("<b>",pcmdpcr$regione,"</b> <br />  in Positive in %: ", formatC(pcmdpcr$totale_positivi/pcmdpcr$num_residenti*100, big.mark = ","),"<br />  Positive: ", formatC(pcmdpcr$totale_positivi, big.mark = ","),"<br /> Var Positive: ",  formatC(pcmdpcr$variazione_totale_positivi , big.mark = ","),"<br /> Gesundet: ",  formatC(pcmdpcr$dimessi_guariti, big.mark = ","),"<br /> Todesfälle: ",  formatC(pcmdpcr$deceduti , big.mark = ",")), htmltools::HTML)) %>%
        addLegend(pal = pal,title = "Positive in % zur EZ",labFormat = labelFormat(),na.label = "Keine Angabe",position = "bottomright", values = ~pcmdpcr$totale_positivi/pcmdpcr$num_residenti*100, opacity = .7)%>% addControl(titletext, position = "topright", className="map-title")

        # %>%  
        #         
        #         
        #         addMinicharts(
        #         pcmdpcr$long, pcmdpcr$lat, 
        #         chartdata = pcmdpcr[, c("totale_positivi","totale_positivi_ev","deceduti","deceduti_ev")],
        #         time = pcmdpcr$data
        # )                                
 # basemap %>% 
 #  addMinicharts(
 #    pcmdpcr$long, pcmdpcr$lat, 
 #    chartdata = pcmdpcr[, c("totale_positivi", "variazione_totale_positivi", "ricoverati_con_sintomi")],
 #    time = pcmdpcr$data ,
 #    colorPalette = colors,
 #    width = 45, height = 45
 #  )
