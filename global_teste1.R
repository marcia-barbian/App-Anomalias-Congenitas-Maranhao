library(shiny)
library(rgdal)
library(DT)
library('dygraphs')
library('xts')
library(leaflet)
library(tidyverse)
library(xts)
library(plotly)
#####################################
library(readxl)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(shinydashboard)
library(ggrepel)
library(sf)
library(RColorBrewer)
library(ps)
library(rgdal)
library(spdep) #pacote espacial Moran Lisa

############################################

banco_luzivan <- read_excel("dadosANOMALIAScMA_completo.xlsx", skip = 1)
banco_luzivan_novo = banco_luzivan %>% 
  gather(key = "aux", value = "valor", 3:50) %>%
  separate(col = aux, into = c("tipo", "ano"), sep = "_")  %>%
  spread(key= tipo, value = valor) %>%
  mutate(Cidade = str_to_lower(Cidade)) 

banco_luzivan_novo$ano = as.numeric(banco_luzivan_novo$ano)
banco_luzivan_novo$prevalencia=10000*banco_luzivan_novo$proporcao

banco_luzivan_novo_tabela= subset(banco_luzivan_novo, select = -proporcao )
banco_luzivan_novo_tabela$Cidade=banco_luzivan$Cidade
names(banco_luzivan_novo_tabela) = c("Codigo", "Cidade", "ano", "nascidos vivos com anomalias congenitas", 
                                     "nascidos vivos", "prevalencia de anomalias congenitas")


mapa_ma_shp <- sf::st_read("21MUE250GC_SIR.shp", quiet = TRUE)

dplyr::glimpse(mapa_ma_shp)
mapa_ma_shp <- mapa_ma_shp %>%
  mutate(municipio = str_to_lower(NM_MUNICIP)) # todas as cidades com letra minuscula

banco <- banco_luzivan %>%
  mutate(municipio = str_to_lower(Cidade)) # transforma as cidades em letra miniscula
banco$municipio = factor(banco$municipio)

banco_completo <- merge(banco, mapa_ma_shp, by.x = "municipio", by.y = "municipio") 
dplyr::glimpse(banco_completo)
banco_completo = st_as_sf(banco_completo)
banco_completo_trasformado <- st_transform(banco_completo, "+init=epsg:4326")

w <- poly2nb(banco_completo_trasformado$geometry, row.names=banco_completo_trasformado$municipio)
matriz_w <-  nb2listw(w, style='B') #faz a matriz de pesos 0 ou 1

####################################################################################
