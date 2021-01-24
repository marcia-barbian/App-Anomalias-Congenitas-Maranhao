library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)


banco_luzivan <-
  read_excel("dadosANOMALIAScMA_completo.xlsx", skip = 1)
banco_luzivan_novo = banco_luzivan %>%
  gather(key = "aux", value = "valor", 3:50) %>%
  separate(col = aux,
           into = c("tipo", "ano"),
           sep = "_")  %>%
  spread(key = tipo, value = valor) %>%
  mutate(Cidade = str_to_lower(Cidade))

banco_luzivan_novo$ano = as.numeric(banco_luzivan_novo$ano)
banco_luzivan_novo$prevalencia = 10000 * banco_luzivan_novo$proporcao

banco_luzivan_novo_tabela = subset(banco_luzivan_novo, select = -proporcao)
banco_luzivan_novo_tabela$Cidade = banco_luzivan$Cidade
names(banco_luzivan_novo_tabela) = c(
  "Codigo",
  "Cidade",
  "ano",
  "nascidos vivos com anomalias congenitas",
  "nascidos vivos",
  "prevalencia de anomalias congenitas"
)



mapa_ma_shp <- sf::st_read("21MUE250GC_SIR.shp", quiet = TRUE)

dplyr::glimpse(mapa_ma_shp)
mapa_ma_shp <- mapa_ma_shp %>%
  mutate(municipio = str_to_lower(NM_MUNICIP)) # todas as cidades com letra minuscula


banco <- banco_luzivan %>%
  mutate(municipio = str_to_lower(Cidade)) # transforma as cidades em letra miniscula
banco$municipio = factor(banco$municipio)

banco_completo <-
  merge(banco, mapa_ma_shp, by.x = "municipio", by.y = "municipio") # une os dois bancos dado as cidades
dplyr::glimpse(banco_completo) 
banco_completo = st_as_sf(banco_completo)

banco_completo_trasformado <-
  st_transform(banco_completo, "+init=epsg:4326")


#### Trocar os nomes de proporcao 2004 e proporcao 2005
names(banco_completo_trasformado)[15] = c("proporcao_2004")
names(banco_completo_trasformado)[18] = c("proporcao_2005")


# Índice I Moran


## I de Moran

#install.packages("spdep")
library(spdep)


w <-
  poly2nb(banco_completo_trasformado$geometry, row.names = banco_completo_trasformado$municipio)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1


moran_2001=moran.mc(banco_completo_trasformado$proporcao_2001, ww, nsim = 9999)
valores_teste=c(moran_2001$statistic[[1]], moran_2001$p.value) 

valores_teste= as.numeric()
for (i in 1:16)
{
  variaveis_proporcao = paste("proporcao_", 2000 + i, sep = "")
  banco_moran = banco_completo_trasformado %>%
    select(variaveis_proporcao)
  names(banco_moran)[1] = c("variavel")
  moran_teste = moran.mc(banco_moran$variavel, ww, nsim = 9999)
  valores_teste = rbind(valores_teste,
                        c(moran_teste$statistic[[1]], moran_teste$p.value, 2000 + i))
}


write.table(valores_teste, file = "teste_moran.txt", row.names = FALSE, col.names = FALSE)
