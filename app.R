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
library(ps)
library(spdep) 
library(kableExtra)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
################################################
options(OutDec= ",") #Muda de ponto para virgula nos decimais! 


banco_luzivan <- read_excel("dadosANOMALIAScMA_completo.xlsx", skip = 1)
banco_luzivan_novo = banco_luzivan %>% 
  gather(key = "aux", value = "valor", 3:50) %>%
  separate(col = aux, into = c("tipo", "ano"), sep = "_")  %>%
  spread(key= tipo, value = valor) %>%
  mutate(Cidade = str_to_lower(Cidade)) 

banco_luzivan_novo$ano = as.numeric(banco_luzivan_novo$ano)
banco_luzivan_novo$prevalencia=10000*banco_luzivan_novo$proporcao

banco_luzivan_novo_tabela= subset(banco_luzivan_novo, select = -proporcao )
names(banco_luzivan_novo_tabela) = c("Codigo", "Cidade", "ano", "nascidos vivos com anomalias congenitas", 
                                     "nascidos vivos", "prevalencia de anomalias congenitas")


#################################################
################ banco de dados para a parte espacial
mapa_ma_shp <- sf::st_read("shape_files/21MUE250GC_SIR.shp", quiet = TRUE)

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
## Moran 
 w <- poly2nb(banco_completo_trasformado$geometry, row.names=banco_completo_trasformado$municipio)
 matriz_w <-  nb2listw(w, style='B') #faz a matriz de pesos 0 ou 1
 teste_moran <- read.table("teste_moran.txt", quote="\"", comment.char="")
 names(teste_moran)=c("estatistica_teste", "p_valor", "ano_teste")
####################################################################################

#####################################################################################################


ui <- dashboardPage(
  dashboardHeader(title = "Análise de nasc. vivos com anomalias congênitas no MA",
                  titleWidth = 600),
  dashboardSidebar
  (sidebarMenu(
    menuItem("Mapa da prevalência", tabName = "mapa_porporcao"),
    menuItem("Mapa nasc. vivos com anomalia", tabName = "mapa_n_casos"),
    menuItem("Mapa nasc. vivos", tabName = "mapa_populacao"),
    menuItem("Série Temp. prevalência", tabName = "serie_proporcao"),
    menuItem("Série Temp. nasc. vivos com anomalia", tabName = "serie_casos"),
    menuItem("Série Temp. nasc. vivos", tabName = "serie_pop"),
    menuItem("Tabela com dados das prevalências", tabName = "tabela"),
    menuItem("Sobre", tabName = "sobre")
    ),
    width = 260
    ),
  
  dashboardBody(tabItems(
    tabItem("tabela",
            fluidPage(
              titlePanel("Tabela com dados das prevalências dos minicípios do MA por 10.000"),
              mainPanel(DTOutput(outputId = "tabela"))
            )),
    
    tabItem(
      "mapa_porporcao",
      fluidPage(
        titlePanel("Prevalências de anomalias congenitas por 10.000"),
        
        fluidRow(column(
          6,
          selectizeInput(
            "ano_grafico",
            label = "Escolha o ano a ser considerado",
            choices = unique(banco_luzivan_novo_tabela$ano),
            selected = "2016"
          )),
          column(
            width = 6,
            tags$img(src="ufrgs_logo.png", height = 100, width = 127),
            tags$img(src="logo_ime.png", height = 100, width = 270),
            tags$img(src="ppg_genetica.png", height = 80, width = 95)
          )
         
        ),
        fluidRow(
          valueBoxOutput("box_populacao", width = 3),
          valueBoxOutput("box_numero_casos", width = 3),
          valueBoxOutput("box_prevalencia", width = 3)
        ),
        fluidRow(
        column(
          width = 12,
          box(
            title ="Mapa das prevalências de anômalias congênitas no ano selecionado",
            leafletOutput("grafico_mapa_proporcao", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
        )),
        fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 minucípios com maior prevalência no ano selecionado",
            background = "blue",
            plotlyOutput("prevalencia_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal da média das prevalências considerando todos os 
            municípios e todos os anos",
            background = "blue",
            plotlyOutput("prevalencia_serie"),
            width = 12
          )
        )),
        fluidRow(
        column(
            width = 4,
            box(
              title = "Resultado do teste I de moran",
              status = "primary",
              solidHeader = TRUE,
              #background = "blue",
              tableOutput("tabela_teste_moran"),
              width = 12
            )
          ),
          column(
            width = 8,
            box(
              title ="Mapa do Índice Local de Associação Espacial (LISA)",
              #h1("Mapa do Índice Local de Associação Espacial (LISA)"),
              #h4("Dados provenientes da Pre"),
              leafletOutput("mapa_lisa_prevalencia", height = "700px"),
              background = "blue",
              width = 12
            )
          ))
  )), #fluidpaige # Tabitem
  
  tabItem(
    "mapa_n_casos",
    fluidPage(
      titlePanel("Mapa do número de nascidos vivos com anomalias congênitas no ano selecionado"),
      
      fluidRow(column(
        6,
        selectizeInput(
          "ano_grafico",
          label = "Escolha o ano a ser considerado",
          choices = unique(banco_luzivan_novo_tabela$ano),
          selected = "2016"
        )),
        column(
          width = 6,
          tags$img(src="ufrgs_logo.png", height = 100, width = 127),
          tags$img(src="logo_ime.png", height = 100, width = 300),
          tags$img(src="ppg_genetica.png", height = 80, width = 95)
        )
        
      ),
      fluidRow(
        valueBoxOutput("box_populacao_casos", width = 3),
        valueBoxOutput("box_numero_casos_casos", width = 3),
        valueBoxOutput("box_prevalencia_casos", width = 3)
      ),
      fluidRow(
        column(
          width = 12,
          box(
            title ="Mapa do número de nascidos vivos com anomalias congênitas no ano selecionado",
            #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
            leafletOutput("mapa_n_casos", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
         )),
     fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 municípios com maior número de nascidos vivos com anomalias congenitas no ano selecionado",
            background = "blue",
            plotlyOutput("n_casos_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal da média do número de nascidos vivos com anomalias congênitas considerando todos os
            municípios e todos os anos",
            background = "blue",
            plotlyOutput("n_casos_serie"),
            width = 12
          )
        ))
    )), # #fluidpage Tabitem
    # 
  tabItem(
    "mapa_populacao",
    fluidPage(
      titlePanel("Mapa dos número de nascidos vivos"),
      
      fluidRow(column(
        6,
        selectizeInput(
          "ano_grafico",
          label = "Escolha o ano a ser considerado",
          choices = unique(banco_luzivan_novo_tabela$ano),
          selected = "2016"
        )),
        column(
          width = 6,
          tags$img(src="ufrgs_logo.png", height = 100, width = 127),
          tags$img(src="logo_ime.png", height = 100, width = 300),
          tags$img(src="ppg_genetica.png", height = 80, width = 95)
        )
        
      ),
      fluidRow(
        valueBoxOutput("box_populacao_populacao", width = 3),
        valueBoxOutput("box_numero_casos_populacao", width = 3),
        valueBoxOutput("box_prevalencia_populacao", width = 3)
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            title ="Mapa dos número de nascidos vivos no ano selecionado",
            #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
            #h4("Dados provenientes da Pre"),
            leafletOutput("mapa_pop", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
        )),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 munucípios com maior número de nascidos vivos no ano selecionado",
            background = "blue",
            plotlyOutput("pop_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal da média do número de nascidos vivos considerando todos os
            municípios e todos os anos",
            background = "blue",
            plotlyOutput("pop_serie"),
            width = 12
          )
        ))
      
    ) ) , #Tabitem fluid paige
  
  tabItem("serie_pop",
          fluidPage(
            titlePanel("Série temporal dos nascidos vivos por município"),
            
            fluidRow(
              column(
                width = 12,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 300),
                tags$img(src="ppg_genetica.png", height = 80, width = 95),
                offset = 6
              )
            ),
            
            fluidRow(
              sidebarPanel(
                #h3("Escolha a Cidade"),
                selectizeInput("cidade3",
                               label = "Escolha o município",
                               choices = unique(banco_luzivan_novo$Cidade),
                               multiple = T,
                               options = list(maxItems = 300, placeholder = 'Escolha os municípios 
                                              que irão compor o gráfico'),
                               selected = "são luís"),
                               #selected = levels(factor(banco_luzivan_novo$Cidade))),
                sliderInput("limite3",
                            "Limites do eixo vertical",
                            min = 0,
                            max = max(banco_luzivan_novo$populacao)+1,
                            value = c(0,max(banco_luzivan_novo$populacao)+1),
                            step = 10),
                width = 12
              )
            ),
            mainPanel(
              plotlyOutput("grafico_serie_pop", height = "600px"),
              width = 12
            )
          )
  ),
  
  tabItem("serie_casos",
          fluidPage(
            titlePanel("Série Temporal dos nascidos vivos com anomalias congenitas por município"),
            
            fluidRow(
              column(
                width = 12,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 300),
                tags$img(src="ppg_genetica.png", height = 80, width = 95),
                offset = 6
              )
            ),
            
            fluidRow(
              sidebarPanel(
                  #h3("Escolha a Cidade"),
                  #h3("Cursos"),
                  selectizeInput("cidade2",
                                 label = "Escolha o município",
                                 choices = unique(banco_luzivan_novo$Cidade),
                                 multiple = T,
                                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                                 selected = "são luís"),
                  sliderInput("limite2",
                              "Limites do eixo vertical",
                              min = 0,
                              max = max(banco_luzivan_novo$casos),
                              value = c(0,max(banco_luzivan_novo$casos)),
                              step = 1),
                  width = 12
              )
            ),
            mainPanel(
              plotlyOutput("grafico_serie_casos", height = "600px"),
              width = 12
            )
          )
  ),
  
  tabItem("serie_proporcao",
          fluidPage(
            titlePanel("Série Temporal das prevalências de anomalias congenitas por 10.000 
                       por município"),
            
            fluidRow(
              column(
                width = 12,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 300),
                tags$img(src="ppg_genetica.png", height = 80, width = 95),
                offset = 6
              )
            ),
            
            fluidRow(
              sidebarPanel(
                #h3("Escolha a Cidade"),
                #h3("Cursos"),
                selectizeInput("cidade1",
                               label = "Escolha o município",
                               choices = unique(banco_luzivan_novo$Cidade),
                               multiple = T,
                               options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                               selected = "são luís"),
                sliderInput("limite1",
                            "Limites do eixo vertical",
                            min = (0),
                            max = round(max(banco_luzivan_novo$prevalencia),0)+2,
                            value = c(0,round(max(banco_luzivan_novo$prevalencia),0)+2),
                            step = 1,
                            round = TRUE),
                width = 12
              )
            ),
            mainPanel(
              plotlyOutput("grafico_serie_proporcao", height = "600px"),
              width = 12
            )
          )
  ),
  
    tabItem("sobre",
            fluidPage(
              fluidRow(
                
               # setZoom(id = "covidMetrika",class = "small-box"),
              #  setZoom(id = "git_covidMetrika",class = "small-box"),
                
                column(
                  width = 12,
                  valueBoxOutput("covidMetrika",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("git_covidMetrika", width = 12)
                ),
                
                widgetUserBox(title="Aplicativo desenvolvido por Márcia Helena Barbian 
                              com laboração de Luzivan Costa Reis, Lavínia Schuler-Faccini, Augusto Cardoso dos Santos, 
                              Elis Vanessa de Lima e Silva e Juliano Boquett",
                              type=2, collapsible = TRUE,color = "primary", width = 12,
                              tags$div(
                                class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                              )),
                
                widgetUserBox(
                  title = tags$b("Márcia Helena Barbian"),
                  subtitle = "Professora do Departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'marcia.png',
                  color = "blue",
                  "Contato: mhbarbian@ufrgs.br",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Luzivan Costa Reis"),
                  subtitle = "Aluno de Pós-graduação em Genética e Biologia molecular, UFRGS",
                  type = 2,
                  width = 4,
                  src = 'luzivan.jpg',
                  color = "red",
                  "Contato: luzivanreis@gmail.com",
                  footer_padding = F
                ),
                widgetUserBox(
                  title = tags$b("Lavínia Schuler-Faccini"),
                  subtitle = "Professora do programa de Pós-graduação em Genética e Biologia molecular, UFRGS",
                  type = 2,
                  width = 4,
                  src = 'ppg_genetica.png',
                  color = "red",
                  "Contato: lavinia.faccini@ufrgs.br",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Augusto Cardoso dos Santos"),
                  #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                  type = 2,
                  width = 4,
                  src = 'ppg_genetica.png',
                  color = "red",
                  "Contato: santosaccd@gmail.com",
                  footer_padding = F
                ),
                widgetUserBox(
                  title = tags$b("Elis Vanessa de Lima e Silva"),
                  #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                  type = 2,
                  width = 4,
                  src = 'ppg_genetica.png',
                  color = "red",
                  "Contato: evlsilva@hcpa.edu.br",
                  footer_padding = F
                ),
                widgetUserBox(
                  title = tags$b("Juliano Boquett"),
                  #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                  type = 2,
                  width = 4,
                  src = 'ppg_genetica.png',
                  color = "red",
                  "Contato: Juliano Boquett ",
                  footer_padding = F
                ),
                
                widgetUserBox(title="Fonte de dados: Datasus",
                              type=2, collapsible = TRUE,color = "", width = 12,
                              tags$div(
                                class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                              )),
                # ),
                # 
                # 
                # tags$img(src = "logos.png", 
                #          height = "150", width = "1000")
              )
            ) #fluidpaige
    ) #Tabitem
    
    
  
)) # dabitens dashboard
)

server <- function(input, output) {
  

########## MAPA APLICATIVOS  

  output$mapa_n_casos <- renderLeaflet({
    
    ### BANCO DE DADOS  
    dataset <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano == "2016") %>%
      select(1,4) 
    names(dataset)[2]=c("variavel")
    
    ####### INTERVALO
    dataset_intervalo <- banco_luzivan_novo_tabela %>%
      filter(ano == "2016") %>%
      select(4)
    variavel_intervalo = dataset_intervalo[[1]]
    variavel_intervalo = c(min(banco_luzivan_novo_tabela[,4]), max(banco_luzivan_novo_tabela[,4]),
                           variavel_intervalo) 
    
    intervalos <- classInt::classIntervals(var = variavel_intervalo, n = 5, style = "jenks")
    intervalos[["brks"]][[2]] <- 1
    pal <- colorBin(palette="RdYlBu", domain = variavel_intervalo, bins = intervalos[["brks"]], reverse = TRUE)
    
    tidy <- merge(banco_completo_trasformado, dataset, by.x = "Codigo", by.y = "Codigo")
    tidy = st_as_sf(tidy)
    tidy <- st_transform(tidy, "+init=epsg:4326")
    
    
    leaflet(tidy) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
      addPolygons(fillColor = ~pal(variavel), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s n nasc. c; anomalia %s", tidy$Cidade, round(tidy$variavel,3)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = round(tidy$variavel,4), labFormat = function(type, cuts, p) {  
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))},
        title = "Numero de nascidos vivos com anomalia",
        labels = ~tidy$Cidade,
        position = "bottomright")
  })
  
###########################################################################################  
  output$mapa_pop <- renderLeaflet({
    
    ### BANCO DE DADOS  
    dataset <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano == "2016") %>%
      select(1,5) 
    names(dataset)[2]=c("variavel")
    
    ####### INTERVALO
    dataset_intervalo <- banco_luzivan_novo_tabela %>%
      filter(ano == "2016") %>%
      select(5)
    variavel_intervalo = dataset_intervalo[[1]]
    variavel_intervalo = c(min(banco_luzivan_novo_tabela[,5]), max(banco_luzivan_novo_tabela[,5]),
                           variavel_intervalo) 
    
    intervalos <- classInt::classIntervals(var = variavel_intervalo, n = 5, style = "jenks")
    intervalos[["brks"]][[2]] <- 1
    pal <- colorBin(palette="RdYlBu", domain = variavel_intervalo, bins = intervalos[["brks"]], reverse = TRUE)
    #pal = colorQuantile(palette="RdYlBu", domain =tidy$variavel, n = 4)
    #########################################################################################
    #### MAPA  
    #########################################################################################
    tidy <- merge(banco_completo_trasformado, dataset, by.x = "Codigo", by.y = "Codigo")
    tidy = st_as_sf(tidy)
    tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
    
    
    leaflet(tidy) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
      addPolygons(fillColor = ~pal(variavel), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s n de nasc. %s", tidy$Cidade, round(tidy$variavel,3)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = round(tidy$variavel,4), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))},
        title = "Numero de nascidos",
        labels = ~tidy$Cidade,
        position = "bottomright")
  })
  
  ###########################################################################################  
  
  output$grafico_mapa_proporcao <- renderLeaflet({
    
    ### BANCO DE DADOS  
    dataset <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano == "2016") %>%
      select(1,6) 
    names(dataset)[2]=c("variavel")
    
    ####### INTERVALO
    dataset_intervalo <- banco_luzivan_novo_tabela %>%
      filter(ano == "2016") %>%
      select(6)
    variavel_intervalo = dataset_intervalo[[1]]
    variavel_intervalo = c(min(banco_luzivan_novo_tabela[,6]), max(banco_luzivan_novo_tabela[,6]),
                           variavel_intervalo) 
    
    
    intervalos <- classInt::classIntervals(var = variavel_intervalo, n = 5, style = "jenks")
    intervalos[["brks"]][[2]] <- 1
    pal <- colorBin(palette="RdYlBu", domain = variavel_intervalo, bins = intervalos[["brks"]], reverse = TRUE)
    #pal = colorQuantile(palette="RdYlBu", domain =tidy$variavel, n = 5)
    #########################################################################################
    #### MAPA  
    #########################################################################################
    tidy <- merge(banco_completo_trasformado, dataset, by.x = "Codigo", by.y = "Codigo")
    tidy = st_as_sf(tidy)
    tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
    
    
    leaflet(tidy) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
      addPolygons(fillColor = ~pal(variavel), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s prevalência %s", tidy$Cidade, round(tidy$variavel,3)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = round(tidy$variavel,4), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))},
        title = "Prevalência por 10000",
        labels = ~tidy$Cidade,
        position = "bottomright")
  })
  
  
  
### MAPA LISA
  
  output$mapa_lisa_prevalencia <- renderLeaflet({
  
  ### BANCO DE DADOS  
  dataset <- banco_luzivan_novo_tabela %>%
    filter(ano == input$ano_grafico) %>%
    #filter(ano == "2016") %>%
    select(1,6) 
  names(dataset)[2]=c("variavel")
  
  tidy <- merge(banco_completo_trasformado, dataset, by.x = "Codigo", by.y = "Codigo")
  tidy = st_as_sf(tidy)
  tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet

  #### teste moran
  #moran= moran.mc(tidy$variavel, matriz_w, nsim=999)
  
  ### LISA
  locm <- localmoran(tidy$variavel, matriz_w)
  tidy$Sgini <- scale(tidy$variavel)
  tidy$lag <- lag.listw(matriz_w, tidy$Sgini)
  tidy$pval <- locm[,5]
  
  tidy$quad_sig <- ifelse(tidy$Sgini >= 0 & tidy$lag >= 0 & tidy$pval <= 0.05, 1, 
                        ifelse(tidy$Sgini <= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 2, 
                               ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 3, 
                                      ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 4, 5))))
  
  
  breaks <- seq(1, 5, 1)
  labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
  np <- findInterval(tidy$quad_sig, breaks)
  colors <- c("red", "blue", "lightpink", "skyblue2", "white")
  
  tidy$quad_sig <- ifelse(tidy$Sgini >= 0 & tidy$lag >= 0 & tidy$pval <= 0.05, 1, 
                        ifelse(tidy$Sgini <= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 2, 
                               ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 3, 
                                      ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 4, 5))))

  tidy$quad_sig <- factor(
    tidy$quad_sig,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Alto-Alto",
      "Baixo-Baixo",
      "Alto-Baixo",
      "Baixo-Alto",
      "Nao Significativo"
    )
  )
  
  pal <-
    colorFactor(
      palette = c("red", "blue", "purple", "orange", "gray"),
      domain = tidy$quad_sig
    )
  y = tidy$quad_sig
  
  leaflet(tidy) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
    addPolygons(
      fillColor = ~ pal(tidy$quad_sig),
      weight = 1.5,
      opacity = 1,
      fillOpacity = 0.7,
      color = "gray",
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = sprintf("%s - %s", tidy$Cidade, tidy$quad_sig),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 11px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~ tidy$quad_sig,
      opacity = 0.7,
      title = NULL,
      labFormat = labelFormat(digits = 5),
      position = "bottomright"
    ) %>%
    addScaleBar(position = 'bottomleft')
    
})

  
############# TABELA APLICATIVO ###########################################################
  
  output$tabela <- renderDT(banco_luzivan_novo_tabela)
  
  output$tabela_teste_moran = function(){
    
    tempor = round(teste_moran, 3) %>%
              filter(ano_teste ==input$ano_grafico) %>%
              #filter(ano_teste == 2002) %>%
              select(1,2) 
            
    
    colnames(tempor) <- c('Valor da estatística de Teste', 'p-valor')
    rownames(tempor) <- c(input$ano_grafico)
    
     #knitr::kable(tempor, align = 'r', digits = 2, caption = 'Estatística Índice Geral de Moran')
    
     tempor %>%
       knitr::kable("html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive", 
                                            background = "withe", color = "black"),
                       font_size = 15)#%>%
        #column_spec(c(1,2,3), background = "withe", color = "black")
  
  }
  
############### CAIXAS APLICATIVO ###########################################################  
  
  output$box_numero_casos <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(4)
    
    
    valueBox(
      sum(aux),
      "Total nascidos vivos com anomalias congenitas",
      icon = icon("notes-medical"),
      color = "red"
    )
  })
  
  output$box_prevalencia <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      select(6)
    aux = as.numeric(aux[[1]])
    
    valueBox(
      round(mean(aux),3),
      "Média da Prevalência por 10000",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  
  output$box_populacao <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(5)
    
    valueBox(
      sum(aux),
      "Total nascidos vivos",
      #icon = icon("notes-medical"),
      color = "blue"
    )
  })
  
###
  
  output$box_numero_casos_casos <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(4)
    
    
    valueBox(
      sum(aux),
      "Total nascidos vivos com anomalias congenitas",
      icon = icon("notes-medical"),
      color = "red"
    )
  })
  
  output$box_prevalencia_casos <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      select(6)
    aux = as.numeric(aux[[1]])
    
    valueBox(
      round(mean(aux),3),
      "Média da Prevalencia por 10000",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  
  output$box_populacao_casos <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(5)
    
    valueBox(
      sum(aux),
      "Total nascidos vivos",
      #icon = icon("notes-medical"),
      color = "blue"
    )
  })
  
## populacao
  
  output$box_numero_casos_populacao <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(4)
    
    
    valueBox(
      sum(aux),
      "Total nascidos vivos com anomalias congenitas",
      icon = icon("notes-medical"),
      color = "red"
    )
  })
  
  output$box_prevalencia_populacao <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      select(6)
    aux = as.numeric(aux[[1]])
    
    valueBox(
      round(mean(aux),3),
      "Média da Prevalencia por 10000",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  
  output$box_populacao_populacao <- renderValueBox({
    aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
      #filter(ano =="2001") %>%
      select(5)
    
    valueBox(
      sum(aux),
      "Total nascidos vivos",
      #icon = icon("notes-medical"),
      color = "blue"
    )
  })
  
  
    
####################### GRAFICOS BARRAS E SERIE

output$prevalencia_barras <- renderPlotly({
  
  aux <- banco_luzivan_novo_tabela %>%
    filter(ano == input$ano_grafico) %>%
  # filter(ano == 2016) %>%
    select(2, 6) %>%
    mutate(prevalencia = `prevalencia de anomalias congenitas`) %>%
    arrange(prevalencia) %>%
    top_n(20, prevalencia)
  
  ordem <- aux$Cidade
  
  plot_barras <- ggplot(aux, aes(x = Cidade, y = prevalencia)) +
    geom_col(fill = "purple") +
    #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
    scale_x_discrete(limits = ordem) +
    #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
    coord_flip()
  
  ggplotly(plot_barras)
  
})

output$prevalencia_serie <- renderPlotly({
  
    aux <- banco_luzivan_novo_tabela %>%
      select(ano, `prevalencia de anomalias congenitas`) %>%
      group_by(ano) %>%
      summarise(media = mean(`prevalencia de anomalias congenitas`))%>%
      arrange(ano)
    
    #ordem <- as.character(aux$ano)
    aux$ano = as.character(aux$ano)
    aux=data.frame(aux)
    
   plotar = ggplot(aux) + 
      geom_point(aes(x = ano,y = media), color = "purple" ) +
      geom_line(aes(x = ano, y = media, group = 1), color = "purple" ) +
      #geom_text(aes(x = ano,y = media, label = round(media,3))) +
      #scale_x_discrete(limits = ordem) +
      labs(x = "Ano", y = "Média Prevalência") +
      ylim(min(aux$media)-5, max(aux$media)+5)+
      #scale_fill_manual(values = cores_uti_emergencia) +
      #scale_color_manual(values = cores_uti_emergencia) +
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
    ggplotly(plotar)
  
})

#######

output$n_casos_barras <- renderPlotly({
  
  aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
    #filter(ano == 2016) %>%
    select(2, 4) %>%
    mutate(n_casos = `nascidos vivos com anomalias congenitas`) %>%
    arrange(n_casos) %>%
    top_n(20, n_casos)
  
  ordem <- aux$Cidade
  
  plot_barras <- ggplot(aux, aes(x = Cidade, y = n_casos)) +
    geom_col(fill = "red") +
    #labs(x = "Municipio", y = "Gráfico das 20 cidades com maior número de nascidos vivos com anomalias congenitas") +
    scale_x_discrete(limits = ordem) +
    #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
    coord_flip()
  
  ggplotly(plot_barras)
  
})

output$n_casos_serie <- renderPlotly({
  
  aux <- banco_luzivan_novo_tabela %>%
    select(ano, `nascidos vivos com anomalias congenitas`) %>%
    group_by(ano) %>%
    summarise(media = mean(`nascidos vivos com anomalias congenitas`))%>%
    arrange(ano)
  
  #ordem <- as.character(aux$ano)
  aux$ano = as.character(aux$ano)
  aux=data.frame(aux)
  
  plotar = ggplot(aux) + 
    geom_point(aes(x = ano,y = media), color = "red" ) +
    geom_line(aes(x = ano, y = media, group = 1), color = "red" ) +
    #geom_text(aes(x = ano,y = media, label = round(media,3))) +
    #scale_x_discrete(limits = ordem) +
    labs(x = "Ano", y = "Média dos nascidos vivos com anomalias congenitas") +
    ylim(min(aux$media)-5, max(aux$media)+5)+
    #scale_fill_manual(values = cores_uti_emergencia) +
    #scale_color_manual(values = cores_uti_emergencia) +
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
  ggplotly(plotar)
  
})

#####
output$pop_barras <- renderPlotly({
  
  aux <- banco_luzivan_novo_tabela %>%
      filter(ano == input$ano_grafico) %>%
    #filter(ano == 2016) %>%
    select(2, 5) %>%
    mutate(n_casos = `nascidos vivos`) %>%
    arrange(n_casos) %>%
    top_n(20, n_casos)
  
  ordem <- aux$Cidade
  
  plot_barras <- ggplot(aux, aes(x = Cidade, y = n_casos)) +
    geom_col(fill = "blue") +
    #labs(x = "Municipio", y = "Gráfico das 20 cidades com maior número de nascidos vivos") +
    scale_x_discrete(limits = ordem) +
    #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
    coord_flip()
  
  ggplotly(plot_barras)
  
})

output$pop_serie <- renderPlotly({
  
  aux <- banco_luzivan_novo_tabela %>%
    select(ano, `nascidos vivos`) %>%
    group_by(ano) %>%
    summarise(media = mean(`nascidos vivos`))%>%
    arrange(ano)
  
  #ordem <- as.character(aux$ano)
  aux$ano = as.character(aux$ano)
  aux=data.frame(aux)
  
  plotar = ggplot(aux) + 
    geom_point(aes(x = ano,y = media), color = "blue" ) +
    geom_line(aes(x = ano, y = media, group = 1), color = "blue" ) +
    #geom_text(aes(x = ano,y = media, label = round(media,3))) +
    #scale_x_discrete(limits = ordem) +
    labs(x = "Ano", y = "Média dos número de nascidos vivos") +
    ylim(min(aux$media)-5, max(aux$media)+5)+
    #scale_fill_manual(values = cores_uti_emergencia) +
    #scale_color_manual(values = cores_uti_emergencia) +
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
  ggplotly(plotar)
  
})

######################################################################

output$grafico_serie_pop <- renderPlotly({
  
  serie <- banco_luzivan_novo %>%
    filter(Cidade %in% input$cidade3) 
  
  ggplotly(
    ggplot(serie, aes(x = ano , y = populacao, colour = `Cidade`)) +
      geom_line() +
      geom_point(size = 2)+
      ylim(input$limite3[1],input$limite3[2])+
      labs(x = "Ano", y = "Numero total de nascidos vivos")+
      theme(legend.position = "none")
  )
  
})

output$grafico_serie_casos <- renderPlotly({
  
  serie <- banco_luzivan_novo %>%
    filter(Cidade %in% input$cidade2) 
  
  ggplotly(
    ggplot(serie, aes(x = ano , y = casos, colour = `Cidade`)) +
      geom_line() +
      geom_point(size = 2)+
      ylim(input$limite2[1],input$limite2[2])+
      labs(x = "Ano", y = "Nascidos vivos com anomalias congenitas")+
      theme(legend.position = "none")
  )
  
})

output$grafico_serie_proporcao <- renderPlotly({
  
  serie2 <- banco_luzivan_novo %>%
    filter(Cidade %in% input$cidade1) 
  
  ggplotly(
    ggplot(serie2, aes(x = ano , y = prevalencia, colour = `Cidade`)) +
      geom_line() +
      geom_point(size = 2)+
      ylim(input$limite1[1],input$limite1[2])+
      labs(x = "Ano", y = "Prevalência de anomalias congenitas")+
      theme(legend.position = "none")
  )
  
})


}
shinyApp(ui, server)
