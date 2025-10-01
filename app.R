

######################################################

# Cargado de base de datos y guardado de datos de pob en México en la forma de una tabla resumen
library(tidyverse)
library(readxl)
library(gt)
library(shiny)
library(bslib)
library(sf)


rm(list=ls())

df <- read_excel("data/PIESM (2024) nivel estado-eleccion.xlsx")
# df <- read_excel("PIESM (2024) nivel estado-eleccion.xlsx")

base <- df

entidades <- st_read(dsn = "data/shapefiles/mapas_ent/marcogeoestatal2015_gw.shp")
# entidades <- st_read(dsn = "shapefiles/mapas_ent/marcogeoestatal2015_gw.shp")

entidades <- entidades %>% 
  mutate(estado=case_when(NOM_ENT=="Distrito Federal"~"Ciudad de México",
                          NOM_ENT=="México"~"Estado de México",
                          NOM_ENT=="Michoacán de Ocampo"~"Michoacán",
                          NOM_ENT=="Coahuila de Zaragoza"~"Coahuila",
                          NOM_ENT=="Veracruz de Ignacio de la Llave"~"Veracruz",
                          TRUE~NOM_ENT))

base <- base %>% 
  mutate('Tipo de elección'= factor(case_when(str_ends(election,pattern = "_D1")~"Diputados locales",
                                              str_ends(election,pattern = "_G1")~"Gubernatura",
                                              str_ends(election,pattern = "_M1")~"Munícipes"),
                                    levels = c("Gubernatura","Diputados locales","Munícipes")))
# table(base$`Tipo de elección`)

# Unir variables con shape
z <- base %>% 
  select(estado,year,`Tipo de elección`)

z <- left_join(entidades,z,by="estado")

x <- left_join(entidades,base,by="estado")




# Listado de categorias en vector
tipos <- c("Gubernatura","Diputados locales","Munícipes")
vars <- c("PEIIndexp", "laws.1", "procedures.1", "boundaries.1", 
          "votereg", "partyreg", "media", "finance", 
          "voting", "count", "results", "EMBs")
vars2 <- c("PEIIndexp_i", "laws.1_i", "procedures.1_i", "boundaries.1_i", 
           "votereg_i", "partyreg_i", "media_i", "finance_i", 
           "voting_i", "count_i", "results_i", "EMBs_i")




# ui
# ui <- page_fluid(
#   br(),
#   br(),
#   br(),
#   
#   h1("Proyecto de Integridad Electoral Subnacional en México 2024"),
#   # br(),
#   br(),
#   p("Visualizador de resultados de la encuesta a expertos para evaluar la integridad de las elecciones subnacionales analizadas en México en 2024"),
#   br(),
#   br(),
#   
#   h5("Recuento de elecciones analizadas"),
#   gt_output("tabla"),
#   br(),
#   em("Selección por tipo de elección"),
#   selectInput("tipo",
#               "",
#               choices = tipos),
#   br(),
#   plotOutput("graf"),
#   br(),
#   gt_output("tabla2"),
# 
# 
#   br(),
#   br(),
#   h5("Resultados de los indicadores por elección"),
#   em("Selección por tipo de elección"),
#   selectInput("tipo",
#               "",
#               choices = tipos),
#   selectInput("var", 
#               "",
#               choices = vars),
#   br(),
#   plotOutput("graf2", height = "600px"),
# 
#   br(),
#   br(),
#   h5("Resultados de los indicadores con valores imputados"),
#   em("Selección por tipo de elección"),
#   selectInput("tipo",
#               "",
#               choices = tipos),
#   selectInput("var2", 
#               "",
#               choices = vars2),
#   br(),
#   plotOutput("graf3", height = "600px"),
#   br(),
#   
#   
#   br(),
#   br(),
#   br()
#   
#   
#   
# )



# ui
ui <- page_navbar(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Slab"),
    primary = "#0029a3",
    secondary = "#7F7F7F"
  ) |> bs_add_rules("
  .navbar { background-color: #0029a3 !important; }
  .navbar .nav-link, .navbar-brand { color: white !important; }
"),
  
  
  title = "PIE México 2024",
  
  # Pestaña introducción
  nav_panel("Inicio",
            layout_column_wrap(width = 1,
                               card(
                                 card_header("Acerca del visualizador"),
                                 card_body(
                                   h2("Proyecto de Integridad Electoral Subnacional en México 2024"),
                                   p("Este visualizador interactivo presenta los resultados de la 
             encuesta a expertos del Proyecto de 
             Integridad Electoral (PIE) México 2024."),
                                   p("Desde 2012 el Electoral Integrity Project (EIP), 
                                   originalmente encabezado por Pippa Norris, de las universidades de Harvard y Sydney, 
                                   realiza encuestas a expertas y expertos por país para conocer su percepción de la 
                                   integridad de las elecciones en cada nación. En México, en 2015 y 2016, 
                                   investigadores/as de FLACSO México asociados/as al equipo del EIP, han entrevistado 
                                   a expertas/os locales para conocer la integridad electoral de las contiendas en los 
                                   32 estados de la República mexicana. De 2015 a 2024 se ha calibrado la integridad de 
                                   142 procesos electorales locales entrevistando a 1,703 expertas/os con el objetivo 
                                   de es evaluar la integridad de las elecciones 
             subnacionales en México a través de distintos indicadores, 
             permitiendo la comparación entre entidades federativas y 
             tipos de elección."),
                                   p("A continuación se presentan los resultados de la edición 2024, que integra un total de 
                                     336 respuestas para un agregado de 32 procesos electorales subnacionales 
                                     (nueve elecciones de gobernadores/as, 22 de diputados/as locales y una de munícipes"),
                                   p("En las pestañas superiores encontrarás las siguientes secciones:"),
                                   tags$ul(
                                     tags$li(strong("Resumen:"), " panorama general de las elecciones analizadas y 
                   mapa de cobertura."),
                                     tags$li(strong("Indicadores:"), " resultados detallados de cada indicador, 
                   con y sin imputación en valores perdidos."),
                                     tags$li(strong("Puntuaciones:"), " resultados detallados de los indicadores con imputación 
                                             en valores perdidos para cada entidad.")
                                   ),
                                   p("Fuente de datos: PIES México 2024. Disponible en Harvard Dataverse."),
                                   p(em("Loza, Nicolas; Elvira Torres, Diego Enrique; Coca Rios, Itzel, 2025, 
                                        PIESM (2024) nivel estado-eleccion, https://doi.org/10.7910/DVN/60BT7S, Harvard Dataverse"))
                                 )
                               )
            )
  ),
  
  # Pestaña 1
  nav_panel("Resumen",
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Elecciones analizadas (2024)"),
                gt_output("tabla")
              ),
              card(
                card_header("Mapa de elecciones por tipo"),
                selectInput("tipo", "Selecciona tipo de elección", choices = tipos),
                plotOutput("graf", height = "600px"),
                gt_output("tabla2")
              )
            )
  ),
  
  # Pestaña 2
  nav_panel("Indicadores",
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Resultados"),
                selectInput("tipo3", "Tipo de elección", choices = tipos),
                selectInput("var", "Indicador", choices = vars),
                plotOutput("graf2", height = "600px")
              ),
              card(
                card_header("Resultados con imputación"),
                selectInput("tipo2", "Tipo de elección", choices = tipos),
                selectInput("var2", "Indicador (imputado)", choices = vars2),
                plotOutput("graf3", height = "600px")
              )
            )
  ),
  
  # Pestaña 3
  nav_panel("Puntuaciones",
            card(
              card_header("Resultados por entidad"),
              selectInput("tipo4", "Tipo de elección", choices = tipos),
              selectInput("var3", "Indicador (imputado)", choices = vars2),
              plotOutput("graf4", height = "600px")
            )
            
  )
)



# server
server <- function(input, output, session) {
  
  datos1 <- reactive({
    z %>%
      filter(`Tipo de elección`==input$tipo)
    
  })
  
  datos2 <- reactive({
    base %>%
      filter(`Tipo de elección`==input$tipo)
    
  })
  
  datos3 <- reactive({
    base %>%
      filter(`Tipo de elección`==input$tipo3) %>% 
      rename(Var=!!sym(input$var))
  })
  
  datos4 <- reactive({
    base %>%
      filter(`Tipo de elección`==input$tipo2) %>% 
      rename(Var=!!sym(input$var2))
  })
  
  datos5 <- reactive({
    x %>%
      filter(`Tipo de elección` == input$tipo4) %>% 
      rename(Var = !!sym(input$var3))
  })
  
  
  output$tabla <- render_gt({ 
    base %>%
      group_by(`Tipo de elección`) %>% 
      summarise(n=n(),
                n_exp=sum(numresponses),
                tasa=sum(numresponses)/sum(contacted)
      ) %>% 
      ungroup() %>% 
      gt() %>% cols_label(n="# de elecciones",n_exp="Respuestas de expertos",
                          tasa="Tasa de respuesta"
      ) %>%
      tab_header(title = paste0("Elecciones analizadas (2024)")) %>% 
      gt::fmt_percent(columns = tasa, decimals = 1) %>%
      data_color(columns = `Tipo de elección`, rows = `Tipo de elección`=="Gubernatura", palette = "#0029a3") %>% 
      data_color(columns = `Tipo de elección`, rows = `Tipo de elección`=="Diputados locales", palette = "grey50") %>%
      data_color(columns = `Tipo de elección`, rows = `Tipo de elección`=="Munícipes", palette = "grey69")
  })
  
  
  output$tabla2 <- render_gt({ 
    datos2() %>%
      select(estado) %>% 
      gt() %>% cols_label(estado=paste0("Entidades con elección de ", input$tipo)
      )
  })
  
  
  output$graf <- renderPlot({
    datos1() %>%
      ggplot() +
      geom_sf(data = entidades, color="black",fill="white",alpha=1) +
      
      geom_sf(aes(fill=`Tipo de elección`),color="grey50") +
      # geom_sf_label(aes(label=CVE_ENT),size=2.5) +
      # labs(title="Entidades analizadas") +
      scale_fill_manual(values = c("#0029a3","grey50","grey69")) +
      guides(color=F)+
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  output$graf2 <- renderPlot({
    datos3() %>%
      ggplot(aes(x=reorder(estado,Var),y=Var)) +
      geom_bar(stat = 'identity',alpha=1,fill = "#0029a3",color="grey50",width=.75) +
      scale_y_continuous(limits = c(0,90),breaks = seq(10,100,10)) +
      xlab("")+ylab("") +
      labs(title=paste0("Puntuación por entidad del ",input$var),
           caption="Fuente: Datos el PIE México 2024.Disponibles en Integridad electoral FLACSO México 2024 
           https://doi.org/10.7910/DVN/60BT7S") +
      geom_text(aes(label = round(Var,1)), hjust = -.1, cex=5.75,color="grey10")+
      geom_text(aes(label = estado), hjust = 1.15, cex=4.5,color="white") +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size=14),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      ) +
      coord_flip()
  }) 
  
  
  
  output$graf3 <- renderPlot({
    datos4() %>%
      ggplot(aes(x=reorder(estado,Var),y=Var)) +
      geom_bar(stat = 'identity',alpha=1,fill = "#0029a3",color="grey50",width=.75) +
      scale_y_continuous(limits = c(0,90),breaks = seq(10,100,10)) +
      xlab("")+ylab("") +
      labs(title=paste0("Puntuación por entidad del ",input$var2),
           caption="Fuente: Datos el PIE México 2024. Disponibles en Integridad electoral FLACSO México 2024 
           https://doi.org/10.7910/DVN/60BT7S") +
      geom_text(aes(label = round(Var,1)), hjust = -.1, cex=5.75,color="grey10")+
      geom_text(aes(label = estado), hjust = 1.15, cex=4.5,color="white") +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size=14),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      ) +
      coord_flip()
  }) 
  
  
  output$graf4 <- renderPlot({
    datos5() %>%
      mutate(Var_cat = factor(case_when(
        Var <= 40 ~ "Muy baja (< 40)",
        Var > 40 & Var <= 50 ~ "Baja (< 50)",
        Var > 50 & Var <= 60 ~ "Media (< 60)",
        Var > 60 & Var <= 70 ~ "Alta (< 70)",
        Var > 70 ~ "Muy alta (> 70)"
      ), levels = c("Muy baja (< 40)","Baja (< 50)","Media (< 60)","Alta (< 70)","Muy alta (> 70)"))) %>% 
      ggplot() +
      geom_sf(data = entidades, color="black",fill="white",alpha=1) +
      geom_sf(aes(fill = Var_cat), color = "grey50") +
      scale_fill_manual(name="",values = c(
        "Muy baja (< 40)" = "red",
        "Baja (< 50)" = "orange",
        "Media (< 60)" = "grey69",
        "Alta (< 70)" = "green4",
        "Muy alta (> 70)" = "limegreen"
      )) +
      guides(color = FALSE) +
      theme_void() +
      theme(legend.position = "left")
  })
  
  
}



shinyApp(ui, server)




# library(shinylive)
#
# # Exporta la app a HTML y recursos estáticos
shinylive::export(appdir = ".", destdir = "docs")

# remotes::install_version("ggplot2", version = "3.5.2", repos = "https://cran.r-project.org")
# packageVersion("ggplot2")


