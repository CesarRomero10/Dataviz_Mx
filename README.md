
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "readr", 
                   "shinythemes", "plotly", "rsconnect", "scales", 
                   "RColorBrewer", "shinyWidgets", "webshot2"), 
                 repos = "https://cran.rstudio.com/")
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(shinythemes)
library(plotly)
library(rsconnect)
library(scales)
library(RColorBrewer)
library(shinyWidgets)
library(webshot2)

# Cargar datos y transformar las columnas a mayúscula inicial y minúsculas
data <- read_csv("IDEFF_dic24.csv", locale = locale(encoding = "latin1")) %>%
  mutate(Total = rowSums(select(., ENERO:DICIEMBRE))) %>%
  select(AÑO, ENTIDAD, LEY, CONCEPTO, TIPO, Total) %>%
  rename(
    Año = AÑO,
    Entidad = ENTIDAD,
    Ley = LEY,
    Concepto = CONCEPTO,
    Tipo = TIPO
  ) %>%
  mutate(
    Entidad = tools::toTitleCase(tolower(Entidad)),
    Entidad = case_when(
      Entidad == "Ciudad De Mexico" ~ "Ciudad De México",
      Entidad == "Yucatan" ~ "Yucatán",
      Entidad == "San Luis Potosi" ~ "San Luis Potosí",
      Entidad == "Queretaro" ~ "Querétaro",
      Entidad == "Nuevo Leon" ~ "Nuevo León",
      Entidad == "Michoacan" ~ "Michoacán",
      Entidad == "Mexico" ~ "México",
      TRUE ~ Entidad
    ),
    Ley = tools::toTitleCase(tolower(Ley)),
    Concepto = tools::toTitleCase(tolower(Concepto)),
    Tipo = tools::toTitleCase(tolower(Tipo))
  )

# Definir paleta de colores para todas las entidades (33 colores únicos: Nacional + 32 entidades federativas)
entity_colors <- c(
  "Nacional" = "white",
  "Aguascalientes" = "#FF4500",
  "Baja California" = "#32CD32",
  "Baja California Sur" = "#1E90FF",
  "Campeche" = "#FFD700",
  "Chiapas" = "#8A2BE2",
  "Chihuahua" = "#FF6347",
  "Ciudad De México" = "#FF0000",
  "Coahuila" = "#9ACD32",
  "Colima" = "#DAA520",
  "Durango" = "#BA55D3",
  "Guanajuato" = "#20B2AA",
  "Guerrero" = "#F08080",
  "Hidalgo" = "#6A5ACD",
  "Jalisco" = "#FF1493",
  "México" = "#00FA9A",
  "Michoacán" = "#4169E1",
  "Morelos" = "#FF69B4",
  "Nayarit" = "#FFA500",
  "Nuevo León" = "#ADFF2F",
  "Oaxaca" = "#DC143C",
  "Puebla" = "#00CED1",
  "Querétaro" = "#9400D3",
  "Quintana Roo" = "#FFDAB9",
  "San Luis Potosí" = "#7B68EE",
  "Sinaloa" = "#20B2AA",
  "Sonora" = "#F0E68C",
  "Tabasco" = "#FF4500",
  "Tamaulipas" = "#32CD32",
  "Tlaxcala" = "#1E90FF",
  "Veracruz" = "#FFD700",
  "Yucatán" = "#8A2BE2",
  "Zacatecas" = "#FF6347"
)

pie_palette <- brewer.pal(8, "Set2")
pie_colors <- c(pie_palette, adjustcolor(pie_palette, offset = c(0.2, 0.2, 0.2, 0)), adjustcolor(pie_palette, offset = c(-0.2, -0.2, -0.2, 0)))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #494b4b;
        color: #d4ce46;
      }
      .btn-primary, .btn-info {
        background-color: #0e5135;
        border-color: #0e5135;
        color: #d4ce46;
      }
      .btn-primary:hover, .btn-info:hover {
        background-color: #0d9263;
        border-color: #0d9263;
        color: #d4ce46;
      }
      .dropdown-menu {
        background-color: #0e5135;
        border-color: #0e5135;
      }
      .dropdown-menu > li > a {
        color: #d4ce46;
      }
      .dropdown-menu > li > a:hover {
        background-color: #0d9263;
      }
      .download-btn, .download-graph-btn {
        background-color: #4aba91;
        border-color: #4aba91;
        color: #494b4b;
        margin-right: 10px;
      }
      .download-btn:hover, .download-graph-btn:hover {
        background-color: #0d9263;
        border-color: #0d9263;
        color: #494b4b;
      }
      .centered-plot {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .centered-title {
        text-align: center;
        width: 100%;
      }
      table thead th {
        color: #d4ce46;
      }
      table tbody td {
        color: #FFFFFF;
      }
      .section-line {
        border-top: 4px solid #494b4b;
        margin: 20px 0;
      }
      .button-group {
        display: flex;
        justify-content: center;
        margin-bottom: 10px;
      }
      #bar_filter_value, #trend_filter_value {
        height: 150px;
        overflow-y: auto;
        background-color: #0e5135;
        color: #d4ce46;
      }
      #trend_states {
        height: 150px;
        overflow-y: auto;
        display: block;
        background-color: #0e5135;
        color: #d4ce46;
      }
      #trend_states input[value='all'] + span, #trend_states input[value='none'] + span {
        font-weight: bold;
      }
      div[style*='background-color'] {
        background-color: #494b4b !important;
        border-color: #0e5135 !important;
      }
      .pie-box {
        background-color: #494b4b;
        border: 3px solid #0e5135;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
      }
      #dropdown_bar_filter > button, #dropdown_trend_filter > button, #dropdown_trend_states > button {
        color: #0e5135 !important;
      }
    "))
  ),
  
  tags$h1("Visualización de Víctimas de Incidencia Delictiva en México", style = "color: #d4ce46; font-weight: bold; text-align: center;"),
  tags$hr(class = "section-line"),
  
  tags$div(
    style = "background-color: #494b4b; border-radius: 10px; padding: 15px; margin: 20px; border: 3px solid #0e5135; text-align: justify; font-size: 16px; color: #d4ce46;",
    tags$p("Mtro. José César Romero Galván"),
    tags$p("Estudiante de Doctorado en Ciencias Políticas y Sociales en el campo disciplinario de la Administración Pública en la Universidad Nacional Autónoma de México (UNAM), obtuve Mención Honorífica en mis estudios de maestría en Gobierno y Asuntos Públicos (Políticas Públicas) en la misma institución. Fui reconocido con el segundo lugar en el 1er Premio Nacional de Políticas Públicas, organizado por El Colegio de México (COLMEX) y la Universidad de Monterrey (UDEM) y como uno de los seis Enlaces Universitarios del Banco de México con mejor desempeño en el año 2018. Además, representé al alumnado de Maestría en el Comité Académico del Programa de Posgrado en la UNAM en el periodo 2023 - 2025. He tomado distintos cursos sobre econometría, estadística inferencial y de Ciencia de Datos para las Ciencias Sociales en instituciones como la UNAM, CIDE y FLACSO México. He trabajado en proyectos de investigación y voluntariados en instituciones como CEPAL, UNESCO y UNAM, donde he realizado análisis y visualización de datos utilizando RStudio.")
  ),
  tags$hr(class = "section-line"),
  
  tags$div(
    style = "text-align: justify; font-size: 16px; padding: 10px; color: #d4ce46;",
    tags$p("Este visualizador contiene gráficos interactivos que representan estadísticas oficiales de víctimas de incidencia delictiva en México, obtenidas del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública. El objetivo de esta aplicación es proporcionar una herramienta de visualización de los datos relacionados con la incidencia delictiva, permitiendo analizar las tendencias a lo largo de los años, con el fin de apoyar a los tomadores de decisiones en la mejora de políticas públicas de seguridad."),
    tags$p(style = "color: #d4ce46;", "Datos más recientes disponibles: Diciembre 2024.")
  ),
  tags$hr(class = "section-line"),
  
  fluidRow(
    column(3,
           tags$h4("Filtros para barras", style = "color: #d4ce46; font-weight: bold;"),
           radioButtons("bar_filter_type", "Filtrar por:",
                        choices = c("Concepto" = "concepto", "Tipo de Delito" = "tipo"),
                        selected = "concepto", inline = TRUE),
           dropdownButton(
             selectInput("bar_filter_value", "Valor:",
                         choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Concepto)),
                         selected = "", size = 5, selectize = FALSE, multiple = FALSE),
             label = "Seleccionar valor",
             circle = FALSE,
             status = "custom",
             inputId = "dropdown_bar_filter")
    ),
    column(9,
           tags$h3("Total de víctimas por año", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
           tags$p("Total de víctimas anual, sumando todas las entidades.", 
                  style = "color: #FFFFFF; font-size: 14px; text-align: center;"),
           tags$div(class = "button-group",
                    downloadButton("downloadGraph1", "Descargar gráfica", class = "download-graph-btn"),
                    downloadButton("download_barras", "Descargar datos", class = "download-btn")
           ),
           tags$div(class = "centered-plot",
                    plotlyOutput("dynamicPlot1", height = "500px", width = "90%"))
    )
  ),
  tags$hr(class = "section-line"),
  
  fluidRow(
    column(3,
           tags$h4("Filtros para tendencias por entidad", style = "color: #d4ce46; font-weight: bold;"),
           radioButtons("trend_filter_type", "Filtrar por:",
                        choices = c("Concepto" = "concepto", "Tipo de Delito" = "tipo"),
                        selected = "concepto", inline = TRUE),
           dropdownButton(
             selectInput("trend_filter_value", "Valor:",
                         choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Concepto)),
                         selected = "", size = 5, selectize = FALSE, multiple = FALSE),
             label = "Seleccionar valor",
             circle = FALSE,
             status = "custom",
             inputId = "dropdown_trend_filter")
    ),
    column(9,
           tags$h3("Tendencia de víctimas por entidad", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
           tags$p("Tendencias de víctimas por entidad seleccionada a lo largo de los años.", 
                  style = "color: #FFFFFF; font-size: 14px; text-align: center;"),
           tags$div(class = "button-group",
                    downloadButton("downloadGraph2", "Descargar gráfica", class = "download-graph-btn"),
                    downloadButton("download_tendencia_entidad", "Descargar datos", class = "download-btn")
           ),
           tags$div(class = "centered-plot",
                    plotlyOutput("dynamicPlot2", height = "500px", width = "90%")),
           dropdownButton(
             checkboxGroupInput("trend_states", "Entidades a mostrar:", 
                                choices = c("Marcar todos" = "all", "Desmarcar todos" = "none", "Nacional" = "Nacional", unique(data$Entidad)), 
                                selected = "all"),
             label = "Seleccionar entidades",
             circle = FALSE,
             status = "custom",
             inputId = "dropdown_trend_states")
    )
  ),
  tags$hr(class = "section-line"),
  
  fluidRow(
    column(12, 
           tags$h3("Distribución de Víctimas de Incidencia Delictiva", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
           tags$p("Distribución acumulada de víctimas desde 2012 hasta 2024 por entidad, tipo de delito y concepto.", 
                  style = "color: #FFFFFF; font-size: 14px; text-align: center;")
    ),
    column(4,
           tags$div(class = "pie-box",
                    tags$h4("Por entidad", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
                    tags$p("Distribución porcentual de víctimas por entidad acumulada desde 2012 hasta 2024.", 
                           style = "color: #FFFFFF; font-size: 14px; text-align: center;"),
                    tags$p("Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.", 
                           style = "color: #FFFFFF; font-size: 12px; text-align: center;"),
                    tags$div(class = "button-group",
                             downloadButton("downloadGraphPieState", "Descargar gráfica", class = "download-graph-btn"),
                             downloadButton("download_pie_entidad", "Descargar datos", class = "download-btn")
                    ),
                    tags$div(class = "centered-plot",
                             plotlyOutput("pieState", height = "500px", width = "90%"))
           )
    ),
    column(4,
           tags$div(class = "pie-box",
                    tags$h4("Por tipo de delito", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
                    tags$p("Distribución porcentual de víctimas por tipo de delito acumulada desde 2012 hasta 2024.", 
                           style = "color: #FFFFFF; font-size: 14px; text-align: center;"),
                    tags$p("Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.", 
                           style = "color: #FFFFFF; font-size: 12px; text-align: center;"),
                    tags$div(class = "button-group",
                             downloadButton("downloadGraphPieTipo", "Descargar gráfica", class = "download-graph-btn"),
                             downloadButton("download_pie_tipo", "Descargar datos", class = "download-btn")
                    ),
                    tags$div(class = "centered-plot",
                             plotlyOutput("pieTipo", height = "500px", width = "90%"))
           )
    ),
    column(4,
           tags$div(class = "pie-box",
                    tags$h4("Por concepto", style = "color: #d4ce46; font-weight: normal; text-align: center;"),
                    tags$p("Distribución porcentual de víctimas por concepto acumulada desde 2012 hasta 2024.", 
                           style = "color: #FFFFFF; font-size: 14px; text-align: center;"),
                    tags$p("Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.", 
                           style = "color: #FFFFFF; font-size: 12px; text-align: center;"),
                    tags$div(class = "button-group",
                             downloadButton("downloadGraphPieConcepto", "Descargar gráfica", class = "download-graph-btn"),
                             downloadButton("download_pie_concepto", "Descargar datos", class = "download-btn")
                    ),
                    tags$div(class = "centered-plot",
                             plotlyOutput("pieConcepto", height = "500px", width = "90%"))
           )
    )
  ),
  tags$hr(class = "section-line"),
  
  tags$div(
    tags$h3("Tabla de víctimas de incidencia delictiva", style = "color: #d4ce46; text-align: center; font-weight: normal;"),
    DTOutput("data_table"),
    downloadButton("download_table", "Descargar datos", 
                   class = "download-btn", style = "margin-top: 10px;")
  ),
  tags$hr(class = "section-line"),
  
  tags$footer(
    tags$div(
      style = "text-align: center; padding: 20px; font-size: 14px; color: #d4ce46; background-color: #494b4b; border-radius: 10px; border: 3px solid #0e5135;",
      "Aplicación desarrollada con Shiny de Rstudio por José César Romero Galván.",
      tags$br(),
      "Fuente de los datos: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.",
      tags$br(),
      "Código fuente de esta app y del procesamiento de los datos disponible en",
      tags$a(href = "https://github.com/CesarRomero10/Incidencia_Delictiva", target = "_blank", "GitHub")
    ),
    tags$div(
      style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/e/ed/Logo_SESNSP.png", height = "250px")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if (input$bar_filter_type == "concepto") {
      updateSelectInput(session, "bar_filter_value",
                        choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Concepto)),
                        selected = "")
    } else {
      updateSelectInput(session, "bar_filter_value",
                        choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Tipo)),
                        selected = "")
    }
    
    if (input$trend_filter_type == "concepto") {
      updateSelectInput(session, "trend_filter_value",
                        choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Concepto)),
                        selected = "")
    } else {
      updateSelectInput(session, "trend_filter_value",
                        choices = c("Seleccionar" = "", "Nacional" = "Nacional", unique(data$Tipo)),
                        selected = "")
    }
  })
  
  observeEvent(input$trend_states, {
    all_entities <- c("Nacional", unique(data$Entidad))
    
    if ("all" %in% input$trend_states) {
      updateCheckboxGroupInput(session, "trend_states",
                               selected = all_entities)
    } else if ("none" %in% input$trend_states) {
      updateCheckboxGroupInput(session, "trend_states",
                               selected = character(0))
    } else if (length(input$trend_states) == 0) {
      updateCheckboxGroupInput(session, "trend_states",
                               selected = "Nacional")
    }
  })
  
  filteredDataBars <- reactive({
    df <- data
    if (input$bar_filter_type == "concepto" && input$bar_filter_value != "" && input$bar_filter_value != "Nacional") {
      df <- df %>% filter(Concepto == input$bar_filter_value)
    } else if (input$bar_filter_type == "tipo" && input$bar_filter_value != "" && input$bar_filter_value != "Nacional") {
      df <- df %>% filter(Tipo == input$bar_filter_value)
    }
    if ("Nacional" %in% input$bar_filter_value) {
      df <- df %>% group_by(Año) %>% summarise(Total = sum(Total), .groups = 'drop')
    }
    df
  })
  
  filteredDataTrends <- reactive({
    df <- data
    if (!is.null(input$trend_states) && length(input$trend_states) > 0) {
      selected_entities <- input$trend_states[!input$trend_states %in% c("all", "none")]
      if ("Nacional" %in% selected_entities) {
        total_df <- df %>%
          group_by(Año) %>%
          summarise(Total = sum(Total), .groups = 'drop') %>%
          mutate(Entidad = "Nacional")
        if (length(selected_entities) > 1) {
          entity_df <- df %>% filter(Entidad %in% selected_entities[selected_entities != "Nacional"])
          df <- bind_rows(total_df, entity_df)
        } else {
          df <- total_df
        }
      } else {
        df <- df %>% filter(Entidad %in% selected_entities)
      }
    } else {
      df <- df %>% group_by(Año) %>% summarise(Total = sum(Total), .groups = 'drop') %>% mutate(Entidad = "Nacional")
    }
    
    if (input$trend_filter_type == "concepto" && input$trend_filter_value != "" && input$trend_filter_value != "Nacional") {
      df <- df %>% filter(Concepto == input$trend_filter_value)
    } else if (input$trend_filter_type == "tipo" && input$trend_filter_value != "" && input$trend_filter_value != "Nacional") {
      df <- df %>% filter(Tipo == input$trend_filter_value)
    }
    df
  })
  
  filteredDataEntity <- reactive({
    data
  })
  
  filteredDataTipo <- reactive({
    data
  })
  
  filteredDataConcepto <- reactive({
    data
  })
  
  output$dynamicPlot1 <- renderPlotly({
    df <- filteredDataBars() %>%
      group_by(Año) %>%
      summarise(Total = sum(Total), .groups = 'drop')
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>% 
        layout(title = list(text = "No existe un valor asociado a la entidad seleccionada", 
                            font = list(size = 16, color = "#d4ce46")),
               plot_bgcolor = "#494b4b", 
               paper_bgcolor = "#494b4b",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }
    
    p <- ggplot(df, aes(x = Año, y = Total)) +
      geom_bar(stat = "identity", fill = "#d4ce46", alpha = 0.9) +
      scale_y_continuous(labels = comma) +
      theme_classic(base_size = 12) +
      annotate("text", x = 2015, y = max(df$Total) * 1.1, label = "Enrique Peña Nieto", color = "#d4ce46", size = 4) +
      annotate("text", x = 2021.5, y = max(df$Total) * 1.1, label = "Andrés Manuel López Obrador", color = "#d4ce46", size = 4) +
      geom_vline(xintercept = 2018.5, linetype = "solid", color = "#FFFFFF") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#FFFFFF"),
        axis.text.y = element_text(color = "#FFFFFF"),
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(plot_bgcolor = "#494b4b", paper_bgcolor = "#494b4b", showlegend = FALSE)
  })
  
  output$downloadGraph1 <- downloadHandler(
    filename = function() { "grafica_barras.png" },
    content = function(file) {
      df <- filteredDataBars() %>%
        group_by(Año) %>%
        summarise(Total = sum(Total), .groups = 'drop')
      
      if (nrow(df) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0, y = 0, label = "No existe un valor asociado a la entidad seleccionada", 
                   color = "#d4ce46", size = 5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#494b4b"), 
                panel.background = element_rect(fill = "#494b4b"))
      } else {
        p <- ggplot(df, aes(x = Año, y = Total)) +
          geom_bar(stat = "identity", fill = "#d4ce46", alpha = 0.9) +
          scale_y_continuous(labels = comma) +
          theme_classic(base_size = 12) +
          annotate("text", x = 2015, y = max(df$Total) * 1.1, label = "Enrique Peña Nieto", color = "#d4ce46", size = 4) +
          annotate("text", x = 2021.5, y = max(df$Total) * 1.1, label = "Andrés Manuel López Obrador", color = "#d4ce46", size = 4) +
          geom_vline(xintercept = 2018.5, linetype = "solid", color = "#FFFFFF") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            axis.title = element_blank(),
            plot.background = element_rect(fill = "#494b4b"),
            panel.background = element_rect(fill = "#494b4b")
          )
      }
      
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  output$download_barras <- downloadHandler(
    filename = function() { "datos_barras.csv" },
    content = function(file) {
      write.csv(filteredDataBars() %>% group_by(Año) %>% summarise(Total = sum(Total), .groups = 'drop'), 
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dynamicPlot2 <- renderPlotly({
    df <- filteredDataTrends() %>%
      group_by(Año, Entidad) %>%
      summarise(Total = sum(Total), .groups = 'drop')
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>% 
        layout(title = list(text = "No existe un valor asociado a la entidad seleccionada", 
                            font = list(size = 16, color = "#d4ce46")),
               plot_bgcolor = "#494b4b", 
               paper_bgcolor = "#494b4b",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }
    
    unique_entities <- unique(df$Entidad)
    colors_for_plot <- entity_colors[match(unique_entities, names(entity_colors))]
    
    p <- ggplot(df, aes(x = Año, y = Total, color = Entidad, group = Entidad)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = colors_for_plot) +
      scale_y_continuous(labels = comma) +
      theme_classic(base_size = 12) +
      annotate("text", x = 2015, y = max(df$Total) * 1.1, label = "Enrique Peña Nieto", color = "#d4ce46", size = 4) +
      annotate("text", x = 2021.5, y = max(df$Total) * 1.1, label = "Andrés Manuel López Obrador", color = "#d4ce46", size = 4) +
      geom_vline(xintercept = 2018.5, linetype = "solid", color = "#FFFFFF") +
      theme(
        axis.text = element_text(color = "#FFFFFF"),
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(plot_bgcolor = "#494b4b", paper_bgcolor = "#494b4b", showlegend = FALSE)
  })
  
  output$downloadGraph2 <- downloadHandler(
    filename = function() { "grafica_tendencia_entidad.png" },
    content = function(file) {
      df <- filteredDataTrends() %>%
        group_by(Año, Entidad) %>%
        summarise(Total = sum(Total), .groups = 'drop')
      
      if (nrow(df) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0, y = 0, label = "No existe un valor asociado a la entidad seleccionada", 
                   color = "#d4ce46", size = 5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#494b4b"), 
                panel.background = element_rect(fill = "#494b4b"))
      } else {
        unique_entities <- unique(df$Entidad)
        colors_for_plot <- entity_colors[match(unique_entities, names(entity_colors))]
        
        p <- ggplot(df, aes(x = Año, y = Total, color = Entidad, group = Entidad)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          scale_color_manual(values = colors_for_plot) +
          scale_y_continuous(labels = comma) +
          theme_classic(base_size = 12) +
          annotate("text", x = 2015, y = max(df$Total) * 1.1, label = "Enrique Peña Nieto", color = "#d4ce46", size = 4) +
          annotate("text", x = 2021.5, y = max(df$Total) * 1.1, label = "Andrés Manuel López Obrador", color = "#d4ce46", size = 4) +
          geom_vline(xintercept = 2018.5, linetype = "solid", color = "#FFFFFF") +
          theme(
            axis.text = element_text(color = "#FFFFFF"),
            axis.title = element_blank(),
            plot.background = element_rect(fill = "#494b4b"),
            panel.background = element_rect(fill = "#494b4b")
          )
      }
      
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  output$download_tendencia_entidad <- downloadHandler(
    filename = function() { "datos_tendencia_entidad.csv" },
    content = function(file) {
      write.csv(filteredDataTrends() %>% group_by(Año, Entidad) %>% summarise(Total = sum(Total), .groups = 'drop'), 
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$pieState <- renderPlotly({
    df <- filteredDataEntity() %>%
      group_by(Entidad) %>%
      summarise(Total = sum(Total), .groups = 'drop') %>%
      mutate(Percentage = Total / sum(Total) * 100) %>%
      filter(Percentage > 4)
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>% 
        layout(title = list(text = "No existe un valor asociado a la entidad seleccionada", 
                            font = list(size = 16, color = "#d4ce46")),
               plot_bgcolor = "#494b4b", 
               paper_bgcolor = "#494b4b",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }
    
    plot_ly(df, labels = ~Entidad, values = ~Total, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = pie_colors[1:nrow(df)]),
            showlegend = FALSE,
            height = 500) %>%
      layout(margin = list(t = 100),
             plot_bgcolor = "#494b4b", paper_bgcolor = "#494b4b")
  })
  
  output$downloadGraphPieState <- downloadHandler(
    filename = function() { "grafica_pie_entidad.png" },
    content = function(file) {
      df <- filteredDataEntity() %>%
        group_by(Entidad) %>%
        summarise(Total = sum(Total), .groups = 'drop') %>%
        mutate(Percentage = Total / sum(Total) * 100) %>%
        filter(Percentage > 4)
      
      if (nrow(df) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0, y = 0, label = "No existe un valor asociado a la entidad seleccionada", 
                   color = "#d4ce46", size = 5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#494b4b"), 
                panel.background = element_rect(fill = "#494b4b"))
      } else {
        p <- ggplot(df, aes(x = "", y = Total, fill = Entidad)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = pie_colors[1:nrow(df)]) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "#494b4b"),
            panel.background = element_rect(fill = "#494b4b"),
            legend.position = "none"
          ) +
          geom_text(aes(label = paste0(Entidad, "\n", round(Percentage, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3)
      }
      
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  output$download_pie_entidad <- downloadHandler(
    filename = function() { "datos_pie_entidad.csv" },
    content = function(file) {
      write.csv(filteredDataEntity() %>% group_by(Entidad) %>% summarise(Total = sum(Total), .groups = 'drop') %>%
                  mutate(Percentage = Total / sum(Total) * 100) %>% filter(Percentage > 4), 
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$pieTipo <- renderPlotly({
    df <- filteredDataTipo() %>%
      group_by(Tipo) %>%
      summarise(Total = sum(Total), .groups = 'drop') %>%
      mutate(Percentage = Total / sum(Total) * 100) %>%
      filter(Percentage > 4)
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>% 
        layout(title = list(text = "No existe un valor asociado a la entidad seleccionada", 
                            font = list(size = 16, color = "#d4ce46")),
               plot_bgcolor = "#494b4b", 
               paper_bgcolor = "#494b4b",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }
    
    plot_ly(df, labels = ~Tipo, values = ~Total, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = pie_colors[1:nrow(df)]),
            showlegend = FALSE,
            height = 500) %>%
      layout(margin = list(t = 100),
             plot_bgcolor = "#494b4b", paper_bgcolor = "#494b4b")
  })
  
  output$downloadGraphPieTipo <- downloadHandler(
    filename = function() { "grafica_pie_tipo.png" },
    content = function(file) {
      df <- filteredDataTipo() %>%
        group_by(Tipo) %>%
        summarise(Total = sum(Total), .groups = 'drop') %>%
        mutate(Percentage = Total / sum(Total) * 100) %>%
        filter(Percentage > 4)
      
      if (nrow(df) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0, y = 0, label = "No existe un valor asociado a la entidad seleccionada", 
                   color = "#d4ce46", size = 5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#494b4b"), 
                panel.background = element_rect(fill = "#494b4b"))
      } else {
        p <- ggplot(df, aes(x = "", y = Total, fill = Tipo)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = pie_colors[1:nrow(df)]) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "#494b4b"),
            panel.background = element_rect(fill = "#494b4b"),
            legend.position = "none"
          ) +
          geom_text(aes(label = paste0(Tipo, "\n", round(Percentage, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3)
      }
      
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  output$download_pie_tipo <- downloadHandler(
    filename = function() { "datos_pie_tipo.csv" },
    content = function(file) {
      write.csv(filteredDataTipo() %>% group_by(Tipo) %>% summarise(Total = sum(Total), .groups = 'drop') %>%
                  mutate(Percentage = Total / sum(Total) * 100) %>% filter(Percentage > 4), 
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$pieConcepto <- renderPlotly({
    df <- filteredDataConcepto() %>%
      group_by(Concepto) %>%
      summarise(Total = sum(Total), .groups = 'drop') %>%
      mutate(Percentage = Total / sum(Total) * 100) %>%
      filter(Percentage > 4)
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>% 
        layout(title = list(text = "No existe un valor asociado a la entidad seleccionada", 
                            font = list(size = 16, color = "#d4ce46")),
               plot_bgcolor = "#494b4b", 
               paper_bgcolor = "#494b4b",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }
    
    plot_ly(df, labels = ~Concepto, values = ~Total, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = pie_colors[1:nrow(df)]),
            showlegend = FALSE,
            height = 500) %>%
      layout(margin = list(t = 100),
             plot_bgcolor = "#494b4b", paper_bgcolor = "#494b4b")
  })
  
  output$downloadGraphPieConcepto <- downloadHandler(
    filename = function() { "grafica_pie_concepto.png" },
    content = function(file) {
      df <- filteredDataConcepto() %>%
        group_by(Concepto) %>%
        summarise(Total = sum(Total), .groups = 'drop') %>%
        mutate(Percentage = Total / sum(Total) * 100) %>%
        filter(Percentage > 4)
      
      if (nrow(df) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0, y = 0, label = "No existe un valor asociado a la entidad seleccionada", 
                   color = "#d4ce46", size = 5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#494b4b"), 
                panel.background = element_rect(fill = "#494b4b"))
      } else {
        p <- ggplot(df, aes(x = "", y = Total, fill = Concepto)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = pie_colors[1:nrow(df)]) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "#494b4b"),
            panel.background = element_rect(fill = "#494b4b"),
            legend.position = "none"
          ) +
          geom_text(aes(label = paste0(Concepto, "\n", round(Percentage, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3)
      }
      
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  output$download_pie_concepto <- downloadHandler(
    filename = function() { "datos_pie_concepto.csv" },
    content = function(file) {
      write.csv(filteredDataConcepto() %>% group_by(Concepto) %>% summarise(Total = sum(Total), .groups = 'drop') %>%
                  mutate(Percentage = Total / sum(Total) * 100) %>% filter(Percentage > 4), 
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$data_table <- renderDT({
    datatable(data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              style = "bootstrap")
  })
  
  output$download_table <- downloadHandler(
    filename = function() { "datos_completos.csv" },
    content = function(file) {
      write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)



