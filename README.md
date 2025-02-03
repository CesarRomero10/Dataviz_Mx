# Dataviz_Mx
Este código es una aplicación Shiny en R que permite visualizar y analizar datos de víctimas de incidencia delictiva en México.

# Visualización Interactiva de Incidencia Delictiva con Shiny

## Descripción General
Esta aplicación, desarrollada en **Shiny para R**, permite la exploración interactiva de datos de incidencia delictiva en México a través de filtros por entidad, concepto, tipo de delito y rango de años. Los resultados se presentan en **gráficos interactivos** (con *Plotly*) y una **tabla dinámica**, con la opción de descargar los datos filtrados en formato CSV.

El diseño de la interfaz está personalizado con un esquema visual que incluye el color **guinda**, representativo del Gobierno de México. Este proyecto representa mi primera aplicación desarrollada en **Shiny**, me permitió fortalecer mis habilidades en **RStudio** y en visualización de datos.

---

## Visualización de Víctimas de Incidencia Delictiva en México

Este repositorio contiene una aplicación desarrollada en R utilizando el paquete Shiny para la visualización de datos sobre víctimas de incidencia delictiva en México. La aplicación permite la exploración de datos a través de filtros interactivos, gráficos y tablas de datos.

## Archivos principales

## Archivo ui.R

library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      /* Personalizar el sliderInput */
      .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
        background-color: #800000;  /* Color guinda */
        border-color: #800000;
      }
      .irs-line {
        background-color: #e0e0e0;
        border-color: #e0e0e0;
      }
      .irs-grid-text {
        color: #800000;
      }

      /* Personalizar botones */
      .btn-primary {
        background-color: #800000;
        border-color: #800000;
        color: #FFFFFF;
      }
      .btn-primary:hover {
        background-color: #600000;
        border-color: #600000;
      }
      .btn-info {
        background-color: #800000;  /* Color guinda */
        border-color: #800000;
        color: #FFFFFF;  /* Texto blanco */
      }
      .btn-info:hover {
        background-color: #600000;  /* Guinda más oscuro */
        border-color: #600000;
      }

      /* Color de la letra del título en guinda y negrita */
      .navbar-brand {
        color: #800000 !important;
        font-weight: bold !important;
      }

      /* Cambiar color de los enlaces de los gráficos a guinda */
      .nav-tabs > li > a {
        color: #800000 !important; /* Color guinda */
        font-weight: bold;
      }
      .nav-tabs > li > a:hover {
        color: #600000 !important; /* Guinda más oscuro al pasar el mouse */
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        color: #fff !important; /* Texto blanco */
        background-color: #800000 !important;
        border-color: #800000 !important;
      }
    "))
  ),
  
  tags$div(
    style = "display: flex; justify-content: flex-start;",
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/f/f8/Logo_del_Gobierno_de_M%C3%A9xico_%282018%29.png",
      height = "150px"
    )
  ),
  
  titlePanel(
    span("Visualización de Víctimas de Incidencia Delictiva en México", 
         style = "color: #800000; font-weight: bold;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state1", "Selecciona la primera Entidad:", choices = ""),
      selectInput("state2", "Selecciona la segunda Entidad:", choices = ""),
      selectInput("concept", "Por concepto:", choices = ""),
      selectInput("crimeType", "Por tipo de delito:", choices = ""),
      sliderInput("yearRange", "Rango de Años:", 
                  min = 2012, max = 2024, value = c(2012, 2024), step = 1, sep = ""),
      
      downloadButton("downloadData", "Descargar Datos Filtrados", class = "btn-info")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico 1: Número de Víctimas por Año y Estado",
                 fluidRow(column(12, plotlyOutput("dynamicPlot1", height = "400px")))),
        tabPanel("Gráfico 2: Tendencia de Víctimas por Entidad",
                 fluidRow(column(12, plotlyOutput("dynamicPlot2", height = "400px")))),
        tabPanel("Tabla de Datos Filtrados", DTOutput("table")),
        tabPanel("Ayuda",
                 h3("Cómo usar la aplicación", style = "color: #800000;"),
                 p("Esta aplicación permite analizar los datos de víctimas de incidencia delictiva en México.", style = "color: #800000;"),
                 p("1. Selecciona las dos entidades, el concepto y/o el tipo de delito que deseas analizar.", style = "color: #800000;"),
                 p("2. Usa el rango de años para filtrar los datos.", style = "color: #800000;"),
                 p("3. Haz clic en los filtros para aplicar los cambios.", style = "color: #800000;"),
                 p("4. Explora las gráficas y la tabla de datos filtrados.", style = "color: #800000;"),
                 p("5. Puedes descargar los datos filtrados haciendo clic en Descargar Datos Filtrados.", style = "color: #800000;"))
      )
    )
  ),
  
  tags$footer(
    style = "text-align: center; padding: 20px; font-size: 12px; color: #800000;",
    "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.",
    br(),
    "Aplicación creada con Shiny de Rstudio por José César Romero Galván",
    tags$a(
      href = "https://github.com/CesarRomero10/Dataviz_Mx/blob/main/README.md",
      "disponible en el repositorio de GitHub"
    )
  )
)


## Archivo server.R

library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)  # Para usar str_to_title()

# Leer los datos y transformar los nombres de las entidades
datos <- read_csv("IDEFF_dic24.csv", locale = locale(encoding = "latin1")) %>%
  mutate(Total = rowSums(select(., ENERO:DICIEMBRE))) %>%
  select(AÑO, ENTIDAD, LEY, CONCEPTO, TIPO, Total) %>%
  mutate(
    ENTIDAD = str_to_title(ENTIDAD),  # Convertir ENTIDAD a formato "Primera Letra Mayúscula"
    CONCEPTO = str_to_title(CONCEPTO),  # Convertir CONCEPTO a formato "Primera Letra Mayúscula"
    TIPO = str_to_title(TIPO)  # Convertir TIPO a formato "Primera Letra Mayúscula"
  )

server <- function(input, output, session) {
  
  # Actualizar opciones de los selectInput
  updateSelectInput(session, "state1", choices = c("", "Todos", unique(datos$ENTIDAD)))
  updateSelectInput(session, "state2", choices = c("", "Todos", unique(datos$ENTIDAD)))
  updateSelectInput(session, "concept", choices = c("", "Seleccionar", unique(datos$CONCEPTO)))
  updateSelectInput(session, "crimeType", choices = c("", "Seleccionar", unique(datos$TIPO)))
  
  # Filtrar datos según los inputs
  filteredData <- reactive({
    df <- datos %>%
      filter(AÑO >= input$yearRange[1] & AÑO <= input$yearRange[2])
    
    selectedStates <- c()
    if (input$state1 != "" && input$state1 != "Todos") {
      selectedStates <- c(selectedStates, input$state1)
    }
    if (input$state2 != "" && input$state2 != "Todos") {
      selectedStates <- c(selectedStates, input$state2)
    }
    if (length(selectedStates) > 0) {
      df <- df %>% filter(ENTIDAD %in% selectedStates)
    }
    
    if (input$concept != "" && input$concept != "Seleccionar") {
      df <- df %>% filter(CONCEPTO == input$concept)
    }
    if (input$crimeType != "" && input$crimeType != "Seleccionar") {
      df <- df %>% filter(TIPO == input$crimeType)
    }
    
    df
  })
  
  # Gráfico 1: Barras
  output$dynamicPlot1 <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = as.factor(AÑO), y = Total, fill = ENTIDAD, 
                        text = paste("Año:", AÑO, "<br>Víctimas:", Total, "<br>Entidad:", ENTIDAD))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Número de Víctimas por Año y Estado", 
           x = "Año", y = "Número de Víctimas", fill = "Entidad")  # Cambiar etiqueta de la leyenda
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico 2: Líneas
  output$dynamicPlot2 <- renderPlotly({
    df <- filteredData() %>%
      group_by(AÑO, ENTIDAD) %>%
      summarise(Total = sum(Total), .groups = 'drop')
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No hay datos para las entidades seleccionadas."))
    }
    
    p <- ggplot(df, aes(x = AÑO, y = Total, color = ENTIDAD, group = ENTIDAD,
                        text = paste("Año:", AÑO, "<br>Víctimas:", Total, "<br>Entidad:", ENTIDAD))) +
      geom_line() +
      theme_minimal() +
      labs(title = "Tendencia de Víctimas por Entidad", 
           x = "Año", y = "Número de Víctimas", color = "Entidad")  # Cambiar etiqueta de la leyenda
    ggplotly(p, tooltip = "text")
  })
  
  # Tabla de datos
  output$table <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  # Descargar datos filtrados
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}


## Fuente de Datos

Los datos utilizados provienen del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.

## Referencias 

Blog de Bastián Olea Herrera: https://bastianolea.rbind.io/apps/

Cuenta de X de Rosana Ferrero: @RosanaFerrero

Documentación de Shiny: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/

Canales de YouTube: Ravinder Ram y Academatica

Autor

Aplicación creada por José César Romero Galván.



