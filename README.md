# Dataviz_Mx
Este código es una aplicación Shiny en R que permite visualizar y analizar datos de víctimas de incidencia delictiva en México.
# Instalar paquetes necesarios si no están instalados
required_packages <- c("shiny", "ggplot2", "dplyr", "DT", "readr", "shinythemes", "plotly")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

# Carga de librerias
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(shinythemes)
library(plotly)

# Cargar datos
data <- read_csv("~/Downloads/IDEFF_dic24.csv", locale = locale(encoding = "latin1")) %>%  
  mutate(Total = rowSums(select(., ENERO:DICIEMBRE))) %>%
  select(AÑO, ENTIDAD, LEY, CONCEPTO, TIPO, Total)

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
        background-color: #800000;  /* Cambiado a guinda */
        border-color: #800000;
        color: #FFFFFF;  /* Texto blanco */
      }
      .btn-info:hover {
        background-color: #600000;  /* Guinda más oscuro al pasar el mouse */
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
  
  titlePanel(span("Visualización de Víctimas de Incidencia Delictiva en México", style = "color: #800000; font-weight: bold;")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state1", "Selecciona la primera Entidad:", choices = c("", "Todos", unique(data$ENTIDAD))),
      selectInput("state2", "Selecciona la segunda Entidad:", choices = c("", "Todos", unique(data$ENTIDAD))),
      selectInput("concept", "Por concepto:", choices = c("", "Seleccionar", unique(data$CONCEPTO))),
      selectInput("crimeType", "Por tipo de delito:", choices = c("", "Seleccionar", unique(data$TIPO))),
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
                 p("1. Selecciona las dos entidades, el concepto y el tipo de delito que deseas analizar.", style = "color: #800000;"),
                 p("2. Usa el rango de años para filtrar los datos.", style = "color: #800000;"),
                 p("3. Haz clic en los filtros para aplicar los cambios.", style = "color: #800000;"),
                 p("4. Explora los gráficos y la tabla de datos filtrados.", style = "color: #800000;"),
                 p("5. Puedes descargar los datos filtrados haciendo clic en 'Descargar Datos Filtrados'.", style = "color: #800000;"))
      )
    )
  ),
  
  tags$footer(
    style = "text-align: center; padding: 20px; font-size: 12px; color: #800000;",
    "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.",
    br(),
    "Aplicación creada con Shiny de Rstudio por José César Romero Galván."
  )
)

server <- function(input, output, session) {
  
  # Datos filtrados
  filteredData <- reactive({
    df <- data %>%
      filter(AÑO >= input$yearRange[1] & AÑO <= input$yearRange[2])
    
    if (input$state1 != "" && input$state1 != "Todos") {
      df <- df %>% filter(ENTIDAD == input$state1)
    }
    if (input$state2 != "" && input$state2 != "Todos") {
      df <- df %>% filter(ENTIDAD == input$state2)
    }
    if (input$concept != "" && input$concept != "Seleccionar") {
      df <- df %>% filter(CONCEPTO == input$concept)
    }
    if (input$crimeType != "" && input$crimeType != "Seleccionar") {
      df <- df %>% filter(TIPO == input$crimeType)
    }
    
    return(df)
  })
  
  # Gráfico 1: Tendencia de Víctimas por Año y Estado
  output$dynamicPlot1 <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = as.factor(AÑO), y = Total, fill = ENTIDAD)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom") + 
      labs(title = "Tendencia de Víctimas por Año y Estado", x = "Año", y = "Número de Víctimas")
    ggplotly(p)
  })
  
  # Gráfico 2: Número de Víctimas por Entidad
  output$dynamicPlot2 <- renderPlotly({
    df <- filteredData() %>%
      group_by(AÑO, ENTIDAD) %>%
      summarise(Total = sum(Total), .groups = 'drop')
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No hay datos para las entidades seleccionadas."))
    }
    
    p <- ggplot(df, aes(x = AÑO, y = Total, color = ENTIDAD, group = ENTIDAD)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Número de Víctimas por Entidad", 
           x = "Año", y = "Número de Víctimas", color = "Entidad")
    ggplotly(p)
  })
  
  # Tabla de Datos Filtrados
  output$table <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  # Lógica para descargar los datos filtrados
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
