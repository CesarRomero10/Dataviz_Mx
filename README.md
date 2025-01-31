# Dataviz_Mx
Este código es una aplicación Shiny en R que permite visualizar y analizar datos de víctimas de incidencia delictiva en México.
# Visualización Interactiva de Incidencia Delictiva con Shiny

## Descripción General
Esta aplicación, desarrollada en **Shiny para R**, permite la exploración interactiva de datos de incidencia delictiva en México a través de filtros por entidad, concepto, tipo de delito y rango de años. Los resultados se presentan en **gráficos interactivos** (con *Plotly*) y una **tabla dinámica**, con la opción de descargar los datos filtrados en formato CSV.

El diseño de la interfaz está personalizado con un esquema visual que incluye el color **guinda**, representativo del Gobierno de México. Este proyecto representa mi primera incursión en **Shiny**, permitiéndome fortalecer mis habilidades en **RStudio** y visualización de datos.

---

## Instalación y Configuración

### Requisitos
La aplicación requiere los siguientes paquetes de R:
- **shiny**: Para crear la aplicación web interactiva.
- **ggplot2**: Para generar gráficos estáticos.
- **dplyr**: Para manipular y filtrar datos.
- **DT**: Para mostrar tablas interactivas.
- **readr**: Para leer el archivo CSV.
- **shinythemes**: Para aplicar un tema visual predefinido.
- **plotly**: Para convertir los gráficos en interactivos.

Para instalar los paquetes necesarios, ejecute en R:
```r
# Instalar paquetes si no están instalados
required_packages <- c("shiny", "ggplot2", "dplyr", "DT", "readr", "shinythemes", "plotly")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Cargar paquetes
lapply(required_packages, library, character.only = TRUE)
```

---

## Carga y Preparación de Datos

1. Se carga un archivo CSV (`IDEFF_dic24.csv`) con datos de incidencia delictiva, utilizando `read_csv` con codificación *latin1* para manejar caracteres especiales.
2. Se realiza una transformación de datos:
   - Se crea una columna `Total` que suma las víctimas mensuales.
   - Se seleccionan las columnas clave: **AÑO, ENTIDAD, LEY, CONCEPTO, TIPO, Total**.

```r
# Cargar datos
archivo <- "IDEFF_dic24.csv"
datos <- read_csv(archivo, locale = locale(encoding = "latin1"))

# Transformar datos
datos <- datos %>% 
  mutate(Total = rowSums(select(., starts_with("MES")), na.rm = TRUE)) %>%
  select(AÑO, ENTIDAD, LEY, CONCEPTO, TIPO, Total)
```

---

## Interfaz de Usuario (UI)

La interfaz está construida con `fluidPage()` y usa el tema *flatly* de **shinythemes**. Se incluyen:

- **Controles de filtrado**: Selectores para entidad, concepto, tipo de delito y rango de años.
- **Gráficos interactivos**:
  - Un **gráfico de barras** para visualizar tendencias de víctimas.
  - Un **gráfico de líneas** para variaciones temporales.
- **Tabla dinámica** con los datos filtrados.
- **Botón de descarga** para exportar los datos en CSV.
- **Sección de ayuda** con instrucciones.

---

## Lógica del Servidor (Server)

El servidor maneja:

- **Filtrado de datos**: Se crea un objeto reactivo (`filteredData`).
- **Generación de visualizaciones**:
  - **Gráfico de barras** (ggplot2 + plotly).
  - **Gráfico de líneas** interactivo.
- **Tabla interactiva** con `DT::datatable()`.
- **Descarga de datos filtrados** en CSV.

```r
# Filtrado de datos
data_filtered <- reactive({
  datos %>%
    filter(
      ENTIDAD %in% input$entidad,
      CONCEPTO %in% input$concepto,
      TIPO %in% input$tipo,
      AÑO >= input$anio[1], AÑO <= input$anio[2]
    )
})
```

---

## Personalización Visual

Se aplican estilos CSS para mejorar la apariencia:
- **Colores personalizados** en botones y gráficos.
- **Tipografía y estilos** para mejorar la legibilidad.
- **Incorporación de un logotipo** del Gobierno de México.

---

## Funcionalidad Adicional

- **Manejo de datos vacíos**: Mensajes personalizados en caso de falta de datos.
- **Interactividad avanzada** con `plotly`.
- **Accesibilidad**: Sección de ayuda clara y concisa.

---

## Referencias

- Blog de Bastián Olea Herrera: [https://bastianolea.rbind.io/apps/](https://bastianolea.rbind.io/apps/)
- Documentación de Shiny: [https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/)
- Videos en YouTube sobre Shiny y visualización de datos.

---

## Próximos Pasos



Mis planes de mejora incluyen:
- Optimizar las visualizaciones.
- Incorporar más tipos de gráficos.
- Mejorar el rendimiento para grandes volúmenes de datos.

---

## Fuente de Datos

Los datos provienen del **Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública**
