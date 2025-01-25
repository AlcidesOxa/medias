library(shiny)
library(plotly)
library(dplyr)
library(readxl)
library(shinythemes)
df = read_excel("satisfaccion_consumidor.xls")

df$genero = factor(df$genero)
df$tipocliente = factor(df$tipocliente)
df$tipoviaje = factor(df$tipoviaje)
df$clase = factor(df$clase)

ui <- fluidPage(theme = shinytheme("darkly"),
  titlePanel("Gráfico de medias por dos variables categóricas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Variable categórica 1", choices = NULL),
      selectInput("cat2", "Variable categórica 2", choices = NULL),
      selectInput("quant", "Variable cuantitativa", choices = NULL)
    ),
    mainPanel(
      plotlyOutput("anovaPlot")
    )
  )
)

server <- function(input, output, session) {
  # Poblar selectores dinámicos
  observe({
    updateSelectInput(session, "cat1", choices = names(Filter(is.factor, df)))
    updateSelectInput(session, "cat2", choices = names(Filter(is.factor, df)))
    updateSelectInput(session, "quant", choices = names(Filter(is.numeric, df)))
  })
  output$anovaPlot <- renderPlotly({
    req(input$cat1, input$cat2, input$quant) # Asegurarse de que los inputs estén seleccionados
    summary_data <- df %>%
      group_by(across(all_of(c(input$cat1, input$cat2)))) %>%
      summarize(mean_value = mean(.data[[input$quant]], na.rm = TRUE), .groups = "drop")
    plot <- plot_ly(
      data = summary_data,
      x = ~.data[[input$cat1]],
      y = ~mean_value,
      color = ~.data[[input$cat2]],
      type = "scatter",
      mode = "lines+markers",
      text = ~round(mean_value, 2),
      textposition = "outside"
    ) %>%
      layout(
        title = "Medias por 2 variables",
        xaxis = list(title = input$cat1),
        yaxis = list(title = paste("Media de", input$quant)),
        barmode = "group"
      )
    plot
  })
}

shinyApp(ui, server)

