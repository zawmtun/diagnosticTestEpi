library(shiny)

ui <- fluidPage(
  textInput("name", "Type your name", placeholder = "Name"),
  verbatimTextOutput("greeting")
)

server <- function(input, output, session) {
  text <- reactive(paste0("Hello ", input$name, "!"))

  output$greeting <- renderText(text())

  observeEvent(input$name, {
    message("Greeting performed")
  })
}

shinyApp(ui, server)
