#shiny app for History of Earth project

library(shiny)

ui <- fluidPage(
  #App Title ---
  titlePanel("History of Earth"),

  fluidRow(

    column(5,
      h4("Tree")),

    column(7,
      h4("Map"))
  )

)

server <- function(input, output){}

shinyApp(ui = ui, server = server)
