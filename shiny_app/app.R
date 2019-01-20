#shiny app for History of Earth project

library(shiny)
library(shinydashboard)


taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","Araucarioxylon")


ui <- fluidPage(
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(width = 175,
      sidebarMenu(menuItem(
      # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        selectInput("genus", "Choose a genus:",
             c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","Araucarioxylon"),
             multiple = TRUE)))),

    dashboardBody(
      tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #000000;
                                  color: #ffffff
                                }
                                '))),

        fluidRow(

          column(5,
            h4("Tree")),

            column(7,
              h4("Map"))
      )

    )

  )
)

server <- function(input, output) {

}

shinyApp(ui, server)






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
