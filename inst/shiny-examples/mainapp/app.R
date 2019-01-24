ui <- fluidPage(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title= "History of Earth"),
    shinydashboard::dashboardSidebar(width = 175,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      #  id = "tabs",
        selectInput("genus", "Choose a genus:",
             choices = HistoryOfEarth::GetTaxa(),
             multiple = TRUE)),

          shinydashboard::menuItem(
        selectInput("period", "Choose a period:",
             choices = HistoryOfEarth::GetAgeDF()$Period,
             multiple = FALSE)))),

    shinydashboard::dashboardBody(
      tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #000000;
                                  color: #ffffff
                                }
                                '))),

        fluidRow(

          column(5,
            h4("Tree"),
            shinydashboard::box(plotOutput("tree"))),

            column(7,
              h4("Map"),
              shinydashboard::box(plotOutput("map")))
      )

    )

  )
)


server <- function(input, output) {

  output$tree <- renderPlot({
  #  library(paleotree)

    data(chronogram, package="HistoryOfEarth")
    paleotree::plotPhylopicTreePBDB(tree = HistoryOfEarth::GetTree())
    ape::axisPhylo()
  })
  
  data(paleomaps, package="HistoryOfEarth")

  chosen_period <- reactive({c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary") == input$period})
  output$map <- renderPlot({paleomaps[[chosen_period()]]})
  #error in [[: attempt to select less than one element in integerOneIndex


}

shinyApp(ui, server)