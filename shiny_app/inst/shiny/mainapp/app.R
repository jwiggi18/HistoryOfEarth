ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title= "History of Earth"),
    dashboardSidebar(width = 175,
      sidebarMenu(
        menuItem(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      #  id = "tabs",
        selectInput("genus", "Choose a genus:",
             choices = HistoryOfEarth::GetTaxa(),
             multiple = TRUE)),

          menuItem(
        selectInput("period", "Choose a period:",
             choices = HistoryOfEarth::GetAgeDF()$Period,
             multiple = FALSE)))),

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
            h4("Tree"),
            box(plotOutput("tree"))),

            column(7,
              h4("Map"),
              box(plotOutput("map")))
      )

    )

  )
)


server <- function(input, output) {

  output$tree <- renderPlot({
  #  library(paleotree)


    paleotree::plotPhylopicTreePBDB(tree = HistoryOfEarth::GetTree())
    ape::axisPhylo()
  })

  chosen_period <- reactive({c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary") == input$period})
  output$map <- renderPlot({maplist[[chosen_period()]]})
  #error in [[: attempt to select less than one element in integerOneIndex


}

shinyApp(ui, server)
