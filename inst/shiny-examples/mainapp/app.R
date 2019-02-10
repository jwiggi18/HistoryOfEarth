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
             multiple = FALSE)),

          shinydashboard::menuItem(
        selectInput("period", "Choose a period:",
             choices = c("all", HistoryOfEarth::GetAgeDF()$Period),
             multiple = FALSE, selected="all")))),

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
            plotOutput("tree")),

            column(7,
              textOutput("period_name"),
            #  h4("Map"),
              plotOutput("map"))
      )

    )

  )
)


server <- function(input, output) {
  library(HistoryOfEarth)
  output$tree <- renderPlot({
  #  library(paleotree)

    data(chronogram, package="HistoryOfEarth")
    #ape::plot.phylo(chronogram)
    #ape::axisPhylo()


    #to plot phylopic tree
    HistoryOfEarth::get_pictree()
  }, height=1000)

  #data(paleomaps, package="HistoryOfEarth")

#  chosen_period <- reactive({c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary") == input$period})
#chosen_period <- reactive({c(HistoryOfEarth::GetAgeDF()$Period) == input$period})
chosen_period <- reactive({input$period})
  output$period_name <- renderText({input$period})
  #output$map <- renderPlot({paleomaps[[chosen_period()]]})
  #error in [[: attempt to select less than one element in integerOneIndex
  #output$map <- renderPlot({paleomaps[["Cambrian"]]})

#  output$map <- renderPlot({paleomaps[[chosen_period()]]})
  output$map <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/',
                              paste('map_all_', input$period, '.gif', sep='')))

    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)


}

shinyApp(ui, server)
