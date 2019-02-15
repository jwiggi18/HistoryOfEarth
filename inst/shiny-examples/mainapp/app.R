# ui <- fluidPage(
#   shinydashboard::dashboardPage(
#     shinydashboard::dashboardHeader(title= "History of Earth"),
#     shinydashboard::dashboardSidebar(width = 175,
#       shinydashboard::sidebarMenu(
#         shinydashboard::menuItem(
#       # Setting id makes input$tabs give the tabName of currently-selected tab
#       #  id = "tabs",
#         selectInput("genus", "Choose an organism:",
#              choices = c("all", HistoryOfEarth::GetTaxa()),
#              multiple = FALSE, selected="all")),
#
#           shinydashboard::menuItem(
#         selectInput("period", "Choose a period:",
#              choices = c("all", HistoryOfEarth::GetAgeDF()$Period),
#              multiple = FALSE, selected="all")))),
#
#     shinydashboard::dashboardBody(
#       tags$head(tags$style(HTML('
#                                 /* body */
#                                 .content-wrapper, .right-side {
#                                   background-color: #000000;
#                                   color: #ffffff
#                                 }
#                                 '))),
#
#         fluidRow(
#
#           column(5,
#             h4("Tree"),
#             plotOutput("tree")),
#
#             column(7,
#               textOutput("period_name"),
#             #  h4("Map"),
#               plotOutput("map"),
#               h4("Picture"),
#               uiOutput("img"))
#       )
#
#     )
#
#   )
# )

ui <- fluidPage(theme = shinythemes::shinytheme("cyborg"),
    fluidRow(
        column(4, align="center",
            actionButton("paleozoic_button", "The Paleozoic Era",
                          onclick = "window.open('https://youtu.be/RDQa0okkpf0', '_blank')")
        ),
        column(4, align="center",
            actionButton("mesozoic_button", "The Mesozoic Era",
                          onclick = "window.open('https://youtu.be/ZoHO3fAj_78', '_blank')")
        ),
        column(4, align="center",
            actionButton("cenozoic_button", "The Cenozoic Era",
                          onclick = "window.open('https://youtu.be/2ofNufZVcMU', '_blank')")
        )
      ),
    fluidRow(
        column(6, align="center",
            selectInput("genus", "Choose an organism:",
                choices = c("all", HistoryOfEarth::GetTaxa()),
                multiple = FALSE, selected="all")
        ),
        column(6, align="center",
            selectInput("period", "Choose a period:",
                choices = c("all", HistoryOfEarth::GetAgeDF()$Period),
                multiple = FALSE, selected="all")
        )
    ),
    fluidRow(
        column(6, align="center",
               uiOutput("img"),
               uiOutput("taxon_link"),
               tags$style("#taxon_link{font-size: 25px;}")
        ),
        column(6, align="center",
               textOutput("period_name"),
               plotOutput("map"),
               uiOutput("period_link"),
               tags$style("#period_link{font-size: 25px;}")
        )
    ),
    fluidRow(plotOutput("tree"))
)




server <- function(input, output) {
  library(HistoryOfEarth)
  output$tree <- renderPlot({
  #  library(paleotree)

    data(chronogram, package="HistoryOfEarth")
    #ape::plot.phylo(chronogram)
    #ape::axisPhylo()


    #to plot phylopic tree
    lastPP <- HistoryOfEarth::get_pictree(focalTaxon=input$genus)
    HistoryOfEarth::AddAxis(lastPP, focalPeriod=input$period)
  }, height=1000)

  #data(paleomaps, package="HistoryOfEarth")

#  chosen_period <- reactive({c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary") == input$period})
#chosen_period <- reactive({c(HistoryOfEarth::GetAgeDF()$Period) == input$period})
chosen_period <- reactive({input$period})
  output$period_name <- renderText(paste0("Organism: ", {input$genus}, ", Period: ",{input$period}))
  #output$map <- renderPlot({paleomaps[[chosen_period()]]})
  #error in [[: attempt to select less than one element in integerOneIndex
  #output$map <- renderPlot({paleomaps[["Cambrian"]]})

#  output$map <- renderPlot({paleomaps[[chosen_period()]]})
  output$map <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www/',
                              paste('map_',input$genus,'_', input$period, '.gif', sep='')))

    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)

  output$img <- renderUI({
      tags$img(src=HistoryOfEarth::GetWikipediaThumbnail(input$genus), width='50%')
     # list(src="https://upload.wikimedia.org/wikipedia/commons/c/c1/La_Brea_Tar_Pits.jpg")
  })

  output$taxon_link <- renderUI({
    tags$a(href=HistoryOfEarth::get_genuslink(input$genus), paste0("Learn about ", {input$genus}))
  })

  output$period_link <- renderUI({
    tags$a(href=HistoryOfEarth::get_periodlink(input$period), paste0("Learn about the ", {input$period}), "period")
  })


}

shinyApp(ui, server)
