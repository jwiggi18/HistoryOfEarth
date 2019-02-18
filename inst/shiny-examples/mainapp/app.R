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

    titlePanel("History of Earth"),

    fluidRow(class = "Eras",
        column(6, style = "background-color:#FFFF99", align = "center",
            actionButton("paleozoic_button", "The Paleozoic Era", #yellow
                          onclick = "window.open('https://youtu.be/RDQa0okkpf0', '_blank')",
                          style = "color: #000000; background-color:#FFFF99; border-color:#FFFF99")
        ),
        column(3, style = "background-color:#1F78B4", align = "center",
            actionButton("mesozoic_button", "The Mesozoic Era", #blue
                          onclick = "window.open('https://youtu.be/ZoHO3fAj_78', '_blank')",
                          style = "background-color:#1F78B4; border-color:#1F78B4")
        ),
        column(3, style = "background-color:#B2DF8A", align = "center",
            actionButton("cenozoic_button", "The Cenozoic Era", #green
                          onclick = "window.open('https://youtu.be/2ofNufZVcMU', '_blank')",
                          style = "color: #000000; background-color:#B2DF8A; border-color:#B2DF8A")
        )
      ),
      fluidRow(class = "Eras",
          column(6, style = "background-color:#FFFF99", align = "center",
              actionButton("paleozoic_vg", "Viewer Guide", #yellow
                            onclick = "window.open('https://quizlet.com/365224880/test', '_blank')",
                            style = "color: #000000; background-color:#FFFF99; border-color:#FFFF99")
          ),
          column(3, style = "background-color:#1F78B4", align = "center",
              actionButton("mesozoic_vg", "Viewer Guide", #blue
                            onclick = "window.open('https://quizlet.com/365535631/test', '_blank')",
                            style = "background-color:#1F78B4; border-color:#1F78B4")
          ),
          column(3, style = "background-color:#B2DF8A", align = "center",
              actionButton("cenozoic_vg", "Viewer Guide", #green
                            onclick = "window.open('https://quizlet.com/365527235/test', '_blank')",
                            style = "color: #000000; background-color:#B2DF8A; border-color:#B2DF8A")
          )
        ),
    fluidRow(class = "Periods",
        column(1, style = "background-color:#A6CEE3", align = "center",
            actionButton("cambrian_button", "Cambrian",
            onclick = "window.open('https://www.nationalgeographic.com/science/prehistoric-world/cambrian/', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#A6CEE3; border-color:#A6CEE3")
          ),
        column(1, style = "background-color:#1F78B4", align = "center",
            actionButton("ordovician_button", "Ordovician",
            onclick = "window.open('https://youtu.be/yQhlUqLFDxQ', '_blank')",
            style = "padding: 6px 0px; background-color:#1F78B4; border-color:#1F78B4")
          ),
        column(1, style = "background-color:#B2DF8A", align = "center",
            actionButton("silurian_button", "Silurian",
            onclick = "window.open('http://www.ucmp.berkeley.edu/silurian/silurian.php', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#B2DF8A; border-color:#B2DF8A")
          ),
        column(1, style = "background-color:#33A02C", align = "center",
            actionButton("devonian_button", "Devonian",
            onclick = "window.open('https://youtu.be/6Fr8vL9-j2Q', '_blank')",
            style = "padding: 6px 0px; background-color:#33A02C; border-color:#33A02C")
          ),
        column(1, style = "background-color:#FB9A99", align = "center",
            actionButton("carboniferous_button", "Carboniferous",
            onclick = "window.open('http://www.ucmp.berkeley.edu/carboniferous/carboniferous.php', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#FB9A99; border-color:#FB9A99")
          ),
        column(1, style = "background-color:#E31A1C", align = "center",
            actionButton("permian_button", "Permian",
            onclick = "window.open('https://youtu.be/FlEC6tp36nw', '_blank')",
            style = "padding: 6px 0px; background-color:#E31A1C; border-color:#E31A1C")
          ),
        column(1, style = "background-color:#FDBF6F", align = "center",
            actionButton("triassic_button", "Triassic",
            onclick = "window.open('https://youtu.be/moxu_uTemNg', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#FDBF6F; border-color:#FDBF6F")
          ),
        column(1, style = "background-color:#FF7F00", align = "center",
            actionButton("jurassic_button", "Jurassic",
            onclick = "window.open('https://youtu.be/OYUwrA-jZok', '_blank')",
            style = "padding: 6px 0px; background-color:#FF7F00; border-color:#FF7F00")
          ),
        column(1, style = "background-color:#CAB2D6", align = "center",
            actionButton("cretaceous_button", "Cretaceous",
            onclick = "window.open('https://www.nationalgeographic.com/science/prehistoric-world/cretaceous/', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#CAB2D6; border-color:#CAB2D6")
          ),
        column(1, style = "background-color:#6A3D9A", align = "center",
            actionButton("paleogene_button", "Paleogene",
            onclick = "window.open('https://www.nationalgeographic.com/science/prehistoric-world/paleogene/', '_blank')",
            style = "padding: 6px 1px; background-color:#6A3D9A; border-color:#6A3D9A")
          ),
        column(1, style = "background-color:#FFFF99", align = "center",
            actionButton("neogene_button", "Neogene",
            onclick = "window.open('https://www.nationalgeographic.com/science/prehistoric-world/neogene/', '_blank')",
            style = "padding: 6px 0px; color: #000000; background-color:#FFFF99; border-color:#FFFF99")
          ),
        column(1, style = "background-color:#B15928", align = "center",
            actionButton("quaternary_button", "Quaternary",
            onclick = "window.open('https://www.nationalgeographic.com/science/prehistoric-world/quaternary/', '_blank')",
            style = "padding: 6px 0px; background-color:#B15928; border-color:#B15928")
          )
      ),
      br(),
      br(),
    fluidRow(
        column(4, align="center",
            selectInput("genus", "Choose an organism:",
                choices = c("all", HistoryOfEarth::GetTaxa()),
                multiple = FALSE, selected="all")
        ),
        column(8, align="center",
            selectInput("period", "Choose a period:",
                choices = c("all", HistoryOfEarth::GetAgeDF()$Period),
                multiple = FALSE, selected="all")
        )
    ),
    fluidRow(
        column(4, align="center",
               uiOutput("img"),
               uiOutput("taxon_link"),
               tags$style("#taxon_link{font-size: 25px;}")
        ),
        column(8, align="center",
               textOutput("period_name"),
               plotOutput("map")
               #uiOutput("period_link"),
               #tags$style("#period_link{font-size: 25px;}")
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
    list(src = filename, alt="Animated map of plates moving", onerror='./www/map_none_all.gif')
  }, deleteFile = FALSE)

  output$img <- renderUI({
      tags$img(src=HistoryOfEarth::GetWikipediaThumbnail(input$genus), width='75%', alt="Thumbnail from wikipedia")
     # list(src="https://upload.wikimedia.org/wikipedia/commons/c/c1/La_Brea_Tar_Pits.jpg")
  })

  output$taxon_link <- renderUI({
    tags$a(href=HistoryOfEarth::get_genuslink(input$genus), paste0("Learn about ", {input$genus}))
  })

  #output$period_link <- renderUI({
    #tags$a(href=HistoryOfEarth::get_periodlink(input$period), paste0("Learn about the ", {input$period}), "period")
  #})


}

shinyApp(ui, server)
