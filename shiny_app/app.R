#shiny app for History of Earth project

library(shiny)
library(shinydashboard)
library(devtools)
devtools::install_github("LunaSare/gplatesr")
library(gplatesr)
library(ggplot2)
library(ggthemes)
library(sp)

GetLatLongAnytime <- function(taxon){
  pbdb_data <- read.csv(paste0("http://paleobiodb.org/",
    "data1.2/occs/list.txt?base_name=",taxon,"&level=3&show=paleoloc"),
    stringsAsFactors = FALSE)
  lat_long <- data.frame(pbdb_data$accepted_name, pbdb_data$paleolng, pbdb_data$paleolat, pbdb_data$max_ma, pbdb_data$min_ma, pbdb_data$early_interval, pbdb_data$late_interval)
  lat_long$searched_taxon <- taxon
  return(lat_long)
}

Period <- c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary")

MinMa <- c(485,444,419,359,299,252,201,145,65,23,2.58, 0)

MaxMa <- c(541,485,444,419,359,299,252,201,145,65,23,2.58)

MidMa <- c(513,464,431,389,328,278,226,173,105,44,12, 1)


latlong_age <- function(taxa){
  #store pbdb_data.accepted_name, pbdb_data.lng, pbdb_data.lat, taxon, pbdb_data.early_interval, pbdb_data.late_interval, pbdb_data.max_ma, pbdb_data.min_ma
  lat_long_df <- data.frame()

  for (taxon_index in seq_along(taxa)) {
            latlong.result <- GetLatLongAnytime(taxa[taxon_index])
            latlong.result$taxon=taxa[taxon_index]
            lat_long_df<- rbind(lat_long_df, latlong.result)
        }
  return(lat_long_df)
}

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","Araucarioxylon")

Period <- c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary")

age_df <- data.frame(Period, MinMa, MaxMa, MidMa) #have to create this df because latlong_age pulls from the GetLatLong fn, which calls the paleobiodb API that doesn't use MidMa

#create map list
maplist <- lapply(age_df$MidMa, black_white)

#name maplist according to period
names(maplist) <- age_df$Period


ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title= "History of Earth"),
    dashboardSidebar(width = 175,
      sidebarMenu(
        menuItem(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      #  id = "tabs",
        selectInput("genus", "Choose a genus:",
             choices = c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","Araucarioxylon"),
             multiple = TRUE)),

          menuItem(
        selectInput("period", "Choose a period:",
             choices = Period,
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
    library(paleotree)

    data <-getSpecificTaxaPBDB(paste0(
      "Gorilla,Panthera,Homo,Tyto,Dromaius,Aedes,Solenopsis,",
      "Caretta,Crocodylus,Solanum,Prunus,Rosa,",
      "Climacograptus,Anomalocaris,Dunkleosteus,",
      "Halysites,Histiodella,Agathiceras,Archaeopteryx,",
      "Juramaia,Hylonomus,Elginerpeton,Rhyniognatha,",
      "Dunkleosteus,Aculeisporites,Canadaspis,Arandaspis,",
      "Tyrannosaurus,Velociraptor,Triceratops,Diplodocus,",
      "Brachiosaurus,Quetzalcoatlus,Smilodon,Megalonyx,Mammuthus,",
      "Meganeura,Eldredgeops,Exaeretodon,Redondasaurus,Araucarioxylon"))
    tree <- makePBDBtaxonTree(data, rank = "genus")
    #plotPhylopicTreePBDB(tree = tree)
    timeTree <- dateTaxonTreePBDB(tree)
    plotPhylopicTreePBDB(tree = timeTree)
    axisPhylo()
  })

  chosen_period <- reactive({c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary") == input$period})
  output$map <- renderPlot({maplist[[chosen_period()]]})


}

shinyApp(ui, server)
