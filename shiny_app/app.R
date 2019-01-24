#shiny app for History of Earth project

library(shiny)
library(shinydashboard)
library(devtools)
devtools::install_github("LunaSare/gplatesr")
library(gplatesr)
library(ggplot2)
library(ggthemes)
library(sp)
devtools::install_github("dwbapst/paleotree", ref="developmentBranch")
library(paleotree)

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


## Map plotting
age_df <- data.frame(Period, MinMa, MaxMa, MidMa) #have to create this df because latlong_age pulls from the GetLatLong fn, which calls the paleobiodb API that doesn't use MidMa

#create map list
maplist <- lapply(age_df$MidMa, black_white)

#name maplist according to period
names(maplist) <- age_df$Period

##Point plotting
# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age(taxa) #produces list with many empty or NA pbdb_data.late_interval entries

#filtering out pbdb_data.early_interval & pbdb_data.late_interval so that complete_cases wont remove taxa
latlong_df <- subset(latlong_df, select = c(pbdb_data.paleolng:pbdb_data.min_ma, searched_taxon))

#Subsetting data frame to get pbdb_data.paleolng and pbdb_data.paleolat for each period and putting them into a list
period_list <- list() #create empty list

  for (period_index in seq_along(Period)) {
    period.result <- subset_latlongdf(minage=MinMa[period_index], maxage=MaxMa[period_index]) #subset by each min and max age
    period.result$minage=MinMa[period_index]
    period.result$maxage=MaxMa[period_index]
    period_list[[period_index]] <- period.result
    names(period_list)[length(period_list)] <- Period[period_index]
  }

  #add points to map
  # Colorblind friendly pallete produced by RColorBrewer
  # Cambrian "#A6CEE3"
  # Ordivician "#1F78B4"
  # Sularian "#B2DF8A"
  # Devonian "#33A02C"
  # Carboniferous "#FB9A99"
  # Permian "#E31A1C"
  # Triassic "#FDBF6F"
  # Jurassic "#FF7F00"
  # Cretacous "#CAB2D6"
  # Paleogene "#6A3D9A"
  # Neogene "#FFFF99"
  # Quaternary "#B15928"

point_colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")

#maps are in maplist[["period_name"]], points are in period_list[["period_name"]], colors = point_colors vector

#create a list of maps with points for each period
#not working
points_list <- list() #create empty list

    for (map_index in seq_along(maplist)) {
      for (points_index in seq_along(period_list)){
        for (color_index in seq_along(point_colors)){
      points.result <- add_points(map=Period[map_index], df=Period[points_index], ptcolor=color_index)
      points_list[[points_index]] <- points.result
      names(points_list)[length(points_list)] <- Period[points_index]
      }
    }
  }




ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title= "History of Earth"),
    dashboardSidebar(width = 175,
      sidebarMenu(
        menuItem(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      #  id = "tabs",
        selectInput("genus", "Choose a genus:",
             choices = taxa,
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
  #error in [[: attempt to select less than one element in integerOneIndex


}

shinyApp(ui, server)
