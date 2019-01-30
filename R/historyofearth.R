#' Run the interactive website
#'
#' Uses example from https://deanattali.com/2015/04/21/r-package-shiny-app/
#'
#' In R, just do HistoryOfEarth::runSite()
#' @export
runSite <- function() {
  appDir <- system.file("shiny-examples", "mainapp", package = "HistoryOfEarth")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `HistoryOfEarth`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' Cache tree information
#'
#' @param taxa vector of taxa
#' @export
CacheTree <- function(taxa=GetTaxa()) {
  chronogram <- GetTree(taxa=taxa)
  usethis::use_data(chronogram, overwrite=TRUE)
}

#' Cache map information
#'
#' @param age_df output of GetAgeDF()
#' @export
CacheMaps <- function(age_df=GetAgeDF()) {
  paleomaps <- CreateMapList(age_df)
  usethis::use_data(paleomaps,   overwrite=TRUE)
}

#' Cache specimen information
#'
#' Localities of fossils from PBDB
#
#' @param taxa vector of taxa
#' @export
CacheSpecimenAges <- function(taxa=GetTaxa()) {
  specimens <- latlong_age(taxa)
  usethis::use_data(specimens,   overwrite=TRUE)
}

#' Get information on specimens from pbdb
#'
#' @param taxon a string with a taxon name
#' @return a data.frame with information on this taxon
#' @export
GetLatLongAnytime <- function(taxon){
  pbdb_data <- read.csv(paste0("http://paleobiodb.org/",
    "data1.2/occs/list.txt?base_name=",utils::URLencode(taxon),"&level=3&show=paleoloc"),
    stringsAsFactors = FALSE)
  lat_long <- data.frame(pbdb_data$accepted_name, pbdb_data$paleolng, pbdb_data$paleolat, pbdb_data$max_ma, pbdb_data$min_ma, pbdb_data$early_interval, pbdb_data$late_interval)
  lat_long$searched_taxon <- taxon
  return(lat_long)
}


#' Get boundaries and names of geological periods
#'
#' @return a data.frame of Periods, min, max, and mid points
#' @export
GetAgeDF <- function() {
  Period <- c("Cambrian","Ordivician","Silurian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary")

  MinMa <- c(485,444,419,359,299,252,201,145,65,23,2.58, 0)

  MaxMa <- c(541,485,444,419,359,299,252,201,145,65,23,2.58)

  MidMa <- c(513,464,431,389,328,278,226,173,105,44,12, 1)

  Color <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")

  ## Map plotting
  age_df <- data.frame(Period, MinMa, MaxMa, MidMa, Color, stringsAsFactors=FALSE) #have to create this df because latlong_age pulls from the GetLatLong fn, which calls the paleobiodb API that doesn't use MidMa
  return(age_df)
}

#' Return vector of taxa
#'
#' @return vector of names
#' @export
GetTaxa <- function() {
  return(c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","Araucarioxylon"))
}

#' Get information on specimens from pbdb
#'
#' @param taxa a vector of taxon names
#' @return a data.frame with information on all these taxa
#' @export
latlong_age <- function(taxa=GetTaxa(), age_df=GetAgeDF()){
  #store pbdb_data.accepted_name, pbdb_data.lng, pbdb_data.lat, taxon, pbdb_data.early_interval, pbdb_data.late_interval, pbdb_data.max_ma, pbdb_data.min_ma
  lat_long_df <- data.frame()

  for (taxon_index in seq_along(taxa)) {
            latlong.result <- GetLatLongAnytime(taxa[taxon_index])
            latlong.result$taxon=taxa[taxon_index]
            lat_long_df<- rbind(lat_long_df, latlong.result)
        }
  #create empty column called Period
  lat_long_df$Period <- NA

  for (period_index in seq_along(age_df$Period)){
          lat_long_df$Period[which(lat_long_df$pbdb_data.min_ma>=age_df$MinMa[period_index] & lat_long_df$pbdb_data.max_ma <= age_df$MaxMa[period_index])] <- age_df$Period[period_index]
        }
  return(lat_long_df)
}


#' Make a list of maps for all periods
#'
#' @param age_df Output of GetAgeDF()
#' @return list of maps, with names for periods
#' @export
CreateMapList <- function(age_df=GetAgeDF()) {
  #create map list
  maplist <- lapply(age_df$MidMa, gplatesr::black_white)

  #name maplist according to period
  names(maplist) <- age_df$Period

  return(maplist)
}

#' function to add pbdb paleo data points (lat and long) to gplatesr created maps
#'@param map a map created using gplatesr plot_gplates or black_white functions
#'@param df a data frame containing pbdb_data.paleolng and pbdb_data.paleolat
#'@param ptcolor a character vector indicating the desired color of added data points
#'@return a map with points
#'@export
add_points <- function(map, df) {
  map + ggplot2::geom_point(data = df, ggplot2::aes(x=pbdb_data.paleolng, y=pbdb_data.paleolat))
}

#' Get tree
#'
#' @param taxa Vector of taxon names
#' @param rank What taxonomic rank to use
#' @return a phylo object
#' @export
GetTree <- function(taxa = GetTaxa(), rank="genus") {
  if(length(taxa)>1) {
    taxa <- paste(taxa, collapse=",")
  }
  data <-paleotree::getSpecificTaxaPBDB(taxa)

  tree <- paleotree::makePBDBtaxonTree(data, rank = rank)
#plotPhylopicTreePBDB(tree = tree)
  timeTree <- paleotree::dateTaxonTreePBDB(tree)
  return(timeTree)
}

#' filter data frame produced by latong_age() exclude pbdb_data.early_interval & pbdb_data.late_interval (due to large # of NAs) and then remaining NAs (taxa with no lat long)
#'@return filtered data frame
#'@export
FilterDF <- function() {
# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age() #produces list with many empty or NA pbdb_data.late_interval entries

#filtering out pbdb_data.early_interval & pbdb_data.late_interval so that na.omit wont remove taxa
latlong_df <- subset(latlong_df, select = c(pbdb_data.paleolng:pbdb_data.min_ma, searched_taxon, Period))

#get rid of taxa with no fossils
latlong_df <- na.omit(latlong_df)

return(latlong_df)
}

maplist <- CreateMapList()

#' Plot points
#' @param taxa vector of taxon names, default is GetTaxa()
#' @param age_df dataframe with Period, MinMa, MaxMa, MidMa, default is GetAgeDF()
#' @return a list of period maps with points for fossils from that period
#' @export
PlotPoints <- function(taxa = GetTaxa(), age_df=GetAgeDF(), latlong_df=FilterDF()) {
##Point plotting

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

#maps are in maplist[["period_name"]], points are in latlong_df$Period

points_list <- list() #create empty list

    for (map_index in seq_along(maplist)) {
      for (points_index in seq_along(latlong_df$Period)){
        for (period_index in seq_along(age_df$Period)){
      map <- maplist == age_df$Period[period_index]
      latlong_df_periodname <- latlong_df[latlong_df$Period == age_df$Period[period_index],]
      points.result <- add_points(map=map, df=latlong_df_periodname)
      points_list[[points_index]] <- points.result
      names(points_list)[length(points_list)] <- latlong_df$Period[points_index]
      }
    }
  }

  return(points_list)
}
