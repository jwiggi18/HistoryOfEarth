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

#' Cache everything
#'
#' Just to save typing, run all the caching functions
#'
#' @param taxa vector of taxa
#' @param age_df output of GetAgeDF()
#' @export
CacheEverything <- function(taxa=GetTaxa(), age_df=GetAgeDF()) {
  CacheSpecimenAges(taxa)
  CacheMaps(age_df)
  CacheTree(taxa)
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
  Period <- c("Cambrian","Ordivician","Silurian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretaceous","Paleogene","Neogene","Quaternary")

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
  return(c("Attercopus", "Gorilla", "Panthera", "Homo", "Tyto", "Dromaius", "Aedes", "Solenopsis", "Caretta", "Crocodylus", "Solanum", "Prunus", "Rosa", "Climacograptus", "Anomalocaris", "Dunkleosteus", "Halysites", "Histiodella", "Agathiceras", "Archaeopteryx", "Juramaia", "Hylonomus", "Elginerpeton", "Rhyniognatha", "Aculeisporites", "Canadaspis", "Arandaspis", "Tyrannosaurus", "Velociraptor", "Triceratops", "Diplodocus", "Brachiosaurus", "Quetzalcoatlus", "Smilodon", "Megalonyx", "Mammuthus", "Meganeura", "Eldredgeops", "Exaeretodon", "Redondasaurus", "araucarioxylon", "Pelagiella", "Burgessia", "Naraoia", "Hallucigenia", "Pikaia", "Cameroceras", "Acanthostega", "Tulerpeton", "Pareiasaurus", "Dimetrodon", "Estemmenosuchus", "Cartorhynchus", "Ichthyosaurus", "Gracilisuchus", "Nyasasaurus", "Eoraptor", "Herrerasaurus", "Eudimorphodon", "Plesiosaurus", "Lesothosaurus", "Stegosaurus", "Kulindadromeus", "Megazostrodon", "Anchiornis", "Allosaurus", "Giraffatitan", "Teleoceras", "Diceratherium", "Nimravus", "Enhydrocyon", "Sahelanthropus"))
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
  map + ggplot2::geom_point(data = df, ggplot2::aes(x=pbdb_data.paleolng, y=pbdb_data.paleolat, colour=Color))
}

#' Get tree
#'
#' @param taxa Vector of taxon names
#' @param rank What taxonomic rank to use
#' @return a phylo object
#' @export
GetTree <- function(taxa = GetTaxa(), rank="genus") {
  if(length(taxa)>1) {
    taxa <- unique(taxa)
    taxa <- paste(taxa, collapse=",")
  }
  data <-paleotree::getSpecificTaxaPBDB(taxa)

  tree <- paleotree::makePBDBtaxonTree(data, rank = rank)
#plotPhylopicTreePBDB(tree = tree)
  timeTree <- paleotree::dateTaxonTreePBDB(tree)
  return(timeTree)
}

# #' filter data frame produced by latong_age() exclude pbdb_data.early_interval & pbdb_data.late_interval (due to large # of NAs) and then remaining NAs (taxa with no lat long)
# #'@return filtered data frame
# #'@export
# FilterDF <- function() {
#   # Produce a dataframe with paleo lat and long for taxa list
#   latlong_df <- latlong_age() #produces list with many empty or NA pbdb_data.late_interval entries
#
#   #filtering out pbdb_data.early_interval & pbdb_data.late_interval so that na.omit won't remove taxa
#   latlong_df <- subset(latlong_df, select = c(pbdb_data.paleolng:pbdb_data.min_ma, searched_taxon, Period))
#
#   #get rid of taxa with no fossils
#   latlong_df <- na.omit(latlong_df)
#
#   return(latlong_df)
# }

#recreates the maplist everytime the package loads - where should this go?
# maplist <- CreateMapList()

#' Put points on a map
#' @param taxa vector of taxon names, default is GetTaxa()
#' @param specimen_df data.frame of specimen info
#' @param age_df dataframe with Period, MinMa, MaxMa, MidMa, default is GetAgeDF()
#' @param maps paleomaps list of maps (stored internally)
#' @return a list of period maps with points for fossils from that period
#' @export
PutPointsOnMap <- function(taxa = GetTaxa(), specimen_df=specimens, age_df=GetAgeDF(), maps=paleomaps) {
  specimen_df <- specimen_df[specimen_df$searched_taxon %in% taxa,] # subset of the taxa we want
  points_maplist <- maps
  for (map_index in seq_along(points_maplist)) {
    chosen_period <- names(points_maplist)[map_index]
    local_df <- specimen_df[specimen_df$Period==chosen_period,]
    local_df$Color <- age_df$Color[age_df$Period==chosen_period]
    if (nrow(local_df)>0) {
      points_maplist[[map_index]] <- add_points(map=points_maplist[[map_index]], df=local_df)
    }
    points_maplist[[map_index]] <- points_maplist[[map_index]] + ggplot2::theme(legend.position="none")
  }
  return(points_maplist)
}
