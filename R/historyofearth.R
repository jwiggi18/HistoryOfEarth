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
  chronogram <- NULL
  try(chronogram <- GetTree(taxa=taxa))
  if(!is.null(chronogram)) {
    usethis::use_data(chronogram, overwrite=TRUE)
  }
}

#' Cache phylotree tree information
#'
#' @param taxa vector of taxa
#' @export
cache_pictree <- function(tree=GetTree()) {
  pictree <- NULL
  try(pictree <- ctree(tree = tree))
  if(!is.null(pictree)) {
    usethis::use_data(pictree, overwrite=TRUE)
  }
}


#' Cache map information
#'
#' @param age_df output of GetAgeDF()
#' @export
CacheMaps <- function(age_df=GetAgeDF()) {
  paleomaps <- NULL
  try(paleomaps <- CreateMapList(age_df))
  if(!is.null(paleomaps)) {
    usethis::use_data(paleomaps,   overwrite=TRUE)
  }
}

#' Create map information all ages
#'
#' It used to store inside the package, but it's too huge
#'
#' @param base_url What URL to use for gplates
#' @return paleomaps_allages
#' @export
CacheMapsAllAges <- function(base_url='http://gws.gplates.org/') {
  paleomaps_allages <- NULL
  try(paleomaps_allages <- CreateMapListAllTimes(base_url=base_url))
#  if(!is.null(paleomaps_allages)) {
#    usethis::use_data(paleomaps_allages,   overwrite=TRUE)
#  }
  return(paleomaps_allages)
}

#' Cache specimen information
#'
#' Localities of fossils from PBDB
#
#' @param taxa vector of taxa
#' @export
CacheSpecimenAges <- function(taxa=GetTaxa()) {
  specimens <- NULL
  try(specimens <- latlong_age(taxa))
  if(!is.null(specimens)) {
    usethis::use_data(specimens,   overwrite=TRUE)
  }
}

#' Cache taxon images from phylopic
#'
#' @param tree Tree from dateTaxonTreePBDB
#' @param cacheDir Where to store trees
#' @export
CacheTaxonImagesFromPhylopic <- function(tree=chronogram, cacheDir = "/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img") {
  paleotree::cachePhyloPicPNG(tree=tree, cacheDir=cacheDir)
  print("Remember to add these using git")
}

#' Cache taxon information
#'
#' Mainly to store images. If an image isn't found for a taxon, an image of DNA is used instead
#'
#' @param taxa Vector of taxon names
#' @export
CacheTaxonImagesByName <- function(taxa=GetTaxa()) {
  taxonimages <- list()
  for (taxon.index in seq_along(taxa)) {
    taxon_id <- NULL
    found_image <- NULL
    try(taxon_id <- rphylopic::name_search(taxa[taxon.index]))
    if(class(taxon_id)=="data.frame") {
      if(nrow(taxon_id)>0) {
        picture_data <- rphylopic::name_images(taxon_id[1,1])
        if(length(picture_data$same)>0) {
          found_image <- rphylopic::image_data(picture_data$same[[1]]$uid,size=128)[[1]]
        } else {
          if(length(picture_data$subtaxa)>0) {
            found_image <- rphylopic::image_data(picture_data$subtaxa[[1]]$uid,size=128)[[1]]
          } else {
            if(length(picture_data$supertaxa)>0) {
              found_image <- rphylopic::image_data(picture_data$supertaxa[[1]]$uid,size=128)[[1]]
            }
          }
        }
      }
    }
    if(!is.null(found_image)) {
      rphylopic::save_png(found_image, paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/taxon_", gsub(" ", "_", taxa[taxon.index]),".png"))
    #  taxonimages[[taxon.index]] <- found_image
    #  print(paste0("Cached image for ", taxa[taxon.index]))
  #  } else {
  #    taxonimages[[taxon.index]] <- rphylopic::image_data("5d646d5a-b2dd-49cd-b450-4132827ef25e",size=128)[[1]] #just DNA
  #    print(paste0("Cached just a placeholder (DNA) for ", taxa[taxon.index]))
    }
  }
  #names(taxonimages) <- taxa
  #usethis::use_data(taxonimages, overwrite=TRUE)
}

#' Cache animated maps
#'
#' Create an array of animated maps, where the rows are taxa and the columns periods. "all" and "none" are possible taxa, and "all" is a possible period
#' @inheritParams AnimatePlot
#' @param MaximumAnimate If TRUE, animate everything. If false, static maps for  taxa
#' @export
CacheAnimatedMaps <- function(start_time=NULL, stop_time=NULL, periods=NULL, taxa=GetTaxa(), step_size=1, age_df=GetAgeDF(), specimen_df=specimens, interval=0.5, use_cached_maps_only=TRUE, use_phylopics=FALSE, point_color="red", gif_name=NULL, single_frame=FALSE, MaximumAnimate=FALSE) {
  paleomaps_allages <-  CreateMapListAllTimes()
  all_taxa <- c("All", "None", taxa)
  all_periods <- c("All", age_df$Period)
  all_periods_liststub <- vector("list",length(all_periods))
  names(all_periods_liststub) <- all_periods
  animatedmaps <- vector("list", length(all_taxa))
  for (i in sequence(length(animatedmaps))) {
    animatedmaps[[i]] <- all_periods_liststub
  }
  names(animatedmaps) <- all_taxa
  #animatedmaps <- array(list(), c(2+length(taxa), 1+nrow(age_df)))

  #first do all taxa, all periods
  animatedmaps[["All"]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa, gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_All_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)


  #second do no taxa, all periods
  animatedmaps[["None"]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=NULL, gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_None_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)

  # Now for backup copy this everywhere in case they don't generate elsewhere
  for (period_index in sequence(length(age_df$Period))) {
      system(paste0("cp /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_none_All.gif /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_All_", age_df$Period[period_index], ".gif"))
      system(paste0("cp /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_none_All.gif /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_None_", age_df$Period[period_index], ".gif"))
      for (taxon_index in seq_along(taxa)) {
        system(paste0("cp /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_None_All.gif /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_", taxa[taxon_index], "_", age_df$Period[period_index], ".gif"))
        system(paste0("cp /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_None_All.gif /Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_", taxa[taxon_index], "_All.gif"))
      }
  }


  # now loop over periods, all taxa
  for (period_index in sequence(length(age_df$Period))) {
    print(paste("Making map for all taxa, ", age_df$Period[period_index]))
    animatedmaps[["All"]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa, periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_All_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)
  }


  # now loop over periods, all taxa
  for (period_index in sequence(length(age_df$Period))) {
    print(paste("Making map for no taxa, ", age_df$Period[period_index]))
    animatedmaps[["None"]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=NULL, periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_All_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)
  }

  #third do single taxa, all periods

  for (taxon_index in seq_along(taxa)) {
    print(paste("Making map for taxon ",  taxa[taxon_index], " all periods"))
    animatedmaps[[taxon_index+2]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa[taxon_index], periods=NULL, gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_",taxa[taxon_index], "_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=!MaximumAnimate)
    # now loop over periods, all taxa
    for (period_index in sequence(length(age_df$Period)-1)) {
      print(paste("Making map for taxon ",  taxa[taxon_index], ", period ", age_df$Period[period_index]))
      animatedmaps[[taxon_index+2]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa[taxon_index], periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_",taxa[taxon_index], "_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=!MaximumAnimate)
    }
  }


  if(!is.null(animatedmaps)) {
    for (t_index in seq_along(all_taxa)) {
      for (p_index in seq_along(all_periods)) {
        #try(magick::image_write(animatedmaps[[all_taxa[t_index]]][[all_periods[p_index]]], gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_",all_taxa[t_index], "_", all_periods[p_index], ".gif"))))
      }
    }
    #usethis::use_data(animatedmaps, overwrite=TRUE)
  }
}

#' Cache trees for website
#'
#' @param taxa Vector of taxa
#' @param age_df From GetAgeDF
#' @param height Height in pixels
#' @param width Width in pixels
#' @export
CacheIndividualTrees <- function(taxa=GetTaxa(), age_df=GetAgeDF(), height=800, width=900) {
  CacheTaxonImagesFromPhylopic()
  taxa <- c("All", taxa)
  periods <- c("All", age_df$Period)
  for (taxon_index in seq_along(taxa)) {
    for (period_index in seq_along(periods)) {
      jpeg(filename=paste0('/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/tree_', taxa[taxon_index], "_", periods[period_index],".jpg"), width=width, height=height)
      lastPP <- HistoryOfEarth::get_pictree(focalTaxon=taxa[taxon_index], cacheDir="/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/")
      HistoryOfEarth::AddAxis(lastPP, focalPeriod=periods[period_index])
      dev.off()
    }
  }
}

#' Cache everything
#'
#' Just to save typing, run all the caching functions
#' For all but the images, this won't overwrite if the new object is empty
#'
#' @param taxa vector of taxa
#' @param age_df output of GetAgeDF()
#' @export
CacheEverything <- function(taxa=GetTaxa(), age_df=GetAgeDF()) {
  CacheSpecimenAges(taxa)
#  CacheMaps(age_df)
  CacheTree(taxa)
  #CacheTaxonImages(taxa)
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

  MinMa <- c(485,444,419,359,299,252,201,145,66,23,2.58, 0)

  MaxMa <- c(540,485,444,419,359,299,252,201,145,66,23,2.58)

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
  data(taxa_links, package="HistoryOfEarth")
  return(taxa_links$Genus[which(taxa_links$Genus != "All")])
#  return(c("Acanthostega", "Aedes", "Amphicyon", "Anchiornis", "Anomalocaris", "Arandaspis", "Archaeopteryx", "Attercopus", "Cameroceras", "Camponotus", "Caretta", "Cartorhynchus", "Climacograptus", "Crocodylus", "Dimetrodon", "Diplodocus", "Dunkleosteus", "Eldredgeops",  "Estemmenosuchus", "Eudimorphodon", "Giraffatitan", "Gorilla", "Homo", "Hylonomus", "Ichthyosaurus", "Juramaia", "Mammuthus", "Megalonyx", "Meganeura", "Megazostrodon", "Nimravus", "Pelagiella", "Phidippus", "Plesiosaurus", "Purussaurus", "Quetzalcoatlus", "Redondasaurus", "Rhyniognatha", "Sahelanthropus", "Smilodon", "Stegosaurus", "Teleoceras", "Triceratops", "Tusoteuthis", "Tyrannosaurus", "Tyto"))
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


#' Get a preselected image
#'
#' @param taxon Taxon to get
#' @return URL of the image
#' @export
GetPreselectedThumbnail <- function(taxon) {
  if(taxon=="All") {
    return("https://upload.wikimedia.org/wikipedia/commons/c/c1/La_Brea_Tar_Pits.jpg")
  }
  data(taxa_links, package="HistoryOfEarth")
  for (genus_index in seq_along(taxa_links$Genus)) {
      if(taxon==taxa_links$Genus[genus_index]) {
        return(taxa_links$pic_url[genus_index])
      }
  }
#  url <- paste0('https://en.wikipedia.org/w/api.php?action=query&titles=', utils::URLencode(taxon), '&prop=pageimages&format=json&pithumbsize=', size)
#  json <- jsonlite::fromJSON(url)
#  return(json$query$pages[[1]]$thumbnail$source)
}

#' Get an image from Wikipedia
#'
#' @param taxon Taxon to get
#' @param size Size of larger dimension
#' @return URL of the image
#' @export
GetWikipediaThumbnail <- function(taxon, size=200) {
  if(taxon=="All") {
    return("https://upload.wikimedia.org/wikipedia/commons/c/c1/La_Brea_Tar_Pits.jpg")
  }
  url <- paste0('https://en.wikipedia.org/w/api.php?action=query&titles=', utils::URLencode(taxon), '&prop=pageimages&format=json&pithumbsize=', size)
  json <- jsonlite::fromJSON(url)
  return(json$query$pages[[1]]$thumbnail$source)
}

#' get link for each genus
#'
#' @param taxon which genus to return a url for
#' @return URL
#' @export
get_genuslink <- function(taxon) {
  if(taxon=="All") {
    return("http://www.onezoom.org/life.html/@cellular_organisms=93302#x-16,y872,w1.6839")
  }
  for (genus_index in seq_along(taxa_links$Genus)) {
      if(taxon==taxa_links$Genus[genus_index]) {
        return(taxa_links$Link[genus_index])
      }
    }
  #taxa_links <- read.csv(system.file("extdata", "taxa_links.csv", package = "HistoryOfEarth", mustWork = TRUE))
  #usethis::use_data(taxa_links, overwrite = TRUE)
}


#' get link for each period
#'
#' @param period which period to return a url for
#' @return URL
#' @export
get_periodlink <- function(period) {
  if(period=="All") {
    return("https://youtu.be/rWp5ZpJAIAE")
  }
  for (per_index in seq_along(period_links$Period)) {
      if(period==period_links$Period[per_index]) {
        return(period_links$Link[per_index])
      }
    }
    #period_links <- read.csv("~/period_links.csv")
    #usethis::use_data(period_links)
}

#' Make a list of maps for all periods
#'
#' By default, uses, gws.gplates.org
#'
#' However, you can do gplatesr::launch_docker() if you are on a machine with docker running, then use base_url="http://localhost:8888/" to use an instance running locally
#'
#' @param age_df Output of GetAgeDF()
#' @param base_url What URL to use for gplates
#' @return list of maps, with names for periods
#' @export
CreateMapList <- function(age_df=GetAgeDF(), base_url='http://gws.gplates.org/') {
  #create map list
  maplist <- lapply(age_df$MidMa, gplatesr::land_sea(base_url=base_url))

  #name maplist according to period
  names(maplist) <- age_df$Period

  return(maplist)
}

#' Make a list of maps for all times
#'
#' By default, uses, gws.gplates.org
#'
#' However, you can do gplatesr::launch_docker() if you are on a machine with docker running, then use base_url="http://localhost:8888/" to use an instance running locally
#'
#' @param start_age How many MYA to start making the map
#' @param stop_age How many MYA to stop making the map
#' @param step_size How many MY between maps
#' @param age_df Output of GetAgeDF()
#' @param base_url What URL to use for gplates
#' @return list of maps, with names for periods
#' @export
CreateMapListAllTimes <- function(start_age=0, stop_age=540, step_size=10, age_df=GetAgeDF(), base_url='http://gws.gplates.org/') {
  #create map list
  ages <- sort(unique(c(seq(from=start_age, to=stop_age, by=step_size), age_df$MinMa, age_df$MaxMa, age_df$MidMa, 0, 1, 2, 3, 4, 5)))
  ages <- ages[ages<=540] # Cannot reconstruct that long ago
  maplist <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    maplist[[i]] <- gplatesr::land_sea(mya=ages[i], base_url=base_url)
  }

  #name maplist according to age
  names(maplist) <- ages

  return(maplist)
}

#' Fixes error in adding phylopics to maps
#'
#' Get this error:
#' Error: annotation_custom only works with Cartesian coordinates
#' Using rphylopic::add_phylopic
#' This follows the advice of https://github.com/oswaldosantos/ggsn/issues/19
#' And just uses ggmap::inset instead; otherwise, it's identical to rphylopic::add_phylopic
add_phylopic_to_map <- function(img, alpha = 0.2, x = NULL, y = NULL, ysize = NULL,
    color = NULL)
{
    mat <- recolor_phylopic_for_map(img, alpha, color)
    if (!is.null(x) && !is.null(y) && !is.null(ysize)) {
        aspratio <- nrow(mat)/ncol(mat)
        ymin <- y - ysize/2
        ymax <- y + ysize/2
        xmin <- x - ysize/aspratio/2
        xmax <- x + ysize/aspratio/2
    }
    else {
        ymin <- -Inf
        ymax <- Inf
        xmin <- -Inf
        xmax <- Inf
    }
    imgGrob <- grid::rasterGrob(mat)
    return(ggmap::inset(xmin = xmin, ymin = ymin, xmax = xmax,
        ymax = ymax, imgGrob))
}

#' Fixes error in adding phylopics to maps
#'
#' Just copying recolor_phylopic_for_map for now from the rphylopic package
recolor_phylopic_for_map <- function (img, alpha = 0.2, color = NULL)
{
    if (is.null(color)) {
        mat <- matrix(rgb(img[, , 1], img[, , 2], img[, , 3],
            img[, , 4] * alpha), nrow = dim(img)[1])
    }
    else {
        cols <- grDevices::col2rgb(color)
        imglen <- length(img[, , 1])
        mat <- matrix(ifelse(img[, , 4] > 0, rgb(rep(cols[1,
            1], imglen), rep(cols[2, 1], imglen), rep(cols[3,
            1], imglen), img[, , 4] * 255 * alpha, maxColorValue = 255),
            rgb(rep(1, imglen), rep(1, imglen), rep(1, imglen),
                img[, , 4] * alpha)), nrow = dim(img)[1])
    }
    return(mat)
}

#' Create an animated gif of a map
#'
#' This can work with or without taxa.
#'
#' The age range to plot can be set by the periods, the taxa, or fixed ages (which by default go from 0 to 540 MY). If taxa are specified, it uses the times those are found. If periods are specified, it uses the start and stop of those periods. If both periods and taxa are specified, it defaults to using the periods.
#'
#' a <- ot(use_cached_maps_only=TRUE, step_size=1, taxa=GetTaxa())
#'
#' @param start_time The time of the first frame of the animation
#' @param stop_time The time of the last frame of the animation before it starts looping back
#' @param periods A vector of period names, capitalized properly (can be left blank)
#' @param taxa A vector of taxon names (can be left blank)
#' @param step_size How many million years to take in a single step
#' @param age_df Data.frame of ages, typically from GetAgeDF()
#' @param specimen_df Cached fossil localities and times
#' @param interval How many seconds per frame
#' @param use_cached_maps_only If TRUE, only uses the maps already in the package, rather than pulling from gplates
#' @param use_phylopics If TRUE, use phylopic images; otherwise, use dots
#' @param point_color If just plotting points, what color
#' @param gif_name Path to gif, including its name
#' @param paleomaps_allages Cache of maps
#' @param single_frame If TRUE, just does a single frame. It picks the best time.
#' @export
AnimatePlot <- function(start_time=NULL, stop_time=NULL, periods=NULL, taxa=NULL, step_size=1, age_df=GetAgeDF(), specimen_df=specimens, interval=0.5, use_cached_maps_only=FALSE, use_phylopics=FALSE, point_color="red", gif_name=NULL, paleomaps_allages = NULL, single_frame=FALSE) {
  plotlist <- list()
  if(!is.null(periods)) {
    if("All" %in% periods) {
      periods <- NULL # since it was probably passed in accidentally
    }
  }
  if(is.null(paleomaps_allages)) {
    paleomaps_allages <-CreateMapListAllTimes(start_age=ifelse(is.null(start_time),0,start_time), stop_age=ifelse(is.null(stop_time),0,stop_time), step_size=step_size)
  }
  paleomap_info <- as.numeric(gsub("Ma", "", gsub("Time = ", "", unlist(lapply(lapply(paleomaps_allages, "[[", "labels"), "[[", "title")))))
  #names(paleomap_info) <- names(paleomaps)
  if(!is.null(taxa)) {
    specimen_df <- specimen_df[specimen_df$searched_taxon %in% taxa,] # subset of the taxa we want
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.paleolng),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.paleolat),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.max_ma),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.min_ma),]
    if(nrow(specimen_df)>0) {
      possible_start_time <- min(specimen_df$pbdb_data.min_ma, na.rm=TRUE)
      possible_stop_time <- max(specimen_df$pbdb_data.max_ma, na.rm=TRUE)
      #if(possible_stop_time - possible_start_time > 40) { #otherwise, not enough intervals
      #  stop_time <- possible_stop_time
      #  start_time <- possible_start_time
      #}
    }
  }
  if(!is.null(periods)) {
    relevant_periods <- age_df[age_df$Period %in% periods,]
    start_time <- min(relevant_periods$MinMa, na.rm=TRUE)
    stop_time <- max(relevant_periods$MaxMa, na.rm=TRUE)
  }
  if(is.null(start_time)) {
    start_time <- 0
  }
  if(is.null(stop_time)) {
    stop_time <- 540
  }
  if(single_frame) {
    start_time <- 0
    stop_time <- 0
    specimen_df_local <- specimen_df
    if(!is.null(periods)) {
      relevant_period <- which(age_df$Period %in% periods)
      start_time <- min(age_df$MidMa[relevant_period], na.rm=TRUE)
      stop_time <- max(age_df$MidMa[relevant_period], na.rm=TRUE)
      if(nrow(specimen_df_local)>0) {
        specimen_df_local <- specimen_df_local[specimen_df_local$pbdb_data.max_ma<=age_df$MaxMa[relevant_period] & specimen_df_local$pbdb_data.min_ma>=age_df$MinMa[relevant_period],]
      }
    }
    if(nrow(specimen_df_local)>0) {

      median_start <- median(specimen_df_local$pbdb_data.min_ma, na.rm=TRUE)
      median_stop <- median(specimen_df_local$pbdb_data.max_ma, na.rm=TRUE)
      start_time <- median(c(0, rep(median(c(median_start, median_stop), na.rm=TRUE), 2)), na.rm=TRUE) # so that if there are no dates, it uses 0, otherwise, it uses the dates
      start_time <- round(start_time,0)
      stop_time <- start_time
      use_cached_maps_only <- FALSE #so we get plot of the right time
    }
  }
  ages<-seq(from=start_time, to=stop_time, by=step_size)
  for (i in seq_along(ages)) {
    period_color <- NULL
    period_color <- age_df[ ages[i]>=age_df$MinMa & ages[i]<age_df$MaxMa,]$Color
    if(!is.null(period_color)) {
      if(length(period_color)>0) {
        point_color <- plotrix::color.id(period_color)
        #point_color <- period_color
    #  print(point_color)
      }
    }
    my_plot <- NULL
    if(!use_cached_maps_only) {
      try(my_plot <- gplatesr::land_sea(ages[i]))
    }
    if(is.null(my_plot)) { #as a backup, go to the cache
      matching_map_index <- which(paleomap_info==ages[i])
      if(length(matching_map_index)>0) {
        my_plot <- paleomaps_allages[[matching_map_index]]
      }
    }
    if(!is.null(my_plot)) {
      if(!is.null(taxa)) {
        for(taxon_index in seq_along(taxa)) {
          img <- NULL
          if(use_phylopics) {
            try(img <- taxonimages[taxa[taxon_index]][[1]])
            if(is.null(img)) {
              img <- rphylopic::image_data("5d646d5a-b2dd-49cd-b450-4132827ef25e",size=128)[[1]]
            }
          }
          taxon_df <- specimen_df[specimen_df$searched_taxon==taxa[taxon_index],]
          taxon_df <- taxon_df[taxon_df$pbdb_data.max_ma>ages[i],]
          taxon_df <- taxon_df[taxon_df$pbdb_data.min_ma<ages[i],]
          if(use_phylopics) {

            for (taxon_to_add in sequence(nrow(taxon_df))) {
            #  my_plot <-  my_plot + rphylopic::add_phylopic(img, 1, taxon_df$pbdb_data.paleolng[taxon_to_add], taxon_df$pbdb_data.paleolat[taxon_to_add] , ysize = 0.2)
                my_plot <-  my_plot + add_phylopic_to_map(img, 1, taxon_df$pbdb_data.paleolng[taxon_to_add], taxon_df$pbdb_data.paleolat[taxon_to_add] , ysize = 0.2)
            }
          } else {
            if(nrow(taxon_df)>0) {
              taxon_df$Color <- point_color
              my_plot <- add_points(my_plot, taxon_df)
              my_plot <- my_plot + ggplot2::theme(legend.position="none")
            }
            if(ages[i]==0) {
              gbif_points <- GetGBIFPoints(taxa[taxon_index])
              if(nrow(gbif_points)>0) {
                gbif_points$Color <- '#B15928'
                my_plot <- add_points(my_plot, gbif_points)
                my_plot <- my_plot + ggplot2::theme(legend.position="none")
              }
            }
          }
        }
      }
      plotlist[[length(plotlist)+1]] <- my_plot
    }
  }
  if(length(plotlist)>0) {
    animation::ani.options(interval = interval, loop=TRUE, ani.height=315, ani.width=650, interval=1, autobrowse = FALSE)

    movie.name <- tempfile(pattern="animation", fileext="gif")
    if(!is.null(gif_name)) {
      movie.name <- gif_name
    }
    animation::saveGIF({
      for (i in seq_along(plotlist)) {
        anim <- plotlist[[i]]
        plot(anim)
      }
      if(length(plotlist)>1) {
        for (i in (length(plotlist)-1):1) {
          anim <- plotlist[[i]]
          plot(anim)
        }
      }
    }, movie.name=movie.name)
  #  return(list(gif=movie.name, plots=plotlist))
#  } else {
  #  return(list(gif=NA, plots=NA))
  }
}

#' function to add pbdb paleo data points (lat and long) to gplatesr created maps
#'@param map a map created using gplatesr plot_gplates or black_white functions
#'@param df a data frame containing pbdb_data.paleolng and pbdb_data.paleolat
#'@param ptcolor a character vector indicating the desired color of added data points
#'@return a map with points
#'@export
add_points <- function(map, df) {
#  map + ggplot2::geom_point(data = df, ggplot2::aes(x=pbdb_data.paleolng, y=pbdb_data.paleolat, colour=df$Color))
map + ggplot2::geom_point(data = df, colour=df$Color, size=2, ggplot2::aes(x=pbdb_data.paleolng, y=pbdb_data.paleolat))
}

#' function to getPhyloPicIDNum axis to a tree plot
#'
#' It can highlight chosen periods
#'
#' @param lastPP The output of plotting functions
#' @param focalPeriod The period to highlight, using names form GetAgeDF()
#' @param age_df Return from GetAgeDF(), which has colors
#' @return modifies current plot
#' @export
AddAxis <- function(lastPP = get("last_plot.phylo", envir = .PlotPhyloEnv), focalPeriod=NULL, age_df=GetAgeDF()) {
#  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  focal_index <- 0
  if(!is.null(focalPeriod)) {
    if(focalPeriod != "All") {
        focal_index <- which(age_df$Period==focalPeriod)
    }
  }
  for (i in sequence(nrow(age_df))) {
      if(i%%2==0 & i!=focal_index) {
          rect(xleft=max(lastPP$xx)-age_df$MaxMa[i], ybottom=min(lastPP$yy), xright=max(lastPP$xx)-age_df$MinMa[i], ytop=max(lastPP$yy), col=rgb(0,0,0,.2), border=NA)
      }
      if(focal_index>0) {
          rect(xleft=max(lastPP$xx)-age_df$MaxMa[focal_index], ybottom=min(lastPP$yy), xright=max(lastPP$xx)-age_df$MinMa[focal_index], ytop=max(lastPP$yy), col=grDevices::adjustcolor(age_df$Color[focal_index],alpha.f=0.1), border=NA)
      }
      abline(v=max(lastPP$xx)-age_df$MaxMa[i], col=rgb(0,0,0,.1))
      abline(v=max(lastPP$xx)-age_df$MinMa[i], col=rgb(0,0,0,.1))
  }
  boundaries <- sort(unique(c(age_df$MaxMa, age_df$MinMa)))
  boundary_positions <- max(lastPP$xx) - boundaries

  axis(side=1, at=boundary_positions, labels=round(boundaries), lwd=0, lwd.ticks=1, cex.axis=0.7, col.axis="darkgray", col.ticks="darkgray")

  positions <- sequence(nrow(age_df))
  axis(side=3, at=max(lastPP$xx) - age_df$MidMa[positions%%2==0], labels=age_df$Period[positions%%2==0], lwd=0, cex.axis=0.7, padj=0, col.axis="black")
  axis(side=3, at=max(lastPP$xx) - age_df$MidMa[positions%%2==1], labels=age_df$Period[positions%%2==1], lwd=0, cex.axis=0.7, padj=1, col.axis="black")
  if(focal_index>0) {
     axis(side=3, at=max(lastPP$xx) - age_df$MidMa[focal_index], labels=age_df$Period[focal_index], col.axis=age_df$Color[focal_index], cex.axis=0.7, lwd=0, padj=ifelse(focal_index%%2==0,0,1))
  }
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

#  tree <- amb(tree)
#plotPhylopicTreePBDB(tree = tree)
  timeTree <- paleotree::dateTaxonTreePBDB(tree)
  return(timeTree)
}

#' Get phylopic tree
#'
#' If no tree is provided, will use the cached tree
#'
#' @param tree phylo object
#' @param cacheDir where to load images from if they exist
#' @param ... additional arguments to pass
#' @return a plotted tree with phylopics
#' @export
get_pictree <- function(tree = NULL, cacheDir='./www/', ...) {
  if(is.null(tree)) {
    data(chronogram)
    tree <- chronogram
  }
  paleotree::plotPhylopicTreePBDB(tree = tree, cacheDir=cacheDir, ...)
}

#' flips the direction of the tree at each node
#'
#' @param phy an object of class phylo
#' @return a phylo object
#'@export
amb <- function (phy) #ambidextrous tree
{
    foo <- function(node, END, where) {
        right <-  (node%%2==0) # new line
        start <- which(phy$edge[, 1] == node)
        end <- c(start[-1] - 1, END)
        size <- end - start + 1
        desc <- phy$edge[start, 2]
        Nclade <- length(desc)
        n <- N[desc]
        o <- order(n, decreasing = right)
        newpos <- c(0, cumsum(size[o][-Nclade])) + where
        desc <- desc[o]
        end <- end[o]
        start <- start[o]
        neworder[newpos] <<- start
        for (i in 1:Nclade) if (desc[i] > nb.tip)
            foo(desc[i], end[i], newpos[i] + 1)
    }
    phy <- reorder(phy)
    nb.tip <- length(phy$tip.label)
    nb.node <- phy$Nnode
    nb.edge <- dim(phy$edge)[1]
    tmp <- reorder(phy, "postorder")
    N <- .C(node_depth, as.integer(nb.tip), as.integer(tmp$edge[,
                                                                1]), as.integer(tmp$edge[, 2]), as.integer(nb.edge),
            double(nb.tip + nb.node), 1L)[[5]]
    neworder <- integer(nb.edge)
    foo(nb.tip + 1, nb.edge, 1)
    phy$edge <- phy$edge[neworder, ]
    if (!is.null(phy$edge.length))
        phy$edge.length <- phy$edge.length[neworder]
    phy
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

#' Get locations for modern species using GBIF
#'
#' Note that to make this work with the add_points function, needs odd output column names
#'
#' @param taxa vector of taxon names, default is GetTaxa()
#' @param the rank to search at for each taxon
#' @param fossils If TRUE, include locations of fossils
#' @return data frame with pbdb_data.paleolng and pbdb_data.paleolat columns reflecting points from gbif
#' @export
GetGBIFPoints <- function(taxa = GetTaxa(), rank="genus", fossils=FALSE) {
  location_points <- data.frame()
  for (taxon_index in seq_along(taxa)) {
    key <- rgbif::name_suggest(q=taxa[taxon_index], rank=rank)$key[1]
    location_points <- plyr::rbind.fill(location_points, rgbif::occ_search(taxonKey=key, hasCoordinate=TRUE)$data)
  }
  if(!fossils) {
    location_points <- location_points[location_points$basisOfRecord != "FOSSIL_SPECIMEN",]
  }
  final_points <- data.frame(pbdb_data.paleolng=location_points$decimalLongitude, pbdb_data.paleolat=location_points$decimalLatitude)
  return(final_points)
}
