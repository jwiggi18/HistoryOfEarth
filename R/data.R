#' Saved tree
#'
#' A phylo object containing a chronogram made with the help of Dave Bapst's paleotree and Paleobiology Database
#'
#' @docType data
#' @keywords datasets
#' @name chronogram
#' @usage data(chronogram)
#' @format A phylo object
NULL

#' Paleomaps
#'
#' Cached maps using LunaSare/gplatesr and gplates, based on example code from https://github.com/GPlates/gplates_web_service_doc by Michael Chin. It uses the https://www.gplates.org/ for its source.
#'
#' @docType data
#' @keywords datasets
#' @name paleomaps
#' @usage data(paleomaps)
#' @format A list of map objects
NULL

#' Specimens
#'
#' Fossil locations from PBDB
#'
#' @docType data
#' @keywords datasets
#' @name specimens
#' @usage data(specimens)
#' @format A data.frame with info on specimens
NULL

#' Taxon Images
#'
#' Images of taxa from phylopic
#'
#' @docType data
#' @keywords datasets
#' @name taxonimages
#' @usage data(taxonimages)
#' @format A list of images with names equal to the taxon names
NULL

#' Animated maps
#'
#' List containing animated maps. It's a list of lists: overall list has "all", "none", and then individual taxa. Then, each of those has "all", and then individual periods. For example, you could get the maps for Stegosaurus in the Jurassic by doing animatedmaps[["Stegosaurus"]][["Jurassic"]]
#'
#' Each element in that list of lists is itself a list, the output of AnimatePlot()
#'
#' @docType data
#' @keywords datasets
#' @name animatedmaps
#' @usage data(animatedmaps)
#' @format A list of lists of outputs of AnimatePlots
NULL
