#' Add a Geological Time Scale to a Plot of a Phylogenetic Tree
#'
#' Plots the geologic periods of the Phanerozoic on a plot of a
#' phylogenetic tree; principally developed to be used with function \code{plotPhyloPicTree}.
#'

#' @param lastPP The plotting environment for the last \code{phylo} object plotted.

#' @param focalPeriod A geological \emph{period} to highlight, using names
#' from \code{geologicalPeriodsDataFrame}. 

#' @param geologicalPeriodsDataFrame A dataframe that includes the names,
#' times and color scheme for a set of geological periods. By default,
#' this is obtained from the internal function \code{getGeologicalPeriodsDataFrame}.

#' @return Nothing is returned.

#' @export



plotGeologicalPeriodsAxis <- function(
		lastPP = get("last_plot.phylo", envir = .PlotPhyloEnv), 
		focalPeriod = NULL, 
		age_df = getGeologicalPeriodsDataFrame(selection = "forOnline")
		) {
	###########################
	#	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	focal_index <- 0
	if(!is.null(focalPeriod)) {
		if(focalPeriod != "All") {
			focal_index <- which(age_df$Period == focalPeriod)
			}
		}
	
	if(is.data.frame(geologicalPeriodsDataFrame)){
		age_df <- geologicalPeriodsDataFrame
	}else{
		stop("geologicalPeriodsDataFrame is not a data.frame")
		}
		
	for (i in sequence(nrow(age_df))) {
		if(i%%2 == 0 & i != focal_index) {
			rect(xleft=max(lastPP$xx)-age_df$MaxMa[i], 
				ybottom=min(lastPP$yy),
				xright=max(lastPP$xx)-age_df$MinMa[i], 
				ytop=max(lastPP$yy),
				col=rgb(0,0,0,.2), 
				border=NA)
			}
		if(focal_index > 0) {
			rect(xleft=max(lastPP$xx)-age_df$MaxMa[focal_index], 
				ybottom=min(lastPP$yy), 
				xright=max(lastPP$xx)-age_df$MinMa[focal_index], 
				ytop=max(lastPP$yy),
				col=grDevices::adjustcolor(age_df$Color[focal_index],alpha.f=0.1),
				border=NA)
			}
		abline(
			v=max(lastPP$xx) - age_df$MaxMa[i],
			col=rgb(0,0,0,.1))
		abline(
			v=max(lastPP$xx) - age_df$MinMa[i],
			col=rgb(0,0,0,.1))
		}
		
	boundaries <- sort(unique(c(age_df$MaxMa, age_df$MinMa)))
	boundary_positions <- max(lastPP$xx) - boundaries

	axis(side=1, at=boundary_positions, 
		labels=round(boundaries),
		lwd=0, lwd.ticks=1, cex.axis=0.7, 
		col.axis="darkgray", col.ticks="darkgray")

	positions <- sequence(nrow(age_df))
	axis(side=3, 
		at=max(lastPP$xx) - age_df$MidMa[positions%%2==0],
		labels=age_df$Period[positions%%2==0], lwd=0, 
		cex.axis=0.7, padj=0, col.axis="black")
	axis(side=3, 
		at=max(lastPP$xx) - age_df$MidMa[positions%%2==1],
		labels=age_df$Period[positions%%2==1], lwd=0, 
		cex.axis=0.7, padj=1, col.axis="black")
	if(focal_index>0) {
		 axis(side=3, 
			at=max(lastPP$xx) - age_df$MidMa[focal_index],
			labels=age_df$Period[focal_index], 
			col.axis=age_df$Color[focal_index], 
			cex.axis=0.7, lwd=0, 
			padj=ifelse(focal_index%%2==0,0,1))
		}
	}

# Get boundaries and names of geological periods
#
# @return a data.frame of Periods, min, max, and mid points
# @export
getGeologicalPeriodsDataFrame <- function(
		selection = "forOnline"
		) {
	########
	
	if(selection == "forOnline"){
	
		Period <- c("Cambrian","Ordivician","Silurian","Devonian",
			"Carboniferous","Permian","Triassic","Jurassic",
			"Cretaceous","Paleogene","Neogene","Quaternary")

		MinMa <- c(485,444,419,359,299,252,201,145,66,23,2.58, 0)

		MaxMa <- c(540,485,444,419,359,299,252,201,145,66,23,2.58)

		MidMa <- c(513,464,431,389,328,278,226,173,105,44,12, 1)

		Color <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C",
			"#FB9A99","#E31A1C","#FDBF6F","#FF7F00",
			"#CAB2D6","#6A3D9A","#FFFF99","#B15928"
			)

		## for map plotting:
			# have to create this df because latlong_age pulls from GetLatLong()
			# which calls the paleobiodb API that doesn't use MidMa
		
		age_df <- data.frame(
			Period, MinMa, MaxMa, MidMa,
			Color, stringsAsFactors=FALSE
			) 
		}
		
	return(age_df)
	}