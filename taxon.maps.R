############# Create maps for each taxa ###################

rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/map_libraries.R")
source("~/Functions/functions.R")

#Produce points for plotting

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","araucarioxylon")

# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age(taxa)

#Subset dataframe by taxon
subset_taxon <- function(df, taxon){
  df <- df[complete.cases(df),]
  taxon_latlong <- df[which(df$taxon == taxon), ]
  return(taxon_latlong)
}

Anomalocaris_df <- subset_taxon(latlong_df, "Anomalocaris")
