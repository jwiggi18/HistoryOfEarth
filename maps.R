#creating maps

Cambrian	 <- paste(as.character(c(490,543)), collapse=",")
#Ordivician	490-443
#Sularian	443-417
#Devonian	417-354
#Carboniferous	354-290
#Permian	290-248
#Triassic	248-206
#Jurassic	206-144
#Cretacous	144-65
#Paleogene	65-33.7
#Neo	33.7-1.8

rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/map_libraries.R")
source("~/packages/functions.R")

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","araucarioxylon")

taxa5 <- c("Gorilla","Panthera","Homo","Tyto","Dromaius")

taxa6 <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes")

taxa17 <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella")


Period <- c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo")

MinMa <- c(490,443,417,354,290,248,206,144,65,33.7,1.8)

MaxMa <- c(543,490,443,417,354,290,248,206,144,65,33.7)

geoage <- data.frame(Period, MinMa, MaxMa)

#GetLatLong(taxon,minage,maxage)
#want to loop over taxon list inserting each min and max and saving as Period_taxonname

for (taxon_index in seq_along(taxa)) {
    for (period_index in seq_along(Period)) {
        storage_thingie <- GetLatLong(taxa[taxon_index], minage=MinMa[period_index], maxage=MaxMa[period_index])
    }
}

for (taxon_index in seq_along(taxa5)) {
    for (period_index in seq_along(Period)) {
        storage_thingie <- GetLatLong(taxa[taxon_index], minage=MinMa[period_index], maxage=MaxMa[period_index])
    }
}

for (taxon_index in seq_along(taxa6)) {
    for (period_index in seq_along(Period)) {
        storage_thingie <- GetLatLong(taxa[taxon_index], minage=MinMa[period_index], maxage=MaxMa[period_index])
    }
}

#try storing in a data frame
lat_long_df <- data.frame()

for (taxon_index in seq_along(taxa)) {
    for (period_index in seq_along(Period)) {
        lat_long_df<- GetLatLong(taxa[taxon_index], minage=MinMa[period_index], maxage=MaxMa[period_index])
    }
}






rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/map_libraries.R")
source("~/packages/functions.R")

# working
Cambrian_map <- black_white(490)
Ordivician_map <- black_white(443)
Sularian_map	<- black_white(417)
Devonian_map	<- black_white(354)
Carboniferous_map	<- black_white(290)
Permian_map	<- black_white(248)
Triassic_map	<- black_white(206)
Jurassic_map	<- black_white(144)
Cretacous_map	<- black_white(65)
Paleogene_map	<- black_white(33.7)
Neo_map	<- black_white(1.8)

#The default model reconstruction only goes back 200 Ma
# GOLONKA goes back to 550 Ma & PALEOMAP to 750 but only has Coastlines available as a layer (no Topological Plate Polygons)

#How to add points of relevant fossils
#Create an object for each age
Cambrian	 <- paste(as.character(c(490,543)), collapse=",")
#Get latitude and longitude from PBDB for taxa of interest
GetLatLong("tyrannosaurus", 490, 543)
