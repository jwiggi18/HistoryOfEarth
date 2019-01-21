#creating maps

#Cambrian	 <- paste(as.character(c(490,543)), collapse=",")
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

#Produce points for plotting

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","araucarioxylon")

# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age(taxa)

#subset data frame by period
Cambrian_latlong <- subset_ma(latlong_df, 490, 543)
Ordivician_latlong <- subset_ma(latlong_df, 443, 490)
Sularian_latlong <- subset_ma(latlong_df, 417, 443)
Devonian_latlong <- subset_ma(latlong_df, 354, 417)
Carboniferous_latlong <- subset_ma(latlong_df, 290, 354)
Permian_latlong <- subset_ma(latlong_df, 248, 290)
Triassic_latlong <- subset_ma(latlong_df, 206, 248)
Jurassic_latlong <- subset_ma(latlong_df, 144, 206)
Cretacous_latlong <- subset_ma(latlong_df, 65, 144)
Paleogene_latlong <- subset_ma(latlong_df, 33.7, 65)
Neogene_latlong <- subset_ma(latlong_df, 1.8, 33.7)


# Produce maps for each period
Cambrian_map <- black_white(513)
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

#add points to map
Cambrian_map + geom_point(data = Cambrian_latlong, aes(x=pbdb_data.paleolng, y=pbdb_data.paleolat, color = "#A6CEE3"))
