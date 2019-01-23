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
source("~/Functions/functions.R")

#Produce points for plotting

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","araucarioxylon")

# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age(taxa)

################ CREATE MAPS FOR EACH PERIOD WITH ALL FOSSILS FROM THAT PERIOD #################

age_df <- data.frame(Period, MinMa, MaxMa, MidMa)

#create map list
maplist <- lapply(age_df$MidMa, black_white)

#name maplist according to period
names(maplist) <- age_df$period




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
Ordivician_map <- black_white(464)
Sularian_map	<- black_white(431)
Devonian_map	<- black_white(389)
Carboniferous_map	<- black_white(328)
Permian_map	<- black_white(278)
Triassic_map	<- black_white(226)
Jurassic_map	<- black_white(173)
Cretacous_map	<- black_white(105)
Paleogene_map	<- black_white(44)
Neo_map	<- black_white(12)

#The default model reconstruction only goes back 200 Ma
# GOLONKA goes back to 550 Ma & PALEOMAP to 750 but only has Coastlines available as a layer (no Topological Plate Polygons). Paleobiodb uses GOLONKA.

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
# Present_day "#B15928"


Cambrian_points <- add_points(Cambrian_map, Cambrian_latlong, "#A6CEE3")
Ordivician_points <- add_points(Ordivician_map, Ordivician_latlong, "#1F78B4")
Sularian_points <- add_points(Sularian_map, Sularian_latlong, "#B2DF8A")
Devonian_points <- add_points(Devonian_map, Devonian_latlong, "#33A02C")
Carboniferous_points <- add_points(Carboniferous_map, Carboniferous_latlong, "#FB9A99")
Permian_points <- add_points(Permian_map, Permian_latlong, "#E31A1C")
Triassic_points <- add_points(Triassic_map, Triassic_latlong, "#FDBF6F")
Jurassic_points <- add_points(Jurassic_map, Jurassic_latlong, "#FF7F00")
Cretacous_points <- add_points(Cretacous_map, Cretacous_latlong, "#CAB2D6")
Paleogene_points <- add_points(Paleogene_map, Paleogene_latlong, "#6A3D9A")
Neogene_points <- add_points(Neogene_map, Neogene_latlong, "#6A3D9A")
