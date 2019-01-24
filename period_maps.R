#creating maps

rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/map_libraries.R")
source("~/Functions/functions.R")

#Produce points for plotting

taxa <-c("Gorilla","Panthera","Homo","Tyto","Dromaius","Aedes","Solenopsis","Caretta","Crocodylus","Solanum","Prunus","Rosa","Climacograptus","Anomalocaris","Dunkleosteus","Halysites","Histiodella","Agathiceras","Archaeopteryx","Juramaia","Hylonomus","Elginerpeton","Rhyniognatha","Dunkleosteus","Aculeisporites","Canadaspis","Arandaspis","Tyrannosaurus","Velociraptor","Triceratops","Diplodocus","Brachiosaurus","Quetzalcoatlus","Smilodon","Megalonyx","Mammuthus","Meganeura","Eldredgeops","Exaeretodon","Redondasaurus","araucarioxylon")

Period <- c("Cambrian","Ordivician","Sularian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretacous","Paleogene","Neo","Quaternary")

MinMa <- c(485,444,419,359,299,252,201,145,65,23,2.58, 0)

MaxMa <- c(541,485,444,419,359,299,252,201,145,65,23,2.58)

MidMa <- c(513,464,431,389,328,278,226,173,105,44,12, 1)

geoage <- data.frame(Period, MinMa, MaxMa)

# Produce a dataframe with paleo lat and long for taxa list
latlong_df <- latlong_age(taxa)

################ CREATE MAPS FOR EACH PERIOD WITH ALL FOSSILS FROM THAT PERIOD #################



age_df <- data.frame(Period, MinMa, MaxMa, MidMa) #have to create this df because latlong_age pulls from the GetLatLong fn, which calls the paleobiodb API that doesn't use MidMa

#create map list
maplist <- lapply(age_df$MidMa, black_white)

#name maplist according to period
names(maplist) <- age_df$Period

#To call map
maplist[["Cambrian"]]
maplist[["Ordivician"]]

#subset data frame by period manually (dates are wrong, are correct above in MinMa etc.) Don't do it this way
#Cambrian_latlong <- subset_ma(latlong_df, 485, 541)
#Ordivician_latlong <- subset_ma(latlong_df, 443, 490)
#Sularian_latlong <- subset_ma(latlong_df, 417, 443)
#Devonian_latlong <- subset_ma(latlong_df, 354, 417)
#Carboniferous_latlong <- subset_ma(latlong_df, 290, 354)
#Permian_latlong <- subset_ma(latlong_df, 248, 290)
#Triassic_latlong <- subset_ma(latlong_df, 206, 248)
#Jurassic_latlong <- subset_ma(latlong_df, 144, 206)
#Cretacous_latlong <- subset_ma(latlong_df, 65, 144)
#Paleogene_latlong <- subset_ma(latlong_df, 33.7, 65)
#Neogene_latlong <- subset_ma(latlong_df, 1.8, 33.7)

#Subsetting data frame and putting them into a list by period "automated"
period_list <- list() #create empty list

  for (period_index in seq_along(Period)) {
    period.result <- subset_latlongdf(minage=MinMa[period_index], maxage=MaxMa[period_index]) #subset by each min and max age
    period.result$minage=MinMa[period_index]
    period.result$maxage=MaxMa[period_index]
    period_list[[period_index]] <- period.result
    names(period_list)[length(period_list)] <- Period[period_index]
  }

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

maplist <- lapply(age_df$MidMa, black_white)

#name maplist according to period
names(maplist) <- age_df$Period

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
