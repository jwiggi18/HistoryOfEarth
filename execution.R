# create tree using paleotree's developmental Branch which includes
# fns makePBDBtaxonTree, getTaxaDataPBDB, dateTaxonTreePBDB, and plotPhylopicTree
rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/tree_libraries.R")
source("~/packages/functions.R")




#works
hoeData <-paleotree::getSpecificTaxonTreePBDB("Gorilla,Panthera,Homo,Tyto,Dromaius,Aedes,Solenopsis,Caretta,Crocodylus,Solanum,Prunus,Rosa,Climacograptus,Anomalocaris,Dunkleosteus,Halysites,Histiodella,Agathiceras,Archaeopteryx,Juramaia,Hylonomus,Elginerpeton,Rhyniognatha,Dunkleosteus,Aculeisporites,Canadaspis,Arandaspis,Tyrannosaurus,Velociraptor,Triceratops,Diplodocus,Brachiosaurus,Quetzalcoatlus,Smilodon,Megalonyx,Mammuthus,Meganeura,Eldredgeops,Exaeretodon,Redondasaurus,araucarioxylon", show="app")

hoeTree <- makePBDBtaxonTree(hoeData, "genus")

#make hoeTree ambidextrous
ahoeTree <- amb(hoeTree)

#add phylopics
phyloTree <- plotPhylopicTreePBDB(ahoeTree, hoeData$image_no)

#date the tree
datehoeTree <- dateTaxonTreePBDB(ahoeTree, hoeData)



plot(datehoeTree, direction = "upwards")




#if date first then try to add phylopics nothing gets added
phyloTree <- plotPhylopicTreePBDB(datehoeTree, hoeData$image_no)

#try dating the phylopic tree
datehoeTree <- dateTaxonTreePBDB(phyloTree, hoeData)

#visually inspect
layout(matrix(1:2 ,1 ,2))
for (i in 1:1) plot(hoeTree)
for (i in 1:1) plot(amb(hoeTree))




rm(list=ls())

source("~/GitHubRepos/HistoryOfEarth/map_libraries.R")
source("~/packages/functions.R")

## Plotting maps
#Cambrian	543-490
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


Ordivician_amp <- plot_gplates(mya = 443, polyoutline = "black", polyfill = "black", coastoutline = "#d8d8d6", coastfill = "white", mapoutline = "white", mapbackground = "black")

#The default model reconstruction only goes back 200 Ma
# GOLONKA goes back to 550 Ma & PALEOMAP to 750 but only has Coastlines available as a layer (no Topological Plate Polygons)
