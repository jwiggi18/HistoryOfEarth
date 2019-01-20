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
