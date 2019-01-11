# create tree using paleotree's developmental Branch which includes
# fns makePBDBtaxonTree, getTaxaDataPBDB, dateTaxonTreePBDB, and plotPhylopicTree
rm(list=ls())
#working directory is balalnced_tree to use the balanced function all other functions come from paleotree package
setwd("~/GitHubRepos/balanced_tree")
source("libraries.R")
source("functions.R")

library(devtools)
devtools::install_github("dwbapst/paleotree@develop")
library(paleotree)


#works
hoeData <-paleotree::getSpecificTaxonTreePBDB("Gorilla,Panthera,Homo,Tyto,Dromaius,Aedes,Solenopsis,Caretta,Crocodylus,Solanum,Prunus,Rosa,Climacograptus,Anomalocaris,Dunkleosteus,Halysites,Histiodella,Agathiceras,Archaeopteryx,Juramaia,Hylonomus,Elginerpeton,Rhyniognatha,Dunkleosteus,Aculeisporites,Canadaspis,Arandaspis,Tyrannosaurus,Velociraptor,Triceratops,Diplodocus,Brachiosaurus,Quetzalcoatlus,Smilodon,Megalonyx,Mammuthus,Meganeura,Eldredgeops,Exaeretodon,Redondasaurus,araucarioxylon", show="app")

#checking the ladderized vs. ambidextrous
hoeTree <- makePBDBtaxonTree(hoeData, "genus")
layout(matrix(1:2 ,1 ,2))
for (i in 1:1) plot(hoeTree)
for (i in 1:1) plot(balance(hoeTree))

#add phylopics
phyloTree <- plotPhylopicTreePBDB(hoeTree, hoeData) #not working


bht <- balance(hoeTree)

#date the tree
datehoeTree <- dateTaxonTreePBDB(bht, hoeData)

plot(datehoeTree, direction = "upwards")
