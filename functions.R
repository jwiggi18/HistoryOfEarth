#function to pull points of fossil locations

#min_ma - at least this old
#max_ma - at most this old

#data1.2/occs/geosum.json?base_name=""&min_ma=""&max_ma=""&level=2

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

#  http://paleobiodb.org/data1.2/occs/geosum.json?base_name=Gorilla,Panthera,Tyto,Dromaius,Aedes,Solenopsis,Caretta,Crocodylus,Prunus,Rosa,Climacograptus,Anomalocaris,Dunkleosteus,Halysites,Histiodella,Agathiceras,Archaeopteryx,Juramaia,Hylonomus,Elginerpeton,Rhyniognatha,Dunkleosteus,Hepaticae,Canadaspis,Pikaia,Tyrannosaurus,Velociraptor,Triceratops,Diplodocus,Brachiosaurus,Quetzalcoatlus&min_ma=65&max_ma=248&level=3

#http://paleobiodb.org/data1.2/occs/list.txt?base_name=Cetacea&interval=Miocene&show=coords

#"http://paleobiodb.org/data1.2/occs/list.txt?base_name=Tyrannosaurus&min_ma=65&max_ma=144&level=3&show=coords"


#works
GetLatLong <- function(taxon,minage,maxage){
  pbdb_data <- read.csv(paste0("http://paleobiodb.org/",
    "data1.2/occs/list.txt?base_name=",taxon,
    "&min_ma=",minage,"&max_ma=",maxage,"&level=3&show=coords"),
    stringsAsFactors = FALSE)
  lat_long <- data.frame(pbdb_data$accepted_name, pbdb_data$lng, pbdb_data$lat) #narrow to just name, lat, and long
  return(lat_long)
}



# function to plot map with black background and white landmasses 
black_white <- function(mya) {
  age_plot <- plot_gplates(mya = mya, polyoutline = "black", polyfill = "black", coastoutline = "#d8d8d6", coastfill = "white", mapoutline = "white", mapbackground = "black")
  return(age_plot)
}
