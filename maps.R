#creating maps

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

setwd("~/packages/gplatesr/R")
source("plot_gplates.R")


age <- function(mya) {
  age_plot <- plot_gplates(mya = mya, polyoutline = "black", polyfill = "black", coastoutline = "#d8d8d6", coastfill = "white", plateoutline = "red", platefill = "black")
  return(age_plot)
}

#notworking
Cambrian_map <- plot_gplates(mya = 490, polyoutline = "black", polyfill = "black", coastoutline = "#d8d8d6", coastfill = "white", plateoutline = "red", platefill = "black")
Ordivician_map <- age(443)
Sularian_map	<- age(417
Devonian_map	<- age(354)
Carboniferous_map	<- age(290)
Permian_map	<- age(248)
Triassic_map	<- age(206)
Jurassic_map	<- age(144)
Cretacous_map	<- age(65)
Paleogene_map	<- age(33.7)
Neo_map	<- age(1.8)

plot_gplates(mya = 140, polyoutline = "black", polyfill = "black", coastoutline = "#d8d8d6", coastfill = "white", plateoutline = "red", platefill = "black")
