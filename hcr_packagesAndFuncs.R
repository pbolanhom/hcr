
#The grind.R script
#In this analysis, we use the grind.R script developed by Rob J de Boer at the Utrecht University (https://tbb.bio.uu.nl/rdb/grindR.html).
#The grind.R script has a repertoire of functions to the analysis of models composed by ordinary differential equations (ODE's).
#The grind.R script can be found at https://tbb.bio.uu.nl/rdb/grindR/
#After the script is saved on the folder containing the workspace of the R code, the grind.R script can be loaded with the code:
##Packages "deSolve", "rootSolve" and "FME" are automatic loaded (as long as installed) when sourcing the grind.R script.
#install.packages(deSolve)
#install.packages(rootSolve)
#install.packages(FME)
source("grind.R") #where the name "grind" varies according with the name saved by the user.

#Packages
#install.packages(igraph)
#install.packages(DescTools)
#install.packages(vegan)
library(igraph)
library(DescTools)
library(vegan)

#Defining the color gradient for the plots
colfunc1=colorRampPalette(c("blue","skyblue"))
colfunc2=colorRampPalette(c("firebrick","tomato"))

